;;; ess-literate-tests.el --- Literate unit testing  -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))
(require 'ert)


(defvar-local elt-case-title-re nil
  "Regexp matching the commented test case title.
This starts a new test case and defines a title for it. The text
between the case title and the first code chunk is taken as the
starting chunk.")

(defvar-local elt-code-chunk-re nil
  "Regexp matching the start of a commented code chunk.
The elisp code in this comment chunk is run in a buffer
containing the starting chunk.")

(defvar-local elt-code-chunk-next-re nil
  "Regexp matching the start of a commented continuation chunk.
The elisp code in this comment chunk is run in a buffer
containing the output of the precedent chunk.")

(defvar-local elt-section-title-re nil
  "Regexp matching a section title.
These titles don't have any effect except for being printed in
the output.")

(defvar-local elt-init-alist nil
  "Alist of mode settings for test chunks.
This alist is appended to the file-local variables. It should
contain a `mode' entry. To use file-local variables specific to a
test file, add a .el file with the same base name.")

(defun elt--get-local-config ()
  (list elt-section-title-re            ; 0
        elt-case-title-re               ; 1
        elt-code-chunk-re               ; 2
        elt-code-chunk-next-re          ; 3
        elt-init-alist))                ; 4

(defun elt--set-local-config (cfg)
  (setq-local elt-section-title-re (nth 0 cfg))
  (setq-local elt-case-title-re (nth 1 cfg))
  (setq-local elt-code-chunk-re (nth 2 cfg))
  (setq-local elt-code-chunk-next-re (nth 3 cfg))
  (setq-local elt-init-alist (nth 4 cfg)))

(defun elt--code-chunk-re ()
  (concat
   "\\("
   elt-code-chunk-re
   "\\)\\|\\("
   elt-code-chunk-next-re
   "\\)"))

(defmacro elt-deftest (name args file)
  `(progn
     ;; Record current ELT config in the property list of NAME
     (define-symbol-prop ',name 'elt--test-config ',(elt--get-local-config))
     (ert-deftest ,name ()
       (let ((inhibit-message ess-inhibit-message-in-tests)
             (path (expand-file-name ,file "literate")))
         (elt-do 'test ',name path)))))

(defmacro with-elt-r (&rest body)
  (declare (indent 0)
           (debug (&rest form)))
  `(let ((elt-section-title-re "^##### \\([^\n]*\\)$")
         (elt-case-title-re "^###[ \t]*\\([0-9]+[a-zA-Z]*\\) \\([^\n]*\\)$")
         (elt-code-chunk-re "^##!")
         (elt-code-chunk-next-re "^##>")
         (elt-init-alist '((mode . R))))
     ,@body))

(defmacro elt-r-deftest (name args file)
  `(with-elt-r
     (elt-deftest ,name ,args ,file)))

(defun elt-do (action name file)
  (unless (memq action '(test regenerate))
    (error "Invalid literate test action"))
  (let ((verb (if (eq action 'test)
                  "Testing"
                "Regenerating")))
    (message "---%s %s" verb (file-name-nondirectory file)))
  (let* ((src-buffer (if (file-exists-p file)
                         (find-file-noselect file)
                       (error "Can't find literate test file")))
         (src-string (with-current-buffer src-buffer
                       (buffer-string)))
         (config (or (get name 'elt--test-config)
                     (elt--get-local-config)))
         (output (elt-buffer-string file src-string config)))
    (pcase action
      (`test (should (string= src-string output)))
      (`regenerate (with-current-buffer src-buffer
                     (erase-buffer)
                     (insert output)
                     (save-buffer))))))

(defun elt--activate-font-lock-keywords ()
  "Activate font-lock keywords for some of ELT's symbols."
  (font-lock-add-keywords
   nil
   '(("(\\(\\<elt-.*deftest\\)\\>\\s *\\(\\(?:\\sw\\|\\s_\\)+\\)?"
      (1 font-lock-keyword-face nil t)
      (2 font-lock-function-name-face nil t)))))
(add-hook 'emacs-lisp-mode-hook #'elt--activate-font-lock-keywords)

;; Need to make -pattern buffer-local-variables
(defun elt-buffer-string (file src-string config)
  (let ((el-file (concat (file-name-sans-extension file) ".el")))
    (when (file-exists-p el-file)
      (load-file el-file)))
  (with-temp-buffer
    (insert src-string)
    ;; Don't check safety of local variables declared in test files
    (cl-letf (((symbol-function 'safe-local-variable-p) (lambda (sym val) t)))
      (let ((enable-dir-local-variables nil))
        (hack-local-variables)
        (elt--set-local-config config)
        (unless (and elt-case-title-re
                     elt-code-chunk-re
                     elt-code-chunk-next-re
                     elt-section-title-re
                     elt-init-alist)
          (error "ELT configuration must be set"))))
    (elt-this-buffer)
    (buffer-string)))

(defun elt-this-buffer ()
  (goto-char 1)
  (let ((undo-inhibit-record-point t))
    (undo-boundary)
    ;; Print first section header
    (elt-print-section-header)
    (when (elt-search-chunk nil t)
      (while (looking-at elt-case-title-re)
        (elt-print-chunk-id)
        (elt-process-next-chunk)))
    (skip-chars-backward "\n")
    (let ((point-max (or (elt-local-variables-pos)
                         (point-max))))
      (delete-region (1+ (point)) point-max)
      (insert "\n"))
    (when (looking-at (concat "\n" comment-start "+ +Local Variables:"))
      (insert "\n"))
    (undo-boundary)))

(defun elt-local-variables-pos ()
  (save-excursion
    (let ((pattern (concat "^" comment-start "+ +Local Variables:")))
      (when (re-search-forward pattern nil t)
        (match-beginning 0)))))

(defun elt-print-section-header ()
  (save-excursion
    (skip-chars-forward " \n\t")
    (when (looking-at elt-section-title-re)
      (message (match-string-no-properties 1)))))

(defun elt-print-chunk-id ()
  (let ((number (concat "#" (match-string-no-properties 1)))
        (msg (match-string-no-properties 2)))
    (setq msg (substring msg 0 (string-match "-+$" msg)))
    (message (if (> (length msg) 0)
                 (concat number " - " msg)
               number))))

(defun elt-search-chunk (&optional n skip-section)
  (let* ((next-chunk (save-excursion
                       (cond ((re-search-forward elt-case-title-re
                                                 nil t (or n 1))
                              (match-beginning 0))
                             ((elt-local-variables-pos))
                             (t
                              (point-max)))))
         (next-section (save-excursion
                         (when (re-search-forward elt-section-title-re
                                                  next-chunk t)
                           (match-beginning 0)))))
    (goto-char (if (and (not skip-section) next-section)
                   next-section
                 next-chunk))))

(defun elt-process-next-chunk ()
  (let* ((chunk-beg (point))
         (chunk-end (progn
                      (forward-line)
                      (save-excursion
                        (elt-search-chunk)
                        (point-marker))))
         (orig-chunk (buffer-substring chunk-beg chunk-end)))
    (condition-case cnd
        (let* ((test-case (progn
                            (skip-chars-forward " \t\n")
                            (elt-process-case chunk-end)))
               (test-case-state test-case))
          (while (looking-at (elt--code-chunk-re))
            (setq test-case-state (elt-process-next-subchunk chunk-end test-case-state test-case)))
          (insert "\n")
          (elt-print-section-header)
          (when (looking-at elt-section-title-re)
            (insert "\n")
            (elt-search-chunk nil t)))
      (ert-test-skipped
        (message (concat "  Skipping test: " (cadr cnd)))
        (goto-char chunk-beg)
        (delete-region chunk-beg chunk-end)
        (insert orig-chunk)
        nil))))

(defun elt-process-next-subchunk (chunk-end test-case-state test-case)
  (let* ((continuation (looking-at elt-code-chunk-next-re))
         (test-code (elt-process-code chunk-end))
         (test-result (elt-run- (if continuation test-case-state test-case)
                                test-code elt-init-alist
                                continuation))
         (subchunk-end (save-excursion
                         (if (re-search-forward (elt--code-chunk-re) chunk-end t)
                             (match-beginning 0)
                           chunk-end))))
    (delete-region (point) subchunk-end)
    (insert (concat "\n" test-result "\n\n"))
    test-result))

(defun elt-process-case (chunk-end)
  (let ((case-start (progn
                      (skip-chars-forward " \t\n")
                      (goto-char (line-beginning-position))
                      (point)))
        (code-start (if (re-search-forward (elt--code-chunk-re) chunk-end t)
                        (goto-char (match-beginning 0))
                      (error "No code chunk found")))
        (case-end (progn
                    (skip-chars-backward " \t\n")
                    (point))))
    (forward-char 1)
    (delete-region (point) code-start)
    (insert "\n")
    (buffer-substring-no-properties case-start case-end)))

(defun elt-process-code (chunk-end)
  (let* ((test-start (point))
         (test-end (if (re-search-forward "^$" chunk-end t)
                       (1- (match-beginning 0))
                     (goto-char chunk-end)))
         (test-code (buffer-substring-no-properties test-start test-end)))
    ;; Remove comment prefix
    (while (string-match (elt--code-chunk-re) test-code)
      (setq test-code (replace-match "" t t test-code)))
    ;; Parse elisp
    (setq test-code (concat "(" test-code ")"))
    (car (read-from-string test-code))))


;; The following functions are borrowed from Lispy's testing
;; framework. The main difference is that they restore state when
;; `keep-state' is t. They also run `(kbd)' on strings.

(defvar elt--state-buffer nil
  "Evaluation buffer of previous test chunk.")

(defmacro elt-run (init &rest body)
  (apply 'elt-run- `(,init (,@body))))

(defun elt-run- (init body local-variables &optional keep-state)
  (unless keep-state
    (and elt--state-buffer
         (buffer-name elt--state-buffer)
         (kill-buffer elt--state-buffer))
    (setq elt--state-buffer (generate-new-buffer " *temp*")))
  (save-window-excursion
    (switch-to-buffer elt--state-buffer)
    (if keep-state
        (delete-region (point-min) (point-max))
      (transient-mark-mode 1)
      (setq-local file-local-variables-alist (copy-alist local-variables))
      (hack-local-variables-apply))
    (insert init)
    (goto-char (point-min))
    (when (search-forward "×" nil t)
      (backward-delete-char 1)
      (set-mark (point))
      (when (search-forward "×" nil t)
        (error "There can only be one mark cursor")))
    (goto-char (point-max))
    (let (cursors-start
          cursors-end)
      (while (search-backward "¶" nil t)
        (delete-char 1)
        (let ((marker (point-marker)))
          (set-marker-insertion-type marker t)
          (push marker cursors-start)))
      (unless cursors-start
        (error "There must be at least one point cursor"))
      ;; Fontification must take place after removing "¶"
      ;; FIXME Emacs 25: Use `font-lock-ensure'
      (font-lock-default-fontify-buffer)
      (dolist (cursor cursors-start)
        (goto-char cursor)
        ;; Reset Emacs state
        (unless keep-state
          (setq last-command nil)
          (setq current-prefix-arg nil))
        (mapcar (lambda (x)
                  (cond ((equal x '(kbd "C-u"))
                         (setq current-prefix-arg (list 4)))
                        ((stringp x)
                         (if (string= x "C-u")
                             (setq current-prefix-arg (list 4))
                           (elt-unalias (kbd x))))
                        ((and (listp x)
                              (eq (car x) 'kbd))
                         (elt-unalias x))
                        (t (eval x))))
                body)
        (let ((marker (point-marker)))
          (set-marker-insertion-type marker t)
          (push marker cursors-end)))
      (dolist (cursor cursors-end)
        (goto-char cursor)
        (insert "¶")))
    (when (region-active-p)
      (exchange-point-and-mark)
      (insert "×"))
    (buffer-substring-no-properties
     (point-min)
     (point-max))))

(defun elt-decode-keysequence (str)
  "Decode STR from e.g. \"23ab5c\" to '(23 \"a\" \"b\" 5 \"c\")"
  (let ((table (copy-sequence (syntax-table))))
    (cl-loop for i from ?0 to ?9 do
             (modify-syntax-entry i "." table))
    (cl-loop for i from ? to ? do
             (modify-syntax-entry i "w" table))
    (cl-loop for i in '(? ?\( ?\) ?\[ ?\] ?{ ?} ?\" ?\' ?\ )
             do (modify-syntax-entry i "w" table))
    (cl-mapcan (lambda (x)
                 (let ((y (ignore-errors (read x))))
                   (if (numberp y)
                       (list y)
                     (mapcar #'string x))))
               (with-syntax-table table
                 (split-string str "\\b" t)))))

(defun elt-unalias (seq)
  "Emulate pressing keys decoded from SEQ."
  (if (vectorp seq)
      (elt--unalias-key seq)
    (let ((lkeys (elt-decode-keysequence seq))
          key)
      (while (setq key (pop lkeys))
        (if (numberp key)
            (let ((current-prefix-arg (list key)))
              (when lkeys
                (elt--unalias-key (pop lkeys))))
          (elt--unalias-key key))))))

(defun elt--unalias-key (key)
  "Call command that corresponds to KEY.
Insert KEY if there's no command."
  (setq last-input-event (aref key 0))
  (let ((cmd (key-binding key)))
    (if (eq cmd 'self-insert-command)
        (insert key)
      (setq last-command-event (aref key 0))
      (call-interactively cmd)
      (setq last-command cmd))))
