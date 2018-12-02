;;; ess-literate-tests.el --- Literate unit testing  -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-lib))
(require 'ert)

;; FIXME
(require 'ess-r-mode)


;;*;; Configuration

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


(defvar elt--chunk-head-re
  "^[ \t]*```\\([[:alpha:]]+\\) ?\\([[:alpha:]]+\\)? ?\\([[:alpha:] \t].*\\)?$")

(defvar elt--chunk-tail-re
  "^[ \t]*```[ \t]*$")

(defvar elt--section-re
  "^#+[ \t]\\(.+\\)$")

(defun elt--define-test (info file)
  (let ((name (intern (concat "elt-test-" (elt--normalise-title (pop info))))))
    (put name 'elt-file file)
    (ert-set-test name
                  (make-ert-test
                   :name name
                   :body (lambda () (elt--run-tests name info nil))))))

(defun elt-load-file (&optional path)
  (let* ((path (elt--path path))
         (cases (elt--scan-cases path)))
    (mapc (lambda (info) (elt--define-test info path)) cases)))

(defun elt--normalise-title (title)
  (downcase (replace-regexp-in-string "[^[:alnum:]]" "-" title)))

(defun elt--run-tests (name chunks generate)
  (let* ((test-buffer (find-file-noselect (get name 'elt-file)))
         (case-chunk (pop chunks))
         (case-code (elt--read-chunk case-chunk test-buffer))
         (case-init (list (assq 'mode case-chunk))))
    (while chunks
      (let* ((test-chunk (pop chunks))
             (test-forms (elt--read-test-chunk test-chunk test-buffer))
             (test-output (elt-run- case-code test-forms case-init nil)))
        (let* ((result-chunk (when (elt--result-chunk-p (car chunks))
                               (pop chunks))))
          (if generate
              (error "TODO")
            (unless result-chunk
              (signal 'elt--missing-result nil))
            (should (string= test-output
                             (elt--read-chunk result-chunk test-buffer)))))))))

(defun elt--read-chunk (chunk buffer)
  (let ((beg (alist-get 'beg chunk))
        (end (alist-get 'end chunk)))
    (with-current-buffer buffer
      (buffer-substring-no-properties beg end))))

(defun elt--read-test-chunk (chunk buffer)
  (unless (eq (alist-get 'type chunk) 'test)
    (error "Expected a test chunk"))
  (let ((chunk-string (elt--read-chunk chunk buffer)))
    (car (read-from-string (concat "(" chunk-string ")")))))

(defun elt--result-chunk-p (chunk)
  (eq (alist-get 'type chunk) 'result))

(defun elt--path (path)
  (let ((path (or path
                  buffer-file-name
                  (error "No PATH provided"))))
    (unless (file-exists-p path)
      (error "PATH does not exist"))
    path))

(defun elt--scan-cases (&optional path)
  (with-current-buffer
      (find-file-noselect (elt--path path))
    (let ((chunks (elt--scan-chunks))
          cases last-title)
      (while (cdr chunks)
        (unless (eq (alist-get 'type (car chunks)) 'case)
          (signal 'elt--bad-case-chunk nil))
        (let ((title (save-excursion
                       (goto-char (alist-get 'beg (car chunks)))
                       (if (re-search-backward elt--section-re nil t)
                           (match-string-no-properties 1)
                         (error "Can't find case title")))))
          (if (string= title last-title)
              (error "Can't have multiple case chunks in a single section")
            (setq last-title title))
          (push (cons title chunks) cases)
          (while (and (cdr chunks)
                      (not (eq (alist-get 'type (cadr chunks)) 'case)))
            (setq chunks (cdr chunks)))
          (let ((last-chunk chunks))
            (setq chunks (cdr chunks))
            (setcdr last-chunk nil))))
      (nreverse cases))))

(defun elt--scan-chunks ()
  (save-excursion
    (goto-char (point-min))
    (let (chunks)
      (while (re-search-forward elt--chunk-head-re nil t)
        (let* ((mode (intern (match-string-no-properties 1)))
               (type (intern (or (match-string-no-properties 2)
                                 (if (eq mode 'elisp)
                                     "test"
                                   (signal 'elt--unknown-chunk-type nil)))))
               (chunk (list (cons 'type type)
                            (cons 'beg (1+ (match-end 0)))
                            (cons 'end (save-match-data
                                         (if (re-search-forward elt--chunk-tail-re nil t)
                                             (1- (match-beginning 0))
                                           (error "Can't find end of chunk"))))
                            (cons 'mode mode)
                            (cons 'args (match-string-no-properties 3)))))
          (push chunk chunks)))
      (nreverse chunks))))

;;*;; Defining test files

(defmacro elt-deftest (name _args file)
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


;;*;; Processing test files

(defun elt-buffer-string (file src-string config)
  (let ((el-file (concat (file-name-sans-extension file) ".el")))
    (when (file-exists-p el-file)
      (load-file el-file)))
  (with-temp-buffer
    (insert src-string)
    ;; Don't check safety of local variables declared in test files
    (cl-letf (((symbol-function 'safe-local-variable-p) (lambda (&rest _args) t)))
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
    (elt--print-section-title)
    (when (elt--search-case nil t)
      (while (looking-at elt-case-title-re)
        (elt--print-case-title)
        (elt--process-case)
        (when (elt--print-section-title)
          (insert "\n")
          (elt--search-case nil t))))
    (skip-chars-backward "\n")
    (let ((point-max (or (elt--find-local-variables-pos)
                         (point-max))))
      (delete-region (1+ (point)) point-max)
      (insert "\n"))
    (when (looking-at (concat "\n" comment-start "+ +Local Variables:"))
      (insert "\n"))
    (undo-boundary)))

(defun elt--search-case (&optional n skip-section)
  (let* ((next-chunk (save-excursion
                       (cond ((re-search-forward elt-case-title-re
                                                 nil t (or n 1))
                              (match-beginning 0))
                             ((elt--find-local-variables-pos))
                             (t
                              (point-max)))))
         (next-section (save-excursion
                         (when (re-search-forward elt-section-title-re
                                                  next-chunk t)
                           (match-beginning 0)))))
    (goto-char (if (and (not skip-section) next-section)
                   next-section
                 next-chunk))))

(defun elt--find-local-variables-pos ()
  (save-excursion
    (let ((pattern (concat "^" comment-start "+ +Local Variables:")))
      (when (re-search-forward pattern nil t)
        (match-beginning 0)))))

(defun elt--process-case ()
  (let* ((case-beg (point))
         (case-end (progn
                      (forward-line)
                      (save-excursion
                        (elt--search-case)
                        (point-marker))))
         (saved-case (buffer-substring case-beg case-end)))
    (condition-case cnd
        (let* ((starting-chunk (elt--get-case-chunk case-end))
               (current-chunk starting-chunk))
          (while (looking-at (elt--code-chunk-re))
            (setq current-chunk (elt--process-test-chunk case-end current-chunk starting-chunk)))
          (insert "\n"))
      (ert-test-skipped
        (message (concat "  Skipping test: " (cadr cnd)))
        (goto-char case-beg)
        (delete-region case-beg case-end)
        (insert saved-case)
        nil))))

(defun elt--get-case-chunk (case-end)
  (let ((case-start (progn
                      (skip-chars-forward " \t\n")
                      (goto-char (line-beginning-position))
                      (point)))
        (code-start (if (re-search-forward (elt--code-chunk-re) case-end t)
                        (goto-char (match-beginning 0))
                      (error "No code chunk found")))
        (case-end (progn
                    (skip-chars-backward " \t\n")
                    (point))))
    (forward-char 1)
    (delete-region (point) code-start)
    (insert "\n")
    (buffer-substring-no-properties case-start case-end)))

(defun elt--process-test-chunk (case-end current-chunk starting-chunk)
  (let* ((continuation (looking-at elt-code-chunk-next-re))
         (test-forms (elt---read-test-chunk case-end))
         (test-output (elt-run- (if continuation current-chunk starting-chunk)
                                test-forms elt-init-alist
                                continuation))
         (output-end-pos (save-excursion
                           (if (re-search-forward (elt--code-chunk-re) case-end t)
                               (match-beginning 0)
                             case-end))))
    (delete-region (point) output-end-pos)
    (insert (concat "\n" test-output "\n\n"))
    test-output))

(defun elt---read-test-chunk (case-end)
  (let* ((test-start (point))
         (test-end (if (re-search-forward "^$" case-end t)
                       (1- (match-beginning 0))
                     (goto-char case-end)))
         (test-code (buffer-substring-no-properties test-start test-end)))
    ;; Remove comment prefix
    (while (string-match (elt--code-chunk-re) test-code)
      (setq test-code (replace-match "" t t test-code)))
    ;; Parse elisp
    (setq test-code (concat "(" test-code ")"))
    (car (read-from-string test-code))))


;;;*;;; Printing progress

(defun elt--print-section-title ()
  (save-excursion
    (skip-chars-forward " \n\t")
    (when (looking-at elt-section-title-re)
      (message (match-string-no-properties 1))
      t)))

(defun elt--print-case-title ()
  (let ((number (concat "#" (match-string-no-properties 1)))
        (msg (match-string-no-properties 2)))
    (setq msg (substring msg 0 (string-match "-+$" msg)))
    (message (if (> (length msg) 0)
                 (concat number " - " msg)
               number))))


;;*;; Running test code

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
        (dolist (x body)
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


;;; Compatibility

(when (<= emacs-major-version 25)
  (defun define-symbol-prop (symbol prop val)
    "Define the property PROP of SYMBOL to be VAL.
This is to `put' what `defalias' is to `fset'."
    ;; Can't use `cl-pushnew' here (nor `push' on (cdr foo)).
    ;; (cl-pushnew symbol (alist-get prop
    ;;                               (alist-get 'define-symbol-props
    ;;                                          current-load-list)))
    (let ((sps (assq 'define-symbol-props current-load-list)))
      (unless sps
        (setq sps (list 'define-symbol-props))
        (push sps current-load-list))
      (let ((ps (assq prop sps)))
        (unless ps
          (setq ps (list prop))
          (setcdr sps (cons ps (cdr sps))))
        (unless (member symbol (cdr ps))
          (setcdr ps (cons symbol (cdr ps))))))
    (put symbol prop val)))


(define-error 'elt--bad-case-chunk
  "Section must open with a case chunk")

(define-error 'elt--unknown-chunk-type
  "Can't find chunk type")

(define-error 'elt--missing-result
  "Can't find the result chunk. Do you need to generate results?")


(provide 'elt)
