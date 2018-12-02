
(require 'ert)
(require 'elt)

(defvar elt-test-path (file-name-directory load-file-name))

(ert-deftest elt-scan-bad-case-chunk ()
  (should-error (elt-load-file (expand-file-name "scan-bad-case-chunk.md" elt-test-path))
                :type 'elt--bad-case-chunk))

(ert-deftest elt-unknown-chunk-type ()
  (should-error (elt-load-file (expand-file-name "scan-unknown-chunk-type.md" elt-test-path))
                :type 'elt--unknown-chunk-type))

(ert-deftest elt-test-file-prop ()
  (let ((test-file (expand-file-name "scan-chunks.md" elt-test-path)))
    (elt-load-file test-file)
    (should (string= (get 'elt-test-section 'elt-file)
                     test-file))))

(ert-deftest elt-test-pass ()
  (elt-load-file (expand-file-name "test-pass.md" elt-test-path))
  (should (eq (type-of (ert-run-test (ert-get-test 'elt-test-section)))
              'ert-test-passed)))

(ert-deftest elt-test-missing-result ()
  (elt-load-file (expand-file-name "test-missing-result.md" elt-test-path))
  (should-error (ert-run-test (ert-get-test 'elt-test-section))
                :type 'elt--missing-result))
