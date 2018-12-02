
(require 'ert)
(require 'elt)

(defvar elt-test-path (file-name-directory load-file-name))

(ert-deftest elt-scan-bad-case-chunk ()
  (should-error (elt-load-file (expand-file-name "scan-bad-case-chunk.md" elt-test-path))
                :type 'elt--bad-case-chunk))

(ert-deftest elt-unknown-chunk-type ()
  (should-error (elt-load-file (expand-file-name "scan-unknown-chunk-type.md" elt-test-path))
                :type 'elt--unknown-chunk-type))
