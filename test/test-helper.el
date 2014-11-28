(require 'f)

(defvar imgix-test/test-path
  (f-parent (f-this-file)))

(defvar imgix-test/root-path
  (f-parent imgix-test/test-path))

(require 'ert)
(require 'imgix (f-expand "imgix" imgix-test/root-path))
