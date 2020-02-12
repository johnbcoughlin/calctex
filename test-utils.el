(defvar calctex-test-resources-dir nil
  "The directory where test resources live")

(defun assert-overlay-image-equals (ov reference-image)
  (let* ((disp (overlay-get ov 'display))
         (actual-file (plist-get (cdr disp) :file)))
    (assert-image-equals actual-file reference-image)))

(defun assert-image-equals (actual expected)
  (let* ((file-comp (format "cmp %s %s/%s" actual calctex-test-resources-dir expected))
         (equality (shell-command file-comp)))
    (should (= equality 0))))

(provide 'test-utils)
