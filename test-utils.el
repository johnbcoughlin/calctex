(defvar calctex-test-resources-dir nil
  "The directory where test resources live")

(defun assert-overlay-image-equals (ov reference-image)
  (let* ((disp (overlay-get ov 'display))
         (actual-file (plist-get (cdr disp) :file))
         (file-comp (format "cmp %s %s/%s" actual-file calctex-test-resources-dir reference-image))
         (equality (shell-command file-comp)))
    (if (not (= equality 0))
        (message "command returned nonzero exit code: %s" file-comp)
      ())
    (should (= equality 0))))

(provide 'test-utils)
