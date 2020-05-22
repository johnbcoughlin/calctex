;;; test-helper.el --- Helpers for calctex-test.el

(setq debug-on-error t)
(setq debug-on-quit t)

(set-face-attribute 'default nil :foreground "black" :background "white")

(defvar calctex-test-resources-dir nil "The directory where test resources live")

(setq calctex-dvichop-sty (format "%s/dvichop" (getenv "TEXD_DIR")))
(setq calctex-dvichop-bin (format "%s/bin/dvichop" (getenv "TEXD_DIR")))

(setq calctex-test-resources-dir (expand-file-name "resources" (file-name-directory load-file-name)))

(defun resource-file (name)
  (expand-file-name name calctex-test-resources-dir))

(defun assert-converts-latex-to-file (src reference-image)
  (assert-image-descriptor-file-equals
   (funcall calctex-render-process src)
   reference-image))

(defun assert-overlay-image-equals (ov reference-image)
  (let* ((disp (overlay-get ov 'display)))
    (progn
      (message "%s" disp)
      (assert-image-descriptor-file-equals (cdr disp) reference-image)))
      )

(defun assert-image-descriptor-file-equals (img reference-image)
  (let ((actual-file (plist-get img :file)))
    (assert-image-equals actual-file reference-image)))

(defun assert-image-equals (actual expected)
  (let* ((file-comp (format
                     "if (( $(echo \"0.03 > $(compare -metric DSSIM %s %s diff.png 2>&1)\" | bc -l) )); then exit 0; else exit 1; fi"
                     actual (resource-file expected)))
         (equality (shell-command file-comp)))
    (progn
      (unless (= equality 0)
        (message "ran: %s" file-comp))
      (should (= equality 0)))))

;;; test-helper.el ends here
