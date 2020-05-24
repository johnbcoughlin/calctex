;;; test-helper.el --- Helpers for calctex-test.el

(setq debug-on-error t)
(setq debug-on-quit t)

(set-face-attribute 'default nil :foreground "black" :background "white")

(defvar calctex-test-resources-dir nil "The directory where test resources live")

(setq calctex-dvichop-sty "../../vendor/texd/dvichop.sty")
(setq calctex-dvichop-bin "../../vendor/texd/dvichop")

(setq calctex-test-resources-dir (expand-file-name "resources" (file-name-directory load-file-name)))

(defun resource-file (name)
  (expand-file-name name calctex-test-resources-dir))

(defun assert-overlay-image-equals (ov reference-image)
  (message "%s" reference-image)
  (let* ((disp (overlay-get ov 'display))
         (actual-file (plist-get (cdr disp) :file)))
    (assert-image-equals actual-file reference-image)))

(defun assert-image-equals (actual expected)
  (message "%s" expected)
  (message "%s" (resource-file expected))
  (let* ((file-comp (format "cmp %s %s" actual (resource-file expected)))
         (equality (shell-command file-comp)))
    (progn
      (message "ran: %s" file-comp)
      (should (= equality 0)))))

;;; test-helper.el ends here
