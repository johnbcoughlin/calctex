(require 'calctex)

(defun with-calc-test-harness (body)
  (calc)
  (with-current-buffer "*Calculator*"
    (set-face-attribute 'default nil :foreground "dark red" :background "navajo white")
    (let* ((tempdir-name (make-temp-name temporary-file-directory))
           (calctex-latex-image-directory tempdir-name))
      (make-directory calctex-latex-image-directory)
      (funcall body))))

(ert-deftest test-displays-image-correctly ()
  (with-calc-test-harness
   (lambda ()
      (calctex-mode 1)
      (execute-kbd-macro (kbd "M-m M-t"))
      (execute-kbd-macro (kbd "' a <RET>"))
      (execute-kbd-macro (kbd "2 <RET> * d L"))
      (let* ((ovs (overlays-in (point-min) (point-max)))
             (ov (car ovs))
             (disp (overlay-get ov 'display))
             (file (plist-get (cdr disp) :file))
             (equality (shell-command (format "cmp %s %s" file "resources/displays-image-correctly-output.png")))
             )
        (should (= equality 0))
        ))))

(provide 'calctex-test)
