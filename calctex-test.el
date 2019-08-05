(require 'calctex)

(defun with-calc-test-harness (body)
  (calc)
  (calc-reset 0)
  (with-current-buffer "*Calculator*"
    (set-face-attribute 'default nil :foreground "dark red" :background "navajo white")
    (let* ((tempdir-name (make-temp-name temporary-file-directory))
           (calctex-latex-image-directory tempdir-name))
      (make-directory calctex-latex-image-directory)
      (funcall body))))

(defun assert-nth-overlay-equals (n reference-image type)
  (let* ((ovs (overlays-in (point-min) (point-max)))
         (ov (nth n ovs))
         (disp (overlay-get ov 'display))
         (file (plist-get (cdr disp) :file))
         (format (plist-get (cdr disp) :format))
         (equality (shell-command (format "cmp %s %s" file reference-image))))
    (should (= equality 0))
    (should (equal format type))))

(defun assert-no-overlays ()
  (let* ((ovs (overlays-in (point-min) (point-max))))
    (should (= 0 (length ovs)))))

(ert-deftest test-displays-image-correctly ()
  (with-calc-test-harness
   (lambda ()
      (calctex-mode 1)
      (execute-kbd-macro (kbd "d L"))
      (execute-kbd-macro (kbd "' a <RET>"))
      (execute-kbd-macro (kbd "2 <RET> *"))
      (assert-nth-overlay-equals 0 "resources/2a.png" 'png))))

(ert-deftest test-changes-image-overlay-live ()
  (with-calc-test-harness
   (lambda ()
     (calctex-mode 1)
     (execute-kbd-macro (kbd "d L"))
     (execute-kbd-macro (kbd "' a <RET>"))
     (execute-kbd-macro (kbd "2 <RET> *"))
     (assert-nth-overlay-equals 0 "resources/2a.png" 'png)
     (execute-kbd-macro (kbd "2 <RET> *"))
     (assert-nth-overlay-equals 0 "resources/4a.png" 'png))))

(ert-deftest test-turning-off-calctex-hides-overlays ()
  (with-calc-test-harness
   (lambda ()
     (calctex-mode 1)
     (execute-kbd-macro (kbd "d L"))
     (execute-kbd-macro (kbd "' a <RET>"))
     (execute-kbd-macro (kbd "2 <RET> *"))
     (calctex-mode -1)
     (assert-no-overlays))))

(ert-deftest test-switching-language-mode-hides-overlays ()
  (with-calc-test-harness
   (lambda ()
     (calctex-mode 1)
     (execute-kbd-macro (kbd "d L"))
     (execute-kbd-macro (kbd "' a <RET>"))
     (execute-kbd-macro (kbd "2 <RET> *"))
     (execute-kbd-macro (kbd "d N"))
     (assert-no-overlays))))

(provide 'calctex-test)
