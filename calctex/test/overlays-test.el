(require 'calctex)
(require 'ert)

(defun with-calc-test-harness (body)
  (set-face-attribute 'default nil :foreground "black" :background "white")
  (calc)
  (calc-reset 0)
  (with-current-buffer "*Calculator*"
    (let* ((tempdir-name (make-temp-name temporary-file-directory))
           (calctex-latex-image-directory tempdir-name))
      (make-directory calctex-latex-image-directory)
      (funcall body))))

(defun nth-overlay (n)
  (let* ((ovs (overlays-in (point-min) (point-max)))
         (ov (nth n ovs)))
    ov))

(defun overlay-display-property (ov prop)
  (let* ((disp (overlay-get ov 'display)))
    (plist-get (cdr disp) prop)))

(defun assert-no-overlays ()
  (let* ((ovs (overlays-in (point-min) (point-max))))
    (should (= 0 (length ovs)))))

(defun assert-nth-overlay-image-equals (n reference-image)
  (assert-overlay-image-equals
   (nth-overlay n)
   reference-image))

(ert-deftest test-displays-image-correctly ()
  (with-calc-test-harness
   (lambda ()
      (calctex-mode 1)
      (execute-kbd-macro (kbd "' a <RET>"))
      (execute-kbd-macro (kbd "2 <RET> *"))
      (let ((ov (nth-overlay 0)))
        (assert-overlay-image-equals ov "2a.png")))))

(ert-deftest test-changes-image-overlay-live ()
  (with-calc-test-harness
   (lambda ()
     (calctex-mode 1)
     (execute-kbd-macro (kbd "' a <RET>"))
     (execute-kbd-macro (kbd "2 <RET> *"))
     (assert-nth-overlay-image-equals 0 "2a.png")
     (execute-kbd-macro (kbd "2 <RET> *"))
     (assert-nth-overlay-image-equals 0 "4a.png"))))

(ert-deftest test-turning-off-calctex-hides-overlays ()
  (with-calc-test-harness
   (lambda ()
     (calctex-mode 1)
     (execute-kbd-macro (kbd "' a <RET>"))
     (execute-kbd-macro (kbd "2 <RET> *"))
     (calctex-mode -1)
     (assert-no-overlays))))

(ert-deftest test-switching-language-mode-hides-overlays ()
  (with-calc-test-harness
   (lambda ()
     (calctex-mode 1)
     (execute-kbd-macro (kbd "' a <RET>"))
     (execute-kbd-macro (kbd "2 <RET> *"))
     (execute-kbd-macro (kbd "d N"))
     (assert-no-overlays))))

(ert-deftest test-multiple-overlays ()
  (with-calc-test-harness
   (lambda ()
     (calctex-mode 1)
     (execute-kbd-macro (kbd "' a <RET>"))
     (execute-kbd-macro (kbd "2 <RET> *"))
     (execute-kbd-macro (kbd "<RET>"))
     (execute-kbd-macro (kbd "2 <RET> *"))
     (assert-nth-overlay-image-equals 0 "2a.png")
     (assert-nth-overlay-image-equals 1 "4a.png")
     )))
