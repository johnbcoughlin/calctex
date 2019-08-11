(require 'calctex)

(defun with-calc-test-harness (body)
  (calc)
  (calc-reset 0)
  (with-current-buffer "*Calculator*"
    (set-face-attribute 'default nil :foreground "black" :background "white")
    (let* ((tempdir-name (make-temp-name temporary-file-directory))
           (calctex-latex-image-directory tempdir-name))
      (make-directory calctex-latex-image-directory)
      (funcall body))))

(defun nth-overlay (n)
  (let* ((ovs (overlays-in (point-min) (point-max)))
         (ov (nth n ovs)))
    (if ov
        ov
      (message "
==========Render error output:==========
%s

========================================
" (with-current-buffer "*CalcTeX Log*"
    (buffer-string))))))

(defun overlay-display-property (ov prop)
  (let* ((disp (overlay-get ov 'display)))
    (plist-get (cdr disp) prop)))

(defun assert-overlay-image-equals (ov reference-image)
  (let* ((disp (overlay-get ov 'display))
         (actual-file (plist-get (cdr disp) :file))
         (file-comp (format "cmp %s %s" actual-file reference-image))
         (equality (shell-command file-comp)))
    (if (not (= equality 0))
        (message "command returned nonzero exit code: %s" file-comp)
      ())
    (should (= equality 0))))

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
        (assert-overlay-image-equals ov "resources/2a.png")))))

(ert-deftest test-changes-image-overlay-live ()
  (with-calc-test-harness
   (lambda ()
     (calctex-mode 1)
     (execute-kbd-macro (kbd "' a <RET>"))
     (execute-kbd-macro (kbd "2 <RET> *"))
     (assert-nth-overlay-image-equals 0 "resources/2a.png")
     (execute-kbd-macro (kbd "2 <RET> *"))
     (assert-nth-overlay-image-equals 0 "resources/4a.png"))))

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
     (assert-nth-overlay-image-equals 0 "resources/2a.png")
     (assert-nth-overlay-image-equals 1 "resources/4a.png")
     )))

(ert-deftest test-imagemagick-disabled ()
  (with-calc-test-harness
   (lambda ()
     (let ((calctex-imagemagick-enabledp nil))
       (calctex-mode 1)
       (execute-kbd-macro (kbd "' a <RET>"))
       (execute-kbd-macro (kbd "2 <RET> *"))
       (let ((ov (nth-overlay 0)))
         (assert-overlay-image-equals ov "resources/2a_lowres.png")
         (should (equal (overlay-display-property ov :type) 'png)))
       ))))

(ert-deftest test-imagemagick-enabled ()
  (with-calc-test-harness
   (lambda ()
     (let ((calctex-imagemagick-enabledp t))
       (calctex-mode 1)
       (execute-kbd-macro (kbd "' a <RET>"))
       (execute-kbd-macro (kbd "2 <RET> *"))
       (let ((ov (nth-overlay 0)))
         (assert-overlay-image-equals ov "resources/2a.png")
         (should (equal (overlay-display-property ov :type) 'imagemagick))
         (should (equal (overlay-display-property ov :format) 'png))
         (should (equal (overlay-display-property ov :scale) calctex-imagemagick-png-scaling))))
     )))

(ert-deftest creates-parents-of-image-cache ()
  (with-calc-test-harness
   (lambda ()
     (setq calctex-latex-image-directory "~/foo/bar/calctex")
     (calctex-mode 1)
     (execute-kbd-macro (kbd "' a <RET>"))
     (execute-kbd-macro (kbd "2 <RET> *"))
     (assert-nth-overlay-equals 0 "resources/2a.png" 'png))))

(provide 'calctex-test)
