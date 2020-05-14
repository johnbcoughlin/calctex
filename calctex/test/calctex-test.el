;; -*- lexical-binding: t; -*-

;; Copyright (C) 2019 John B Coughlin

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

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
    (if ov
        ov
      (message "
==========Render error output:==========
%s

========================================
" (with-current-buffer "*CalcTeX-DVIPNG*"
    (buffer-string))))))

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

(defun assert-renders-tex-to-image (tex reference-image)
  (let* ((img (funcall calctex-render-process tex))
         (img-file (plist-get img 'file))
         (img-type (plist-get img 'type)))
    (assert-image-equals img-file reference-image)))

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

(ert-deftest test-imagemagick-disabled ()
  (with-calc-test-harness
   (lambda ()
     (let ((calctex-imagemagick-enabled-p nil))
       (calctex-mode 1)
       (execute-kbd-macro (kbd "' a <RET>"))
       (execute-kbd-macro (kbd "2 <RET> *"))
       (let ((ov (nth-overlay 0)))
         (assert-overlay-image-equals ov "2a_lowres.png")
         (should (equal (overlay-display-property ov :type) 'png)))
       ))))

(ert-deftest test-imagemagick-enabled ()
  (with-calc-test-harness
   (lambda ()
     (let ((calctex-imagemagick-enabled-p t))
       (calctex-mode 1)
       (execute-kbd-macro (kbd "' a <RET>"))
       (execute-kbd-macro (kbd "2 <RET> *"))
       (let ((ov (nth-overlay 0)))
         (assert-overlay-image-equals ov "2a.png")
         (should (equal (overlay-display-property ov :type) 'imagemagick))
         (should (equal (overlay-display-property ov :format) 'png))
         (should (equal (overlay-display-property ov :scale) (/ calctex-imagemagick-png-scaling calctex-base-imagemagick-png-scaling)))))
     )))

(ert-deftest creates-parents-of-image-cache ()
  (progn
    (with-calc-test-harness
     (lambda ()
       (let ((calctex-latex-image-directory "./calctex-image-cache"))
         (progn
           (calctex-mode 1)
           (execute-kbd-macro (kbd "' a <RET>"))
           (execute-kbd-macro (kbd "2 <RET> *"))
           (assert-nth-overlay-image-equals 0 "2a.png")))))))

(ert-deftest renders-si ()
  (progn
    (set-face-attribute 'default nil :foreground "black" :background "white")
    (assert-renders-tex-to-image
     "\\[ \\boxed{\\eta = \\frac{\\SI{6.27e8}{\\joule}}{\\SI{7.96e9}{\\joule}} = 0.0787} \\]"
     "joules.png")))

(provide 'calctex-test)
