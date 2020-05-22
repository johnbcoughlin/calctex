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

(defvar src-2a "\\[ 2a \\]")

(ert-deftest test-imagemagick-disabled ()
  (calctex-mode 1)
  (let* ((calctex-imagemagick-enabled-p nil)
         (image-desc (funcall calctex-render-process src-2a)))
    (assert-image-descriptor-file-equals image-desc "2a_lowres.png")
    (should (equal (plist-get image-desc :type) 'png))
    (should (equal (plist-get image-desc :format) nil))
    (should (equal (plist-get image-desc :scale) nil))))

(ert-deftest test-imagemagick-enabled ()
  (calctex-mode 1)
  (let* ((calctex-imagemagick-enabled-p t)
         (image-desc (funcall calctex-render-process src-2a)))
         (assert-image-descriptor-file-equals image-desc "2a.png")
         (should (equal (plist-get image-desc :type) 'imagemagick))
         (should (equal (plist-get image-desc :format) 'png))
         (should (equal (plist-get image-desc :scale)
                        (/ calctex-imagemagick-png-scaling calctex-base-imagemagick-png-scaling)))))

(ert-deftest creates-parents-of-image-cache ()
  (calctex-mode 1)
  (let ((calctex-latex-image-directory "./calctex-image-cache"))
    (assert-converts-latex-to-file src-2a "2a.png")))

(ert-deftest renders-si ()
  (calctex-mode 1)
  (set-face-attribute 'default nil :foreground "black" :background "white")
  (assert-converts-latex-to-file
   "\\[ \\boxed{\\eta = \\frac{\\SI{6.27e8}{\\joule}}{\\SI{7.96e9}{\\joule}} = 0.0787} \\]"
   "joules.png"))

(ert-deftest recovers-from-error ()
  (calctex-mode 1)
  (let ((err (should-error (funcall calctex-render-process "\\[ \\left( a^2 + b^2 = c^2 ) \\]"))))
    (should (string= (error-message-string err) "LaTeX Render Error. Renderer restarted.")))
  (assert-converts-latex-to-file "\\[ a^2 + b^{2} = c^2 \\]" "pythag.png")
  )

(ert-deftest renders-2a ()
  (calctex-mode 1)
  (assert-converts-latex-to-file src-2a "2a.png"))

(ert-deftest renders-multiple-sources ()
  (calctex-mode 1)
  (assert-converts-latex-to-file "\\[ 4a \\]" "4a.png")
  (assert-converts-latex-to-file "\\[ 4 a \\]" "4a.png")
  (assert-converts-latex-to-file "\\[ {4}{a} \\]" "4a.png")
  (assert-converts-latex-to-file "\\[ a^2 + b^{2} = c^2 \\]" "pythag.png")
  )

(provide 'calctex-test)
