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

(require 'org-calctex)
(require 'test-utils)
(require 'ert)

(defun with-org-calctex-test-harness (body)
  (find-file "resources/test.org")
  (revert-buffer nil t)
  (set-face-attribute 'default nil :foreground "black" :background "white")
  (goto-char 1)
  (funcall body))

(ert-deftest test-next-formula ()
  (with-org-calctex-test-harness
   (lambda ()
     (org-calctex-next-formula)
     (should (equal (point) 28))
     (org-calctex-next-formula)
     (should (equal (point) 65)))))

(ert-deftest test-next-formula-point-within-formula ()
  (with-org-calctex-test-harness
   (lambda ()
     (goto-char 30)
     (org-calctex-next-formula)
     (should (equal (point) 65)))))

(ert-deftest test-previous-formula ()
  (with-org-calctex-test-harness
   (lambda ()
     (goto-char 90)
     (org-calctex-prev-formula)
     (should (equal (point) 65))
     (org-calctex-prev-formula)
     (should (equal (point) 28)))))

(ert-deftest test-previous-formula-point-within-formula ()
  (with-org-calctex-test-harness
   (lambda ()
     (goto-char 75)
     (org-calctex-prev-formula)
     (should (equal (point) 28)))))

(ert-deftest test-activate-and-accept ()
  (with-org-calctex-test-harness
   (lambda ()
     (org-calctex-next-formula)
     (org-calctex-activate-formula)
     (execute-kbd-macro (kbd "' f ( x ) <RET>"))
     (execute-kbd-macro (kbd "' x ^ 2 <RET>"))
     (execute-kbd-macro (kbd "a = <RET>"))
     (org-calctex-accept-formula))))

(ert-deftest test-accept-jumps-to-end-of-formula ()
  (with-org-calctex-test-harness
   (lambda ()
     (org-calctex-next-formula)
     (let ((formula-start (point)))
       (org-calctex-activate-formula)
       (execute-kbd-macro (kbd "' f ( x ) <RET>"))
       (org-calctex-accept-formula)
       (progn
         (should (equal (point) (+ formula-start 4 4)))
         (should (equal (buffer-substring formula-start (point)) "\\[f(x)\\]")))))))

(ert-deftest test-show-hide-overlay ()
  (with-org-calctex-test-harness
   (lambda ()
     (org-calctex-next-formula)
     (org-calctex-hide-overlay-at-point)
     (assert-overlay-image-equals
      (org-calctex-overlay-at-point)
      "pythag.png")
     (org-calctex-hide-overlay-at-point)
     (should (equal nil (org-calctex-overlay-at-point)))
     )))


(provide 'org-calctex-test)
