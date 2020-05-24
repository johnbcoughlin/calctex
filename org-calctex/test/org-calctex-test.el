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
(require 'ert)

(defun with-org-calctex-test-harness (body &optional file)
  (message "%s" (resource-file "test.org"))
  (find-file (if file file (resource-file "test.org")))
  (revert-buffer nil t)
  (message "Removing overlays")
  (remove-overlays)
  (set-face-attribute 'default nil :foreground "black" :background "white")
  (calctex-mode 1)
  (goto-char 1)
  (funcall body))

(ert-deftest test-next-formula ()
  (with-org-calctex-test-harness
   (lambda ()
     (org-calctex-next-formula)
     (should (equal (point) 28))
     (org-calctex-next-formula)
     (should (equal (point) 87)))))

(ert-deftest test-next-formula-point-within-formula ()
  (with-org-calctex-test-harness
   (lambda ()
     (goto-char 30)
     (org-calctex-next-formula)
     (should (equal (point) 87)))))

(ert-deftest test-previous-formula ()
  (message "FOO!!" (buffer-substring (point-min) (point-max)))
  (with-org-calctex-test-harness
   (lambda ()
     (message "%s" (buffer-substring (point-min) (point-max)))
     (goto-char 133)
     (org-calctex-prev-formula)
     (should (equal (point) 87))
     (org-calctex-prev-formula)
     (should (equal (point) 28)))))

(ert-deftest test-previous-formula-point-within-formula ()
  (with-org-calctex-test-harness
   (lambda ()
     (goto-char 75)
     (org-calctex-prev-formula)
     (should (equal (point) 28)))))

;; (ert-deftest test-activate-and-accept ()
;;   (with-org-calctex-test-harness
;;    (lambda ()
;;      (org-calctex-next-formula)
;;      (org-calctex-activate-formula)
;;      (execute-kbd-macro (kbd "' f ( x ) <RET>"))
;;      (execute-kbd-macro (kbd "' x ^ 2 <RET>"))
;;      (execute-kbd-macro (kbd "a = <RET>"))
;;      (org-calctex-accept-formula))))

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

(ert-deftest test-jump-to-align ()
  (with-org-calctex-test-harness
   (lambda ()
     (org-calctex-next-formula)
     (should (equal (point) 13)))
   (resource-file "alignstar.org")))

(ert-deftest test-activate-align ()
  (with-org-calctex-test-harness
   (lambda ()
     (org-calctex-next-formula)
     (let ((formula-start (point)))
       (org-calctex-activate-formula)
       (execute-kbd-macro (kbd "' j <RET>"))
       (execute-kbd-macro (kbd "+"))
       (org-calctex-accept-formula)
       ))
   (resource-file "alignstar.org")))

(ert-deftest test-show-hide-overlay ()
  (with-org-calctex-test-harness
   (lambda ()
     (org-calctex-next-formula)
     (redisplay)
     (message "%s" (current-buffer))
     (org-calctex-hide-overlay-at-point)
     (message "%s" (current-buffer))
     (message "%s" (point))
     (message "%s" (org-calctex-overlay-at-point))
     (redisplay)
     (sleep-for 3)
     (assert-overlay-image-equals (org-calctex-overlay-at-point) "pythag.png")
  ;   (org-calctex-hide-overlay-at-point)
     ;(should (equal nil (org-calctex-overlay-at-point)))
     )))


(provide 'org-calctex-test)
