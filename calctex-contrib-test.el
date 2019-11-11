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

(require 'calc)
(require 'calctex-contrib)
(require 'ert)
(require 'test-utils)

(defun with-calc-test-harness (body)
  (calc)
  (calc-reset 0)
  (calc-no-simplify-mode t)
  (calc-latex-language nil)
  (with-current-buffer "*Calculator*" (funcall body)))

(defun calc-stack-line (n)
  (with-current-buffer "*Calculator*"
    (goto-char (point-min))
    (forward-line n)
    (let* ((line-start (if calc-line-numbering
                           (+ (line-beginning-position) 4)
                         (line-beginning-position)))
           (line-end (line-end-position))
           (line-contents (buffer-substring line-start line-end)))
      line-contents)))

(ert-deftest test-displays-vector-variable ()
  (with-calc-test-harness
   (lambda ()
     (setq calctex-contrib-context-decls
           (list "[a, vector]"))
     (calctex-contrib-refresh)
     (calc-eval "a" 'push)
     (should (equal (calc-stack-line 1) "\\vec{a}")))))

(ert-deftest test-displays-matrix-no-particular-way ()
  (with-calc-test-harness
   (lambda ()
     (setq calctex-contrib-context-decls
           (list "[A, matrix]"))
     (calctex-contrib-refresh)
     (calc-eval "A" 'push)
     (should (equal (calc-stack-line 1) "A")))))

(ert-deftest test-displays-total-derivative-of-var ()
  (with-calc-test-harness
   (lambda ()
     (calc-eval "f" 'push)
     (calc-eval "Hadx\r" 'macro)
     (should (equal (calc-stack-line 1) "\\frac{\\mathrm{d}f}{\\mathrm{d}x}")))))

(ert-deftest test-displays-partial-derivative-of-var ()
  (with-calc-test-harness
   (lambda ()
     (calc-eval "g" 'push)
     (calc-eval "adx\r" 'macro)
     (should (equal (calc-stack-line 1) "\\frac{\\partial g}{\\partial x}")))))

(ert-deftest test-multieq ()
  (with-calc-test-harness
   (lambda ()
     (calc-eval "eq(a, b, c)" 'push)
     (should (equal (calc-stack-line 1) "a = b = c")))))
