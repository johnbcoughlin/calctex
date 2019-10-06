;;; org-calctex.el --- CalcTeX integration with Org mode -*- lexical-binding: t; -*-

;; Author: Jack Coughlin <jack@johnbcoughlin.com>
;; URL: https://github.com/johnbcoughlin/calctex
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))

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

;;; Commentary:

;; org-calctex integrates calctex with Org mode.

;;; Code:

(require 'org)
(require 'calctex)

(defvar calctex--last-frag nil)

(defvar org-calctex--equation-jump-mark nil)

(defun calctex--render-just-exited-overlay ()
  (if (and (not (calctex-latex-fragment-at-point))
           calctex--last-overlay
           calctex--last-frag)
      (progn
        (calctex--render-overlay-at-frag calctex--last-frag calctex--last-overlay)
        (setq calctex--last-overlay nil
              calctex--last-frag nil))))

(defun calctex--get-or-create-overlay-at-frag (frag)
  (let* ((beg (org-element-property :begin frag))
         (end (save-excursion
                (goto-char (org-element-property :end frag))
                (skip-chars-backward " \r\t\n")
                (point))))
    (calctex--get-or-create-overlay beg end)))

(defun calctex--render-overlay-at-frag (frag ov)
  (let* ((tex (org-element-property :value frag))
         (margin (if (or (string-prefix-p "\\[" tex)
                         (string-prefix-p "\begin" tex))
                     4
                   1)))
    (message "margin: %s" margin)
    (overlay-put ov 'modification-hooks '(calctex--modification-hook))
    (calctex--render-overlay-at tex ov margin)))

(defun calctex-latex-fragment-at-point ()
  "Returns the LaTeX fragment at point, or nil if none"
  (let ((ctx (org-element-context)))
    (if (or (eq 'latex-fragment (org-element-type ctx))
            (eq 'latex-environment (org-element-type ctx)))
        ctx
      nil)))

(defun calctex--modification-hook (ov after beg end &optional len)
  (message "overlay modified: %s" ov)
  (condition-case nil
      (calctex--render-overlay-at-point)
    (error (progn
             (message "error happened!")
             (calctex--remove-overlay-at-point)))))

(defun calctex--render-overlay-at-point ()
  (let ((frag (calctex-latex-fragment-at-point)))
    (if frag
        (let ((ov (calctex--get-or-create-overlay-at-frag frag)))
          (if ov
              (progn
                (calctex--render-overlay-at-frag frag ov)
                (setq calctex--last-overlay ov
                      calctex--last-frag frag)
                )
            ()))
      ())))

(defun org-calctex-overlay-at-point ()
  (let* ((overlays (overlays-at (point)))
         (overlays (seq-filter (lambda (ov)
                                 (eq (overlay-get ov 'calctex-overlay-type) 'calctex-overlay))
                               overlays)))
    (car overlays)))

(defun org-calctex-hide-overlay-at-point ()
  (interactive)
  (let* ((ov (org-calctex-overlay-at-point)))
    (if ov (delete-overlay ov)
      (calctex--render-overlay-at-point))))

(defun calctex-move-to-end-of-frag ()
  (let ((ov (org-calctex-overlay-at-point)))
    (if ov (goto-char (overlay-end ov)))))

(defun calctex--marker-within-frag (marker frag)
  (if marker
      (let* ((begin (org-element-property :begin frag))
             (pt (- marker begin)))
        pt)
    nil))

;; Activate all formulas for embedded mode
(defun org-calctex--activate-all ()
  (interactive)
  ;; Straight out of org.el
  (let* ((math-regexp "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}")
         (cnt 0))
    (goto-char (point-min))
    (while (re-search-forward math-regexp (point-max) t)
      (let* ((context (org-element-context))
             (type (org-element-type context)))
        (when (memq type '(latex-environment latex-fragment))
          (calc-embedded nil))))))

;;; Movement

(defun org-calctex-next-formula ()
  (interactive)
  (let* ((frag (calctex-latex-fragment-at-point))
         (math-regexp "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}")
         )
    (progn
      (if frag
          (progn
            (goto-char (org-element-property :end frag))
            (forward-char))
        ())
      (re-search-forward math-regexp (point-max) t)
      (let* ((frag (calctex-latex-fragment-at-point))
             (begin (org-element-property :begin frag)))
        (goto-char begin))
      (setq disable-point-adjustment t))))

(defun org-calctex-prev-formula ()
  (interactive)
  (let* ((frag (calctex-latex-fragment-at-point))
         (math-regexp "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}"))
    (progn
      (if frag
          (progn
            (goto-char (org-element-property :begin frag))
            (backward-char))
        ())
      (re-search-backward math-regexp (point-min) t)
      (if (eq ?$ (char-after (point)))
          ;; Go backwards a character because frag-at-point doesn't work on the closing $
          (backward-char))
      (let* ((frag (calctex-latex-fragment-at-point))
             (begin (org-element-property :begin frag)))
        (goto-char begin))
      (setq disable-point-adjustment t))))

;;; Activate/Accept

;; Activate the formula at point with calc Embedded mode.
(defun org-calctex-activate-formula ()
  "Activate the formula at point with calc Embedded mode,
and opens the calc application to edit it..

Saves point in the register `f'. The function `org-calctex-accept-formula'
jumps back to register `f'."
  (interactive)
  (let* ((frag (calctex-latex-fragment-at-point)))
    (if frag
        (progn
          (goto-char (org-element-property :begin frag))
          ; 102 = f
          (point-to-register 102 '(4))
          (when (equal (char-after (point)) ?$) (forward-char))
          (calc-embedded nil)
          ;(calctex--render-overlay-at-point)
          (calc))
      (message "no frag"))))

(defun org-calctex-accept-formula ()
  "Accept the formula and jump to the end of it"
  (interactive)
  ; 102 = f
  (jump-to-register 102)
  (calc-embedded t)
  (let ((frag (calctex-latex-fragment-at-point)))
    (goto-char (org-element-property :end frag))))

;;; Insertion

(defun calctex-append-inline-formula ()
  (interactive)
  (let* ((frag (calctex-latex-fragment-at-point)))
    (if frag
        (progn
          (goto-char (org-element-property :end frag))
          (save-excursion (insert " "))
          )))
  (let ((calc-embedded-open-new-formula "$")
        (calc-embedded-close-new-formula "$"))
    (progn
      (calc-embedded-new-formula)
      (bookmark-set "calctex-formula")
      (calctex--render-overlay-at-point)
      (calc))))

(defun calctex-insert-display-formula ()
  (interactive)
  (evil-insert-newline-below)
  (let ((calc-embedded-open-new-formula "\\[ ")
        (calc-embedded-close-new-formula " \\]"))
    (progn
      (calc-embedded-new-formula)
      (bookmark-set "calctex-formula")
      (calctex--render-overlay-at-point)
      (calc))))

(with-eval-after-load 'evil-org
  (evil-define-key 'normal evil-org-mode-map
    "o" '(lambda ()
           (interactive)
           (calctex-move-to-end-of-frag)
           (evil-org-eol-call 'clever-insert-item))))

;;; Post-insert hooks

(defun calctex-mode-hook-hook ()
  (message "hooked: %s" calctex-mode)
  (add-hook 'post-self-insert-hook #'org-calctex-complete-and-activate-formula nil t))
(add-hook 'calctex-mode-hook #'calctex-mode-hook-hook)

(defun org-calctex-complete-and-activate-formula ()
  (message "char: %s" (char-before))
  (cond ((equal (char-before) 36) (org-calctex--complete-inline-formula))
        ((equal (char-before) 91) (org-calctex--maybe-complete-and-activate-display-formula))))

(defun org-calctex--complete-inline-formula ()
  (save-excursion
    (backward-char)
    (when (calctex-latex-fragment-at-point)
      (org-calctex-hide-overlay-at-point))))

(defun org-calctex--maybe-complete-and-activate-display-formula ()
  ; Check if we just typed "\["
  (if (save-excursion
        (backward-char)
        (equal (char-before) 92))
      (progn
        ; Ensure the equation is on its own line
        (save-excursion
          (backward-char 2)
          (unless (equal (char-before) 10)
            (newline)))
        (save-excursion
          (move-end-of-line nil)
          (insert "\\]")
          (org-calctex-hide-overlay-at-point)
          (org-calctex-activate-formula))
        )))

(provide 'org-calctex)
