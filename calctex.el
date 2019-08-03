(require 'org)
(require 'subr-x)

(defvar calctex-render-process nil
  "Function that renders a snippet of LaTeX source into an image.
Will be called with SRC, the LaTeX source code. Should return a
plist with properties 'file and 'type, representing the path to the
rendered image and the image type.
")

(defvar calctex-latex-image-directory "~/org/ltximg/")
(defvar calctex-dpi 400)

(setq calctex-render-process
      (lambda (src)
        (let* ((fg (calctex-latex-color :foreground))
               (bg (calctex-latex-color :background))
               (hash (sha1 (prin1-to-string (list src fg bg))))
               (absprefix (expand-file-name calctex-latex-image-directory "calctex-ltximg"))
               (tofile (format "%s_%s.png" absprefix hash))
               (options '(:background default :foreground default))
               (latex-header calctex-format-latex-header)
               (tmpdir temporary-file-directory)
               (texfilebase (make-temp-name
                             (expand-file-name "calctex_" tmpdir)))
               (texfile (concat texfilebase ".tex"))
               (base-name (file-name-base texfile))
               (full-name (file-truename texfile))
               (out-dir (or (file-name-directory texfile) "./"))
               (dvi-output (expand-file-name (concat base-name ".dvi") out-dir))
               )
          (if (file-exists-p tofile)
              ()
            (with-temp-file texfile
              (insert latex-header)
              (insert "\n\\begin{document}\n"
                      "\\definecolor{fg}{rgb}{" fg "}\n"
                      "\\definecolor{bg}{rgb}{" bg "}\n"
                      "\n\\pagecolor{bg}\n"
                      "\n{\\color{fg}\n"
                      src
                      "\n}\n"
                      "\n\\end{document}\n"))
              (let ((latex-cmd (format "latex -interaction nonstopmode -output-directory %s %s" out-dir texfile))
                    (png-cmd (format "dvipng -fg \"rgb %s\" -bg \"rgb %s\" -D %s -T tight -o %s %s"
                                     fg bg calctex-dpi tofile dvi-output))
                    (log-buf (get-buffer-create "*CalcTeX Log*")))
                (shell-command latex-cmd log-buf)
                (shell-command latex-cmd log-buf)
                (unless (file-exists-p dvi-output)
                  (error "dvi output not created"))
                (shell-command png-cmd)))
          `(file ,tofile type png))))

(defvar calctex-format-latex-header
  "
\\documentclass{article}
\\usepackage[usenames]{color}
\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}% Set up highlighting for simulating the cursor
\\usepackage{xcolor}
%\\setlength{\\fboxsep}{0pt}
\\usepackage{soul}
\\usepackage{adjustbox}

\\usepackage{xparse}

\\NewDocumentCommand{\\colornucleus}{omme{_^}}{%
  \\begingroup\\colorlet{currcolor}{.}%
  \\IfValueTF{#1}
   {\\textcolor[#1]{#2}}
   {\\textcolor{#2}}
    {%
     #3% the nucleus
     \\IfValueT{#4}{_{\\textcolor{currcolor}{#4}}}% subscript
     \\IfValueT{#5}{^{\\textcolor{currcolor}{#5}}}% superscript
    }%
  \\endgroup
}
"
  "docstring")

(defvar calctex--last-overlay nil)
(defvar calctex--last-frag nil
  "Sidechannel used to store the value of calc-line-numbering, so that it is usable
in our hook without depending on hook execution ordering.")
(defvar calctex--calc-line-numbering nil)
(defvar calctex--calc-line-breaking)

(define-minor-mode calctex-mode
  "Toggle CalcTeX mode."
  nil
  "cTX"
  :keymap (make-sparse-keymap)
  (if calctex-mode
      (progn
        (add-hook 'pre-command-hook 'calctex--precommand)
        (add-hook 'post-command-hook 'calctex--postcommand)
        (add-hook 'post-self-insert-hook 'calctex--postcommand)
        (setq calctex--calc-line-breaking calc-line-breaking)
        )
    (remove-hook 'pre-command-hook 'calctex--precommand)
    (remove-hook 'post-command-hook 'calctex--postcommand)
    (remove-hook 'post-self-insert-hook 'calctex--postcommand)
    (calctex--remove-overlays)
    ))

(defun calctex--precommand ()
  (progn
    (setq calctex--calc-line-numbering calc-line-numbering)
    (setq calctex--calc-line-breaking calc-line-breaking)
    (setq calc-line-breaking nil)
    ))

(defun calctex--postcommand ()
  (progn
    (calctex--render-just-exited-overlay)
    ;; This function will override the variables used by the previous one
    (calctex--create-line-overlays)
    (setq calc-line-breaking calctex--calc-line-breaking)
))

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

(defun calctex--render-just-exited-overlay ()
  (if (and (not (calctex-latex-fragment-at-point))
           calctex--last-overlay
           calctex--last-frag)
      (progn
        (calctex--render-overlay-at-frag calctex--last-frag calctex--last-overlay)
        (setq calctex--last-overlay nil
              calctex--last-frag nil))))

(defun calctex--modification-hook (ov after beg end &optional len)
  (condition-case nil
      (calctex--render-overlay-at-point)
    (error (progn
             (message "error happened!")
             (calctex--remove-overlay-at-point)))))

(defun calctex--get-or-create-overlay-at-frag (frag)
  (let* ((beg (org-element-property :begin frag))
         (end (save-excursion
                (goto-char (org-element-property :end frag))
                (skip-chars-backward " \r\t\n")
                (point))))
    (calctex--get-or-create-overlay beg end)))

(defun calctex--get-or-create-overlay (beg end)
  (let* ((overlays (cl-remove-if-not
                    (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-calctex-overlay))
                    (overlays-in beg end)))
         (overlay (if overlays (car overlays) nil)))
    (if overlay
        overlay
      (let ((ov (make-overlay beg end)))
        (progn (overlay-put ov 'org-overlay-type 'org-calctex-overlay)
               (overlay-put ov 'priority -60)
               (overlay-put ov 'evaporate t)
               (overlay-put ov 'modification-hooks (list 'calctex--modification-hook))
               (overlay-put ov 'calctex-overlay-id (sha1 (buffer-substring beg end)))
               ov)))))

(defun calctex--render-overlay-at-frag (frag ov)
  (let* ((tex (org-element-property :value frag)))
    (calctex--render-overlay-at tex ov)))

(defun calctex--render-overlay-at (tex ov)
  (let* ((fg (calctex-latex-color :foreground))
         (cursor-color (calctex-latex-color-format (face-background 'cursor)))
         (img (funcall calctex-render-process tex))
         (img-file (plist-get img 'file))
         (img-type (plist-get img 'type)))
    (progn
      (if img-file
          (overlay-put ov
                       'display
                       (list 'image
                             :type 'imagemagick
                             :format img-type
                             :file img-file
                             :ascent 'center
                             :scale 0.35
                             :margin 4
                             )))
      (setq disable-point-adjustment t))))

(defun calctex-latex-color (attr)
  "Return a RGB color for the LaTeX color package."
  (calctex-latex-color-format (face-attribute 'default attr nil)))

(defun calctex-latex-color-format (color-name)
  "Convert COLOR-NAME to a RGB color value."
  (apply #'format "%s %s %s"
         (mapcar 'org-normalize-color
                 (color-values color-name))))

(defun calctex--overlay-image-file (ov)
  (let* ((id (overlay-get ov 'calctex-overlay-id))
         (file (buffer-file-name (buffer-base-buffer)))
         (parent-dir (if (or (not file) (file-remote-p file))
                  temporary-file-directory
                  default-directory))
         (dir (concat parent-dir calctex-latex-image-directory))
         (img_file (concat dir (format "org-ltximg_%s.png" id))))
    img_file))

(defun calctex-latex-fragment-at-point ()
  "Returns the LaTeX fragment at point, or nil if none"
  (let ((ctx (org-element-context)))
    (if (or (eq 'latex-fragment (org-element-type ctx))
            (eq 'latex-environment (org-element-type ctx)))
        ctx
      nil)))

(defun calctex--overlay-at-point ()
  (car (cl-remove-if-not
        (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-calctex-overlay))
        (overlays-at (point)))))

(defun calctex--remove-overlay-at-point ()
  (let ((ov (calctex--overlay-at-point)))
    (if ov
        (delete-overlay ov)
      ())))

(defun calctex-overlay-wrapper (body)
  (progn
    ;; Attempt the operation
    (condition-case nil
        (funcall body)
      ;; If it fails, remove the overlay at point if any
      (error (progn
                       (message "error happened!")
                       (calctex--remove-overlay-at-point))))
    (let ((overlay (calctex--overlay-at-point)))
      (if overlay
          (progn
            (setq disable-point-adjustment t)
            (setq cursor-type nil))
        (setq cursor-type t)))))

;; Create or update an overlay on every calc stack entry
(defun calctex--create-line-overlays ()
  (if (and (string= calc-language "latex")
           (string= "*Calculator*" (buffer-name)))
      (progn
        (goto-char (point-min))
        ; Skip the header line --- Emacs Calculator Mode ---
        (forward-line 1)
        (while (not (eobp))
          (calctex--overlay-line)
          (forward-line 1)))
    ()))

;; Create or update an overlay on the line at point
(defun calctex--overlay-line ()
  (if (string=
       "."
       (string-trim (buffer-substring
                     (line-beginning-position)
                     (line-end-position))))
      ()
    (let* ((line-start (if calctex--calc-line-numbering
                           (+ (line-beginning-position) 4)
                         (line-beginning-position)))
           (line-end (line-end-position))
           (line-contents (buffer-substring line-start line-end))
           (selected-line-contents (calctex--lift-selection line-contents line-start line-end))
           (ov (calctex--get-or-create-overlay line-start line-end))
           (tex (format "\\[ %s \\]" selected-line-contents)))
      (progn
        (move-overlay ov line-start line-end)
        (calctex--render-overlay-at tex ov))
          )))

(defun calctex--lift-selection (text line-start line-end)
  (save-excursion
    (progn
      (goto-char line-start)
      (let ((min)
            (max))
        (progn
          (while (and (< (point) line-end)
                      (not max))
            (let* ((pt (point))
                   (face (get-text-property pt 'face))
                   (selected (equal 'calc-selected-face face)))
              (progn
                (if min
                    (if (not selected)
                        (setq max pt)
                      ())
                  (if selected
                      (setq min pt)
                    ()))
                (forward-char))))
          (if min
              (progn
                (if (not max)
                    (setq max (point))
                  ())
                (setq min (- min line-start))
                (setq max (- max line-start))
                (concat
                 (substring text 0 min)
                 "\\colornucleus{red}{"
                 (substring text min max)
                 "}"
                 (substring text max))
                )
            text)
        )
      ))))


(defun calctex-hide-overlay-at-point ()
  (interactive)
  (let* ((overlays (overlays-at (point)))
         (overlays (seq-filter (lambda (ov)
                                 (eq (overlay-get ov 'org-overlay-type) 'org-calctex-overlay))
                               overlays)))
    (if (eq 0 (length overlays))
        (calctex--render-overlay-at-point)
      (delete-overlay (car overlays)))))

(defun calctex--remove-overlays ()
  (with-current-buffer "*Calculator*"
    (dolist (ov (overlays-in (point-min) (point-max)))
      (delete-overlay ov))))

(defun calctex--marker-within-frag (marker frag)
  (if marker
      (let* ((begin (org-element-property :begin frag))
             (pt (- marker begin)))
        pt)
    nil))

;; Activate all formulas for embedded mode
(defun calctex--activate-all ()
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

(defun calctex-next-formula ()
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

(defun calctex-prev-formula ()
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

;; Activate the formula at point with calc Embedded mode.
(defun calctex-activate-formula ()
  (interactive)
  (let* ((frag (calctex-latex-fragment-at-point)))
    (if frag
        (progn
          (goto-char (org-element-property :begin frag))
          ;; Set a bookmark to jump back to
          (forward-char)
          (bookmark-set "calctex-formula")
          (calc-embedded nil)
          (goto-char (org-element-property :begin frag))
          (calctex--render-overlay-at-point)
          (calc)))))

(defun calctex-accept-formula ()
  (interactive)
  (spacemacs/alternate-window)
  (bookmark-jump "calctex-formula")
  (calc-embedded t)
  (let ((frag (calctex-latex-fragment-at-point)))
    (goto-char (org-element-property :end frag))))

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

;;;###autoload
(defun turn-on-calctex-mode ()
  "Enable calctex-mode in current buffer."
  (interactive "")
  (calctex-mode 1))

;;;###autoload
(defun turn-off-calctex-mode ()
  "Disable calctex-mode in current buffer."
  (interactive "")
  (calctex-mode -1))

(with-eval-after-load 'hydra 
  (defhydra calctex-hydra (:color red)
    "foo"
    ("h" calctex-hide-overlay-at-point "show/hide overlays")
    ("n" calctex-next-formula "next")
    ("p" calctex-prev-formula "prev")
    ("r" calctex-activate-formula "replace" :color blue)
    ("a" calctex-append-inline-formula "Append $formula$" :color blue)
    ("o" calctex-insert-display-formula "Insert \\[ display formula \\]" :color blue)
    ("q" nil "quit" :color blue)))

(define-key calctex-mode-map (kbd "s-f") 'hypertex-hydra/body)


(provide 'calctex)
