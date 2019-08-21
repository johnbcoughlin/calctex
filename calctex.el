;;; calctex.el --- WYSIWYG LaTeX equation editing with calc.el -*- lexical-binding: t; -*-

;; Author: Jack Coughlin <jack@johnbcoughlin.com>
;; URL: https://github.com/johnbcoughlin/calctex
;; Package-Version: 0.1
;; Package-Requires: ((emacs "24.4"))

;;; Commentary:

;; CalcTeX is an Emacs minor mode that turns the built-in Emacs calculator into
;; a symbolic equation editor.

;;; Code:

(require 'subr-x)
(require 'calc-sel)

(defvar calctex-render-process nil
  "Function that renders a snippet of LaTeX source into an image.
Will be called with SRC, the LaTeX source code. Should return a
plist with properties 'file and 'type, representing the path to the
rendered image and the image type.")

(defvar calctex-latex-image-directory "~/calctex/")

(defvar calctex-base-dpi 150
  "The base dots-per-inch measurement to use for png rendering.
A higher value will give sharper images")

(defvar calctex-imagemagick-png-scaling 0.35
  "Controls the amount to scale a PNG image *down* by.

This is to compensate for the image size change from a higher
value of calctex-base-dpi")

(defvar calctex-imagemagick-enabled-p t
  "Whether imagemagick image display should be used.

If Emacs has not been compiled with imagemagick support, this
variable will have no effect.

Note that imagemagick is required to scale rendered images
*down*. Downscaling allows calctex to render images at a higher
DPI without blowing up the display size in the buffer. If this is
enabled, then, rendered images may appear grainy on some
displays.")

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
  "The LaTeX preamble to use when rendering equation sources.
This will be placed at the front of a file, and followed by the
background and foreground color definitions, then by
\begin{document} <EQUATION> \end{document}")

(setq calctex-render-process
      (lambda (src)
        (if (file-exists-p calctex-latex-image-directory)
            ()
          (make-directory calctex-latex-image-directory 'parents))
        (let* ((fg (calctex-latex-color :foreground))
               (bg (calctex-latex-color :background))
               (hash (sha1 (prin1-to-string (list src fg bg (calctex--dpi)))))
               (absprefix (expand-file-name calctex-latex-image-directory "calctex-ltximg"))
               (tofile (format "%s_%s.png" absprefix hash))
               (latex-header calctex-format-latex-header)
               (tmpdir temporary-file-directory)
               (texfilebase (make-temp-name
                             (expand-file-name "calctex_" tmpdir)))
               (texfile (concat texfilebase ".tex"))
               (base-name (file-name-base texfile))
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
                                     fg bg (calctex--dpi) tofile dvi-output))
                    (log-buf (get-buffer-create "*CalcTeX Log*")))
                (save-window-excursion
                  (message "%s" latex-cmd)
                  (shell-command latex-cmd log-buf)
                  (unless (file-exists-p dvi-output)
                    (error "Error rendering latex to dvi. Check *CalcTeX Log* for command output"))
                  (message "%s" png-cmd)
                  (shell-command png-cmd log-buf)
                  (unless (file-exists-p tofile)
                    (error "Error converting dvi to png. Check *CalcTeX Log* for command output")))))
          `(file ,tofile type png))))


(defvar calctex--last-overlay nil)
(defvar calctex--calc-line-numbering nil
  "Sidechannel used to store the value of variable `calc-line-numbering'.
This is used to modify that variable during hook execution and
then restore its value.")

(defvar calctex--calc-line-breaking nil
  "Sidechannel used to store the value of variable `calc-line-breaking'.
This is used to modify that variable during hook execution and
then restore its value.")

(defvar calctex--calc-highlight-selections-with-faces nil
  "Sidechannel for the value of `calc-highlight-selections-with-faces'.
This is used to modify that variable during hook execution and
then restore its value.")

(define-minor-mode calctex-mode
  "Turn calc into an editor for rendered LaTeX equations."
  nil
  "cTX"
  :keymap (make-sparse-keymap)
  (if calctex-mode
      (progn
        (add-hook 'pre-command-hook 'calctex--precommand)
        (add-hook 'post-command-hook 'calctex--postcommand)
        (add-hook 'post-self-insert-hook 'calctex--postcommand)
        (setq calctex--calc-line-breaking calc-line-breaking)
        (setq calctex--calc-highlight-selections-with-faces calc-highlight-selections-with-faces)
        (setq calc-highlight-selections-with-faces t)
        (calc-show-selections -1)
        (calc-latex-language nil))
    (remove-hook 'pre-command-hook 'calctex--precommand)
    (remove-hook 'post-command-hook 'calctex--postcommand)
    (remove-hook 'post-self-insert-hook 'calctex--postcommand)
    (setq calc-highlight-selections-with-faces calctex--calc-highlight-selections-with-faces)
    (calctex--remove-overlays)))

(defun calctex--precommand ()
  "The precommand hook to run.

Stores the value of calc variables for restoration later."
  (progn
    (setq calctex--calc-line-numbering calc-line-numbering)
    (setq calctex--calc-line-breaking calc-line-breaking)
    (setq calc-line-breaking nil)))

(defun calctex--postcommand ()
  "The postcommand hook to run.

Renders line overlays in the calc buffer."
  (progn
    ;; This function will override the variables used by the previous one
    (calctex--create-line-overlays)
    (setq calc-line-breaking calctex--calc-line-breaking)))

(defun calctex--get-or-create-overlay (beg end)
  "Get or create a calctex overlay between BEG and END."
  (let* ((overlays (cl-remove-if-not
                    (lambda (o) (eq (overlay-get o 'calctex-overlay-type) 'calctex-overlay))
                    (overlays-in beg end)))
         (overlay (if overlays (car overlays) nil)))
    (if overlay
        overlay
      (let ((ov (make-overlay beg end)))
        (progn (overlay-put ov 'calctex-overlay-type 'calctex-overlay)
               (overlay-put ov 'priority -60)
               (overlay-put ov 'evaporate t)
               (overlay-put ov 'calctex-overlay-id (sha1 (buffer-substring beg end)))
               ov)))))

(defun calctex--render-overlay-at (tex ov)
  "Render LaTeX source TEX onto the overlay OV."
  (let* ((img (funcall calctex-render-process tex))
         (img-file (plist-get img 'file))
         (img-type (plist-get img 'type)))
    (progn
      (if img-file
          (overlay-put
           ov
           'display
           (calctex--image-overlay-display img-type img-file)))
      (setq disable-point-adjustment t))))

(defun calctex--image-overlay-display (img-type img-file)
  "Return the 'display property of an image overlay.

Returns a 'display form for IMG-FILE rendered as IMG-TYPE. If
imagemagick support is enabled, use that, otherwise, fall back to
.png."
  (if (calctex--imagemagick-support)
      (list 'image
            :type 'imagemagick
            :format img-type
            :file img-file
            :ascent 'center
            :scale calctex-imagemagick-png-scaling
            :margin 4)
    (list 'image
          :type img-type
          :file img-file
          :ascent 'center
          :margin 4)))

(defun calctex--imagemagick-support ()
  "Whether imagemagick support for images is available and enabled."
  (and (image-type-available-p 'imagemagick) calctex-imagemagick-enabled-p))

(defun calctex--dpi ()
  "Compute the render DPI to request from dvipng."
  (if (calctex--imagemagick-support)
      (round (/ calctex-base-dpi calctex-imagemagick-png-scaling))
    calctex-base-dpi))

(defun calctex-latex-color (attr)
  "Return a RGB color for the LaTeX color package.

Selects the attribute ATTR of the 'default face, and formats it
as an RGB color value."
  (calctex-latex-color-format (face-attribute 'default attr nil)))

(defun calctex-latex-color-format (color-name)
  "Convert COLOR-NAME to a RGB color value."
  (apply #'format "%s %s %s"
         (mapcar 'calctex-normalize-color
                 (color-values color-name))))

;; Copied from org-normalize-color
(defun calctex-normalize-color (value)
  "Convert VALUE to a string usable as a LaTeX RGB color component."
  (format "%g" (/ value 65535.0)))

(defun calctex--remove-overlay-at-point ()
  "Remove the calctex overlay at point, if any."
  (let ((ov (calctex--overlay-at-point)))
    (if ov
        (delete-overlay ov)
      ())))

(defun calctex--overlay-at-point ()
  "Find the calctex overlay at point."
  (car (cl-remove-if-not
        (lambda (o) (eq (overlay-get o 'calctex-overlay-type) 'calctex-overlay))
        (overlays-at (point)))))

;; Create or update an overlay on every calc stack entry
(defun calctex--create-line-overlays ()
  "Render overlays on all lines of the *Calculator* buffer."
  (when (and (string= calc-language "latex")
             (string= "*Calculator*" (buffer-name)))
    (goto-char (point-min))
    ; Skip the header line "--- Emacs Calculator Mode ---"
    (forward-line 1)
    (while (not (eobp))
      (calctex--overlay-line)
      (forward-line 1))))

;; Create or update an overlay on the line at point
(defun calctex--overlay-line ()
  "Render an overlay on one line of the *Calculator* buffer.

Called by calctex--create-line-overlays."
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
        (calctex--render-overlay-at tex ov)))))

(defun calctex--lift-selection (text line-start line-end)
  "Wrap selected portions of a LaTeX formula in a color directive.

Wrap portions of the given line bounded by LINE-START and
LINE-END in TEXT, which are displayed using the face
'calc-selected-face, in a LaTeX macro which will color them in
the rendered output."
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
                 (substring text max)))
            text))))))

(defun calctex--remove-overlays ()
  "Remove all overlays from the *Calculator* buffer."
  (with-current-buffer "*Calculator*"
    (dolist (ov (overlays-in (point-min) (point-max)))
      (delete-overlay ov))))

(provide 'calctex)

;;; calctex.el ends here
