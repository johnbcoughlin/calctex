;;; calctex.el --- WYSIWYG LaTeX equation editing with calc.el -*- lexical-binding: t; -*-

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

;; CalcTeX is an Emacs minor mode that turns the built-in Emacs calculator into
;; a symbolic equation editor.

;;; Code:

(require 'subr-x)
(require 'calc-sel)
(require 'color)

(defcustom calctex-base-dpi 150
  "The base dots-per-inch measurement to use for png rendering.
A higher value will give sharper images"
  :type '(integer)
  :group 'calctex)

(defcustom calctex-base-imagemagick-png-scaling 2.85
  "Controls the amount to increase the dots-per-inch requested from
the rendering program, solely to compensate for image graininess.
If imagemagick is available, calctex will render an image that is this
much higher resolution, and scale it back down to the same size."
  :type '(float)
  :group 'calctex)

(defcustom calctex-imagemagick-png-scaling 1.0
  "Controls the amount by which images will be scaled when rendered to
the buffer. In combination with calctex-base-imagemagick-png-scaling,
allows one to control the size of a rendered image on the page, without
sacrificing dots-per-inch."
  :type '(float)
  :group 'calctex)

(defcustom calctex-imagemagick-enabled-p t
  "Whether imagemagick image display should be used.

If Emacs has not been compiled with imagemagick support, this
variable will have no effect.

Note that imagemagick is required to scale rendered images
*down*. Downscaling allows calctex to render images at a higher
DPI without blowing up the display size in the buffer. If this is
enabled, then, rendered images may appear grainy on some
displays."
  :type '(boolean)
  :group 'calctex)

(defcustom calctex-foreground-darken-percent 0
  "The percentage amount by which to darken the foreground of the
rendered image. Darkening may be desirable to improve the contrast of the
rendered equations, whose font is typically finer than the font in the
rest of the buffer, and so appears lighter.

This value may be negative, in which case it lightens the foreground."
  :type '(integer)
  :group 'calctex)

(defvar calctex-render-process #'calctex-default-render-process
  "Function that renders a snippet of LaTeX source into an image.
Will be called with SRC, the LaTeX source code. Should return a
plist with properties 'file and 'type, representing the path to the
rendered image and the image type.")

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
\\addtolength{\\topmargin}{-2.54cm}
\\usepackage{xcolor}
\\usepackage{soul}
\\usepackage{adjustbox}
\\usepackage{amsmath}
\\usepackage{amssymb}
\\usepackage{siunitx}

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

;;;###autoload
(defun calctex-default-render-process (src)
  "The default function that calctex will use to render LaTeX SRC."
  (let* ((fg (calctex--latex-color :foreground (- calctex-foreground-darken-percent)))
         (bg (calctex--latex-color :background))
         (hash (sha1 (prin1-to-string (list src fg bg (calctex--dpi) calctex-format-latex-header))))
         (absprefix (expand-file-name "calctex-ltximg" (temporary-file-directory)))
         (tofile (format "%s_%s.png" absprefix hash))
         (latex-header calctex-format-latex-header)
         (tmpdir temporary-file-directory)
         (texfilebase (make-temp-name
                       (expand-file-name "calctex_" tmpdir)))
         (texfile (concat texfilebase ".tex"))
         (base-name (file-name-base texfile))
         (out-dir (or (file-name-directory texfile) "./"))
         (dvi-output (expand-file-name (concat base-name ".dvi") out-dir)))
    (unless (file-exists-p tofile)
      (with-temp-file texfile
        (insert "\\newcommand{\\cmt}[1]{\\ignorespaces}"
                "\\begin{document}\n"
                "\\definecolor{fg}{rgb}{" fg "}\n"
                "\\definecolor{bg}{rgb}{" bg "}\n"
                "\n\\pagecolor{bg}\n"
                "\n{\\color{fg}\n"
                src
                "\n}\n"
                "\n\\end{document}\n"))
      (let ((latex-cmd
             (format "latex -fmt %s -interaction nonstopmode -output-directory %s %s"
                     calctex--preprocessed-latex-header-file out-dir texfile))
            (png-cmd
             (format "dvipng -fg \"rgb %s\" -bg \"rgb %s\" -D %s -T tight -o %s %s"
                     fg bg (calctex--dpi) tofile dvi-output))
            (log-buf (get-buffer-create "*CalcTeX Log*")))
        (save-window-excursion
          (shell-command latex-cmd log-buf)
          (unless (file-exists-p dvi-output)
            (error "Error rendering latex to dvi. Check *CalcTeX Log* for command output"))
          (shell-command png-cmd log-buf)
          (unless (file-exists-p tofile)
            (error "Error converting dvi to png. Check *CalcTeX Log* for command output")))))
    `(file ,tofile type png)))

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

;;;###autoload
(define-minor-mode calctex-mode
  "Turn calc into an editor for rendered LaTeX equations."
  nil
  "cTX"
  :keymap (make-sparse-keymap)
  (if calctex-mode
      (progn
        (add-hook 'pre-command-hook #'calctex--precommand)
        (add-hook 'post-command-hook #'calctex--postcommand)
        (add-hook 'post-self-insert-hook #'calctex--postcommand)
        (calctex--preprocess-latex-header)
        (setq calctex--calc-line-breaking calc-line-breaking)
        (setq calctex--calc-highlight-selections-with-faces calc-highlight-selections-with-faces)
        (setq calc-highlight-selections-with-faces t)
        (save-excursion
          (progn
            (message "Starting calc")
            (unless (get-buffer "*Calculator*") (calc))
            (calc-show-selections -1)
            (calc-latex-language nil))))
    (remove-hook 'pre-command-hook #'calctex--precommand)
    (remove-hook 'post-command-hook #'calctex--postcommand)
    (remove-hook 'post-self-insert-hook #'calctex--postcommand)
    (setq calc-highlight-selections-with-faces calctex--calc-highlight-selections-with-faces)
    (calctex--remove-overlays)))

(defvar calctex--preprocessed-latex-header-file nil
  "Location of the cached latex header .fmt file.")

(defun calctex--preprocess-latex-header ()
  (let* ((tmpdir temporary-file-directory)
         (texfilebase (make-temp-name (expand-file-name "calctex-header" tmpdir)))
         (texfile (concat texfilebase ".tex"))
         (header-hash (sha1 calctex-format-latex-header))
         (header-file-name (expand-file-name (format "header%s.fmt" header-hash) tmpdir)))
    (unless (and (file-exists-p header-file-name)
                 (equal header-file-name calctex--preprocessed-latex-header-file))
      (progn
        (message "redoing header file")
        (with-temp-file texfile
          (insert calctex-format-latex-header))
        (let* ((cmd (format "cd %s && latex -ini -jobname=\"header%s\" \"&latex %s\\dump\"" tmpdir header-hash texfile))
               (log-buf (get-buffer-create "*CalcTeX Log*"))
               (result (shell-command cmd log-buf)))
          (if (equal result 0)
              (setq calctex--preprocessed-latex-header-file header-file-name)
            (error "Error preprocessing the LaTeX header. See *CalcTeX Log* for details.")))))))

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
         (overlay (when overlays (car overlays))))
    (if overlay
        overlay
      (let ((ov (make-overlay beg end)))
        (progn (overlay-put ov 'calctex-overlay-type 'calctex-overlay)
               (overlay-put ov 'priority -60)
               (overlay-put ov 'evaporate t)
               (overlay-put ov 'calctex-overlay-id (sha1 (buffer-substring beg end)))
               ov)))))

(defun calctex--render-overlay-at (tex ov margin)
  "Render LaTeX source TEX onto the overlay OV."
  (let* ((img (funcall calctex-render-process tex))
         (img-file (plist-get img 'file))
         (img-type (plist-get img 'type)))
    (progn
      (if img-file
          (overlay-put
           ov
           'display
           (calctex--image-overlay-display img-type img-file margin)))
      )))

(defun calctex--image-overlay-display (img-type img-file margin)
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
            :scale (/ calctex-imagemagick-png-scaling calctex-base-imagemagick-png-scaling)
            :margin margin)
    (list 'image
          :type img-type
          :file img-file
          :ascent 'center
          :margin margin)))

(defun calctex--imagemagick-support ()
  "Whether imagemagick support for images is available and enabled."
  (and (image-type-available-p 'imagemagick) calctex-imagemagick-enabled-p))

(defun calctex--dpi ()
  "Compute the render DPI to request from dvipng."
  (if (calctex--imagemagick-support)
      (round (* calctex-base-dpi calctex-base-imagemagick-png-scaling))
    calctex-base-dpi))

(defun calctex--latex-color (attr &optional lighten-percent)
  "Return a RGB color for the LaTeX color package.

Selects the attribute ATTR of the 'default face, lightens
it by the given percentage,and formats it as an RGB color value.

The LIGHTENPERCENT parameter may be negative, which darkens the color."
  (calctex--latex-color-format (color-lighten-name (face-attribute 'default attr nil) (or lighten-percent 0))))

(defun calctex--latex-color-format (color-name)
  "Convert COLOR-NAME to a RGB color value."
  (apply #'format "%s %s %s"
         (mapcar 'calctex--normalize-color
                 (color-values color-name))))

;; Copied from org-normalize-color
(defun calctex--normalize-color (value)
  "Convert VALUE to a string usable as a LaTeX RGB color component."
  (format "%g" (/ value 65535.0)))

(defun calctex--remove-overlay-at-point ()
  "Remove the calctex overlay at point, if any."
  (let ((ov (calctex--overlay-at-point)))
    (when ov (delete-overlay ov) ())))

(defun calctex--overlay-at-point ()
  "Find the calctex overlay at point."
  (car (cl-remove-if-not
        (lambda (o) (eq (overlay-get o #'calctex-overlay-type) #'calctex-overlay))
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

Called by `calctex--create-line-overlays'."
  (when (string=
       "."
       (string-trim (buffer-substring
                     (line-beginning-position)
                     (line-end-position)))))
    (let* ((line-start (if calctex--calc-line-numbering
                           (+ (line-beginning-position) 4)
                         (line-beginning-position)))
           (line-end (line-end-position))
           (line-contents (buffer-substring line-start line-end))
           (selected-line-contents (calctex--lift-selection line-contents line-start line-end))
           (ov (calctex--get-or-create-overlay line-start line-end))
           (tex (format "\\begin{align*} %s \\end{align*}" selected-line-contents)))
      (progn
        (move-overlay ov line-start line-end)
        (calctex--render-overlay-at tex ov 2))))

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
                    (when (not selected) (setq max pt))
                  (when selected (setq min pt)))
                (forward-char))))
          (if min
              (progn
                (when (not max) (setq max (point)))
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
