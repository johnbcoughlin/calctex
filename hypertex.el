(require 'evil)
(require 'latex)
(require 'tex)
(add-to-list 'load-path "~/src/hypertex/dist")
(require 'libhypertex)

(defvar hypertex-latex-preamble
  "
")

(defvar hypertex--last-overlay nil)
(defvar hypertex--last-frag nil)
;; Sidechannel used to store the value of calc-line-numbering, so that it is usable
;; in our hook without depending on hook execution ordering.
(defvar hypertex--calc-line-numbering nil)

;; Set up the renderer
(defun hypertex-renderer-start ()
  (setq libhypertex-renderer
        (libhypertex-start-renderer
         hypertex-latex-preamble
         10)))

(defun hypertex-renderer-stop ()
  (progn
    (libhypertex-stop-renderer libhypertex-renderer)
    (setq libhypertex-renderer nil)))

(define-minor-mode hypertex-mode
  "Toggle HyperLaTeX mode."
  nil
  " HyperTeX"
  :keymap (make-sparse-keymap)
  (if hypertex-mode
      (progn
        (add-hook 'pre-command-hook 'hypertex--precommand)
        (add-hook 'post-command-hook 'hypertex--postcommand)
        (add-hook 'post-self-insert-hook 'hypertex--postcommand)
        (hypertex-renderer-start)
        )
    (hypertex-renderer-stop)
    (remove-hook 'pre-command-hook 'hypertex--precommand)
    (remove-hook 'post-command-hook 'hypertex--postcommand)
    (remove-hook 'post-self-insert-hook 'hypertex--postcommand)
    (hypertex--remove-overlays)
    ))

(defun hypertex--precommand ()
  (progn
    (setq hypertex--calc-line-numbering calc-line-numbering)
    ))

(defun hypertex--postcommand ()
  (progn
    (hypertex--render-just-exited-overlay)
    ;; This function will override the variables used by the previous one
    (hypertex--create-line-overlays)
))

(defun hypertex--render-overlay-at-point ()
  (let ((frag (hypertex-latex-fragment-at-point)))
    (if frag
        (let ((ov (hypertex--get-or-create-overlay-at-frag frag)))
          (if ov
              (progn
                (hypertex--render-overlay-at-frag frag ov)
                (setq hypertex--last-overlay ov
                      hypertex--last-frag frag)
                )
            ()))
      ())))

(defun hypertex--render-just-exited-overlay ()
  (if (and (not (hypertex-latex-fragment-at-point))
           hypertex--last-overlay
           hypertex--last-frag)
      (progn
        (hypertex--render-overlay-at-frag hypertex--last-frag hypertex--last-overlay)
        (setq hypertex--last-overlay nil
              hypertex--last-frag nil))))

(defun hypertex--modification-hook (ov after beg end &optional len)
  (condition-case nil
      (hypertex--render-overlay-at-point)
    (error (progn
             (message "error happened!")
             (hypertex--remove-overlay-at-point)))))

(defun hypertex--get-or-create-overlay-at-frag (frag)
  (let* ((beg (org-element-property :begin frag))
         (end (save-excursion
                (goto-char (org-element-property :end frag))
                (skip-chars-backward " \r\t\n")
                (point))))
    (hypertex--get-or-create-overlay beg end)))

(defun hypertex--get-or-create-overlay (beg end)
  (let* ((overlays (cl-remove-if-not
                    (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-hypertex-overlay))
                    (overlays-in beg end)))
         (overlay (if overlays (car overlays) nil)))
    (if overlay
        overlay
      (let ((ov (make-overlay beg end)))
        (progn (overlay-put ov 'org-overlay-type 'org-hypertex-overlay)
               (overlay-put ov 'priority -60)
               (overlay-put ov 'evaporate t)
               (overlay-put ov 'modification-hooks (list 'hypertex--modification-hook))
               (overlay-put ov 'hypertex-overlay-id (sha1 (buffer-substring beg end)))
               ov)))))

(defun hypertex--render-overlay-at-frag (frag ov)
  (let* ((tex (org-element-property :value frag))
         (fg (hypertex-latex-color :foreground))
         (cursor-color (hypertex-latex-color-format (face-background 'cursor)))
         )
    (let ((img-file
               (libhypertex-render-tex
                libhypertex-renderer
                fg
                cursor-color
                tex
                "/Users/jack/org/ltximg")))
          (progn
            (if img-file
                (overlay-put ov
                             'display
                             (list 'image
                                   :type 'svg
                                   :file img-file
                                   :ascent 'center
                                   :scale 0.34
                                   ))
              ())
            (setq disable-point-adjustment t)))))

(defun hypertex--render-overlay-at (tex ov)
  (let* ((fg (hypertex-latex-color :foreground))
         (cursor-color (hypertex-latex-color-format (face-background 'cursor)))
         (img-file
          (libhypertex-render-tex
           libhypertex-renderer
           fg
           cursor-color
           tex
           "/Users/jack/org/ltximg")))
    (progn
      (if img-file
          (overlay-put ov
                       'display
                       (list 'image
                             :type 'svg
                             :file img-file
                             :ascent 'center
                             :scale 0.34
                             :margin 2
                             )))
      (setq disable-point-adjustment t))))

(defun hypertex-latex-color (attr)
  "Return a RGB color for the LaTeX color package."
  (hypertex-latex-color-format (face-attribute 'default attr nil)))

(defun hypertex-latex-color-format (color-name)
  "Convert COLOR-NAME to a RGB color value."
  (apply #'format "%s %s %s"
         (mapcar 'org-normalize-color
                 (color-values color-name))))

(defun hypertex--overlay-image-file (ov)
  (let* ((id (overlay-get ov 'hypertex-overlay-id))
         (file (buffer-file-name (buffer-base-buffer)))
         (parent-dir (if (or (not file) (file-remote-p file))
                  temporary-file-directory
                  default-directory))
         (dir (concat parent-dir org-preview-latex-image-directory))
         (img_file (concat dir (format "org-ltximg_%s.png" id))))
    img_file))

(defun hypertex-latex-fragment-at-point ()
  "Returns the LaTeX fragment at point, or nil if none"
  (let ((ctx (org-element-context)))
    (if (or (eq 'latex-fragment (org-element-type ctx))
            (eq 'latex-environment (org-element-type ctx)))
        ctx
      nil)))

(defun hypertex--overlay-at-point ()
  (car (cl-remove-if-not
        (lambda (o) (eq (overlay-get o 'org-overlay-type) 'org-hypertex-overlay))
        (overlays-at (point)))))

(defun hypertex--remove-overlay-at-point ()
  (let ((ov (hypertex--overlay-at-point)))
    (if ov
        (delete-overlay ov)
      ())))

(evil-define-text-object hypertex-atom-text-object (count)
  ""
  (libhypertex-select-atoms (point) count (org-element-property :value (org-element-context))))

(defun hypertex-overlay-wrapper (body)
  (progn
    ;; Attempt the operation
    (condition-case nil
        (funcall body)
      ;; If it fails, remove the overlay at point if any
      (error (progn
                       (message "error happened!")
                       (hypertex--remove-overlay-at-point))))
    (let ((overlay (hypertex--overlay-at-point)))
      (if overlay
          (progn
            (setq disable-point-adjustment t)
            (setq cursor-type nil))
        (setq cursor-type t)))))

;; Create or update an overlay on every calc stack entry
(defun hypertex--create-line-overlays ()
  (if (and (string= calc-language "latex")
           (string= "*Calculator*" (buffer-name)))
      (progn
        (goto-char (point-min))
        ; Skip the header line --- Emacs Calculator Mode ---
        (forward-line 1)
        (while (not (eobp))
          (hypertex--overlay-line)
          (forward-line 1)))
    ()))

;; Create or update an overlay on the line at point
(defun hypertex--overlay-line ()
  (if (string=
       "."
       (string-trim (buffer-substring
                     (line-beginning-position)
                     (line-end-position))))
      ()
    (let* ((line-start (if hypertex--calc-line-numbering
                           (+ (line-beginning-position) 4)
                         (line-beginning-position)))
           (line-end (line-end-position))
           (line-contents (buffer-substring line-start line-end))
           (selected-line-contents (hypertex--lift-selection line-contents line-start line-end))
           (ov (hypertex--get-or-create-overlay line-start line-end))
           (tex (format "\\[ %s \\]" selected-line-contents)))
      (progn
        (message selected-line-contents)
        (move-overlay ov line-start line-end)
        (hypertex--render-overlay-at tex ov))
          )))

(defun hypertex--lift-selection (text line-start line-end)
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


(defun hypertex-hide-overlay-at-point ()
  (interactive)
  (let* ((overlays (overlays-at (point)))
         (overlays (seq-filter (lambda (ov)
                                 (eq (overlay-get ov 'org-overlay-type) 'org-hypertex-overlay))
                               overlays)))
    (if (eq 0 (length overlays))
        (hypertex--render-overlay-at-point)
      (delete-overlay (car overlays)))))

(defun hypertex--remove-overlays ()
  (with-current-buffer "*Calculator*"
    (dolist (ov (overlays-in (point-min) (point-max)))
      (delete-overlay ov))))

(defun hypertex--marker-within-frag (marker frag)
  (if marker
      (let* ((begin (org-element-property :begin frag))
             (pt (- marker begin)))
        pt)
    nil))

;; Activate all formulas for embedded mode
(defun hypertex--activate-all ()
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

(defun hypertex-next-formula ()
  (interactive)
  (let* ((frag (hypertex-latex-fragment-at-point))
         (math-regexp "\\$\\|\\\\[([]\\|^[ \t]*\\\\begin{[A-Za-z0-9*]+}")
         )
    (progn
      (if frag
          (progn
            (goto-char (org-element-property :end frag))
            (forward-char))
        ())
      (re-search-forward math-regexp (point-max) t)
      (let* ((frag (hypertex-latex-fragment-at-point))
             (begin (org-element-property :begin frag)))
        (goto-char begin))
      (setq disable-point-adjustment t))))

(defun hypertex-prev-formula ()
  (interactive)
  (let* ((frag (hypertex-latex-fragment-at-point))
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
      (let* ((frag (hypertex-latex-fragment-at-point))
             (begin (org-element-property :begin frag)))
        (goto-char begin))
      (setq disable-point-adjustment t))))

;; Activate the formula at point with calc Embedded mode.
(defun hypertex-activate-formula ()
  (interactive)
  (let* ((frag (hypertex-latex-fragment-at-point)))
    (if frag
        (progn
          (goto-char (org-element-property :begin frag))
          ;; Set a bookmark to jump back to
          (forward-char)
          (bookmark-set "hypertex-formula")
          (calc-embedded nil)
          (goto-char (org-element-property :begin frag))
          (hypertex--render-overlay-at-point)
          (calc)))))

(defun hypertex-accept-formula ()
  (interactive)
  (spacemacs/alternate-window)
  (bookmark-jump "hypertex-formula")
  (calc-embedded t)
  (let ((frag (hypertex-latex-fragment-at-point)))
    (goto-char (org-element-property :end frag))))

(defun hypertex-append-inline-formula ()
  (interactive)
  (let* ((frag (hypertex-latex-fragment-at-point)))
    (if frag
        (progn
          (goto-char (org-element-property :end frag))
          (save-excursion (insert " "))
          )))
  (let ((calc-embedded-open-new-formula "$")
        (calc-embedded-close-new-formula "$"))
    (progn
      (calc-embedded-new-formula)
      (bookmark-set "hypertex-formula")
      (hypertex--render-overlay-at-point)
      (calc))))

(defun hypertex-insert-display-formula ()
  (interactive)
  (evil-insert-newline-below)
  (let ((calc-embedded-open-new-formula "\\[ ")
        (calc-embedded-close-new-formula " \\]"))
    (progn
      (calc-embedded-new-formula)
      (bookmark-set "hypertex-formula")
      (hypertex--render-overlay-at-point)
      (calc))))

;;;###autoload
(defun turn-on-hypertex-mode ()
  "Enable hypertex-mode in current buffer."
  (interactive "")
  (hypertex-mode 1))

;;;###autoload
(defun turn-off-hypertex-mode ()
  "Disable hypertex-mode in current buffer."
  (interactive "")
  (hypertex-mode -1))

(defhydra hypertex-hydra (:color red)
  "foo"
  ("h" hypertex-hide-overlay-at-point "show/hide overlays")
  ("n" hypertex-next-formula "next")
  ("p" hypertex-prev-formula "prev")
  ("r" hypertex-activate-formula "replace" :color blue)
  ("a" hypertex-append-inline-formula "Append $formula$" :color blue)
  ("o" hypertex-insert-display-formula "Insert \\[ display formula \\]" :color blue)
  ("q" nil "quit" :color blue))

(define-key hypertex-mode-map (kbd "s-f") 'hypertex-hydra/body)


(provide 'hypertex)

(defvar math-rewrite-for-display t)

(advice-add 'math-format-stack-value :around 'math-rewrite-then-format-stack-value)

(defun math-rewrite-then-format-stack-value (fn &rest args)
  (message "args: %s" args)
  (let* ((entry (car args))
         (a (car entry))
         (math-comp-selected (nth 2 entry))
         (calc-simplify-mode 'none)
         (e (cond ((not math-rewrite-for-display) entry)
                  ((or (null a)
                       (eq calc-display-raw t)
                       (stringp a)
                       (eq a 'top-of-stack)
                       calc-prepared-composition
                       (and (Math-scalarp a)
                            (memq calc-language '(nil flat uniform))
                            (null math-comp-selected))
                       (string= calc-language "unform")) entry)
                  (math-rewrite-for-display (math-rewrite-for-display-with-selection entry))
                  (t entry))))
    (message "e: %s" e)
    (funcall fn e)))

(defun math-rewrite-for-display-with-selection (entry)
  (let* ((expr (car entry))
         (calc-rewr-sel nil)
         (sel (nth 2 entry))
         (selected-sel (list 'calcFunc-select sel))
         (math-rewrite-selections nil)
         (rules '(var DispRules var-DispRules))
         (expr-with-selection-wrapped (if sel
                                          (calc-replace-sub-formula expr
                                                                    sel
                                                                    selected-sel)
                                        expr))
         (rewritable-expr (copy-tree expr-with-selection-wrapped))
         (rewritten (let ((math-rewrite-for-display nil))
                      (math-rewrite rewritable-expr rules nil)))
         (wrapped-selection (if sel (locate-select-wrapper rewritten) nil))
         (unwrapped-selection (if sel (car (cdr wrapped-selection)) nil)))
    (progn
    (message "rewritten: %s" rewritten)
    (let*
         ((rewritten-expr (if sel (calc-replace-sub-formula rewritten wrapped-selection unwrapped-selection) rewritten))
         (formattable-entry (list rewritten-expr
                                  (car (cdr entry))
                                  unwrapped-selection)))
    ;; we need to undo the mutation we made to `entry'
    (message "rewritable-expr: %s" rewritable-expr)
    (message "formattable: %s" formattable-entry)
    (if sel (calc-replace-sub-formula expr selected-sel sel))
    formattable-entry
    ))))

(defun locate-select-wrapper (expr)
  (if (Math-primp expr)
      nil
    (if (and (eq (car expr) 'calcFunc-select)
             (= (length expr) 2))
        expr
      (or (locate-select-wrapper (car expr))
          (locate-select-wrapper (cdr expr))))))

(defun math-rewrite (math-rewrite-whole-expr rules &optional math-mt-many)
  (let* ((crules (math-compile-rewrites rules))
         (heads (math-rewrite-heads math-rewrite-whole-expr))
         (trace-buffer (get-buffer "*Trace*"))
         (calc-display-just 'center)
         (calc-display-origin 39)
         (calc-line-breaking 78)
         (calc-line-numbering nil)
         (calc-show-selections t)
         (calc-why nil)
         (math-mt-func (function
                        (lambda (x)
                          (let ((result (math-apply-rewrites x (cdr crules)
                                                             heads crules)))
                            (if result
                                (progn
                                  (if trace-buffer
                                      (let* ((math-rewrite-for-display nil)
                                             (fmt (math-format-stack-value
                                                   (list result nil nil))))
                                        (with-current-buffer trace-buffer
                                          (insert "\nrewrite to\n" fmt "\n"))))
                                  (setq heads (math-rewrite-heads result heads t))))
                            result)))))
    (if trace-buffer
	      (let* ((math-rewrite-for-display nil)
               (fmt (math-format-stack-value (list math-rewrite-whole-expr nil nil))))
	        (with-current-buffer trace-buffer
	          (setq truncate-lines t)
	          (goto-char (point-max))
	          (insert "\n\nBegin rewriting\n" fmt "\n"))))
    (or math-mt-many (setq math-mt-many (or (nth 1 (car crules))
				                                    math-rewrite-default-iters)))
    (if (equal math-mt-many '(var inf var-inf)) (setq math-mt-many 1000000))
    (if (equal math-mt-many '(neg (var inf var-inf))) (setq math-mt-many -1000000))
    (math-rewrite-phase (nth 3 (car crules)))
    (if trace-buffer
	      (let* ((math-rewrite-for-display nil)
               (fmt (math-format-stack-value (list math-rewrite-whole-expr nil nil))))
	        (with-current-buffer trace-buffer
	          (insert "\nDone rewriting"
		                (if (= math-mt-many 0) " (reached iteration limit)" "")
		                ":\n" fmt "\n"))))
    math-rewrite-whole-expr))


(defun calc-user-define-permanent ()
  (interactive)
  (calc-wrapper
   (message "Record in %s the command: z-" calc-settings-file)
   (let* ((key (read-char))
	  (def (or (assq key (calc-user-key-map))
		   (assq (upcase key) (calc-user-key-map))
		   (assq (downcase key) (calc-user-key-map))
		   (and (eq key ?\')
			(cons nil
                              (intern
                               (concat "calcFunc-"
                                       (completing-read
                                        (format "Record in %s the algebraic function: "
                                                calc-settings-file)
                                        (mapcar (lambda (x) (substring x 9))
                                                (all-completions "calcFunc-"
                                                                 obarray))
                                        (lambda (x)
                                          (fboundp
                                           (intern (concat "calcFunc-" x))))
                                        t)))))
                   (and (eq key ?\M-x)
			(cons nil
			      (intern (completing-read
				       (format "Record in %s the command: "
					       calc-settings-file)
				       obarray 'fboundp nil "calc-"))))
		   (error "No command defined for that key"))))
     (set-buffer (find-file-noselect (substitute-in-file-name
				      calc-settings-file)))
     (goto-char (point-max))
     (let* ((cmd (cdr def))
	    (fcmd (and cmd (symbolp cmd) (symbol-function cmd)))
	    (func nil)
	    (pt (point))
	    (fill-column 70)
	    (fill-prefix nil)
	    str q-ok)
       (insert "\n;;; Definition stored by Calc on " (current-time-string)
	       "\n(put 'calc-define '"
	       (if (symbolp cmd) (symbol-name cmd) (format "key%d" key))
	       " '(progn\n")
       (if (and fcmd
		(eq (car-safe fcmd) 'lambda)
		(get cmd 'calc-user-defn))
	   (let ((pt (point)))
	     (and (eq (car-safe (nth 3 fcmd)) 'calc-execute-kbd-macro)
		  (vectorp (nth 1 (nth 3 fcmd)))
		  (progn (and (fboundp 'edit-kbd-macro)
			      (edit-kbd-macro nil))
			 (fboundp 'edmacro-parse-keys))
		  (setq q-ok t)
		  (aset (nth 1 (nth 3 fcmd)) 1 nil))
	     (insert (setq str (prin1-to-string
				(cons 'defun (cons cmd (cdr fcmd)))))
		     "\n")
	     (or (and (string-match "\"" str) (not q-ok))
		 (fill-region pt (point)))
	     (indent-rigidly pt (point) 2)
	     (delete-region pt (1+ pt))
	     (insert " (put '" (symbol-name cmd)
		     " 'calc-user-defn '"
		     (prin1-to-string (get cmd 'calc-user-defn))
		     ")\n")
	     (setq func (calc-stack-command-p cmd))
	     (let ((ffunc (and func (symbolp func) (symbol-function func)))
		   (pt (point)))
	       (and ffunc
		    (eq (car-safe ffunc) 'lambda)
		    (get func 'calc-user-defn)
		    (progn
		      (insert (setq str (prin1-to-string
					 (cons 'defun (cons func
							    (cdr ffunc)))))
			      "\n")
		      (or (and (string-match "\"" str) (not q-ok))
			  (fill-region pt (point)))
		      (indent-rigidly pt (point) 2)
		      (delete-region pt (1+ pt))
		      (setq pt (point))
		      (insert "(put '" (symbol-name func)
			      " 'calc-user-defn '"
			      (prin1-to-string (get func 'calc-user-defn))
			      ")\n")
		      (fill-region pt (point))
		      (indent-rigidly pt (point) 2)
		      (delete-region pt (1+ pt))))))
	 (and (stringp fcmd)
	      (insert " (fset '" (prin1-to-string cmd)
		      " " (prin1-to-string fcmd) ")\n")))
       (or func (setq func (and cmd (symbolp cmd) (fboundp cmd) cmd)))
       (if (get func 'math-compose-forms)
	   (let ((pt (point)))
	     (insert "(put '" (symbol-name func)
		     " 'math-compose-forms '"
		     (prin1-to-string (get func 'math-compose-forms))
		     ")\n")
	     (fill-region pt (point))
	     (indent-rigidly pt (point) 2)
	     (delete-region pt (1+ pt))))
       (if (car def)
	   (insert " (define-key calc-mode-map "
		   (prin1-to-string (concat "z" (char-to-string key)))
		   " '"
		   (prin1-to-string cmd)
		   ")\n")))
     (insert "))\n")
     (save-buffer))))

