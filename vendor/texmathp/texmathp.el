;;; texmathp.el --- Code to check if point is inside LaTeX math environment

;; Copyright (c) 2003 Free Software Foundation, Inc.

;; Author: Carsten Dominik <dominik@science.uva.nl>
;; Keywords: tex

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; NOTE: This has been vendored from
;;   https://staff.fnwi.uva.nl/c.dominik/Tools/cdlatex/texmathp.el
;;
;;  This code provides a function to determine if point in a buffer is
;;  inside a (La)TeX math environment.  This is not trivial since many
;;  different ways are used to switch between the two, for example:
;;
;;    \begin{equation}  ... \end{equation}
;;    $ ... $
;;    $$ ... $$
;;    \[ ... \]
;;    \ensuremath{...}
;;    \mbox{...}
;;
;;  To install, put this file on your load-path and compile it.
;;
;;  To use this in your lisp program, do
;;
;;     (require 'texmathp)
;;
;;  You can then write code like this:
;;    
;;     (if (texmathp) ...)
;;
;;  The call to `texmathp' leaves some extra information in the
;;  variable `texmathp-why'.  It's value is a cons cell (MATCH . POSITION),
;;  specifying which command at what position is responsible for math
;;  mode being on or off.
;;
;;  To configure which macros and environments influence LaTeX math mode,
;;  customize the variable `texmathp-tex-commands'.  By default
;;  it recognizes the LaTeX core as well as AMS-LaTeX (see the variable
;;  `texmathp-tex-commands-default', also as an example).
;;
;;  To try out the code interactively, use `M-x texmathp RET'.
;;
;;--------------------------------------------------------------------------
;;
;;  LIMITATIONS:
;;
;;  Of course, in order to work this function has to assume that the
;;  LaTeX above point is syntactically correct.  In particular:
;;
;;  o The different math delimiters are paired correctly.  Thus if
;;    you do things like "\begin{equation} $"  or "\[ ... \)"
;;    the result of (texmathp) is undefined.  It is in fact possible
;;    in LaTeX to pair \[ with $$ and \( with $, but this will confuse
;;    texmathp (and human readers as well).
;;
;;  o However, texmathp will correctly work with nested delimiters,
;;    e.g. something like this will be parsed correctly at any point:
;;
;;       \begin{equation}
;;          x = y \mbox{abc \ensuremath{\alpha} cba $2^3$}
;;       \end{equation}
;;
;;  o texmathp is somewhat forgiving if you have an empty line inside
;;    the current math environment, which is not legal in TeX but may
;;    easily happen during editing.  Depending upon the variable
;;    `texmathp-search-n-paragraphs' we check several paragraphs,
;;    backwards, by default 2.  Paragraph here means something limited
;;    by an empty line.
;;
;;  Macros which set or cancel math mode for their arguments are
;;  assumed to do this only for madatory arguments.  Optional
;;  arguments are ignored - the mode in these arguments is just like
;;  what is was before the macro.
;;--------------------------------------------------------------------------

;;; Code:

(defgroup texmathp nil
  "Testing TeX and LaTeX documents for math mode."
  :tag "Test For TeX and LaTeX Math Mode"
  :prefix "texmathp-"
  :group 'tex)

(defcustom texmathp-tex-commands nil
  "List of environments and macros influencing (La)TeX math mode.
This user-defined list is used in additions to LaTeX and AMSLaTeX defaults.
The structure of each entry is (NAME TYPE)

- The first item in each entry is the name of an environment or macro.
  If it's a macro, include the backslash.

- The second item is a symbol indicating how the command works: 
    `env-on'     Environment, turns math mode for its body  on
    `env-off'    Environment: turns math mode for its body  off
    `arg-on'     Command: turns math mode for its arguments on
    `arg-off'    Command: turns math mode for its arguments off
    `sw-on'      Switch: turns math-mode of following text  on
    `sw-off'     Switch: turns math-mode of following text  off
    `sw-toggle'  Switch: toggles math mode of following text"
  :group 'texmathp
  :type
  '(repeat
    (list :value ("" env-on)
     (string  :tag "Name")
     (choice  :tag "Type"
      (const :tag "Environment: turns math mode for its body on" env-on)
      (const :tag "Environment: turns math mode for its body off" env-off)
      (const :tag "Command: turns math mode for its argument on" arg-on)
      (const :tag "Command: turns math-mode for its argument off" arg-off)
      (const :tag "Switch: turns math-mode of following text on" sw-on)
      (const :tag "Switch: turns math-mode of following text off" sw-off)
      (const :tag "Switch: toggles math mode of following text" sw-toggle)))))

(defconst texmathp-tex-commands-default
  '(;; Standard LaTeX
    ("equation"      env-on)
    ("eqnarray"      env-on)      ("eqnarray*"     env-on)
    ("displaymath"   env-on)
    ("\\mbox"        arg-off)
    ("\\("           sw-on)       ("\\)"           sw-off)
    ("\\["           sw-on)       ("\\]"           sw-off)
    ("$$"            sw-toggle)   ("$"             sw-toggle)
    ;; AMS-LaTeX
    ("equation*"     env-on)
    ("align"         env-on)      ("align*"        env-on)
    ("gather"        env-on)      ("gather*"       env-on)
    ("multline"      env-on)      ("multline*"     env-on)
    ("flalign"       env-on)      ("flalign*"      env-on)
    ("alignat"       env-on)      ("alignat*"      env-on)
    ("xalignat"      env-on)      ("xalignat*"     env-on)
    ("xxalignat"     env-on)      ("xxalignat*"    env-on)
    ("\\ensuremath"  arg-on)
    ("\\text"        arg-off)     ("\\intertext"   arg-off))
  "The default entries for `texmathp-tex-commands', which see.")

(defcustom texmathp-search-n-paragraphs 2
  "*Number of paragraphs to check before point.
Normally, you cannot have an empty line in a math environment in (La)TeX.
Therefore, the fastest method to test for math mode is limiting the
search backward to the nearest empty line.
However, during editing it happens that such lines exist temporarily.
Therefore we look a little further.  This variable determines how many 
empty lines we go back to fix the search limit."
  :group 'texmathp
  :type 'number)

(defcustom texmathp-allow-detached-args nil
  "*Non-nil means, allow arguments of macros to be detached by whitespace.
When this is t, `aaa' will be considered as argument of \bb in the following
construct:  \bbb [xxx] {aaa}
The disadvantage is that any number of braces expressions will be considered
arguments of the macro independent of its definition."
  :group 'texmathp
  :type 'boolean)

(defvar texmathp-why nil
  "After a call to `texmathp' this variable shows why math-mode is on or off.
The value is a cons cell (MATCH . POSITION).  
MATCH is a string like a car of an entry in `texmathp-tex-commands', e.q.
\"equation\" or \"\\ensuremath\" or \"\\[\" or \"$\".
POSITION is the buffer position of the match.  If there was no match,
it points to the limit used for searches, usually two paragraphs up.")

(defvar texmathp-environments nil)
(defvar texmathp-macros nil)
(defvar texmathp-onoff-regexp nil)
(defvar texmathp-toggle-regexp nil)
(defvar texmathp-tex-commands1 nil)
(defvar texmathp-memory nil)

;; We need our own syntax table to play with the syntax of () [] and {}
;; For speed reasons we define it statically instead of copying it each time.
(defvar texmathp-syntax-table (make-syntax-table)
  "Syntax table used while texmathp is parsing.")
(mapcar
 (lambda (x) (modify-syntax-entry (car x) (cdr x) texmathp-syntax-table))
 '((?\\ . "\\") (?\f .">") (?\n . ">") (?% . "<")
   (?\[ . ".") (?\] . ".") (?\{ . "(}") (?\} . "){")
   (?\( . ".") (?\) . ".") (?\" . ".") (?& . ".") (?_ . ".")
   (?@ . "_") (?~ . " ") (?$ . "$") (?' . "w")))

(defun texmathp-compile ()
  "Compile the value of `texmathp-tex-commands' into the internal lists."

  ;; Extract lists and regexp.
  (setq texmathp-macros nil texmathp-environments nil)
  (setq texmathp-memory
	(cons texmathp-tex-commands texmathp-tex-commands-default))
  (setq texmathp-tex-commands1 (append texmathp-tex-commands
				       texmathp-tex-commands-default))
  (let ((list (reverse texmathp-tex-commands1))
	var entry type switches togglers)
    (while (setq entry (car list))
      (setq type (nth 1 entry)
	    list (cdr list)
	    var (cond ((memq type '(env-on env-off)) 'texmathp-environments)
		      ((memq type '(arg-on arg-off)) 'texmathp-macros)
		      ((memq type '(sw-on sw-off))   'switches)
		      ((memq type '(sw-toggle))      'togglers)))
      (set var (cons (car entry) (symbol-value var))))
    (setq texmathp-onoff-regexp
	  (concat "[^\\\\]\\(" 
		  (mapconcat 'regexp-quote switches "\\|")
		  "\\)")
	  texmathp-toggle-regexp
	  (concat "\\([^\\\\\\$]\\|\\`\\)\\("
		  (mapconcat 'regexp-quote togglers "\\|")
		  "\\)"))))

(defun texmathp ()
  "Determine if point is inside (La)TeX math mode. 
Returns t or nil.  Additional info is placed into `texmathp-why'.
The functions assumes that you have (almost) syntactically correct (La)TeX in
the buffer.
See the variable `texmathp-tex-commands' about which commands are checked."
  (interactive)
  (unless (and (eq (car texmathp-memory) texmathp-tex-commands)
	       (eq (cdr texmathp-memory) texmathp-tex-commands-default))
    (texmathp-compile))
  (let* ((pos (point)) math-on sw-match
	 (bound (save-excursion
		  (if (re-search-backward "[\n\t][ \t]*[\n\r]"
					  nil 1 texmathp-search-n-paragraphs)
		      (match-beginning 0)
		    (point-min))))
	 (env-match (texmathp-match-environment bound))
	 (mac-match (texmathp-match-macro bound))
	 (match (cons nil bound)))

    ;; Select the nearer match
    (and env-match (setq match env-match))
    (and mac-match (> (cdr mac-match) (cdr match)) (setq match mac-match))
    (setq math-on (memq (nth 1 (assoc (car match) texmathp-tex-commands1))
			'(env-on arg-on)))

    ;; Check for switches
    (and (not math-on)
	 (setq sw-match (texmathp-match-switch bound))
	 (> (cdr sw-match) (cdr match))
	 (eq (nth 1 (assoc (car sw-match) texmathp-tex-commands1)) 'sw-on)
	 (setq match sw-match math-on t))

    ;; Check for togglers
    (if (not math-on)
	(save-excursion
	  (goto-char (cdr match))
	  (while (re-search-forward texmathp-toggle-regexp pos t)
	    (if (setq math-on (not math-on))
		(setq sw-match (cons (match-string 2) (match-beginning 2)))
	      (setq sw-match nil)))
	  (and math-on sw-match (setq match sw-match))))

    ;; Store info, show as message when interactive, and return
    (setq texmathp-why match)
    (and (interactive-p)
	 (message "math-mode is %s: %s begins at buffer position %d"
		  (if math-on "on" "off")
		  (or (car match) "new paragraph") (cdr match)))
    (and math-on t)))

(defun texmathp-match-macro (bound)
  ;; Find out if point is within the arguments of any of the Math macros.
  ;; Limit searches to BOUND.  The return value is like ("\\macro" . (point)).
  (catch 'exit
    (and (null texmathp-macros) (throw 'exit nil))
    (let (pos cmd (syntax-table (syntax-table)))
      (unwind-protect
	  (save-restriction
	    (save-excursion
	      (set-syntax-table texmathp-syntax-table)
	      (narrow-to-region (max 1 bound) (point))
	      ;; Move back out of the current parenthesis
	      (while (progn
		       ;; Move up out of {}
		       (modify-syntax-entry ?\{ "(}")
		       (modify-syntax-entry ?\{ "){")
 		       (modify-syntax-entry ?\[ ".")
		       (modify-syntax-entry ?\] ".")
		       (condition-case nil (progn (up-list -1) t) (error nil)))
		;; Move back over touching sexps (in fact also non-touching)
		(while 
		    (and 
		     (cond
		      ((memq (preceding-char) '(?\] ?\})))
		      ((and 
			texmathp-allow-detached-args
			(re-search-backward 
			"[]}][ \t]*[\n\r]?\\([ \t]*%[^\n\r]*[\n\r]\\)*[ \t]*\\="
			bound t))
		       (goto-char (1+ (match-beginning 0))) t))
		     (progn
		       (if (eq (preceding-char) ?\})
			   (progn
			     ;; skip {}
			     (modify-syntax-entry ?\{ "(}")
			     (modify-syntax-entry ?\} "){")
			     (modify-syntax-entry ?\[ ".")
			     (modify-syntax-entry ?\] "."))
			 ;; skip []
			 (modify-syntax-entry ?\{ ".")
			 (modify-syntax-entry ?\} ".")
			 (modify-syntax-entry ?\[ "(]")
			 (modify-syntax-entry ?\] ")["))
		       (condition-case nil 
			   (progn (backward-sexp) t) (error nil)))))
		(setq pos (point))
		(and (memq (following-char) '(?\[ ?\{))
		     (re-search-backward "\\\\[*a-zA-Z]+\\=" nil t)
		     (setq cmd (buffer-substring-no-properties
				(match-beginning 0) (match-end 0)))
		     (member cmd texmathp-macros)
		     (throw 'exit (cons cmd (point))))
		(goto-char pos))
	      (throw 'exit nil)))
	(set-syntax-table syntax-table)))))

(defun texmathp-match-environment (bound)
  ;; Find out if point is inside any of the math environments.
  ;; Limit searched to BOUND.  The return value is like ("equation" . (point)).
  (catch 'exit
    (save-excursion
      (and (null texmathp-environments) (throw 'exit nil))
      (let (end-list env)
	(while (re-search-backward "\\\\\\(begin\\|end\\){\\([^}]+\\)}"
				   bound t)
	  (setq env (buffer-substring-no-properties
		     (match-beginning 2) (match-end 2)))
	  (cond ((string= (match-string 1) "end")
		 (add-to-list 'end-list env))
		((equal env (car end-list))
		 (setq end-list (cdr end-list)))
		((member env texmathp-environments)
		 (throw 'exit (cons env (point))))))
	nil))))

(defun texmathp-match-switch (bound)
  ;; Search backward for any of the math switches.
  ;; Limit searched to BOUND.  The return value is like ("\\(" . (point)).
  (save-excursion
    (if (re-search-backward texmathp-onoff-regexp bound t)
	(cons (buffer-substring-no-properties
               (match-beginning 1) (match-end 1))
	      (match-beginning 1))
      nil)))

(provide 'texmathp)

;;; texmathp.el ends here
