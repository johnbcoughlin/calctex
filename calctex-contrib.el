(require 'calc)

(defun calctex-contrib-prepare ()
  (unless (get-buffer "*Calculator*") (calc))
  (calc-latex-language nil)
  )

;;; Functions
(defmath paren (f)
  (interactive 1 "paren")
  nil)

;;;; Functions that are only used to rewrite into for display purposes
(defmath deefdeex (f x) nil) ; df/dx
(defmath deedeexf (f x) nil) ; d/dx f
(defmath delfdelx (f x) nil) ; ∂f/∂x
(defmath deldelx (f x) nil) ; ∂/∂x f

;;; Display Rewrite Rules
(defvar calctex-contrib-disprules ()
  "The set of default display rewrite rules.")
(setq calctex-contrib-disprules
  (list
   "deriv(f, x) := deefdeex(f, x) :: variable(f) :: dscalar(apply(f, []))"
   "deriv(apply(f, args), x) := deedeexf(f, x) :: dscalar(apply(f, args))"
   "deriv(apply(f, args), x) := deldelxf(f, x) :: lnot(dscalar(apply(f, args)))"
   )
  )

;;;; Display Rewriting Implementation
(setq math-rewrite-for-display t)
(advice-add 'math-format-stack-value :around 'math-rewrite-then-format-stack-value)

(defun math-rewrite-then-format-stack-value (fn &rest args)
  "Apply the DispRules set of rewrite rules before passing to the stack value formatter."
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


;;; Compositions
;;;; Helper function
(defun calctex-contrib-define-composition (lang func comp arglist)
  (calc-wrapper
   (let* ((comps (get func 'math-compose-forms))
          (math-arglist nil)
          (calc-user-formula-alist nil)
          entry entry2)
     (if (math-zerop comp)
	       (if (setq entry (assq calc-language comps))
	           (put func 'math-compose-forms (delq entry comps)))
     (calc-default-formula-arglist comp)
     (setq calc-user-formula-alist arglist)
     (let* ((forms-list (assq calc-language comps)))
       (if (not forms-list)
           (put func 'math-compose-forms
                (cons (setq forms-list (list calc-language)) comps))
         (let ((form-with-arg-count (assq (length calc-user-formula-alist) (cdr entry))))
           (if (not form-with-arg-count)
               (setcdr forms-list
                       (cons (setq form-with-arg-count
                                   (list (length calc-user-formula-alist))) (cdr entry))))
       (setcdr form-with-arg-count
               (list 'lambda calc-user-formula-alist (calc-fix-user-formula comp))))))))))
;;;; Compositions
(let ((comp (calc-eval "choriz([string(\"\\\\left(\"), x, string(\"\\\\right)\")])" 'raw)))
  (calctex-contrib-define-composition "latex" 'calcFunc-paren comp '(x)))
(let ((comp (calc-eval "choriz([string(\"\\\\frac{\\\\mathrm{d}\"), f, string(\"}{\\\\mathrm{d}\"), x, string(\"}\")])" 'raw)))
  (calctex-contrib-define-composition "latex" 'calcFunc-deefdeex comp '(f x)))
(let ((comp (calc-eval "choriz([string(\"\\\\frac{\\\\mathrm{d}}{\\\\mathrm{d}\"), x, string(\"}\"), f])" 'raw)))
  (calctex-contrib-define-composition "latex" 'calcFunc-deedeexf comp '(f x)))
(let ((comp (calc-eval "choriz([string(\"\\\\frac{\\\\partial}{\\\\partial \"), x, string(\"}\"), f])" 'raw)))
  (calctex-contrib-define-composition "latex" 'calcFunc-deldelxf comp '(f x)))
;;; Declarations

(defvar calctex-contrib-common-decls ()
  "The list of declarations common to all calctex contexts.")

(defvar calctex-contrib-context-decls ()
  "The list of declarations specific to the current calctex context.")

(defun calctex-context-complex-analysis ()
  (setq calctex-contrib-context-decls
        (list
         "[[x, y, theta], real]"
         "[rho, [0 .. inf)]"
         "[u(x, y), real]"
         "[v(x, y), real]"
         "[[u, v], real]"))
  (calctex-contrib-refresh))

(defun calctex-context-linear-algebra ()
  (setq calctex-contrib-context-decls
        (list
         "[[x, y, u, v], vector]"
         "[[A, B, U, V], matrix]"
         "[[alpha, beta], scalar]"))
  (calctex-contrib-refresh))

(defun calctex-context-electrodynamics ()
  (setq calctex-contrib-context-decls
        (list
         "[[r, rprime], vector]"
         "[hat(), vector]"
         "[[xhat, yhat, zhat], vector]"))
  (calctex-contrib-refresh))

;;; Variable lifecycles

(defun calctex-contrib-refresh ()
  (setq var-Decls
        (let ((formatted (format "[%s]" (mapconcat #'(lambda (decl) (format "%s" decl))
                                                   (append calctex-contrib-common-decls
                                                           calctex-contrib-context-decls)
                                                   ",\n"))))
          (calc-eval formatted 'raw)))
  (setq var-DispRules
        (calc-eval (format "[%s]" (mapconcat #'(lambda (rule) (format "%s" rule))
                                             calctex-contrib-disprules
                                             ",\n"))
                   'raw)))

(calctex-context-complex-analysis)
(calctex-contrib-refresh)
