(require 'calc)

(defun calctex-contrib-prepare ()
  (unless (get-buffer "*Calculator*") (calc))
  (calc-latex-language nil))

;;; Functions
(defun calctex-contrib-define-functions ()
  (defmath paren (f)
    (interactive 1 "paren")
    nil)

  (defmath align-equal-to (a b)
    (interactive 2)
    nil)

  (defmath adjoint (A)
    (interactive 1 "adjoint")
    (transpose A))

  (defmath laplacian (f)
    (interactive 1 "laplacian")
    nil)

  (defmath limit (f var bound)
    nil)

  (defun calc-limit (var bound)
    (interactive "sTake the limit with respect to: \nsAs %s goes to: \n")
    (calc-slow-wrapper
     (calc-enter-result 1 "lim"
                        (cons 'calcFunc-limit
                              (list (calc-top-list-n 1)
                                    (math-read-expr var)
                                    (math-read-expr bound))))))
  )

;;;; Functions that are only used to rewrite into for display purposes
(defmath multieq3 (x) nil)
(defmath multieq4 (x) nil)
(defmath null (x) nil) ; Invisible; useful for the "upper" bound of an integral over a surface
(defmath deefdeex (f x) nil) ; df/dx
(defmath deedeexf (f x) nil) ; d/dx f
(defmath delfdelx (f x) nil) ; ∂f/∂x
(defmath deldelx (f x) nil) ; ∂/∂x f
(defmath vec (x) nil) ; \vec{x}

;;; Display Rewrite Rules
(defvar calctex-contrib-disprules ()
  "The set of default display rewrite rules.")
(setq calctex-contrib-disprules
  (list
   "iterations(1)"
   "x := vec(x) :: dvector(x) :: lnot(dmatrix(x))"
   "tderiv(f, x) := deefdeex(f, x) :: variable(f)"
   "tderiv(apply(f, args), x) := deedeexf(apply(f, args), x)"
   "deriv(f, x) := delfdelx(f, x) :: variable(f)"
   "deriv(apply(f, args), x) := deldelxf(apply(f, args), x)"
   )
  )

;;;; Display Rewriting Implementation
(setq math-rewrite-for-display t)
(advice-add 'math-format-stack-value :around 'math-rewrite-then-format-stack-value)

(defun math-rewrite-then-format-stack-value (fn &rest args)
  "Apply the DispRules set of rewrite rules before passing to the stack value formatter."
  (let* ((entry (car args))
         (a (car entry))
         (math-comp-selected (nth 2 entry))
         (calctex-avoid-simplification-at-all-costs t)
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
(defun calctex-contrib-define-composition (func comp arglist)
  (calc-wrapper
   (let* ((comps (get func 'math-compose-forms))
          (math-arglist nil)
          (calc-user-formula-alist nil)
          entry entry2)
     (if (math-zerop comp)
	       (if (setq entry (assq 'latex comps))
	           (put func 'math-compose-forms (delq entry comps)))
     (calc-default-formula-arglist comp)
     (setq calc-user-formula-alist arglist)
     (let* ((forms-list (assq 'latex comps)))
       (if (not forms-list)
           (put func 'math-compose-forms
                (cons (setq forms-list (list 'latex)) comps))
         (let ((form-with-arg-count (assq (length calc-user-formula-alist) (cdr entry))))
           (if (not form-with-arg-count)
               (setcdr forms-list
                       (cons (setq form-with-arg-count
                                   (list (length calc-user-formula-alist))) (cdr entry))))
       (setcdr form-with-arg-count
               (list 'lambda calc-user-formula-alist (calc-fix-user-formula comp))))))))))
;;;; Compositions
(defun calctex-contrib-common-compositions ()
  (let ((comp (calc-eval "choriz([string(\"\")])" 'raw)))
    (calctex-contrib-define-composition 'calcFunc-null comp '()))
;;;;; Selections

  (let ((comp (calc-eval "choriz([string(\"\\\\colornucleus{red}{\"), x, string(\"}\")])" 'raw)))
    (calctex-contrib-define-composition 'calcFunc-selected comp '(x)))

;;;;; \vec{} wrapper
  (let ((comp (calc-eval "choriz([string(\"\\\\vec{\"), x, string(\"}\")])" 'raw)))
    (calctex-contrib-define-composition 'calcFunc-vec comp '(x)))

;;;;; Matrices
  (let ((comp (calc-eval "choriz([A, string(\"^*\")])" 'raw)))
    (calctex-contrib-define-composition 'calcFunc-adjoint comp '(A)))

;;;;; paren wrapper
  (let ((comp (calc-eval "choriz([string(\"\\\\left(\"), x, string(\"\\\\right)\")])" 'raw)))
    (calctex-contrib-define-composition 'calcFunc-paren comp '(x)))

;;;;; Derivatives
  (let ((comp (calc-eval "choriz([string(\"\\\\frac{\\\\mathrm{d}\"), f, string(\"}{\\\\mathrm{d}\"), x, string(\"}\")])" 'raw)))
    (calctex-contrib-define-composition 'calcFunc-deefdeex comp '(f x)))

  (let ((comp (calc-eval "choriz([string(\"\\\\frac{\\\\mathrm{d}}{\\\\mathrm{d}\"), x, string(\"}\"), f])" 'raw)))
    (calctex-contrib-define-composition 'calcFunc-deedeexf comp '(f x)))

  (let ((comp (calc-eval "choriz([string(\"\\\\frac{\\\\partial \"), f, string(\"}{\\\\partial \"), x, string(\"}\")])" 'raw)))
    (calctex-contrib-define-composition 'calcFunc-delfdelx comp '(f x)))

  (let ((comp (calc-eval "choriz([string(\"\\\\frac{\\\\partial}{\\\\partial \"), x, string(\"}\"), f])" 'raw)))
    (calctex-contrib-define-composition 'calcFunc-deldelxf comp '(f x)))

  (let ((comp (calc-eval "choriz([string(\"\\\\nabla^2 \"), f])" 'raw)))
    (calctex-contrib-define-composition 'calcFunc-laplacian comp '(f)))

;;;;; Align-eq
  ;; ridiculously, this just does two backslashes at the end of the line.
  (let ((comp (calc-eval "choriz([a, string(\" &= \"), b, string(\" \\\\\\\\ \\\\newline\")])" 'raw)))
    (calctex-contrib-define-composition 'calcFunc-align-equal-to comp '(a b))
    )
  (let ((comp (calc-eval "choriz([a, string(\" = \"), b, string(\" = \"), c])" 'raw)))
    (calctex-contrib-define-composition 'calcFunc-multieq3 comp '(a b c)))

  (let ((comp (calc-eval "choriz([a, string(\" = \"), b, string(\" = \"), c, string(\" = \"), d])" 'raw)))
    (calctex-contrib-define-composition 'calcFunc-multieq4 comp '(a b c d)))

;;;;; Abs and norm
  (let ((comp (calc-eval "choriz([string(\"\\\\left|\"), x, string(\"\\\\right|\")])" 'raw)))
    (calctex-contrib-define-composition 'calcFunc-abs comp '(x)))
;;;;; Limit
  (let ((comp (calc-eval "choriz([string(\"\\\\lim_{\"), var, string(\" \\\\rightarrow \"), bound, string(\"}\"), f])" 'raw)))
    (calctex-contrib-define-composition 'calcFunc-limit comp '(f var bound)))
  )

;;; Declarations
;;;; Helper functions
(defun calctex-reload-declarations-advice (&rest r)
  (math-setup-declarations))

(advice-add 'calc-declare-variable :after #'calctex-reload-declarations-advice)
(advice-add 'calc-edit-Decls :after #'calctex-reload-declarations-advice)

(defmath dmatrix (a)
  (interactive 1 "dmatrix")
    (if (check-declared-matrixp a) 1
      0))

(defmath dvector (a)
  (interactive 1 "dvector")
  (if (check-declared-vectorp a) 1 0))

;;;;; Has the variable a been declared as a vector?
(defun calcFunc-check-declared-vectorp (a)
  (cond ((Math-objectp a) nil)
        (t
         (let ((decl (if (eq (car a) 'var)
                         (or (assq (nth 2 a) math-decls-cache)
                             math-decls-all)
                       (assq (car a) math-decls-cache)))
               val)
           (cond
            ((memq 'vector (nth 1 decl)) t)
            ;; ((and (eq (car a) 'var)
            ;;       (symbolp (nth 2 a))
            ;;       (boundp (nth 2 a))
            ;;       (setq val (symbol-value (nth 2 a))))
            ;;  (math-check-known-vectorp val))
            (t nil))))))

;;;;; Has the variable a been declared as a matrix?
(defun calcFunc-check-declared-matrixp (a)
  (cond ((Math-objectp a) nil)
        (t
         (let ((decl (if (eq (car a) 'var)
                         (or (assq (nth 2 a) math-decls-cache)
                             math-decls-all)
                       (assq (car a) math-decls-cache)))
               val)
           (cond
            ((memq 'matrix (nth 1 decl)) t)
            ;; ((and (eq (car a) 'var)
            ;;       (symbolp (nth 2 a))
            ;;       (boundp (nth 2 a))
            ;;       (setq val (symbol-value (nth 2 a))))
            ;;  (math-check-known-vectorp val))
            (t nil))))))

;;;; Contexts
(defvar calctex-contrib-common-decls ()
  "The list of declarations common to all calctex contexts.")

(defvar calctex-contrib-context-decls ()
  "The list of declarations specific to the current calctex context.")

(defun calctex-context-complex-analysis ()
  (setq calctex-contrib-context-decls
        (list
         "[[x, y, theta], [real, const]]"
         "[rho, [[0 .. inf), const]]"
         "[u(x, y), real]"
         "[v(x, y), real]"
         "[[u, v], real]"
         ))
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
  (calctex-contrib-common-compositions)
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

(put 'calc-define 'calctex-contrib
     '(progn
        (calctex-contrib-define-functions)
        ))

(run-hooks 'calc-check-defines)

(provide 'calctex-contrib)

;;; Function advices
;;;; math-simplify
(defvar calctex-avoid-simplification-at-all-costs nil)

(defun calctex-math-simplify-advice (fn &rest args)
  (unless calctex-avoid-simplification-at-all-costs
    (funcall fn args)))

(advice-add 'math-simplify :around 'calctex-math-simplify-advice)
;;; Function redefinitions
;;;; math-compose-expr
;; We redefine this to not care whether a multiplication expression like a(b + c) looks like a function call.
;; We never want to see a function call in LaTeX; concatenation is always multiplication.
(defun math-compose-expr (a prec &optional div)
  (let ((calc-multiplication-has-precedence t)
        (math-compose-level (1+ math-compose-level))
        (math-expr-opers (math-expr-ops))
        spfn)
    (cond
     ((or (and (eq a math-comp-selected) a)
	        (and math-comp-tagged
	             (not (eq math-comp-tagged a))))
      (let ((math-comp-selected nil))
	      (and math-comp-tagged (setq math-comp-tagged a))
	      (list 'tag a (math-compose-expr a prec))))
     ((and (not (consp a)) (not (integerp a)))
      (concat "'" (prin1-to-string a)))
     ((setq spfn (assq (car-safe a)
                       (get calc-language 'math-special-function-table)))
      (setq spfn (cdr spfn))
      (if (consp spfn)
          (funcall (car spfn) a spfn)
        (funcall spfn a)))
     ((math-scalarp a)
      (if (or (eq (car-safe a) 'frac)
	            (and (nth 1 calc-frac-format) (Math-integerp a)))
	        (if (and
               calc-language
               (not (memq calc-language
                          '(flat big unform))))
	            (let ((aa (math-adjust-fraction a))
		                (calc-frac-format nil))
		            (math-compose-expr (list '/
					                               (if (memq calc-language
                                                   calc-lang-slash-idiv)
					                                   (math-float (nth 1 aa))
					                                 (nth 1 aa))
					                               (nth 2 aa)) prec))
	          (if (and (eq calc-language 'big)
		                 (= (length (car calc-frac-format)) 1))
		            (let* ((aa (math-adjust-fraction a))
		                   (calc-frac-format nil)
		                   (math-radix-explicit-format nil)
		                   (c (list 'horiz
				                        (if (math-negp (nth 1 aa))
				                            "- " "")
				                        (list 'vcent 1
				                              (math-format-number
				                               (math-abs (nth 1 aa)))
				                              '(rule ?-)
				                              (math-format-number (nth 2 aa))))))
		              (if (= calc-number-radix 10)
		                  c
		                (list 'horiz "(" c
			                    (list 'subscr ")"
				                        (int-to-string calc-number-radix)))))
	            (math-format-number a)))
	      (if (not (eq calc-language 'big))
	          (math-format-number a prec)
	        (if (memq (car-safe a) '(cplx polar))
	            (if (math-zerop (nth 2 a))
		              (math-compose-expr (nth 1 a) prec)
		            (list 'horiz "("
		                  (math-compose-expr (nth 1 a) 0)
		                  (if (eq (car a) 'cplx) ", " "; ")
		                  (math-compose-expr (nth 2 a) 0) ")"))
	          (if (or (= calc-number-radix 10)
		                (not (Math-realp a))
		                (and calc-group-digits
			                   (not (assoc calc-group-char '((",") (" "))))))
		            (math-format-number a prec)
	            (let ((s (math-format-number a prec))
		                (c nil))
		            (while (string-match (if (> calc-number-radix 14)
					                               "\\([0-9]+\\)#\\([0-9a-zA-Z., ]+\\)"
				                               "\\([0-9]+\\)#\\([0-9a-dA-D., ]+\\)")
				                             s)
		              (setq c (nconc c (list (substring s 0 (match-beginning 0))
					                               (list 'subscr
					                                     (math-match-substring s 2)
					                                     (math-match-substring s 1))))
			                  s (substring s (match-end 0))))
		            (if (string-match
		                 "\\*\\([0-9.]+\\)\\^\\(-?[0-9]+\\)\\()?\\)\\'" s)
		                (setq s (list 'horiz
				                          (substring s 0 (match-beginning 0)) " "
				                          (list 'supscr
					                              (math-match-substring s 1)
					                              (math-match-substring s 2))
				                          (math-match-substring s 3))))
		            (if c (cons 'horiz (nconc c (list s))) s)))))))
     ((and (get (car a) 'math-compose-forms)
	         (not (eq calc-language 'unform))
	         (let ((comps (get (car a) 'math-compose-forms))
		             temp temp2)
	           (or (and (setq temp (assq calc-language comps))
		                  (or (and (setq temp2 (assq (1- (length a)) (cdr temp)))
			                         (setq temp (apply (cdr temp2) (cdr a)))
			                         (math-compose-expr temp prec))
			                    (and (setq temp2 (assq nil (cdr temp)))
			                         (funcall (cdr temp2) a))))
		             (and (setq temp (assq nil comps))
		                  (or (and (setq temp2 (assq (1- (length a)) (cdr temp)))
			                         (setq temp (apply (cdr temp2) (cdr a)))
			                         (math-compose-expr temp prec))
			                    (and (setq temp2 (assq nil (cdr temp)))
			                         (funcall (cdr temp2) a))))))))
     ((eq (car a) 'vec)
      (let* ((math-comp-left-bracket (if calc-vector-brackets
			                                   (substring calc-vector-brackets 0 1) ""))
	           (math-comp-right-bracket (if calc-vector-brackets
				                                  (substring calc-vector-brackets 1 2) ""))
	           (inner-brackets (memq 'R calc-matrix-brackets))
	           (outer-brackets (memq 'O calc-matrix-brackets))
	           (row-commas (memq 'C calc-matrix-brackets))
	           (math-comp-comma-spc (or calc-vector-commas " "))
	           (math-comp-comma (or calc-vector-commas ""))
	           (math-comp-vector-prec (if (or (and calc-vector-commas
				                                         (math-vector-no-parens a))
				                                    (memq 'P calc-matrix-brackets)) 0 1000))
	           (math-comp-just (cond ((eq calc-matrix-just 'right) 'vright)
                                   ((eq calc-matrix-just 'center) 'vcent)
                                   (t 'vleft)))
	           (break calc-break-vectors))
	      (if (and (memq calc-language '(nil big))
		             (not calc-break-vectors)
		             (math-matrixp a) (not (math-matrixp (nth 1 a)))
		             (or calc-full-vectors
		                 (and (< (length a) 7) (< (length (nth 1 a)) 7))
		                 (progn (setq break t) nil)))
	          (if (progn
		              (setq math-comp-vector-prec (if (or (and calc-vector-commas
                                                           (math-vector-no-parens
                                                            (nth 1 a)))
                                                      (memq 'P calc-matrix-brackets))
                                                  0 1000))
		              (= (length a) 2))
		            (list 'horiz
		                  (concat math-comp-left-bracket math-comp-left-bracket " ")
		                  (math-compose-vector (cdr (nth 1 a)) (concat math-comp-comma " ")
					                                 math-comp-vector-prec)
		                  (concat " " math-comp-right-bracket math-comp-right-bracket))
	            (let* ((rows (1- (length a)))
		                 (cols (1- (length (nth 1 a))))
		                 (base (/ (1- rows) 2))
		                 (calc-language 'flat))
		            (append '(horiz)
			                  (list (append '(vleft)
				                              (list base)
				                              (list (concat (and outer-brackets
							                                           (concat math-comp-left-bracket
								                                                 " "))
						                                        (and inner-brackets
							                                           (concat math-comp-left-bracket
								                                                 " "))))
				                              (make-list (1- rows)
						                                     (concat (and outer-brackets
							                                                "  ")
							                                           (and inner-brackets
							                                                (concat
							                                                 math-comp-left-bracket
							                                                 " "))))))
			                  (math-compose-matrix (cdr a) 1 cols base)
			                  (list (append '(vleft)
				                              (list base)
				                              (make-list (1- rows)
						                                     (if inner-brackets
						                                         (concat " "
							                                               math-comp-right-bracket
							                                               (and row-commas
								                                                  math-comp-comma))
						                                       (if (and outer-brackets
							                                              row-commas)
						                                           ";" "")))
				                              (list (concat
					                                   (and inner-brackets
						                                      (concat " "
							                                            math-comp-right-bracket))
					                                   (and outer-brackets
						                                      (concat
						                                       " "
						                                       math-comp-right-bracket)))))))))
	        (if (and calc-display-strings
		               (cdr a)
		               (math-vector-is-string a))
	            (math-vector-to-string a t)
	          (if (and break (cdr a)
		                 (not (eq calc-language 'flat)))
		            (let* ((full (or calc-full-vectors (< (length a) 7)))
		                   (rows (if full (1- (length a)) 5))
		                   (base (/ (1- rows) 2))
		                   (calc-break-vectors nil))
		              (list 'horiz
			                  (cons 'vleft (cons base
					                                 (math-compose-rows
					                                  (cdr a)
					                                  (if full rows 3) t)))))
	            (if (or calc-full-vectors (< (length a) 7))
                  (if (and
                       (setq spfn (get calc-language 'math-matrix-formatter))
                       (math-matrixp a))
                      (funcall spfn a)
                    (list 'horiz
                          math-comp-left-bracket
                          (math-compose-vector (cdr a)
                                               (concat math-comp-comma " ")
                                               math-comp-vector-prec)
                          math-comp-right-bracket))
		            (list 'horiz
		                  math-comp-left-bracket
		                  (math-compose-vector (list (nth 1 a) (nth 2 a) (nth 3 a))
					                                 (concat math-comp-comma " ")
                                           math-comp-vector-prec)
		                  math-comp-comma
                      (if (setq spfn (get calc-language 'math-dots))
                          (concat " " spfn)
                        " ...")
		                  math-comp-comma " "
		                  (list 'break math-compose-level)
		                  (math-compose-expr (nth (1- (length a)) a)
					                               (if (equal math-comp-comma "") 1000 0))
		                  math-comp-right-bracket)))))))
     ((eq (car a) 'incomplete)
      (if (cdr (cdr a))
	        (cond ((eq (nth 1 a) 'vec)
		             (list 'horiz "["
		                   (math-compose-vector (cdr (cdr a)) ", " 0)
		                   " ..."))
		            ((eq (nth 1 a) 'cplx)
		             (list 'horiz "("
		                   (math-compose-vector (cdr (cdr a)) ", " 0)
		                   ", ..."))
		            ((eq (nth 1 a) 'polar)
		             (list 'horiz "("
		                   (math-compose-vector (cdr (cdr a)) "; " 0)
		                   "; ..."))
		            ((eq (nth 1 a) 'intv)
		             (list 'horiz
		                   (if (memq (nth 2 a) '(0 1)) "(" "[")
		                   (math-compose-vector (cdr (cdr (cdr a))) " .. " 0)
		                   " .. ..."))
		            (t (format "%s" a)))
	      (cond ((eq (nth 1 a) 'vec) "[ ...")
	            ((eq (nth 1 a) 'intv)
	             (if (memq (nth 2 a) '(0 1)) "( ..." "[ ..."))
	            (t "( ..."))))
     ((eq (car a) 'var)
      (let ((v (rassq (nth 2 a) math-expr-variable-mapping)))
	      (if v
	          (symbol-name (car v))
          (if (setq spfn (get calc-language 'math-var-formatter))
              (funcall spfn a prec)
            (math-compose-var a)))))
     ((eq (car a) 'intv)
      (list 'horiz
            (if (memq (nth 1 a) '(0 1)) "(" "[")
	          (math-compose-expr (nth 2 a) 0)
            " .. "
	          (math-compose-expr (nth 3 a) 0)
            (if (memq (nth 1 a) '(0 2)) ")" "]")))
     ((eq (car a) 'date)
      (if (eq (car calc-date-format) 'X)
	        (math-format-date a)
	      (concat "<" (math-format-date a) ">")))
     ((and (eq (car a) 'calcFunc-subscr)
           (setq spfn (get calc-language 'math-compose-subscr)))
      (funcall spfn a))
     ((and (eq (car a) 'calcFunc-subscr) (= (length a) 3)
	         (eq calc-language 'big))
      (let* ((a1 (math-compose-expr (nth 1 a) 1000))
	           (calc-language 'flat)
	           (a2 (math-compose-expr (nth 2 a) 0)))
	      (if (or (eq (car-safe a1) 'subscr)
		            (and (eq (car-safe a1) 'tag)
		                 (eq (car-safe (nth 2 a1)) 'subscr)
		                 (setq a1 (nth 2 a1))))
	          (list 'subscr
		              (nth 1 a1)
		              (list 'horiz
			                  (nth 2 a1)
			                  ", "
			                  a2))
	        (list 'subscr a1 a2))))
     ((and (eq (car a) '^)
	         (eq calc-language 'big))
      (list 'supscr
	          (if (or (math-looks-negp (nth 1 a))
		                (memq (car-safe (nth 1 a)) '(^ / frac calcFunc-sqrt))
		                (and (eq (car-safe (nth 1 a)) 'cplx)
			                   (math-negp (nth 1 (nth 1 a)))
			                   (eq (nth 2 (nth 1 a)) 0)))
		            (list 'horiz "(" (math-compose-expr (nth 1 a) 0) ")")
	            (math-compose-expr (nth 1 a) 201))
	          (let ((calc-language 'flat)
		              (calc-number-radix 10)
                  (calc-twos-complement-mode nil))
	            (math-compose-expr (nth 2 a) 0))))
     ((and (eq (car a) '/)
	         (eq calc-language 'big))
      (let ((a1 (let ((calc-language (if (memq (car-safe (nth 1 a)) '(/ frac))
					                               'flat 'big)))
		              (math-compose-expr (nth 1 a) 0)))
	          (a2 (let ((calc-language (if (memq (car-safe (nth 2 a)) '(/ frac))
					                               'flat 'big)))
		              (math-compose-expr (nth 2 a) 0))))
	      (list 'vcent
	            (math-comp-height a1)
	            a1 '(rule ?-) a2)))
     ((and (eq (car a) 'calcFunc-lambda)
	         (> (length a) 2)
	         (memq calc-language '(nil flat big)))
      (let ((p (cdr a))
	          (ap calc-arg-values)
	          (math-compose-hash-args (if (= (length a) 3) 1 t)))
	      (while (and (cdr p) (equal (car p) (car ap)))
	        (setq p (cdr p) ap (cdr ap)))
	      (append '(horiz "<")
		            (if (cdr p)
		                (list (math-compose-vector
			                     (nreverse (cdr (reverse (cdr a)))) ", " 0)
			                    " : ")
		              nil)
		            (list (math-compose-expr (nth (1- (length a)) a) 0)
		                  ">"))))
     ((and (eq (car a) 'calcFunc-string)
	         (= (length a) 2)
	         (math-vectorp (nth 1 a))
	         (math-vector-is-string (nth 1 a)))
      (if (eq calc-language 'unform)
	        (concat "string(" (math-vector-to-string (nth 1 a) t) ")")
	      (math-vector-to-string (nth 1 a) nil)))
     ((and (eq (car a) 'calcFunc-bstring)
	         (= (length a) 2)
	         (math-vectorp (nth 1 a))
	         (math-vector-is-string (nth 1 a)))
      (if (eq calc-language 'unform)
	        (concat "bstring(" (math-vector-to-string (nth 1 a) t) ")")
	      (let ((c nil)
	            (s (math-vector-to-string (nth 1 a) nil))
	            p)
	        (while (string-match "[^ ] +[^ ]" s)
	          (setq p (1- (match-end 0))
		              c (cons (list 'break math-compose-level)
			                    (cons (substring s 0 p)
				                        c))
		              s (substring s p)))
	        (setq c (nreverse (cons s c)))
	        (or (= prec -123)
	            (setq c (cons (list 'set math-compose-level 2) c)))
	        (cons 'horiz c))))
     ((and (eq (car a) 'calcFunc-cprec)
	         (not (eq calc-language 'unform))
	         (= (length a) 3)
	         (integerp (nth 2 a)))
      (let ((c (math-compose-expr (nth 1 a) -1)))
	      (if (> prec (nth 2 a))
            (if (setq spfn (get calc-language 'math-big-parens))
                (list 'horiz (car spfn) c (cdr spfn))
              (list 'horiz "(" c ")"))
	        c)))
     ((and (eq (car a) 'calcFunc-choriz)
	         (not (eq calc-language 'unform))
	         (memq (length a) '(2 3 4))
	         (math-vectorp (nth 1 a))
	         (if (integerp (nth 2 a))
	             (or (null (nth 3 a))
		               (and (math-vectorp (nth 3 a))
			                  (math-vector-is-string (nth 3 a))))
	           (or (null (nth 2 a))
		             (and (math-vectorp (nth 2 a))
		                  (math-vector-is-string (nth 2 a))))))
      (let* ((cprec (and (integerp (nth 2 a)) (nth 2 a)))
	           (sep (nth (if cprec 3 2) a))
	           (bprec nil))
	      (if sep
	          (math-compose-vector (cdr (nth 1 a))
				                         (math-vector-to-string sep nil)
				                         (or cprec prec))
	        (cons 'horiz (mapcar (function
				                        (lambda (x)
				                          (if (eq (car-safe x) 'calcFunc-bstring)
				                              (prog1
					                                (math-compose-expr
					                                 x (or bprec cprec prec))
					                              (setq bprec -123))
				                            (math-compose-expr x (or cprec prec)))))
			                         (cdr (nth 1 a)))))))
     ((and (memq (car a) '(calcFunc-cvert calcFunc-clvert calcFunc-crvert))
	         (not (eq calc-language 'unform))
	         (memq (length a) '(2 3))
	         (math-vectorp (nth 1 a))
	         (or (null (nth 2 a))
	             (integerp (nth 2 a))))
      (let* ((base 0)
	           (v 0)
	           (prec (or (nth 2 a) prec))
	           (c (mapcar (function
			                   (lambda (x)
			                     (let ((b nil) (cc nil) a d)
			                       (if (and (memq (car-safe x) '(calcFunc-cbase
							                                             calcFunc-ctbase
							                                             calcFunc-cbbase))
				                              (memq (length x) '(1 2)))
				                         (setq b (car x)
				                               x (nth 1 x)))
			                       (if (and (eq (car-safe x) 'calcFunc-crule)
				                              (memq (length x) '(1 2))
				                              (or (null (nth 1 x))
					                                (and (math-vectorp (nth 1 x))
					                                     (= (length (nth 1 x)) 2)
					                                     (math-vector-is-string
						                                    (nth 1 x)))
					                                (and (natnump (nth 1 x))
					                                     (<= (nth 1 x) 255))))
				                         (setq cc (list
					                                 'rule
					                                 (if (math-vectorp (nth 1 x))
					                                     (aref (math-vector-to-string
						                                          (nth 1 x) nil) 0)
					                                   (or (nth 1 x) ?-))))
			                         (or (and (memq (car-safe x) '(calcFunc-cvspace
							                                               calcFunc-ctspace
							                                               calcFunc-cbspace))
					                              (memq (length x) '(2 3))
					                              (eq (nth 1 x) 0))
				                           (null x)
				                           (setq cc (math-compose-expr x prec))))
			                       (setq a (if cc (math-comp-ascent cc) 0)
				                           d (if cc (math-comp-descent cc) 0))
			                       (if (eq b 'calcFunc-cbase)
				                         (setq base (+ v a -1))
			                         (if (eq b 'calcFunc-ctbase)
				                           (setq base v)
				                         (if (eq b 'calcFunc-cbbase)
				                             (setq base (+ v a d -1)))))
			                       (setq v (+ v a d))
			                       cc)))
			                  (cdr (nth 1 a)))))
	      (setq c (delq nil c))
	      (if c
	          (cons (if (eq (car a) 'calcFunc-cvert) 'vcent
		                (if (eq (car a) 'calcFunc-clvert) 'vleft 'vright))
		              (cons base c))
	        " ")))
     ((and (memq (car a) '(calcFunc-csup calcFunc-csub))
	         (not (eq calc-language 'unform))
	         (memq (length a) '(3 4))
	         (or (null (nth 3 a))
	             (integerp (nth 3 a))))
      (list (if (eq (car a) 'calcFunc-csup) 'supscr 'subscr)
	          (math-compose-expr (nth 1 a) (or (nth 3 a) 0))
	          (math-compose-expr (nth 2 a) 0)))
     ((and (eq (car a) 'calcFunc-cflat)
	         (not (eq calc-language 'unform))
	         (memq (length a) '(2 3))
	         (or (null (nth 2 a))
	             (integerp (nth 2 a))))
      (let ((calc-language (if (memq calc-language '(nil big))
			                         'flat calc-language)))
	      (math-compose-expr (nth 1 a) (or (nth 2 a) 0))))
     ((and (eq (car a) 'calcFunc-cspace)
	         (memq (length a) '(2 3))
	         (natnump (nth 1 a)))
      (if (nth 2 a)
	        (cons 'horiz (make-list (nth 1 a)
				                          (if (and (math-vectorp (nth 2 a))
					                                 (math-vector-is-string (nth 2 a)))
				                              (math-vector-to-string (nth 2 a) nil)
				                            (math-compose-expr (nth 2 a) 0))))
	      (make-string (nth 1 a) ?\ )))
     ((and (memq (car a) '(calcFunc-cvspace calcFunc-ctspace calcFunc-cbspace))
	         (memq (length a) '(2 3))
	         (natnump (nth 1 a)))
      (if (= (nth 1 a) 0)
	        ""
	      (let* ((c (if (nth 2 a)
		                  (if (and (math-vectorp (nth 2 a))
			                         (math-vector-is-string (nth 2 a)))
			                    (math-vector-to-string (nth 2 a) nil)
			                  (math-compose-expr (nth 2 a) 0))
		                " "))
	             (ca (math-comp-ascent c))
	             (cd (math-comp-descent c)))
	        (cons 'vleft
		            (cons (if (eq (car a) 'calcFunc-ctspace)
			                    (1- ca)
			                  (if (eq (car a) 'calcFunc-cbspace)
			                      (+ (* (1- (nth 1 a)) (+ ca cd)) (1- ca))
			                    (/ (1- (* (nth 1 a) (+ ca cd))) 2)))
		                  (make-list (nth 1 a) c))))))
     ((and (eq (car a) 'calcFunc-evalto)
	         (setq calc-any-evaltos t)
	         (setq spfn (get calc-language 'math-evalto))
	         (= math-compose-level (if math-comp-tagged 2 1))
	         (= (length a) 3))
      (list 'horiz
            (car spfn)
	          (math-compose-expr (nth 1 a) 0)
	          (cdr spfn)
	          (math-compose-expr (nth 2 a) 0)))
     (t
      (let ((op (and (not (eq calc-language 'unform))
		                 (if (and (eq (car a) 'calcFunc-if) (= (length a) 4))
			                   (assoc "?" math-expr-opers)
		                   (math-assq2 (car a) math-expr-opers)))))
        (message "%s" op)
	      (cond ((and op
		                (or (= (length a) 3) (eq (car a) 'calcFunc-if))
		                (/= (nth 3 op) -1))
	             (cond
		            ((or
                  (> prec (or (nth 4 op) (min (nth 2 op) (nth 3 op))))
                  (and div (eq (car a) '*)))
		             (if (and (memq calc-language '(tex latex))
			                    (not (math-tex-expr-is-flat a)))
		                 (if (eq (car-safe a) '/)
			                   (list 'horiz "{" (math-compose-expr a -1) "}")
		                   (list 'horiz "\\left( "
			                       (math-compose-expr a -1)
			                       " \\right)"))
		               (if (eq calc-language 'eqn)
		                   (if (or (eq (car-safe a) '/)
			                         (= (/ prec 100) 9))
			                     (list 'horiz "{" (math-compose-expr a -1) "}")
			                   (if (math-tex-expr-is-flat a)
			                       (list 'horiz "( " (math-compose-expr a -1) " )")
			                     (list 'horiz "{left ( "
				                         (math-compose-expr a -1)
				                         " right )}")))
		                 (list 'horiz "(" (math-compose-expr a 0) ")"))))
		            ((and (memq calc-language '(tex latex))
		                  (memq (car a) '(/ calcFunc-choose calcFunc-evalto))
		                  (>= prec 0))
		             (list 'horiz "{" (math-compose-expr a -1) "}"))
		            ((eq (car a) 'calcFunc-if)
		             (list 'horiz
		                   (math-compose-expr (nth 1 a) (nth 2 op))
		                   " ? "
		                   (math-compose-expr (nth 2 a) 0)
		                   " : "
		                   (math-compose-expr (nth 3 a) (nth 3 op))))
		            (t
		             (let* ((math-comp-tagged (and math-comp-tagged
					                                     (not (math-primp a))
					                                     math-comp-tagged))
			                  (setlev (if (= prec (min (nth 2 op) (nth 3 op)))
				                            (progn
				                              (setq math-compose-level
					                                  (1- math-compose-level))
				                              nil)
				                          math-compose-level))
			                  (lhs (math-compose-expr (nth 1 a) (nth 2 op)))
			                  (rhs (math-compose-expr (nth 2 a) (nth 3 op) (eq (nth 1 op) '/))))
		               (and (equal (car op) "^")
			                  (eq (math-comp-first-char lhs) ?-)
			                  (setq lhs (list 'horiz "(" lhs ")")))
		               (and (memq calc-language '(tex latex))
			                  (or (equal (car op) "^") (equal (car op) "_"))
			                  (not (and (stringp rhs) (= (length rhs) 1)))
			                  (setq rhs (list 'horiz "{" rhs "}")))
		               (or (and (message "math-expr-opers: %s" math-expr-opers)
                        (eq (car a) '*)
			                      (or (null calc-language)
				                        (assoc "2x" math-expr-opers))
			                      (let* ((prevt (math-prod-last-term (nth 1 a)))
				                           (nextt (math-prod-first-term (nth 2 a)))
				                           (prevc (or (math-comp-last-char lhs)
					                                    (and (memq (car-safe prevt)
							                                           '(^ calcFunc-subscr
							                                               calcFunc-sqrt
							                                               frac))
						                                       (eq calc-language 'big)
						                                       ?0)))
				                           (nextc (or (math-comp-first-char rhs)
					                                    (and (memq (car-safe nextt)
							                                           '(calcFunc-sqrt
							                                             calcFunc-sum
							                                             calcFunc-prod
							                                             calcFunc-integ))
						                                       (eq calc-language 'big)
						                                       ?0))))
			                        (and prevc nextc
				                           (or (and (>= nextc ?a) (<= nextc ?z))
				                               (and (>= nextc ?A) (<= nextc ?Z))
				                               (and (>= nextc ?α) (<= nextc ?ω))
				                               (and (>= nextc ?Α) (<= nextc ?Ω))
				                               (and (>= nextc ?0) (<= nextc ?9))
				                               (memq nextc '(?. ?_ ?#
							                                          ?\( ?\[ ?\{))
				                               (and (eq nextc ?\\)
					                                  (not (string-match
						                                      "\\`\\\\left("
						                                      (math-comp-first-string
						                                       rhs)))))
				                           (not (and (not (memq calc-language '(tex latex)))
                                             (eq (car-safe prevt) 'var)
					                                   (eq nextc ?\()))
				                           (list 'horiz
					                               (list 'set setlev 1)
					                               lhs
					                               (list 'break math-compose-level)
					                               " "
					                               rhs))))
		                   (list 'horiz
			                       (list 'set setlev 1)
			                       lhs
			                       (list 'break math-compose-level)
			                       (if (or (equal (car op) "^")
				                             (equal (car op) "_")
				                             (equal (car op) "**")
				                             (and (equal (car op) "*")
					                                (math-comp-last-char lhs)
					                                (math-comp-first-char rhs))
				                             (and (equal (car op) "/")
					                                (math-num-integerp (nth 1 a))
					                                (math-integerp (nth 2 a))))
				                         (car op)
			                         (if (and (eq calc-language 'big)
					                              (equal (car op) "=>"))
				                           "  =>  "
				                         (concat " " (car op) " ")))
			                       rhs))))))
	            ((and op (= (length a) 2) (= (nth 3 op) -1))
	             (cond
		            ((or (> prec (or (nth 4 op) (nth 2 op)))
		                 (and (not (eq (assoc (car op) math-expr-opers) op))
			                    (> prec 0)))  ; don't write x% + y
		             (if (and (memq calc-language '(tex latex))
			                    (not (math-tex-expr-is-flat a)))
		                 (list 'horiz "\\left( "
			                     (math-compose-expr a -1)
			                     " \\right)")
		               (if (eq calc-language 'eqn)
		                   (if (= (/ prec 100) 9)
			                     (list 'horiz "{" (math-compose-expr a -1) "}")
			                   (if (math-tex-expr-is-flat a)
			                       (list 'horiz "{( " (math-compose-expr a -1) " )}")
			                     (list 'horiz "{left ( "
				                         (math-compose-expr a -1)
				                         " right )}")))
		                 (list 'horiz "(" (math-compose-expr a 0) ")"))))
		            (t
		             (let ((lhs (math-compose-expr (nth 1 a) (nth 2 op))))
		               (list 'horiz
		                     lhs
		                     (if (or (> (length (car op)) 1)
			                           (not (math-comp-is-flat lhs)))
			                       (concat " " (car op))
			                     (car op)))))))
	            ((and op (= (length a) 2) (= (nth 2 op) -1))
	             (cond
		            ((eq (nth 3 op) 0)
		             (let ((lr (and (memq calc-language '(tex latex))
				                        (not (math-tex-expr-is-flat (nth 1 a))))))
		               (list 'horiz
			                   (if lr "\\left" "")
			                   (if (string-match "\\`u\\([^a-zA-Zα-ωΑ-Ω]\\)\\'" (car op))
			                       (substring (car op) 1)
			                     (car op))
			                   (if (or lr (> (length (car op)) 2)) " " "")
			                   (math-compose-expr (nth 1 a) -1)
			                   (if (or lr (> (length (car op)) 2)) " " "")
			                   (if lr "\\right" "")
			                   (car (nth 1 (memq op math-expr-opers))))))
		            ((> prec (or (nth 4 op) (nth 3 op)))
		             (if (and (memq calc-language '(tex latex))
			                    (not (math-tex-expr-is-flat a)))
		                 (list 'horiz "\\left( "
			                     (math-compose-expr a -1)
			                     " \\right)")
		               (if (eq calc-language 'eqn)
		                   (if (= (/ prec 100) 9)
			                     (list 'horiz "{" (math-compose-expr a -1) "}")
			                   (if (math-tex-expr-is-flat a)
			                       (list 'horiz "{( " (math-compose-expr a -1) " )}")
			                     (list 'horiz "{left ( "
				                         (math-compose-expr a -1)
				                         " right )}")))
		                 (list 'horiz "(" (math-compose-expr a 0) ")"))))
		            (t
		             (let ((rhs (math-compose-expr (nth 1 a) (nth 3 op))))
		               (list 'horiz
			                   (let ((ops (if (string-match "\\`u\\([^a-zA-Zα-ωΑ-Ω]\\)\\'"
						                                          (car op))
					                              (substring (car op) 1)
				                              (car op))))
			                     (if (or (> (length ops) 1)
				                           (not (math-comp-is-flat rhs)))
			                         (concat ops " ")
			                       ops))
			                   rhs)))))
	            ((and (eq calc-language 'big)
		                (setq op (get (car a) 'math-compose-big))
		                (funcall op a prec)))
	            ((and (setq op (assq calc-language
				                           '( ( nil . math-compose-normal )
				                              ( flat . math-compose-normal )
				                              ( big . math-compose-normal )
				                              ( c . math-compose-c )
				                              ( pascal . math-compose-pascal )
				                              ( fortran . math-compose-fortran )
				                              ( tex . math-compose-tex )
				                              ( latex . math-compose-latex )
				                              ( eqn . math-compose-eqn )
                                      ( yacas . math-compose-yacas )
                                      ( maxima . math-compose-maxima )
                                      ( giac . math-compose-giac )
				                              ( math . math-compose-math )
				                              ( maple . math-compose-maple ))))
		                (setq op (get (car a) (cdr op)))
		                (funcall op a prec)))
	            (t
	             (let* ((func (car a))
		                  (func2 (assq func '(( mod . calcFunc-makemod )
					                                ( sdev . calcFunc-sdev )
					                                ( + . calcFunc-add )
					                                ( - . calcFunc-sub )
					                                ( * . calcFunc-mul )
					                                ( / . calcFunc-div )
					                                ( % . calcFunc-mod )
					                                ( ^ . calcFunc-pow )
					                                ( neg . calcFunc-neg )
					                                ( | . calcFunc-vconcat ))))
		                  left right args)
		             (if func2
		                 (setq func (cdr func2)))
		             (if (setq func2 (rassq func math-expr-function-mapping))
		                 (setq func (car func2)))
		             (setq func (math-remove-dashes
			                       (if (string-match
				                          "\\`calcFunc-\\([a-zA-Zα-ωΑ-Ω0-9']+\\)\\'"
				                          (symbol-name func))
				                         (math-match-substring (symbol-name func) 1)
			                         (symbol-name func))))
		             (if (memq calc-language calc-lang-allow-percentsigns)
		                 (setq func (math-to-percentsigns func)))
		             (if (memq calc-language calc-lang-allow-underscores)
		                 (setq func (math-to-underscores func)))
                 (if (setq spfn (get calc-language 'math-func-formatter))
                     (funcall spfn func a)
                   (list 'horiz func calc-function-open
		                     (math-compose-vector (cdr a) ", " 0)
		                     calc-function-close))))))))))

