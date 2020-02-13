(TeX-add-style-hook
 "2a"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("color" "usenames")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art10"
    "color"
    "xcolor"
    "soul"
    "adjustbox"
    "xparse")
   (TeX-add-symbols
    '("cmt" 1)
    "colornucleus")
   (LaTeX-add-xcolor-definecolors
    "currcolor"
    "fg"
    "bg"))
 :latex)

