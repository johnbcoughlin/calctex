(TeX-add-style-hook
 "4a"
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
    "colornucleus")
   (LaTeX-add-xcolor-definecolors
    "currcolor"))
 :latex)

