(TeX-add-style-hook
 "pythag"
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
    "xparse"))
 :latex)

