(TeX-add-style-hook
 "joules"
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
    "siunitx"
    "amsmath"
    "xparse")
   (TeX-add-symbols
    '("cmt" 1)))
 :latex)

