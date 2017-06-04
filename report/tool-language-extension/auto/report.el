(TeX-add-style-hook
 "report"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("sigplanconf" "nocopyrightspace" "11pt" "authoryear" "preprint")))
   (add-to-list 'LaTeX-verbatim-environments-local "lstlisting")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "lstinline")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "lstinline")
   (TeX-run-style-hooks
    "latex2e"
    "tool-listings"
    "introduction"
    "semicolon_inference"
    "infix_operators"
    "parameterless_calls"
    "extensions"
    "sigplanconf"
    "sigplanconf11"
    "amsmath"
    "listings"
    "xcolor"
    "lmodern")
   (LaTeX-add-bibliographies
    "bibliography"))
 :latex)

