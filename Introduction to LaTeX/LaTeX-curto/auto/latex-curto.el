(TeX-add-style-hook
 "latex-curto"
 (lambda ()
   (setq TeX-command-extra-options
         " -shell-escape ")
   (add-to-list 'LaTeX-verbatim-environments-local "semiverbatim")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperref")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperimage")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "hyperbaseurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "nolinkurl")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "url")
   (add-to-list 'LaTeX-verbatim-macros-with-braces-local "path")
   (add-to-list 'LaTeX-verbatim-macros-with-delims-local "path")
   (TeX-run-style-hooks
    "latex2e"
    "defs"
    "./conteudo/basico"
    "./conteudo/fontes"
    "./conteudo/secoes"
    "./conteudo/dividindo"
    "./conteudo/floats"
    "./conteudo/modo-matematico"
    "./conteudo/comandos-contadores"
    "./conteudo/final"
    "beamer"
    "beamer10"))
 :latex)

