(TeX-add-style-hook "ADaCGH2-long-examples"
 (lambda ()
    (LaTeX-add-labels
     "all-examples"
     "allex-forking-local"
     "allex-ff-cluster"
     "allex-ff-fork"
     "allex-comparing")
    (TeX-run-style-hooks
     "datetime"
     "inputenc"
     "latin1"
     "geometry"
     "array"
     "hyperref"
     "threeparttable"
     "amsmath"
     ""
     "latex2e"
     "art11"
     "article"
     "11pt"
     "a4paper")))

