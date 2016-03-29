#!/bin/bash
R-3.0 CMD INSTALL ADaCGH2_2.1.0.tar.gz
time R-3.0 CMD Sweave ./ADaCGH2/inst/doc/ADaCGH2-long-examples.Rnw
texi2pdf ADaCGH2-long-examples.tex
## I do not use the "pdf" option because it searches for tex in
## the tmp dir. I thik. The point is it does not find it.
mv ADaCGH2-long-examples.pdf ./ADaCGH2/inst/doc/.
## rm ADaCGH2-long-examples.tex
rm ADaCGH2-long-examples.aux
rm ADaCGH2-long-examples.log
rm ADaCGH2-long-examples.out
rm ADaCGH2-long-examples.toc
