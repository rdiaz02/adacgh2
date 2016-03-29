#!/bin/bash
V_R=$1
V_ADA=$(cat ADaCGH2/DESCRIPTION | grep Version | cut -d' ' -f2)

$V_R CMD INSTALL ADaCGH2_$V_ADA.tar.gz
time $V_R CMD Sweave ./ADaCGH2/inst/sources-tex-Rnw-other/ADaCGH2-long-examples.Rnw
texi2pdf ADaCGH2-long-examples.tex
## I do not use the "pdf" option because it searches for tex in
## the tmp dir. I thik. The point is it does not find it.
mv ADaCGH2-long-examples.pdf ./ADaCGH2/inst/doc/.
## rm ADaCGH2-long-examples.tex
rm ADaCGH2-long-examples.aux
rm ADaCGH2-long-examples.log
rm ADaCGH2-long-examples.out
# rm ADaCGH2-long-examples.tex
rm ADaCGH2-long-examples.toc
time $V_R CMD Stangle ./ADaCGH2/inst/sources-tex-Rnw-other/ADaCGH2-long-examples.Rnw
mv ADaCGH2-long-examples.R ./ADaCGH2/inst/sources-tex-Rnw-other/.
