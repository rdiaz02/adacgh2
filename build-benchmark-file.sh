#!/bin/bash
cd ADaCGH2/inst/sources-tex-Rnw-other
texi2pdf benchmarks.tex
## I do not use the "pdf" option because it searches for tex in
## the tmp dir. I thik. The point is it does not find it.
mv benchmarks.pdf ../../inst/doc/
rm benchmarks.aux
rm benchmarks.log
rm benchmarks.out
rm benchmarks.toc
rm benchmarks.lot
rm benchmarks.lof
rm benchmarks.blg
rm benchmarks.bbl
rm benchmarks.synctex.gz
rm *~
cd auto
rm *.el
cd ..
rmdir auto
