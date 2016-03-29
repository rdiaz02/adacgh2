#!/bin/bash
rm ADaCGH2_2.1.*.tar.gz

rm ./ADaCGH2/src/*.so
rm ./ADaCGH2/src/*.o
rm ./ADaCGH2.Rcheck/* -r -f
rm ./ADaCGH2/inst/doc/auto/*
rmdir ./ADaCGH2/inst/doc/auto
rm ./ADaCGH2/vignettes/auto/*
rmdir ./ADaCGH2/vignettes/auto

time R-3.0 CMD build --keep-empty-dirs --no-resave-data ADaCGH2

time R-3.0 CMD check --no-vignettes --timings ADaCGH2_2.1.6.tar.gz

## the --no-rebuild-vignettes is no longer valid
