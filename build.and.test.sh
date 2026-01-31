#!/bin/bash

export R_CHECK_ENVIRON="~/.R/Renviron.bioc"

V_R=$1

V_ADA=$(cat ADaCGH2/DESCRIPTION | grep Version | cut -d' ' -f2)

rm ADaCGH2_$V_ADA.tar.gz

rm ./ADaCGH2/src/*.so
rm ./ADaCGH2/src/*.o
rm ./ADaCGH2.Rcheck/* -r -f
rm ./ADaCGH2/inst/doc/auto/*
rmdir ./ADaCGH2/inst/doc/auto
rm ./ADaCGH2/vignettes/auto/*
rmdir ./ADaCGH2/vignettes/auto
rm ./ADaCGH2/inst/long-examples-sources/auto/*
rmdir ./ADaCGH2/inst/long-examples-sources/auto


time R_ENVIRON_USER="~/.R/Renviron.bioc" $V_R --vanilla CMD build --keep-empty-dirs --no-resave-data ADaCGH2

time R_ENVIRON_USER="~/.R/Renviron.bioc" $V_R --vanilla CMD check --no-vignettes --timings ADaCGH2_$V_ADA.tar.gz

time R_ENVIRON_USER="~/.R/Renviron.bioc" $V_R --vanilla CMD check --as-cran --no-vignettes --timings ADaCGH2_$V_ADA.tar.gz

## the --no-rebuild-vignettes is no longer valid
