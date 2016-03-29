#!/bin/bash

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


time $V_R CMD build --no-build-vignettes --keep-empty-dirs --no-resave-data ADaCGH2
time $V_R CMD INSTALL ADaCGH2_$V_ADA.tar.gz

## the --no-rebuild-vignettes is no longer valid


