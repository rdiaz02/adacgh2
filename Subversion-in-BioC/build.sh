#!/bin/bash
rm ADaCGH2_2.1.0.tar.gz
rm ./ADaCGH2.Rcheck/* -r -f
rm ./ADaCGH2/inst/doc/auto/*
rmdir ./ADaCGH2/inst/doc/auto
rm ./ADaCGH2/vignettes/auto/*
rmdir ./ADaCGH2/vignettes/auto

time R-3.0 CMD build --no-vignettes --keep-empty-dirs --no-resave-data ADaCGH2

