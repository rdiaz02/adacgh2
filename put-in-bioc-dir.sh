#!/bin/bash

## needs clean up because I am copying things that are not needed
cp ADaCGH2/vignettes/ADaCGH2.Rnw ~/Proyectos/adacgh2-bioc-git/Subversion-in-BioC/ADaCGH2/vignettes/.
cp ADaCGH2/vignettes/ADaCGH2.bib ~/Proyectos/adacgh2-bioc-git/Subversion-in-BioC/ADaCGH2/vignettes/.
cp ADaCGH2/inst/doc/ADaCGH2-long-examples.* ~/Proyectos/adacgh2-bioc-git/Subversion-in-BioC/ADaCGH2/inst/doc/.
cp ADaCGH2/inst/doc/benchmarks.pdf ~/Proyectos/adacgh2-bioc-git/Subversion-in-BioC/ADaCGH2/inst/doc/.
cp ADaCGH2/R/ADaCGH-2.R ~/Proyectos/adacgh2-bioc-git/Subversion-in-BioC/ADaCGH2/R/.
cp ADaCGH2/DESCRIPTION ~/Proyectos/adacgh2-bioc-git/Subversion-in-BioC/ADaCGH2/.
cp ADaCGH2/NAMESPACE ~/Proyectos/adacgh2-bioc-git/Subversion-in-BioC/ADaCGH2/.
cp ADaCGH2/man/*.Rd ~/Proyectos/adacgh2-bioc-git/Subversion-in-BioC/ADaCGH2/man/.
cp ADaCGH2/inst/NEWS ~/Proyectos/adacgh2-bioc-git/Subversion-in-BioC/ADaCGH2/inst/.
cp ADaCGH2/inst/sources-tex-Rnw-other/*.Rnw ~/Proyectos/adacgh2-bioc-git/Subversion-in-BioC/ADaCGH2/inst/sources-tex-Rnw-other/.
cp ADaCGH2/inst/sources-tex-Rnw-other/*.R ~/Proyectos/adacgh2-bioc-git/Subversion-in-BioC/ADaCGH2/inst/sources-tex-Rnw-other/.
cp ADaCGH2/inst/sources-tex-Rnw-other/*.bib ~/Proyectos/adacgh2-bioc-git/Subversion-in-BioC/ADaCGH2/inst/sources-tex-Rnw-other/.
cp ADaCGH2/inst/sources-tex-Rnw-other/*.tex ~/Proyectos/adacgh2-bioc-git/Subversion-in-BioC/ADaCGH2/inst/sources-tex-Rnw-other/.
cp ADaCGH2/inst/sources-tex-Rnw-other/anal-benchmark-fig.pdf ~/Proyectos/adacgh2-bioc-git/Subversion-in-BioC/ADaCGH2/inst/sources-tex-Rnw-other/.
cp ADaCGH2/inst/sources-tex-Rnw-other/reading-benchmark-fig.pdf ~/Proyectos/adacgh2-bioc-git/Subversion-in-BioC/ADaCGH2/inst/sources-tex-Rnw-other/.
cp ADaCGH2/inst/sources-tex-Rnw-other/README-sources-tex-Rnw-other ~/Proyectos/adacgh2-bioc-git/Subversion-in-BioC/ADaCGH2/inst/sources-tex-Rnw-other/.
grep -v "^%" ADaCGH2/inst/sources-tex-Rnw-other/benchmarks.tex > /home/ramon/Proyectos/adacgh2-bioc-git/Subversion-in-BioC/ADaCGH2/inst/sources-tex-Rnw-other/benchmarks.tex
cp ADaCGH2/inst/doc/index.html ~/Proyectos/adacgh2-bioc-git/Subversion-in-BioC/ADaCGH2/inst/doc/.
cp ADaCGH2/README.code.authors ~/Proyectos/adacgh2-bioc-git/Subversion-in-BioC/ADaCGH2/.
cp ADaCGH2/src/* ~/Proyectos/adacgh2-bioc-git/Subversion-in-BioC/ADaCGH2/src/.


cp ADaCGH2/inst/CITATION ~/Proyectos/adacgh2-bioc-git/Subversion-in-BioC/ADaCGH2/inst/.


## check if things need adding

## data not coppied, since very rarely changed

## should we run the tests?

if [[ $# == 1 ]]; then
    V_R=$1
    cd ~/Proyectos/adacgh2-bioc-git/Subversion-in-BioC
    V_P=$(cat ./ADaCGH2/DESCRIPTION | grep Version | cut -d' ' -f2)
    rm ADaCGH2_$V_P.tar.gz
    ## As shown in build report from BioC
    echo " ***************************************** "
    echo " *********     R CMD build  ************** "
    echo " "
    time $V_R --vanilla CMD build --keep-empty-dirs --no-resave-data ADaCGH2
    echo " "
    echo " ===========  done R CMD build   ========== "
    echo " "
    ## As shown in check report from BioC
    echo " ***************************************** "
    echo " **** R CMD check, as in check report **** "
    echo ""
    time $V_R --vanilla CMD check --no-vignettes --timings ADaCGH2_$V_P.tar.gz
    echo " "
    echo " =========   done R CMD check as in check report  =======  "
    echo " "
    # I skip this since not needed with adacgh2
    # ## time as explained in https://www.bioconductor.org/developers/package-guidelines/#correctness
    # echo " ************************************ "
    # echo " *****   R CMD check: time OK?  ***** "
    # echo ""
    # time $V_R CMD check --no-build-vignettes ADaCGH2_$V_P.tar.gz
    # echo " "
    # echo " ===========  done R CMD check time OK?   ========== "
fi


## Check what/if things need adding
cd ~/Proyectos/adacgh2-bioc-git/Subversion-in-BioC/ADaCGH2

svn status --show-updates

## to create the directory
## tar --exclude=.svn -zcvf ADaCGH2-como-esta-en-BioC.tar.gz ADaCGH2
