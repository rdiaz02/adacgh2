#!/bin/bash
V_R=$1

## building the benchmark might, or might not, be a good idea
## it is just texing a file, which makes no sense if there have been no
## changes
## ./build-benchmark-file.sh
./build.and.test.sh $V_R
./build-long-vignette.sh $V_R
./build.and.test.sh $V_R
