#!/bin/bash

echo "Running dummy.R with R CMD BATCH"
R CMD BATCH --vanilla dummy.R ./output-files/dummy-cmd-batch.Rout &
echo "Done"

echo "Running dummy.R with Rscript"
Rscript --vanilla dummy.R > ./output-files/dummy-rscript.Rout &
echo "Done"

echo "Runnning since_born.R as an R executable program"
./since_born.R 1999-04-20
echo "Done"

echo "Running since_born.R Rscript in a bash script"
sh since_born_bash.sh
echo "Done"

echo "Running rcpp.R script, which calls C++ script"
Rscript rcpp.R
echo "Done"
