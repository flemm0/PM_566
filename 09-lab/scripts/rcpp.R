#!/usr/bin/env Rscript

library(Rcpp)

Rcpp::sourceCpp("fib.cpp")

c(fibCpp(1), fibCpp(2), fibCpp(3), fibCpp(4), fibCpp(5))
