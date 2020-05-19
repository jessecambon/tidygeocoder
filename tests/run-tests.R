#!/usr/bin/env Rscript

#renv::load(dirname(getwd()))

library(testthat)
library(tidygeocoder)

test_check("tidygeocoder")
