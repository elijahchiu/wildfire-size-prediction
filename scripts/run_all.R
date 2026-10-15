#!/usr/bin/env Rscript
# Reproducible end-to-end build for the wildfire size classification project.
options(stringsAsFactors = FALSE, scipen = 999)
root <- normalizePath(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value=TRUE)[1])), ".."), mustWork=TRUE)
setwd(root)
source("scripts/pipeline.R")
run_pipeline(root)
if (requireNamespace("rmarkdown", quietly=TRUE)) {
  rmarkdown::render("report/wildfire_analysis.Rmd", output_file="index.html", output_dir="docs", quiet=TRUE, envir=new.env(parent=globalenv()))
} else stop("Package rmarkdown is required to render docs/index.html")
cat("Build completed. See results/ and docs/index.html\n")
