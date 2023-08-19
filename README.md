
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Respiratory Rate Determination by Non-Invasive Means

This repository consists of an R project for COMP90072 - The Art of
Scientific Computation, holding the source files for reproducing the
deliverables, including the code files and their version history.

## Requirements

To reproduce the deliverables of the project, you will need to install
the following packages and their dependencies as follows:

``` r
install.packages("xaringan")
install.packages("xaringanthemer")
install.packages("tidyverse")
install.packages("furrr")
install.packages("zoo")
```

## Reproducible research

The deliverables of the project consist of a PDF report and an HTML
slide rendered by RMarkdown.

``` r
rmarkdown::render("report/report.Rmd")
# rmarkdown::render("slide/slide.Rmd")
```
