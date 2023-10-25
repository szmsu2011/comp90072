
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Respiratory Rate Determination by Non-Invasive Means

This repository consists of an R project for COMP90072 - The Art of
Scientific Computation, holding the source files for reproducing the
deliverable, including the code files and their version history.

## Requirements

To reproduce the deliverable of the project, you will need to install
the following packages and their dependencies as follows:

``` r
install.packages("tidyverse")
install.packages("zoo")
install.packages("patchwork")
install.packages("tinytex")
tinytex::install_tinytex()
```

## Reproducible research

The deliverable of the project consists of a PDF report rendered by
RMarkdown.

``` r
rmarkdown::render("report/report.Rmd")
```

The [report](https://github.com/szmsu2011/comp90072/tree/main/report)
directory consists of source files for reproducing the PDF report. The
[R](https://github.com/szmsu2011/comp90072/tree/main/R) directory
consists of source files for the functions used in the workflow. The
[data-bin](https://github.com/szmsu2011/comp90072/tree/main/data-bin)
directory holds the binary data files for the project.
