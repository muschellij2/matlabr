# matlabr

`matlabr` is an R package to call MATLAB directly using `system` command.  This is a simple package that tries to get around using `R.matlab` and a MATLAB server.

[![Travis-CI Build Status](https://travis-ci.org/muschellij2/matlabr.png?branch=master)](https://travis-ci.org/muschellij2/matlabr)

## Installation

You can install the stable version on
[CRAN](http://cran.rstudio.com/package=matlabr):

```r
install.packages('matlabr', dependencies = TRUE)
```

Install in `R` using `devtools`:
```r
devtools::install_github("muschellij2/matlabr")
```