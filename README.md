# mappp

> **map** in **p**arallel with **p**rogress

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/mappp)](https://CRAN.R-project.org/package=mappp)
[![R build status](https://github.com/cole-brokamp/mappp/workflows/R-CMD-check/badge.svg)](https://github.com/cole-brokamp/mappp/actions)
<!-- badges: end -->

The goal of mappp is to provide a simple implementation of `purrr::map` or `base::lapply` that provides enhanced features like parallel computation, progress bars, error handling, and result caching.

- **progress**: `mappp()` will always report its progress
- **parallel**: if `parallel = TRUE`, `mappp()` will attempt to calculate in parallel by using the maximum number of available cores
- **cache**: if `cache = TRUE`, `mappp()` will memoise the results in a local cache folder
- **error handling**: by default, if `mappp()` encounters an error, it will return `NA` instead of interrupting the entire calculation

## Installation

mappp is currently not hosted on CRAN and the development version of mappp can be installed from GitHub with:

```r
remotes::install_github('cole-brokamp/mappp')
```

<!-- You can install the released version of mappp from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->
<!-- install.packages("mappp") -->
<!-- ``` -->

## Examples

```r
X <- list('x' = 100, 'y' = 'a', 'z' = 200)
slow_log <- function(.x) {Sys.sleep(0.5); log(.x)}

# by default returns NA on error
mappp(X, slow_log)

# when not using error, entire calculation will fail
mappp(X, slow_log, error.value=NULL)
```
