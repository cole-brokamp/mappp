# mappp

> **map** in **p**arallel with **p**rogress

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/mappp)](https://CRAN.R-project.org/package=mappp)
[![R-CMD-check](https://github.com/cole-brokamp/mappp/workflows/R-CMD-check/badge.svg)](https://github.com/cole-brokamp/mappp/actions)
<!-- badges: end -->

The goal of mappp is to provide a simple implementation of `purrr::map` or `base::lapply` that provides enhanced features like parallel computation, progress bars, error handling, and result caching.

- **progress**: `mappp()` will always report its progress
- **error handling**: by default, if `mappp()` encounters an error, it will return `NA` instead of interrupting the entire calculation
- **parallel**: if `parallel = TRUE`, `mappp()` will attempt to calculate in parallel by using the maximum number of available cores
- **cache**: if `cache = TRUE`, `mappp()` will memoise the results in a local cache folder

Please note that this package relies on forking via [`parallel::mclapply()`](https://stat.ethz.ch/R-manual/R-devel/library/parallel/html/mclapply.html) which means that parallel computation is not available on Windows platforms.

## Installation

mappp is currently not hosted on CRAN and the latest version of mappp can be installed from GitHub with:

```r
remotes::install_github('cole-brokamp/mappp')
```
