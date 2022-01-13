#' map in parallel with progress
#'
#' This function is a wrapper around purrr::map() with some extras on top,
#' including parallel computation, progress bar, error handling, and result caching.
#'
#' \code{mappp} is designed for long computations and as such it always uses a progress bar,
#' and always returns a list. Long computations shouldn't worry about being type
#' strict; instead, extract results in the right type from the results list.
#'
#' A progress bar will be shown in the terminal using an interactive R session or
#' in an .Rout file, if using R CMD BATCH and submitting R scripts for
#' non-interactive completion. Although R Studio supports the progress bar for
#' single process workers, it has a problem showing the progress bar if using
#' parallel processing (see the discussion at
#' http://stackoverflow.com/questions/27314011/mcfork-in-rstudio). In this
#' specific case (R Studio + parallel processing), text updates will be printed
#' to the file `.progress`. Use a shell and `tail -f .progress` to see the
#' updates.
#'
#' @param .x list or vector of objects to apply over
#' @param .f function to apply; allows for compact anonymous functions (see
#'   \code{purrr::as_mapper()} for details)
#' @param parallel logical; use parallel processing?
#' @param num.cores the number of cores used for parallel processing.  Can be
#'   specified as an integer, or it will guess the number of cores available
#'   with parallelly::availableCores(). won't have an effect if parallel is FALSE
#' @param cache defaults to FALSE, which means no cache used. If TRUE, cache the results locally in a folder named according to \code{cache.name} using the memoise package
#' @param cache.name a character string to use a custom cache folder name (e.g. "my_cache"); defaults to "cache"
#' @param error.value (defaults to NA) use purrr::possibly to replace errors with this value instead of interrupting the process; set to NULL to not use error handling and instead interrupt the calculation
#' @param quiet logical, suppress error messages until the end of calculation? or show them as they occur
#' @export
#' @examples
#' \dontrun{
#' X <- list("x" = 100, "y" = "a", "z" = 200)
#' slow_log <- function(.x) {
#'   Sys.sleep(0.5)
#'   log(.x)
#' }
#' # by default returns NA on error
#' mappp(X, slow_log)
#' # when not using error, entire calculation will fail
#' mappp(X, slow_log, error.value = NULL)
#' # showing error messages when they occur rather than afterwards can be useful
#' # but will cause problems with error bar displays
#' mappp(X, slow_log, quiet = FALSE)
#' }
#'
mappp <- function(.x, .f,
                  parallel = FALSE,
                  cache = FALSE, cache.name = "cache",
                  error.value = NA,
                  quiet = TRUE,
                  num.cores = NULL) {
  .f <- purrr::as_mapper(.f)

  if (cache) {
    fc <- memoise::cache_filesystem(cache.name)
    .f <- memoise::memoise(.f, cache = fc)
  }

  if (!is.null(error.value)) {
    .f <- purrr::possibly(.f,
      otherwise = error.value,
      quiet = quiet
    )
  }

  if (!is.vector(.x) || is.object(.x)) .x <- as.list(.x)

  # set number of cores
  if (parallel) {
    if (is.null(num.cores)) num.cores <- parallelly::availableCores()
    if (is.na(num.cores)) num.cores <- 1
    if (identical(.Platform$OS.type, "windows")) {
      message("detected a windows platform; disabling parallel processing")
      num.cores <- 1
    }
  } else {
    num.cores <- 1
  }

  if (num.cores == 1) out <- lapply_pb(.x, .f)

  if (num.cores > 1 && identical(.Platform$GUI, "RStudio")) {
    message("progress bar doesn't work in RStudio; follow the file \".progress\" instead")
    out <- mclapply_pb_fallback(.x, .f, num.cores)
  }

  if (num.cores > 1 && !identical(.Platform$GUI, "RStudio")) {
    out <- mclapply_pb(.x, .f, num.cores)
  }

  return(out)
}

lapply_pb <- function(X, FUN) {
  frmt <- "... :what (:percent) [ ETA: :eta | Elapsed: :elapsed ]"
  n <- length(X)
  tmp <- vector("list", n)
  pbb <- progress::progress_bar$new(
    total = 100,
    format = frmt,
    clear = FALSE,
    force = TRUE,
    show_after = 0
  )
  pbb$tick(0)
  for (i in seq_len(n)) {
    pbb$tick(
      len = 100 / n,
      tokens = list(what = paste0("processing ", i, " of ", n))
    )
    tmp[[i]] <- FUN(X[[i]])
  }
  return(tmp)
}

parallel.mcexit <-
  utils::getFromNamespace("mcexit", "parallel")

parallel.mcfork <-
  utils::getFromNamespace("mcfork", "parallel")

mclapply_pb <- function(X, FUN, mc.cores) {
  n <- length(X)
  f <- fifo(tempfile(), open = "w+b", blocking = T)
  on.exit(close(f))
  p <- parallel.mcfork()
  pbb <- pbmcapply::progressBar(0, n, style = "ETA", width = 60)
  utils::setTxtProgressBar(pbb, 0)
  progress <- 0
  if (inherits(p, "masterProcess")) {
    while (progress < n) {
      readBin(f, "double")
      progress <- progress + 1
      utils::setTxtProgressBar(pbb, progress)
    }
    cat("\n")
    parallel.mcexit()
  }
  wrapper_f <- function(i) {
    out <- FUN(i)
    writeBin(1, f)
    return(out)
  }
  parallel::mclapply(X, wrapper_f, mc.cores = mc.cores)
}

mclapply_pb_fallback <- function(X, FUN, num.cores) {
  n <- length(X)
  wrapper_f <- function(i) {
    out <- FUN(X[[i]])
    out_percentage <- round(i / n * 100, digits = 0)
    cat(paste0(
      "   ... processing ",
      i, " of ", n,
      " (", out_percentage, "%)",
      "\n"
    ),
    file = ".progress",
    append = FALSE
    )
    return(out)
  }
  parallel::mclapply(seq_len(n), wrapper_f, mc.cores = num.cores)
}
