#' map in parallel with progress
#'
#' This function is a wrapper around \code{purrr::map()}
#' (which applies a function to each element of a list or atomic vector) with some extras on top,
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
#'   \code{rlang::as_function()} for details)
#' @param parallel logical; use parallel processing?
#' @param num_cores the number of cores used for parallel processing.  Can be
#'   specified as an integer, or it will guess the number of cores available
#'   with parallelly::availableCores(). won't have an effect if parallel is FALSE
#' @param cache defaults to FALSE, which means no cache used. If TRUE, cache the results locally in a folder named according to \code{cache_name} using the memoise package
#' @param cache_name a character string to use a custom cache folder name (e.g. "my_cache"); defaults to "cache"
#' @param error_capture apply function to all elements and return those that error as \code{NA}
#' ; this also messages user with name/index of offending element and resulting error message
#' @param error_quiet quiet individual error messages when capturing error messages? or show them as they occur?
#' @return a list the same length as \code{.x}
#' @export
#' @examples
#' \donttest{
#' X <- list("x" = 100, "y" = "a", "z" = 200)
#' slow_log <- function(.x) {
#'   Sys.sleep(0.5)
#'   log(.x)
#' }
#' # by default returns NA on error
#' mappp(X, slow_log)
#' # when not using error, entire calculation will fail
#' mappp(X, slow_log, error_capture = FALSE)
#' # showing error messages when they occur rather than afterwards can be useful
#' # but will cause problems with progress bar displays
#' mappp(X, slow_log, error_quiet = FALSE)
#' }
#'
mappp <- function(.x, .f,
                  parallel = FALSE,
                  cache = FALSE, cache_name = "cache",
                  error_capture = TRUE,
                  error_quiet = TRUE,
                  num_cores = NULL) {

  .f <- rlang::as_function(.f)

  if (cache) {
    fc <- memoise::cache_filesystem(cache_name)
    .f <- memoise::memoise(.f, cache = fc)
  }

  if (error_capture) {
    .f <- purrr::safely(.f, quiet = error_quiet)
  }

  if (!is.vector(.x) || is.object(.x)) .x <- as.list(.x)

  # set number of cores
  if (parallel) {
    if (is.null(num_cores)) num_cores <- parallelly::availableCores()
    if (is.na(num_cores)) num_cores <- 1
    if (identical(.Platform$OS.type, "windows")) {
      message("detected a windows platform; disabling parallel processing")
      num_cores <- 1
    }
  } else {
    num_cores <- 1
  }

  if (num_cores == 1) out <- lapply_pb(.x, .f)

  if (num_cores > 1 && identical(.Platform$GUI, "RStudio")) {
    message("progress bar doesn't work in RStudio; follow the file \".progress\" instead")
    out <- mclapply_pb_fallback(.x, .f, num_cores)
  }

  if (num_cores > 1 && !identical(.Platform$GUI, "RStudio")) {
    out <- mclapply_pb(.x, .f, num_cores)
  }

  ## if error_capture create output list and emit number of errors
  if (error_capture) {
    out <- purrr::transpose(out)

    which_error <- which(!sapply(out$error, is.null))

    if (length(which_error) > 0) {
      message(length(which_error), " errors occurred")
      message("the first one occurred in element ", which_error[1], ":")
      if (!is.null(names(which_error[1]))) {
        message("(which had the name ", names(which_error[1]), ")")
      }
      message("\n", out$error[[which_error[1]]], "\n")
      out <- purrr::modify_at(out$result, which_error, ~NA)
    } else {
      out <- out$result
    }
  }

  return(out)
}
