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

if (!.Platform$OS.type == "windows") {
  parallel_mcexit <-
    utils::getFromNamespace("mcexit", "parallel")

  parallel_mcfork <-
    utils::getFromNamespace("mcfork", "parallel")
}

pbb_eta <- utils::getFromNamespace("txtProgressBarETA", "pbmcapply")

mclapply_pb <- function(X, FUN, mc.cores) {
  n <- length(X)
  f <- fifo(tempfile(), open = "w+b", blocking = T)
  on.exit(close(f))
  p <- parallel_mcfork()

  pbb <- pbb_eta(min = 0, max = n, initial = 0, char = "=", width = 60, file = "")
  
  utils::setTxtProgressBar(pbb, 0)
  progress <- 0
  if (inherits(p, "masterProcess")) {
    while (progress < n) {
      readBin(f, "double")
      progress <- progress + 1
      utils::setTxtProgressBar(pbb, progress)
    }
    cat("\n")
    parallel_mcexit()
  }
  wrapper_f <- function(i) {
    out <- FUN(i)
    writeBin(1, f)
    return(out)
  }
  parallel::mclapply(X, wrapper_f, mc.cores = mc.cores)
}

mclapply_pb_fallback <- function(X, FUN, num_cores) {
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
  parallel::mclapply(seq_len(n), wrapper_f, mc.cores = num_cores)
}
