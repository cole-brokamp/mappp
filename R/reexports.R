`%:::%` <- function(pkg, fun) {
  get(fun,
    envir = asNamespace(pkg),
    inherits = FALSE
  )
}

parallel.mcexit <- "parallel"%:::%"mcexit"
parallel.mcfork <- "parallel"%:::%"mcfork"
