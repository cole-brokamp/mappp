X <- c(1:5)
slow_log <- function(.x) {
  Sys.sleep(0.5)
  log(.x)
}

test_that("lapply_pb works", {
  expect_equal(
    mappp(X, slow_log, parallel = FALSE),
    lapply(1:5, log)
  )
})

test_that("mclapply_pb works", {
  skip_on_cran()
  expect_equal(
    mappp(X, slow_log, parallel = TRUE),
    lapply(1:5, log)
  )
})

X <- list("x" = 100, "y" = "a", "z" = 200)

test_that("error is ignored and returned as NULL", {
  expect_equal(
    mappp(X, slow_log),
    list(log(100), NA, log(200))
  )
})

test_that("error is not ignored", {
  expect_error(mappp(X, slow_log, error_capture = FALSE))
})
