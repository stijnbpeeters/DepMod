testthat::test_that("run_model returns base and alt results", {
  res <- DepMod::run_model()
  
  testthat::expect_type(res, "list")
  testthat::expect_true(all(c("base", "alt") %in% names(res)))
  testthat::expect_false(is.null(res$base))
  testthat::expect_false(is.null(res$alt))
})