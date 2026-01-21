testthat::test_that("Shiny app directory exists in installed package", {
  app_dir <- system.file("app", package = "DepMod")
  testthat::expect_true(nzchar(app_dir))
  testthat::expect_true(dir.exists(app_dir))
})