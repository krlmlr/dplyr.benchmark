context("code")

test_that("create code", {
  skip_on_cran()

  code_dir <- "code"
  dir.create(code_dir, showWarnings = FALSE)
  expect_output_file(cat(get_microbenchmark_code()), file.path(code_dir, "microbenchmark.R"), update = TRUE)
  expect_output_file(cat(get_covr_code()), file.path(code_dir, "covr.R"), update = TRUE)
})
