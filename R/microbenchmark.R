benchmark <- function(ref = "master", repo = dplyr_repo()) {
  repo_dir <- dplyr_clone(ref, repo = repo)

  pkg_dir <- getwd()

  withr::with_dir(
    repo_dir,
    write_microbenchmark(run_microbenchmark(), pkg_dir)
  )
}

run_microbenchmark <- function() {
  mb <- do_run_microbenchmark()
  mb_tidy <- tidy_microbenchmark(mb)

  mb_tidy
}

# Must run in a different process!!!
# TODO: Write an .R file, run with R -f xxx.R, should save results somewhere
do_run_microbenchmark <- function() {
}

get_microbenchmark_code <- function() {
  load_code <- "devtools::load_all()"
  pre_code <- deparse(pre_code[[2]], width.cutoff = 500)
  quoted_calls_code <- deparse(quoted_calls, width.cutoff = 500, control = "quoteExpressions")
  microbenchmark_code <- deparse(quote(lapply(
    quoted_calls,
    function(call) {
      tryCatch(
        microbenchmark::microbenchmark(list = list(call), times = 1),
        error = function(e) tibble::tribble(~expr, ~time)
      )
    }
  )))
  write_code <- deparse(quote(
    write.csv(mb, commandArgs(trailingOnly = TRUE)[[1]], row.names = FALSE)))

  full_code <- paste(
    c("{", load_code, pre_code,
      "quoted_calls <- ", quoted_calls_code,
      "mb <- ", microbenchmark_code,
      write_code,
      "}"),
    collapse = "\n")

  indent_code(full_code)
}

tidy_microbenchmark <- function(mb) {
  browser()
  mb %>%
    tibble::enframe() %>%
    mutate_(name = ~forcats::fct_inorder(name)) %>%
    mutate_(value = lapply(value, mutate_, expr = ~as.character(expr))) %>%
    tidyr::unnest() %>%
    select(-expr) %>%
    group_by_(~name) %>%
    summarize_(median_time = ~median(time)) %>%
    ungroup %>%
    mutate_(calibrated_time = ~median_time / median_time[[1]])
}

write_microbenchmark <- function(mb_tidy, pkg_dir) {
  sha <- system2("git", c("rev-parse", "HEAD"), stdout = TRUE)
  write.csv(mb_tidy, file.path(pkg_dir, "inst", "benchmark", paste0(sha, ".csv")),
            row.names = FALSE)
}
