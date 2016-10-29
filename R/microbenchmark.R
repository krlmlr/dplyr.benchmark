benchmark <- function(ref = "master", repo = dplyr_repo()) {
  repo_dir <- dplyr_clone(ref, repo = repo)

  mb_tidy <- run_microbenchmark(repo_dir)

  sha <- withr::with_dir(repo_dir, system2("git", c("rev-parse", ref), stdout = TRUE))
  write_microbenchmark(mb_tidy, sha)
}

run_microbenchmark <- function(repo_dir) {
  mb <- withr::with_dir(repo_dir, do_run_microbenchmark())
  mb_tidy <- tidy_microbenchmark(mb)

  mb_tidy
}

do_run_microbenchmark <- function() {
  devtools::load_all()
  eval(pre_code[[2]])
  lapply(
    quoted_calls,
    function(call) {
      tryCatch(
        microbenchmark::microbenchmark(list = list(call), times = 7),
        error = function(e) tibble::tribble(~expr, ~time)
      )
    }
  )
}

tidy_microbenchmark <- function(mb) {
  mb %>%
    tibble::enframe() %>%
    mutate_(name = ~forcats::fct_inorder(name)) %>%
    tidyr::unnest() %>%
    select(-expr) %>%
    group_by_(~name) %>%
    summarize_(median_time = ~median(time)) %>%
    ungroup %>%
    mutate_(calibrated_time = ~median_time / median_time[[1]])
}

write_microbenchmark <- function(mb_tidy, sha) {
  write.csv(mb_tidy, file.path("inst", "benchmark", paste0(sha, ".csv")))
}
