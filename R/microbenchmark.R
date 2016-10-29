run_microbenchmark <- function(ref = "master", repo = dplyr_repo()) {
  repo_dir <- dplyr_clone(ref, repo = repo)

  mb <- withr::with_dir(repo_dir, do_run_microbenchmark())

  mb_tidy <-
    mb %>%
    tibble::enframe() %>%
    mutate_(name = ~forcats::fct_inorder(name)) %>%
    tidyr::unnest() %>%
    select(-expr) %>%
    group_by_(~name) %>%
    summarize_(median_time = ~median(time)) %>%
    ungroup %>%
    mutate_(calibrated_time = ~median_time / median_time[[1]])

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

do_stuff <- ~{
  r <- git2r::repository()
  sha <- devtools%:::%git_repo_sha1(r)

  write.csv(mb_tidy, file.path("benchmark", paste0(sha, ".csv")))
  cat("Done: ", sha, "\n", sep = "")

  print(sessionInfo())
}
