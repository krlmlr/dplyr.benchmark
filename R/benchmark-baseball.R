#' @importFrom magrittr %>%
#' @import tibble
#' @import dplyr
#' @importFrom stats median time
#' @importFrom utils sessionInfo write.csv globalVariables
NULL

globalVariables(".")

run_microbenchmark <- function(pre_code, quoted_calls) {
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
  system.time(mb <- run_microbenchmark(pre_code, quoted_calls))

  mb_tidy <-
    mb %>%
    tibble::enframe() %>%
    mutate(name = forcats::fct_inorder(name)) %>%
    tidyr::unnest() %>%
    select(-expr) %>%
    group_by(name) %>%
    summarize(median_time = median(time)) %>%
    ungroup %>%
    mutate(calibrated_time = median_time / median_time[[1]])

  r <- git2r::repository()
  sha <- devtools%:::%git_repo_sha1(r)

  write.csv(mb_tidy, file.path("benchmark", paste0(sha, ".csv")))
  cat("Done: ", sha, "\n", sep = "")

  print(sessionInfo())
}
