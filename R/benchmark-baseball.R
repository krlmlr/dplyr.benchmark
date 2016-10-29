#' @importFrom magrittr %>%
#' @import tibble
#' @import dplyr
#' @importFrom stats median time
#' @importFrom utils sessionInfo write.csv globalVariables
NULL

globalVariables(".")

extract_quoted_calls <- function(code) {
  expression_list <- as.list(code[[2]])[-1]
  call_names <- lapply(expression_list, "[[", 2) %>% vapply(as.character, character(1)) # name objects
  calls <- lapply(expression_list, "[[", 3) %>% lapply(eval) # calls
  names(calls) <- call_names

  single_calls <- unlist(calls)
  lapply(single_calls, "[[", 2)
}

# requires covr from GitHub to include C++ code
run_covr <- function(pre_code, quoted_calls) {
  pre_code <- deparse(pre_code[[2]], width.cutoff = 500)
  code <- vapply(quoted_calls, deparse, width.cutoff = 500, FUN.VALUE = character(1), USE.NAMES = FALSE)
  full_code <- paste(c("{", pre_code, code, "}"), collapse = "\n")

  line_exclusions <-dir(c("R", "inst/include"), recursive = TRUE, full.names = TRUE) %>%
    `names<-`(., .) %>%
    lapply(readLines) %>%
    lapply(seq_along)

  covr::package_coverage(type = "none", code = full_code, quiet = FALSE, line_exclusions = line_exclusions)
}

#cv <- run_covr(pre_code, quoted_calls)

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
