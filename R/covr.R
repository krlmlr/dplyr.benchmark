# requires covr from GitHub to include C++ code
run_covr <- function(ref = "master", repo = dplyr_repo()) {
  repo_dir <- dplyr_clone(ref, repo = repo)
  full_code <- get_covr_code()

  withr::with_dir(repo_dir, do_run_covr(full_code))
}

get_covr_code <- function() {
  pre_code <- deparse(pre_code[[2]], width.cutoff = 500)
  code <- vapply(quoted_calls, deparse, width.cutoff = 500, FUN.VALUE = character(1), USE.NAMES = FALSE)
  full_code <- paste(c("{", pre_code, code, "}"), collapse = "\n")
  indent_code(full_code)
}

do_run_covr <- function(full_code) {
  line_exclusions <-
    dir(c("R", "inst/include"), recursive = TRUE, full.names = TRUE) %>%
    `names<-`(., .) %>%
    lapply(readLines) %>%
    lapply(seq_along)

  covr::package_coverage(type = "none", code = full_code, quiet = FALSE, line_exclusions = line_exclusions)
}
