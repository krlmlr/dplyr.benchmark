get_dplyr_benchmark_repo_url <- function() {
  if (dir.exists("../dplyr.benchmark/.git")) {
    DPLYR_URL <- "../dplyr.benchmark"
  } else if (dir.exists("../../dplyr.benchmark/.git")) {
    DPLYR_URL <- "../../dplyr.benchmark"
  } else {
    DPLYR_URL <- "https://github.com/krlmlr/dplyr.benchmark.git"
  }
}

dplyr_benchmark_repo_ <- eval(bquote(function(url = get_dplyr_benchmark_repo_url()) {
  temp_dir <- tempfile("dplyr.benchmark", fileext = ".git")

  message("Cloning dplyr.benchmark from ", url)
  git("clone", shQuote(url), "--branch", "master",
      shQuote(temp_dir))

  setup_git_config(temp_dir)

  git2r::repository(temp_dir)
}))

#' Get a dplyr.benchmark repo
#'
#' @param url URL to dplyr repository
#'
#' @export
dplyr_benchmark_repo <- memoise::memoise(dplyr_benchmark_repo_)

setup_git_config <- function(repo_dir) {
  repo <- git2r::repository(repo_dir)
  git2r::config(
    repo,
    user.name = "Kirill M\u00fcller", user.email = "krlmlr+r@mailbox.org")
}

#' Automated data collection
#'
#' Performs the following tasks:
#' - Clones the `dplyr.benchmark` repository to a temporary directory
#' - Collects benchmark data from the given references, by default taken from
#'   command-line args
#' - Commits and pushes
#'
#' @param ref `[character(1)]`\cr A Git refspec for dplyr revisions to test.
#'   See help for `git parse-rev`, use `ref^!` to collect for a single revision.
#'
#' @export
collect_data_in_clone <- function(refs = commandArgs(TRUE)[[1]]) {
  # Make sure bare repo is cloned only once
  sha <- get_log(refs, dplyr_repo())

  if (length(sha) == 0) {
    stop("No revisions to test", call. = FALSE)
  }

  repo <- dplyr_benchmark_repo()
  withr::with_dir(repo@path, collect_data(sha))
  commit_data(repo, refs)
  push_data(repo)
}

collect_data <- function(refs) {
  parallel::mclapply(
    refs, function(ref) try(benchmark(ref)),
    mc.cores = parallel::detectCores() - 1)
}

commit_data <- function(repo, refs) {
  git2r::add(repo, "inst/benchmark")
  if (length(git2r::status(repo, unstaged = FALSE, untracked = FALSE)$staged) > 0) {
    git2r::commit(repo, paste0(
      "Updated data from references:\n\n",
      paste0("hadley/dplyr@", refs, collapse = "\n")))
  } else {
    message("Nothing to commit")
  }
}

push_data <- function(repo) {
  git("push", dir = repo@path)
}
