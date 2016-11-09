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

  git("clone", shQuote(url), "--bare", "--mirror",
      shQuote(temp_dir))

  git2r::repository(temp_dir)
}))

#' Get a dplyr.benchmark repo
#'
#' @param url URL to dplyr repository
#'
#' @export
dplyr_benchmark_repo <- memoise::memoise(dplyr_benchmark_repo_)

dplyr_benchmark_clone_ <- function(repo = dplyr_benchmark_repo()) {
  temp_dir <- tempfile("dplyr.benchmark")
  on.exit(unlink(temp_dir, recursive = TRUE))

  git("clone", shQuote(repo@path), shQuote(temp_dir))

  setup_git_config(temp_dir)

  on.exit(NULL)
  temp_dir
}

dplyr_benchmark_clone <- memoise::memoise(dplyr_benchmark_clone_)

setup_git_config <- function(repo_dir) {
  repo <- git2r::repository(repo_dir)

  git2r::config(
    repo,
    user.name = "Kirill Müller", user.email = "krlmlr+r@mailbox.org")
}

#' @export
collect_data_in_clone <- function(refs) {
  repo_dir <- dplyr_benchmark_clone()
  withr::with_dir(repo_dir, collect_data(refs))

  repo <- git2r::repository(repo_dir)
  commit_data(repo, refs)
  push_data(repo)
}

collect_data <- function(refs) {
  lapply(refs, function(ref) try(benchmark(ref)))
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
  browser()
  git2r::push(repo)
}
