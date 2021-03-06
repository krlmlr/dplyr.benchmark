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
#' - Collects benchmark data from the given references, by default all
#'   references in master that haven't been collected yet
#' - Commits and pushes
#'
#' @param ref `[character(1)]`\cr A Git refspec for dplyr revisions to test.
#'   See help for `git parse-rev`, use `ref^!` to collect for a single revision.
#' @param only_new `[logical(1)]`\cr If `TRUE`, collect data for all references
#'   up to the first for which data has been collected already.
#'
#' @export
collect_data_in_clone <- function(ref = "master", only_new = TRUE) {
  # Make sure bare repo is cloned only once
  sha <- get_log_df(ref)$sha

  if (only_new) {
    sha <- get_only_new(sha)
  }

  if (length(sha) == 0) {
    stop("No revisions to test", call. = FALSE)
  }

  message("Testing revisions:", paste0(substr(sha, 1, 7), collapse = ", "))
  repo <- dplyr_benchmark_repo()
  withr::with_dir(repo@path, collect_data(sha))
  commit_data(repo, ref, sha)
  push_data(repo)
}

get_only_new <- function(sha) {
  sha <- rev(sha)
  known_sha <- names(get_csv_files())
  known_idx <- which(sha %in% known_sha)
  if (length(known_idx) > 0) {
    sha <- sha[seq_len(known_idx[[1]] - 1L)]
  }
  rev(sha)
}

collect_data <- function(sha) {
  assignments <- assign_tasks(sha)

  parallel::mclapply(
    assignments, function(task) {
      parallel::mcaffinity(task$task_id)
      lapply(task$data[[1]]$sha, function(ref) try(benchmark(ref)))
    },
    mc.cores = parallel::detectCores() - 1)
}

assign_tasks <- function(sha) {
  task_id <- rep_len(seq.int(2, parallel::detectCores()), length(sha))
  tibble(task_id, sha) %>%
    tidyr::nest(-task_id) %>%
    purrrlyr::by_row(identity) %>%
    .[[".out"]]
}

commit_data <- function(repo, ref, sha) {
  git2r::add(repo, "inst/benchmark")
  if (length(git2r::status(repo, unstaged = FALSE, untracked = FALSE)$staged) > 0) {
    git2r::commit(repo, paste0(
      "Updated data from ", ref, ":\n\n",
      paste0("hadley/dplyr@", sha, collapse = "\n")))
  } else {
    message("Nothing to commit")
  }
}

push_data <- function(repo) {
  git("push", dir = repo@path)
}
