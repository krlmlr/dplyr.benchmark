get_dplyr_repo_url <- function() {
  if (dir.exists("../dplyr/.git")) {
    DPLYR_URL <- "../dplyr"
  } else if (dir.exists("../../dplyr/.git")) {
    DPLYR_URL <- "../../dplyr"
  } else {
    DPLYR_URL <- "https://github.com/hadley/dplyr.git"
  }
}

dplyr_repo_ <- eval(bquote(function(url = get_dplyr_repo_url()) {
  temp_dir <- tempfile("dplyr", fileext = ".git")

  git("clone", shQuote(url), "--bare", "--mirror",
      shQuote(temp_dir))

  git2r::repository(temp_dir)
}))

#' Get a dplyr repo
#'
#' @param url URL to dplyr repository
#'
#' @export
dplyr_repo <- memoise::memoise(dplyr_repo_)

dplyr_clone_ <- function(ref, repo = dplyr_repo()) {
  temp_dir <- tempfile("dplyr")
  on.exit(unlink(temp_dir, recursive = TRUE))

  git("clone", shQuote(repo@path), shQuote(temp_dir), "--no-checkout", "--no-single-branch")
  withr::with_dir(temp_dir, git("checkout", shQuote(ref), "--"))

  on.exit(NULL)
  temp_dir
}

dplyr_clone <- memoise::memoise(dplyr_clone_)

#' Get a dataframe with dplyr log entries
#'
#' @param ref Git revision, default `master`
#' @param repo A git2r repository, default from GitHub
#'
#' @export
get_log_df <- function(ref = "master", repo = dplyr_repo()) {
  log <- get_log(ref, repo)
  log_to_df(log)
}

# git2r::commits() doesn't report time
get_log <- function(ref, repo) {
  withr::with_dir(repo@path, {
    system2("git", c("log --format='%H %ad' --date=iso --first-parent", ref,
                     "--", "src", "inst/include"),
            stdout = TRUE)
  })
}

log_to_df <- function(log) {
  log_regex <- "^([^ ]+) (.*)$"

  log %>%
    rev %>%
    enframe %>%
    transmute_(commit_id = ~name,
               sha = ~gsub(log_regex, "\\1", value),
               commit_time = ~gsub(log_regex, "\\2", value) %>% as.POSIXct(format = "%Y-%m-%d %T %z"))
}

git <- function(...) {
  args <- c(...)
  exit_code <- system2("git", args)
  if (exit_code != 0) {
    stop("git ", paste(args, collapse = " "), " returned with exit status ", exit_code,
         call. = FALSE)
  }
}
