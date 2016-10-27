library(magrittr)

pre_code <- ~{
  ## ---- echo = FALSE, message = FALSE--------------------------------------
  devtools::load_all(quiet = TRUE)
  library(data.table)
  library(dtplyr)
  library(Lahman)

  ## ----setup---------------------------------------------------------------
  batting_df <- tbl_df(Batting)
  batting_dt <- tbl_dt(Batting)

  ## ------------------------------------------------------------------------
  mean_ <- function(x) .Internal(mean(x))
  min_rank_ <- min_rank

  master_df <- tbl_df(Master[c("playerID", "birthYear")])
  hall_of_fame_df <- tbl_df(HallOfFame[HallOfFame$inducted == "Y",
                                       c("playerID", "votedBy", "category")])

  master_dt <- tbl_dt(Master[c("playerID", "birthYear")])
  hall_of_fame_dt <- tbl_dt(HallOfFame[HallOfFame$inducted == "Y",
                                       c("playerID", "votedBy", "category")])
}

code <- ~{
  calibration = ~as.character(runif(1e6))

  ## ----summarise-mean------------------------------------------------------
  summarise_mean = lazyeval::dots_capture(
    dplyr_df = batting_df %>% group_by(playerID) %>% summarise(ab = mean(AB)),
    dplyr_dt = batting_dt %>% group_by(playerID) %>% summarise(ab = mean(AB)),
    dt_raw =   batting_dt[, list(ab = mean(AB)), by = playerID],
    base =     tapply(batting_df$AB, batting_df$playerID, FUN = mean)
  )

  ## ----sumarise-mean_------------------------------------------------------
  summarise_mean_reg = lazyeval::dots_capture(
    dplyr_df = batting_df %>% group_by(playerID) %>% summarise(ab = mean_(AB)),
    dplyr_dt = batting_dt %>% group_by(playerID) %>% summarise(ab = mean_(AB)),
    dt_raw =   batting_dt[, list(ab = mean_(AB)), by = playerID],
    base =     tapply(batting_df$AB, batting_df$playerID, FUN = mean_)
  )

  ## ----arrange-------------------------------------------------------------
  arrange = lazyeval::dots_capture(
    dplyr_df = batting_df %>% arrange(playerID, yearID),
    dplyr_dt = batting_dt %>% arrange(playerID, yearID),
    dt_raw =   setkey(copy(batting_dt), playerID, yearID),
    base   =   batting_dt[order(batting_df$playerID, batting_df$yearID), ]
  )

  ## ----filter--------------------------------------------------------------
  filter = lazyeval::dots_capture(
    dplyr_df = batting_df %>% group_by(playerID) %>% filter(G == max(G)),
    dplyr_dt = batting_dt %>% group_by(playerID) %>% filter(G == max(G)),
    dt_raw   = batting_dt[batting_dt[, .I[G == max(G)], by = playerID]$V1],
    base   =   batting_df[ave(batting_df$G, batting_df$playerID, FUN = max) ==
      batting_df$G, ]
  )

  ## ----mutate--------------------------------------------------------------
  mutate = lazyeval::dots_capture(
    dplyr_df  = batting_df %>% group_by(playerID) %>% mutate(r = rank(desc(AB))),
    dplyr_dt  = batting_dt %>% group_by(playerID) %>% mutate(r = rank(desc(AB))),
    dt_raw =    copy(batting_dt)[, rank := rank(desc(AB)), by = playerID]
  )

  ## ----mutate2-------------------------------------------------------------
  mutate2 = lazyeval::dots_capture(
    dplyr_df = batting_df %>% group_by(playerID) %>%
      mutate(cyear = yearID - min(yearID) + 1),
    dplyr_dt = batting_dt %>% group_by(playerID) %>%
      mutate(cyear = yearID - min(yearID) + 1),
    dt_raw =   copy(batting_dt)[, cyear := yearID - min(yearID) + 1,
      by = playerID]
  )

  ## ----mutate_hybrid-------------------------------------------------------
  windowed = lazyeval::dots_capture(
    dplyr_df  = batting_df %>% group_by(playerID) %>% mutate(r = min_rank(AB))
  )

  windowed_reg = lazyeval::dots_capture(
    regular  = batting_df %>% group_by(playerID) %>% mutate(r = min_rank_(AB))
  )

  ## ---join-----------------------------------------------------------------
  left_join = lazyeval::dots_capture(
    dplyr_df = left_join(master_df, hall_of_fame_df, by = "playerID"),
    dplyr_dt = left_join(master_dt, hall_of_fame_dt, by = "playerID"),
    base     = merge(master_df, hall_of_fame_df, by = "playerID", all.x = TRUE)
  )

  inner_join = lazyeval::dots_capture(
    dplyr_df = inner_join(master_df, hall_of_fame_df, by = "playerID"),
    dplyr_dt = inner_join(master_dt, hall_of_fame_dt, by = "playerID"),
    base     = merge(master_df, hall_of_fame_df, by = "playerID")
  )

  semi_join = lazyeval::dots_capture(
    dplyr_df = semi_join(master_df, hall_of_fame_df, by = "playerID"),
    dplyr_dt = semi_join(master_dt, hall_of_fame_dt, by = "playerID")
  )

  anti_join = lazyeval::dots_capture(
    dplyr_df = anti_join(master_df, hall_of_fame_df, by = "playerID"),
    dplyr_dt = anti_join(master_dt, hall_of_fame_dt, by = "playerID")
  )

  ## ---bind_rows------------------------------------------------------------
  bind_rows = lazyeval::dots_capture(
    dplyr_df = bind_rows(batting_df, batting_df),
    base = rbind(batting_df, batting_df)
  )

}

extract_quoted_calls <- function(code) {
  expression_list <- as.list(code[[2]])[-1]
  call_names <- lapply(expression_list, "[[", 2) %>% vapply(as.character, character(1)) # name objects
  calls <- lapply(expression_list, "[[", 3) %>% lapply(eval) # calls
  names(calls) <- call_names

  single_calls <- unlist(calls)
  lapply(single_calls, "[[", 2)
}

quoted_calls <- extract_quoted_calls(code)

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

system.time(mb <- run_microbenchmark(pre_code, quoted_calls))

mb_tidy <-
  mb %>%
  tibble::enframe %>%
  mutate(name = forcats::fct_inorder(name)) %>%
  tidyr::unnest %>%
  select(-expr) %>%
  group_by(name) %>%
  summarize(median_time = median(time)) %>%
  ungroup %>%
  mutate(calibrated_time = median_time / median_time[[1]])

r <- git2r::repository()
sha <- git2r::branch_target(git2r::head(r))

write.csv(mb_tidy, file.path("benchmark", paste0(sha, ".csv")))
