pre_code <- ~{
  ## ---- echo = FALSE, message = FALSE--------------------------------------
  try(dplyr:::init_logging("NONE"))
  library(Lahman)

  ## ----setup---------------------------------------------------------------
  batting_df <- tbl_df(Batting)

  ## ------------------------------------------------------------------------
  mean_ <- function(x) .Internal(mean(x))
  min_rank_ <- min_rank

  master_df <- tbl_df(Master[c("playerID", "birthYear")])
  hall_of_fame_df <- tbl_df(HallOfFame[HallOfFame$inducted == "Y",
                                       c("playerID", "votedBy", "category")])
}

pre_code_dt <- ~{
  library(data.table)
  try(library(dtplyr))

  try(batting_dt <- tbl_dt(Batting))

  try({
    master_dt <- tbl_dt(Master[c("playerID", "birthYear")])
    hall_of_fame_dt <- tbl_dt(HallOfFame[HallOfFame$inducted == "Y",
                                         c("playerID", "votedBy", "category")])
  })
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
    dplyr_df  = batting_df %>% group_by(playerID) %>% mutate(r = min_rank_(AB))
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

extract_quoted_calls <- function(code, algo) {
  expression_list <- as.list(code[[2]])[-1]
  call_names <- lapply(expression_list, "[[", 2) %>% vapply(as.character, character(1)) # name objects
  calls <- lapply(expression_list, "[[", 3) %>% lapply(eval) # calls
  names(calls) <- call_names

  single_calls <- unlist(calls)

  selected_calls <- c("calibration", grep(paste0("[.]", algo, "$"), names(single_calls), value = TRUE))

  lapply(single_calls[selected_calls], "[[", 2)
}

quoted_calls <- extract_quoted_calls(code, "dplyr_df")
