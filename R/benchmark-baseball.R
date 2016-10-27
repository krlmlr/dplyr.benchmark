## ---- echo = FALSE, message = FALSE--------------------------------------
library(dplyr)
library(microbenchmark)
library(data.table)
library(dtplyr)
library(Lahman)
knitr::opts_chunk$set(
  comment = "#>",
  error = FALSE,
  tidy = FALSE
)

## ----setup---------------------------------------------------------------
batting_df <- tbl_df(Batting)
batting_dt <- tbl_dt(Batting)

## ------------------------------------------------------------------------
mean_ <- function(x) .Internal(mean(x))
min_rank_ <- min_rank

master_df <- tbl_df(Master) %>% select(playerID, birthYear)
hall_of_fame_df <- tbl_df(HallOfFame) %>% filter(inducted == "Y") %>%
  select(playerID, votedBy, category)

master_dt <- tbl_dt(Master) %>% select(playerID, birthYear)
hall_of_fame_dt <- tbl_dt(HallOfFame) %>% filter(inducted == "Y") %>%
  select(playerID, votedBy, category)

~{
## ----summarise-mean------------------------------------------------------
summarise_mean = lazyeval::dots_capture(
  dplyr_df = batting_df %>% group_by(playerID) %>% summarise(ab = mean(AB)),
  dplyr_dt = batting_dt %>% group_by(playerID) %>% summarise(ab = mean(AB)),
  dt_raw =   batting_dt[, list(ab = mean(AB)), by = playerID],
  base =     tapply(batting_df$AB, batting_df$playerID, FUN = mean),
  times = 5
)

## ----sumarise-mean_------------------------------------------------------
summarise_mean_reg = lazyeval::dots_capture(
  dplyr_df = batting_df %>% group_by(playerID) %>% summarise(ab = mean_(AB)),
  dplyr_dt = batting_dt %>% group_by(playerID) %>% summarise(ab = mean_(AB)),
  dt_raw =   batting_dt[, list(ab = mean_(AB)), by = playerID],
  base =     tapply(batting_df$AB, batting_df$playerID, FUN = mean_),
  times = 5
)

## ----arrange-------------------------------------------------------------
arrange = lazyeval::dots_capture(
  dplyr_df = batting_df %>% arrange(playerID, yearID),
  dplyr_dt = batting_dt %>% arrange(playerID, yearID),
  dt_raw =   setkey(copy(batting_dt), playerID, yearID),
  base   =   batting_dt[order(batting_df$playerID, batting_df$yearID), ],
  times = 2
)

## ----filter--------------------------------------------------------------
filter = lazyeval::dots_capture(
  dplyr_df = batting_df %>% group_by(playerID) %>% filter(G == max(G)),
  dplyr_dt = batting_dt %>% group_by(playerID) %>% filter(G == max(G)),
  dt_raw   = batting_dt[batting_dt[, .I[G == max(G)], by = playerID]$V1],
  base   =   batting_df[ave(batting_df$G, batting_df$playerID, FUN = max) ==
    batting_df$G, ],
  times = 2
)

## ----mutate--------------------------------------------------------------
mutate = lazyeval::dots_capture(
  dplyr_df  = batting_df %>% group_by(playerID) %>% mutate(r = rank(desc(AB))),
  dplyr_dt  = batting_dt %>% group_by(playerID) %>% mutate(r = rank(desc(AB))),
  dt_raw =    copy(batting_dt)[, rank := rank(desc(AB)), by = playerID],
  times = 2
)

## ----mutate2-------------------------------------------------------------
mutate2 = lazyeval::dots_capture(
  dplyr_df = batting_df %>% group_by(playerID) %>%
    mutate(cyear = yearID - min(yearID) + 1),
  dplyr_dt = batting_dt %>% group_by(playerID) %>%
    mutate(cyear = yearID - min(yearID) + 1),
  dt_raw =   copy(batting_dt)[, cyear := yearID - min(yearID) + 1,
    by = playerID],
  times = 5
)

## ----mutate_hybrid-------------------------------------------------------
windowed = lazyeval::dots_capture(
  dplyr_df  = batting_df %>% group_by(playerID) %>% mutate(r = min_rank(AB))
)

windowed_reg = lazyeval::dots_capture(
  regular  = batting_df %>% group_by(playerID) %>% mutate(r = min_rank_(AB)),
  times = 2
)

## ------------------------------------------------------------------------
left_join = lazyeval::dots_capture(
  dplyr_df = left_join(master_df, hall_of_fame_df, by = "playerID"),
  dplyr_dt = left_join(master_dt, hall_of_fame_dt, by = "playerID"),
  base     = merge(master_df, hall_of_fame_df, by = "playerID", all.x = TRUE),
  times = 10
)

inner_join = lazyeval::dots_capture(
  dplyr_df = inner_join(master_df, hall_of_fame_df, by = "playerID"),
  dplyr_dt = inner_join(master_dt, hall_of_fame_dt, by = "playerID"),
  base     = merge(master_df, hall_of_fame_df, by = "playerID"),
  times = 10
)

semi_join = lazyeval::dots_capture(
  dplyr_df = semi_join(master_df, hall_of_fame_df, by = "playerID"),
  dplyr_dt = semi_join(master_dt, hall_of_fame_dt, by = "playerID"),
  times = 10
)

anti_join = lazyeval::dots_capture(
  dplyr_df = anti_join(master_df, hall_of_fame_df, by = "playerID"),
  dplyr_dt = anti_join(master_dt, hall_of_fame_dt, by = "playerID"),
  times = 10
)

}
