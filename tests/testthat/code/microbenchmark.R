{
  devtools::load_all()
  {
    try(dplyr:::init_logging("NONE"))
    library(data.table)
    try(library(dtplyr))
    library(Lahman)
    batting_df <- tbl_df(Batting)
    try(batting_dt <- tbl_dt(Batting))
    mean_ <- function(x) .Internal(mean(x))
    min_rank_ <- min_rank
    master_df <- tbl_df(Master[c("playerID", "birthYear")])
    hall_of_fame_df <- tbl_df(HallOfFame[HallOfFame$inducted == "Y", c("playerID", 
      "votedBy", "category")])
    try({
      master_dt <- tbl_dt(Master[c("playerID", "birthYear")])
      hall_of_fame_dt <- tbl_dt(HallOfFame[HallOfFame$inducted == "Y", c("playerID", 
        "votedBy", "category")])
    })
  }
  quoted_calls <- list(calibration = quote(as.character(runif(1e+06))), summarise_mean.dplyr_df = quote(batting_df %>% 
    group_by(playerID) %>% summarise(ab = mean(AB))), summarise_mean.dplyr_dt = quote(batting_dt %>% 
    group_by(playerID) %>% summarise(ab = mean(AB))), summarise_mean.dt_raw = quote(batting_dt[, 
    list(ab = mean(AB)), by = playerID]), summarise_mean.base = quote(tapply(batting_df$AB, 
    batting_df$playerID, FUN = mean)), summarise_mean_reg.dplyr_df = quote(batting_df %>% 
    group_by(playerID) %>% summarise(ab = mean_(AB))), summarise_mean_reg.dplyr_dt = quote(batting_dt %>% 
    group_by(playerID) %>% summarise(ab = mean_(AB))), summarise_mean_reg.dt_raw = quote(batting_dt[, 
    list(ab = mean_(AB)), by = playerID]), summarise_mean_reg.base = quote(tapply(batting_df$AB, 
    batting_df$playerID, FUN = mean_)), arrange.dplyr_df = quote(batting_df %>% 
    arrange(playerID, yearID)), arrange.dplyr_dt = quote(batting_dt %>% arrange(playerID, 
    yearID)), arrange.dt_raw = quote(setkey(copy(batting_dt), playerID, yearID)), 
    arrange.base = quote(batting_dt[order(batting_df$playerID, batting_df$yearID), 
      ]), filter.dplyr_df = quote(batting_df %>% group_by(playerID) %>% filter(G == 
      max(G))), filter.dplyr_dt = quote(batting_dt %>% group_by(playerID) %>% 
      filter(G == max(G))), filter.dt_raw = quote(batting_dt[batting_dt[, .I[G == 
      max(G)], by = playerID]$V1]), filter.base = quote(batting_df[ave(batting_df$G, 
      batting_df$playerID, FUN = max) == batting_df$G, ]), mutate.dplyr_df = quote(batting_df %>% 
      group_by(playerID) %>% mutate(r = rank(desc(AB)))), mutate.dplyr_dt = quote(batting_dt %>% 
      group_by(playerID) %>% mutate(r = rank(desc(AB)))), mutate.dt_raw = quote(copy(batting_dt)[, 
      `:=`(rank, rank(desc(AB))), by = playerID]), mutate2.dplyr_df = quote(batting_df %>% 
      group_by(playerID) %>% mutate(cyear = yearID - min(yearID) + 1)), mutate2.dplyr_dt = quote(batting_dt %>% 
      group_by(playerID) %>% mutate(cyear = yearID - min(yearID) + 1)), mutate2.dt_raw = quote(copy(batting_dt)[, 
      `:=`(cyear, yearID - min(yearID) + 1), by = playerID]), windowed.dplyr_df = quote(batting_df %>% 
      group_by(playerID) %>% mutate(r = min_rank(AB))), windowed_reg.regular = quote(batting_df %>% 
      group_by(playerID) %>% mutate(r = min_rank_(AB))), left_join.dplyr_df = quote(left_join(master_df, 
      hall_of_fame_df, by = "playerID")), left_join.dplyr_dt = quote(left_join(master_dt, 
      hall_of_fame_dt, by = "playerID")), left_join.base = quote(merge(master_df, 
      hall_of_fame_df, by = "playerID", all.x = TRUE)), inner_join.dplyr_df = quote(inner_join(master_df, 
      hall_of_fame_df, by = "playerID")), inner_join.dplyr_dt = quote(inner_join(master_dt, 
      hall_of_fame_dt, by = "playerID")), inner_join.base = quote(merge(master_df, 
      hall_of_fame_df, by = "playerID")), semi_join.dplyr_df = quote(semi_join(master_df, 
      hall_of_fame_df, by = "playerID")), semi_join.dplyr_dt = quote(semi_join(master_dt, 
      hall_of_fame_dt, by = "playerID")), anti_join.dplyr_df = quote(anti_join(master_df, 
      hall_of_fame_df, by = "playerID")), anti_join.dplyr_dt = quote(anti_join(master_dt, 
      hall_of_fame_dt, by = "playerID")), bind_rows.dplyr_df = quote(bind_rows(batting_df, 
      batting_df)), bind_rows.base = quote(rbind(batting_df, batting_df)))
  mb <- Map(names(quoted_calls), quoted_calls, f = function(name, call) {
    message(name)
    tryCatch(microbenchmark::microbenchmark(list = list(call), times = 7), error = function(e) {
      message(name, " failed")
      tibble::tribble(~expr, ~time)
    })
  })
  saveRDS(mb, commandArgs(trailingOnly = TRUE)[[1]], compress = FALSE)
}
