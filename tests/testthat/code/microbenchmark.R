{
  dir.create(".lib")
  .libPaths(c(normalizePath(".lib"), .libPaths()))
  devtools::install_deps()
  devtools::load_all()
  {
    try(dplyr:::init_logging("NONE"))
    library(Lahman)
    batting_df <- tbl_df(Batting)
    mean_ <- function(x) .Internal(mean(x))
    min_rank_ <- min_rank
    master_df <- tbl_df(Master[c("playerID", "birthYear")])
    hall_of_fame_df <- tbl_df(HallOfFame[HallOfFame$inducted == "Y", c("playerID", 
      "votedBy", "category")])
  }
  quoted_calls <- list(calibration = quote(as.character(runif(1e+06))), summarise_mean.dplyr_df = quote(batting_df %>% 
    group_by(playerID) %>% summarise(ab = mean(AB))), summarise_mean_reg.dplyr_df = quote(batting_df %>% 
    group_by(playerID) %>% summarise(ab = mean_(AB))), arrange.dplyr_df = quote(batting_df %>% 
    arrange(playerID, yearID)), filter.dplyr_df = quote(batting_df %>% group_by(playerID) %>% 
    filter(G == max(G))), mutate.dplyr_df = quote(batting_df %>% group_by(playerID) %>% 
    mutate(r = rank(desc(AB)))), mutate2.dplyr_df = quote(batting_df %>% group_by(playerID) %>% 
    mutate(cyear = yearID - min(yearID) + 1)), windowed.dplyr_df = quote(batting_df %>% 
    group_by(playerID) %>% mutate(r = min_rank(AB))), windowed_reg.dplyr_df = quote(batting_df %>% 
    group_by(playerID) %>% mutate(r = min_rank_(AB))), left_join.dplyr_df = quote(left_join(master_df, 
    hall_of_fame_df, by = "playerID")), inner_join.dplyr_df = quote(inner_join(master_df, 
    hall_of_fame_df, by = "playerID")), semi_join.dplyr_df = quote(semi_join(master_df, 
    hall_of_fame_df, by = "playerID")), anti_join.dplyr_df = quote(anti_join(master_df, 
    hall_of_fame_df, by = "playerID")), bind_rows.dplyr_df = quote(bind_rows(batting_df, 
    batting_df)))
  mb <- Map(names(quoted_calls), quoted_calls, f = function(name, call) {
    message(name)
    tryCatch(microbenchmark::microbenchmark(list = list(call), times = 7), error = function(e) {
      message(name, " failed")
      tibble::tribble(~expr, ~time)
    })
  })
  saveRDS(mb, commandArgs(trailingOnly = TRUE)[[1]], compress = FALSE)
}
