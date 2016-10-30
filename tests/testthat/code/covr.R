{
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
  as.character(runif(1e+06))
  batting_df %>% group_by(playerID) %>% summarise(ab = mean(AB))
  batting_dt %>% group_by(playerID) %>% summarise(ab = mean(AB))
  batting_dt[, list(ab = mean(AB)), by = playerID]
  tapply(batting_df$AB, batting_df$playerID, FUN = mean)
  batting_df %>% group_by(playerID) %>% summarise(ab = mean_(AB))
  batting_dt %>% group_by(playerID) %>% summarise(ab = mean_(AB))
  batting_dt[, list(ab = mean_(AB)), by = playerID]
  tapply(batting_df$AB, batting_df$playerID, FUN = mean_)
  batting_df %>% arrange(playerID, yearID)
  batting_dt %>% arrange(playerID, yearID)
  setkey(copy(batting_dt), playerID, yearID)
  batting_dt[order(batting_df$playerID, batting_df$yearID), ]
  batting_df %>% group_by(playerID) %>% filter(G == max(G))
  batting_dt %>% group_by(playerID) %>% filter(G == max(G))
  batting_dt[batting_dt[, .I[G == max(G)], by = playerID]$V1]
  batting_df[ave(batting_df$G, batting_df$playerID, FUN = max) == batting_df$G, 
    ]
  batting_df %>% group_by(playerID) %>% mutate(r = rank(desc(AB)))
  batting_dt %>% group_by(playerID) %>% mutate(r = rank(desc(AB)))
  copy(batting_dt)[, `:=`(rank, rank(desc(AB))), by = playerID]
  batting_df %>% group_by(playerID) %>% mutate(cyear = yearID - min(yearID) + 1)
  batting_dt %>% group_by(playerID) %>% mutate(cyear = yearID - min(yearID) + 1)
  copy(batting_dt)[, `:=`(cyear, yearID - min(yearID) + 1), by = playerID]
  batting_df %>% group_by(playerID) %>% mutate(r = min_rank(AB))
  batting_df %>% group_by(playerID) %>% mutate(r = min_rank_(AB))
  left_join(master_df, hall_of_fame_df, by = "playerID")
  left_join(master_dt, hall_of_fame_dt, by = "playerID")
  merge(master_df, hall_of_fame_df, by = "playerID", all.x = TRUE)
  inner_join(master_df, hall_of_fame_df, by = "playerID")
  inner_join(master_dt, hall_of_fame_dt, by = "playerID")
  merge(master_df, hall_of_fame_df, by = "playerID")
  semi_join(master_df, hall_of_fame_df, by = "playerID")
  semi_join(master_dt, hall_of_fame_dt, by = "playerID")
  anti_join(master_df, hall_of_fame_df, by = "playerID")
  anti_join(master_dt, hall_of_fame_dt, by = "playerID")
  bind_rows(batting_df, batting_df)
  rbind(batting_df, batting_df)
}
