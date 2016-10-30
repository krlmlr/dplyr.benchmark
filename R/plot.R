#' Get plot data
#'
#' @inheritParams get_log_df
#'
#' @export
get_plot_data <- function(ref = "master") {
  full_data <- get_full_data()
  log_df <- get_log_df(ref = ref)

  plot_data <- log_df %>%
    inner_join(full_data, by = "sha")

  plot_data
}

get_full_data <- function() {
  csv_files <- dir(system.file("benchmark", package = getPackageName(), mustWork = TRUE),
                   full.names = TRUE, pattern = glob2rx("*.csv"))
  names(csv_files) <- gsub("^.*[^0-9a-f]([0-9a-f]+)[.]csv$", "\\1",
                           as.character(csv_files))
  csv_data <- lapply(csv_files, read.csv, row.names = NULL, stringsAsFactors = FALSE)
  full_data <- bind_rows(csv_data, .id = "sha")
  full_data
}
