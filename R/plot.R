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

  detect_jumps(plot_data)
}

get_full_data <- function() {
  csv_files <- dir(system.file("benchmark", package = getPackageName(), mustWork = TRUE),
                   full.names = TRUE, pattern = glob2rx("*.csv"))
  names(csv_files) <- gsub("^.*[^0-9a-f]([0-9a-f]+)[.]csv$", "\\1",
                           as.character(csv_files))
  csv_data <- lapply(csv_files, read.csv, row.names = NULL, stringsAsFactors = FALSE)
  full_data <- bind_rows(csv_data, .id = "sha")
  tbl_df(full_data)
}

detect_jumps <- function(plot_data) {
  plot_data %>%
    tidyr::nest(-name) %>%
    mutate_(data = ~lapply(data, detect_jumps_one)) %>%
    tidyr::unnest()
}

detect_jumps_one <- function(data) {
  data_ts <- ts(data$calibrated_time)

  fit <- arima(data_ts, order = c(0L, 1L, 0L))
  resid <- residuals(fit)
  pars <- tsoutliers::coefs2poly(fit)

  outliers <- tsoutliers::locate.outliers(resid, pars, types = "LS")

  data %>%
    mutate_(ind = ~row_number()) %>%
    left_join(outliers, by = "ind") %>%
    select(-ind)
}
