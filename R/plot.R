#' Get plot data
#'
#' @inheritParams get_log_df
#'
#' @export
get_plot_data <- function(ref = "master") {
  full_data <- get_full_data()
  log_df <- get_log_df(ref = ref)

  plot_data <-
    log_df %>%
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
  tbl_df(full_data)
}

#' Compute calibrated time
#'
#' Median time divided by the time the `calibration` run takes.
#'
#' @param data A data frame with at least the columns `name` and `median_time`
#' @export
compute_calibrated_time <- function(data) {
  data_fct <-
  data %>%
    mutate_(name = ~forcats::fct_inorder(name))

  data_fct %>%
    tidyr::spread_("name", "median_time") %>%
    tidyr::gather_(., "name", "median_time",
                   setdiff(levels(data_fct$name), c("calibration")),
                   factor_key = TRUE) %>%
    tidyr::drop_na_("median_time") %>%
    mutate(calibrated_time = median_time / calibration) %>%
    select(-median_time, -calibration)
}

#' Detect jumps
#'
#' @param plot_data A data frame as returned by [get_plot_data()]
#' @return `plot_data` augmented with columns provided by
#'   [tsoutliers::locate.outliers()]
#'
#' @export
detect_jumps <- function(plot_data) {
  plot_data %>%
    tidyr::nest_(., "data", select_vars_(colnames(.), ~-name)) %>%
    mutate_(data = ~lapply(data, detect_jumps_one)) %>%
    tidyr::unnest()
}

detect_jumps_one <- function(data) {
  data_ts <- log(stats::ts(data$calibrated_time))

  fit <- stats::arima(data_ts, order = c(0L, 1L, 0L))
  resid <- stats::residuals(fit)
  pars <- tsoutliers::coefs2poly(fit)

  outliers <- tsoutliers::locate.outliers(resid, pars, types = "LS")

  data_with_outliers <-
    data %>%
    mutate_(ind = ~row_number()) %>%
    left_join(outliers, by = "ind")

  data_kind <-
    data_with_outliers %>%
    transmute_(~ind, ~type, ~calibrated_time,
               lag_calibrated_time = ~lag(calibrated_time)) %>%
    filter_(~!is.na(type)) %>%
    select_(~-type) %>%
    mutate_(rising = ~lag_calibrated_time < calibrated_time) %>%
    tidyr::gather_("key", "value", c("lag_calibrated_time", "calibrated_time")) %>%
    transmute_(
      ~rising,
      ind = ~ifelse(key == "calibrated_time", ind, ind - 1L),
      kind = ~ifelse(rising,
                           ifelse(key == "calibrated_time", "hi", "lo"),
                           ifelse(key == "calibrated_time", "lo", "hi")
                           )) %>%
    filter_(~!duplicated(ind))

  out_data <-
    data %>%
    mutate_(ind = ~row_number()) %>%
    left_join(data_kind, by = "ind")

  out_data$kind[[1]] <- "b"
  out_data$kind[[length(out_data$kind)]] <- "b"
  out_data$kind <- factor(out_data$kind, levels = c("hi", "b", "lo"))

  out_data
}
