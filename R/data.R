#' Winsconsin county wheather dataset.
#'
#' This dataset contains wheather information summarized for each county in Wisconsin since 1990 to 2021.
#'
#' @format Dataset with 26280 rows and 6 columns:
#' \describe{
#'   \item{county}{County name.}
#'   \item{yday}{Year day.}
#'   \item{precip_mm}{Precipitation (mm).}
#'   \item{max_tempC}{Maximum temperature (°C).}
#'   \item{min_tempC}{Minimum temperature (°C).}
#'   \item{aver_tempC}{Average temperature (°C).}
#'   ...
#' }
#' @source Data retrieved from \href{https://daymet.ornl.gov/}{Daymet website}.
"wi_wheather"
