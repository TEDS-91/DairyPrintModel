#' Estimates the phosphorous losses.
#'
#' @param p_applied Phosphorous applied to the land (kg/year).
#' @param rain_fall Yearly rainfall (mm).
#'
#' @return Phosphorous loss (kg/year).
#' @export
#'
#' @examples
#' phosphorous_loss(p_applied = 30, rain_fall = 4)
phosphorous_loss <- function(p_applied, rain_fall) {

  phosphorous_loss <- (-0.097 + 0.000182 * p_applied) * rain_fall

  return(phosphorous_loss)

}
