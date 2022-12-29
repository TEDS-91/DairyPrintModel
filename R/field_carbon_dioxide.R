#' Estimates the carbon dioxide emissions from lime decomposition applied to croplands.
#'
#' @param lime_applied Amount of lime applied (kg).
#'
#' @return Amount of carbon dioxide emitted (kg) per kg of lime applied.
#' @seealso The equation used here is also available in \href{https://www.ipcc-nggip.iges.or.jp/public/2006gl/pdf/4_Volume4/V4_11_Ch11_N2O&CO2.pdf}{IPCC, 2006}.
#' @export
#'
#' @examples
#' lime_decomposition_co2(lime_applied = 1000)
#'
lime_decomposition_co2 <- function(lime_applied) {

  lime_decomposition_co2 <- 0.45 * lime_applied

  return(lime_decomposition_co2)

}

#' Estimates the carbon dioxide emissions from urea decomposition applied to croplands.
#'
#' @param urea_applied Amount of urea applied (kg).
#'
#' @return Amount of carbon dioxide emitted (kg) per kg of urea.
#' @seealso The equation used here is also available in \href{https://www.ipcc-nggip.iges.or.jp/public/2006gl/pdf/4_Volume4/V4_11_Ch11_N2O&CO2.pdf}{IPCC, 2006}.

#' @export
#'
#' @examples
#' urea_decomposition_co2(urea_applied = 50)
#'
urea_decomposition_co2 <- function(urea_applied) {

  urea_decomposition_co2 <- 0.7 * urea_applied

  return(urea_decomposition_co2)

}
