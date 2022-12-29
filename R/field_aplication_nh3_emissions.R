#' Estimates the NH3 emissions from manure application.
#'
#' @param tan Total ammoniacal nitrogen (kg).
#' @param total_solids Total solids (kg).
#' @param days days to incorporate manure.
#'
#' @return NH3 emissions from manure application (kg).
#' @seealso The equation used here is also available in \href{https://www.ars.usda.gov/northeast-area/up-pa/pswmru/docs/dairy-gas-emissions-model/}{Rotz et al. (2016)}.
#' @export
#'
#' @examples
#' field_nh3(tan = 7000, total_solids = 288000, days = 10)
#'
field_nh3 <- function(tan, total_solids, days) {

  field_nh3 <- (tan * ((20 + 5 * total_solids) * (days / (days + 0.3))) * (17/14)) / 100

  return(field_nh3)

}
