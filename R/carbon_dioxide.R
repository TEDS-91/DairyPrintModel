#' Estimates de daily CO2 respired emissions from the animals.
#'
#' @param dry_matter_intake Dry matter intake (kg).
#' @param body_weight Body weight (kg).
#'
#' @return Daily CO2 emissions (kg).
#' @seealso The equation used here is also available in \href{https://www.osti.gov/etdeweb/biblio/6065141}{Kirchgessner et al. (1991)}.
#' @export
#'
#' @examples
#' co2_emissions(dry_matter_intake = 25, body_weight = 680)
co2_emissions <- function(dry_matter_intake, body_weight) {

  co2_emissions <- -1.4 + 0.42 * dry_matter_intake + 0.045 * body_weight ^ 0.75

  return(co2_emissions)

}
