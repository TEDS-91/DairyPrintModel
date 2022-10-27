#' Estimates de daily carbon dioxide respired emissions from the animals.
#'
#' @param dry_matter_intake Dry matter intake (kg).
#' @param body_weight Body weight (kg).
#'
#' @return Daily carbon dioxide emissions (kg).
#' @seealso The equation used here is also available in \href{https://www.osti.gov/etdeweb/biblio/6065141}{Kirchgessner et al. (1991)}.
#' @export
#'
#' @examples
#' animal_co2_emissions(dry_matter_intake = 25, body_weight = 680)
#'
animal_co2_emissions <- function(dry_matter_intake, body_weight) {

  animal_co2_emissions <- -1.4 + 0.42 * dry_matter_intake + 0.045 * body_weight ^ 0.75

  animal_co2_emissions <- ifelse(animal_co2_emissions < 0, 0, animal_co2_emissions)

  return(animal_co2_emissions)

}
