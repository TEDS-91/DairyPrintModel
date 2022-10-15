#' Estimates the nitrous oxide emissions from the animals.
#'
#' @param dry_matter_intake Dry matter intake (kg).
#' @param crude_protein Diet crude protein (\%).
#'
#' @return Daily nitrous oxide emissions (g).
#' @seealso The equation used here is also available in \href{https://www.ars.usda.gov/northeast-area/up-pa/pswmru/docs/dairy-gas-emissions-model/}{Rotz et al. (2016)}.
#' @export
#'
#' @examples
#'animal_n2o_emissions(dry_matter_intake = 25, crude_protein = 16)
animal_n2o_emissions <- function(dry_matter_intake, crude_protein) {

  animal_n2o_emissions <- ((dry_matter_intake * crude_protein / 100) * 0.8) / 6.25

  return(animal_n2o_emissions)

}
