#' Estimates the daily total volatile solids.
#'
#' @param dry_matter_intake Dry matter intake (kg).
#' @param crude_protein Diet crude protein (\%).
#' @param neutral_detergent_fiber Diet neutral detergent fiber (\%).
#'
#' @return Daily total volatile solids (kg).
#' @seealso The equation used here is also available in \href{https://nap.nationalacademies.org/catalog/25806/nutrient-requirements-of-dairy-cattle-eighth-revised-edition}{NASEM (2021)}.
#' @export
#'
#' @examples
#' volatile_solids(dry_matter_intake = 25,
#'                crude_protein = 16,
#'                neutral_detergent_fiber = 30)
#'
volatile_solids <- function(dry_matter_intake, crude_protein, neutral_detergent_fiber) {

  volatile_solids <- 0.364 * dry_matter_intake + 0.026 * neutral_detergent_fiber - 0.078 * crude_protein

  return(volatile_solids)

}

#' Estimates the digestible fraction of the volatile solids.
#'
#' @param dry_matter_intake Dry matter intake (kg).
#' @param crude_protein Diet crude protein (\%).
#' @param neutral_detergent_fiber Diet neutral detergent fiber (\%).
#' @param acid_detergent_fiber Diet acid detergent fiber (\%).
#'
#' @return Digestible volatile solids (kg).
#' @seealso The equation used here is also available in \href{https://nap.nationalacademies.org/catalog/25806/nutrient-requirements-of-dairy-cattle-eighth-revised-edition}{NASEM (2021)}.
#' @export
#'
#' @examples
#' digestible_volatile_solids(dry_matter_intake = 25,
#'                           crude_protein = 16,
#'                           neutral_detergent_fiber = 30,
#'                           acid_detergent_fiber = 15)
#'
digestible_volatile_solids <- function(dry_matter_intake, crude_protein, neutral_detergent_fiber, acid_detergent_fiber) {

  hemicellulose <- neutral_detergent_fiber - acid_detergent_fiber

  digestible_volatile_solids <- 0.334 * dry_matter_intake + 0.029 * hemicellulose - 0.058 * crude_protein

  return(digestible_volatile_solids)

}

