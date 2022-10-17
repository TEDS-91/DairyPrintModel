#' Estimates the daily total manure excretion of heifers.
#'
#' @param dry_matter_intake Dry matter intake (kg).
#' @param body_weight Body weight (kg).
#'
#' @return Daily total manure excretion (kg).
#' @seealso The equations used here are also available in \href{https://pubmed.ncbi.nlm.nih.gov/16162547/}{Nennich et al. (2005)}.
#' @export
#'
#' @examples
#' heifer_manure_excretion(dry_matter_intake = 6, body_weight = 250)
#'
heifer_manure_excretion <- function(dry_matter_intake, body_weight) {

  if (body_weight < 205) {
    heifer_manure_excretion <- dry_matter_intake * 3.45
  } else {
    heifer_manure_excretion <- 4.158 * dry_matter_intake - body_weight * 0.0246
  }

  return(heifer_manure_excretion)

}

#' Estimates the manure dry matter content of heifers.
#'
#' @param dry_matter_intake Dry matter intake (kg).
#'
#' @return Manure dry matter excretion (kg).
#' @seealso The equation used here is also available in \href{https://pubmed.ncbi.nlm.nih.gov/16162547/}{Nennich et al. (2005)}.
#' @export
#'
#' @examples
#' heifer_dm_manure_excretion(dry_matter_intake = 6)
#'
heifer_dm_manure_excretion <- function(dry_matter_intake) {

  heifer_dm_manure_excretion <- 0.393 * dry_matter_intake

  return(heifer_dm_manure_excretion)

}

#' Estimates the daily total manure excretion of dry cows.
#'
#' @param body_weight Body Weight (kg).
#' @param crude_protein Diet crude protein (\%).
#' @param neutral_detergent_fiber Diet neutral detergent fiber (\%).
#'
#' @return Daily total manure excretion (kg).
#' @seealso The equation used here is also available in \href{https://pubmed.ncbi.nlm.nih.gov/16162547/}{Nennich et al. (2005)}.
#' @export
#'
#' @examples
#' dry_manure_excretion(body_weight = 650, crude_protein = 12, neutral_detergent_fiber = 40)
#'
dry_manure_excretion <- function(body_weight, crude_protein, neutral_detergent_fiber) {

  dry_manure_excretion <- 0.00711 * body_weight + 0.324 * crude_protein + 0.259 * neutral_detergent_fiber + 8.05

  return(dry_manure_excretion)

}

#' Estimates the daily total manure excretion of lactating cows.
#'
#' @param dry_matter_intake Dry matter intake (kg).
#'
#' @return Daily total manure excretion (kg).
#' @seealso The equation used here is also available in \href{https://pubmed.ncbi.nlm.nih.gov/16162547/}{Nennich et al. (2005)}.
#' @export
#'
#' @examples
#' lactation_manure_excretion(dry_matter_intake = 25)
#'
lactation_manure_excretion <- function(dry_matter_intake) {

  lactation_manure_excretion <- 2.63 * dry_matter_intake + 9.4

  return(lactation_manure_excretion)

}

#' Estimates the manure dry matter content of lactating cows.
#'
#' @param dry_matter_intake Dry matter intake (kg).
#'
#' @return Manure dry matter excretion (kg).
#' @seealso The equation used here is also available in \href{https://pubmed.ncbi.nlm.nih.gov/16162547/}{Nennich et al. (2005)}.
#' @export
#'
#' @examples
#'lactation_dm_manure_excretion(dry_matter_intake = 25)
#'
lactation_dm_manure_excretion <- function(dry_matter_intake) {

  lactation_dm_manure_excretion <- 0.8 + 0.356 * dry_matter_intake

  return(lactation_dm_manure_excretion)

}
