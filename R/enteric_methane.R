#' Estimates the gross energy intake based on dry matter intake and some diet components.
#'
#' @param dry_matter_intake Dry matter intake (kg).
#' @param crude_protein Diet crude protein concentration (\%).
#' @param ether_extract Diet ether extract concentration (\%).
#' @param neutral_detergent_fiber Diet neutral detergent fiber concentration (\%).
#'
#' @return Gross energy intake (MJ/day).
#' @seealso The equation used here is also available in \href{https://onlinelibrary.wiley.com/doi/full/10.1111/gcb.12471}{Moraes et al. (2014)}.
#' @export
#'
#' @examples
#' gross_energy_intake(dry_matter_intake = 25,
#'                     crude_protein = 16,
#'                     ether_extract = 4,
#'                     neutral_detergent_fiber = 30)
#'
gross_energy_intake <- function(dry_matter_intake,
                                crude_protein,
                                ether_extract,
                                neutral_detergent_fiber) {

  # soluble residues
  OM <- 90

  SR <- OM - neutral_detergent_fiber - crude_protein - ether_extract

  gross_energy_intake <- (0.263 * crude_protein + 0.522 * ether_extract + 0.198 * neutral_detergent_fiber + 0.16 * SR) * dry_matter_intake

  return(gross_energy_intake)

}

#' Estimates the daily enteric methane emission for lactating cows.
#'
#' @param gross_energy_intake Gross energy intake (MJ/day).
#' @param neutral_detergent_fiber Diet neutral detergent fiber concentration (\%).
#' @param ether_extract Diet ether extract concentration (\%).
#' @param body_weight Body weight (kg).
#' @param milk_fat Milk fat content (\%).
#'
#' @return Lactating cow enteric methane emission (g/day).
#' @seealso The equation used here is also available in \href{https://onlinelibrary.wiley.com/doi/full/10.1111/gcb.12471}{Moraes et al. (2014)}.
#' @export
#'
#' @examples
#' enteric_methane_lactating(gross_energy_intake = 465,
#'                          neutral_detergent_fiber = 30,
#'                          ether_extract = 4,
#'                          body_weight = 650,
#'                          milk_fat = 4)
#'
enteric_methane_lactating <- function(gross_energy_intake,
                                      neutral_detergent_fiber,
                                      ether_extract,
                                      body_weight,
                                      milk_fat) {

  enteric_methane_lactating <- (-9.311 + 0.042 * gross_energy_intake + 0.094 * neutral_detergent_fiber - 0.381 * ether_extract + 0.008 * body_weight + 1.621 * milk_fat) / 0.05

  return(enteric_methane_lactating)

}

#' Estimates the daily enteric methane emission for dry cows.
#'
#' @param gross_energy_intake Gross energy intake (MJ/day).
#' @param ether_extract Diet ether extract concentration (\%).
#'
#' @return Dry cow enteric methane emission (g/day).
#' @seealso The equation used here is also available in \href{https://onlinelibrary.wiley.com/doi/full/10.1111/gcb.12471}{Moraes et al. (2014)}.
#' @export
#'
#' @examples
#' enteric_methane_dry(gross_energy_intake = 221, ether_extract = 3)
enteric_methane_dry <- function(gross_energy_intake, ether_extract) {

  enteric_methane_dry <- (2.88 + 0.053 * gross_energy_intake - 0.190 * ether_extract) / 0.05

  return(enteric_methane_dry)

}

#' Estimates the daily enteric methane emission for heifers.
#'
#' @param gross_energy_intake Gross energy intake (MJ/day).
#' @param neutral_detergent_fiber Diet neutral detergent fiber concentration (\%).
#' @param body_weight Body weight (kg).

#' @return Heifer enteric methane emission (g/day).
#' @seealso The equation used here is also available in \href{https://onlinelibrary.wiley.com/doi/full/10.1111/gcb.12471}{Moraes et al. (2014)}.
#' @export
#'
#' @examples
#' enteric_methane_heifer(gross_energy_intake = 110, neutral_detergent_fiber = 40, body_weight = 260)
enteric_methane_heifer <- function(gross_energy_intake, neutral_detergent_fiber, body_weight) {

  enteric_methane_heifer <- (-1.487 + 0.046 * gross_energy_intake + 0.032 * neutral_detergent_fiber + 0.006 * body_weight) / 0.05

  return(enteric_methane_heifer)

}











