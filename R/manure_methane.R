#' Estimates the daily methane emissions from manure storage (Raw, liquid, and digestate).
#'
#' @param volatile_solids_total Total volatile solids (kg).
#' @param volatile_solids_d Degradable VS in the manure (kg).
#' @param volatile_solids_nd Non degradable VS in the manure (kg).
#' @param temp_c Temperature (°C).
#' @param enclosed Is the manure storage enclosed: yes or no.
#' @seealso The equation used here is also available in \href{https://elibrary.asabe.org/abstract.asp?aid=27781}{Chianese et al. (2009)}.

#' @return Methane daily emissions from manure storage (kg).
#' @export
#'
#' @examples
#' manure_ch4_emission_slurry(volatile_solids_total = 1,
#'                            volatile_solids_d = 0.417,
#'                            volatile_solids_nd = 0.583,
#'                            temp_c = 15,
#'                            enclosed = "no")
#'
manure_ch4_emission_slurry <- function(volatile_solids_total,
                                       volatile_solids_d,
                                       volatile_solids_nd,
                                       temp_c,
                                       enclosed) {

  # Fixed parameters description:
  #
  # A is the Arrhenius parameter (g CH4 kg-1 VS h-1)
  #
  # E is the apparent activation energy (J mol-1)
  #
  # R is the gas constant (J K-1 mol-1)
  #
  # b_1 is the rate correction factor dimensionless
  #
  # b_2 is the rate correction factor dimensionless

  temp_k <- temp_c + 273.15 # converting from °C to Kelvin

  b_1 <- 1

  b_2 <- 0.01

  ln_A <- 43.33

  E <- 112700

  R <- 8.314

  n_eff <- 0.99

  manure_ch4_emission_slurry <- dplyr::if_else(enclosed == "no", 0.024 * volatile_solids_total * (volatile_solids_d * b_1 + volatile_solids_nd * b_2) * exp(ln_A - (E / (R * temp_k))),
                                               (0.024 * volatile_solids_total * (volatile_solids_d * b_1 + volatile_solids_nd * b_2) * exp(ln_A - (E / (R * temp_k)))) * (1 - n_eff))

  return(manure_ch4_emission_slurry)

}

#' Estimates the daily methane emissions from manure storage (solid, raw, and separated).
#'
#' @param volatile_solids Manure volatile solids (kg).
#' @param temp_c Temperature (°C).
#'
#' @return Methane daily emissions from manure storage (kg).
#' @export
#'
#' @examples
#' manure_ch4_emission_solid(volatile_solids = 1, temp_c = 25)
#'
manure_ch4_emission_solid <- function(volatile_solids, temp_c) {

  # Fixed parameters description
  #
  # B_0 is the achievable emission of CH4 during anaerobic digestion (g kg-1 VS) = 0.23
  #
  B_0 <- 0.23
  #
  # MCF is the CH4 conversion factor
  #
  MCF <- (0.201 * temp_c) - 0.29

  manure_ch4_emission_solid <- dplyr::if_else(MCF < 0, (volatile_solids * B_0 * 0.68 * 0) / 100,
                                              (volatile_solids * B_0 * 0.68 * MCF) / 100)

  return(manure_ch4_emission_solid)

}
