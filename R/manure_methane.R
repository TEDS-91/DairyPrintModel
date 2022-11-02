#' Estimates the daily methane emissions from manure storage (Raw, liquid, and digestate).
#'
#' @param vs_total Total volatile solids (kg).
#' @param vs_d Degradable VS in the manure (kg).
#' @param vs_nd Non degradable VS in the manure (kg).
#' @param temp_c Temperature (°C).
#' @param enclosed Is the manure storage enclosed: yes or no.
#'
#' @return Methane daily emissions from manure storage (kg).
#' @export
#'
#' @examples
#' manure_ch4_emission_slurry(1, 0.417, 0.583, temp_c = 15, enclosed = "no")
#'
manure_ch4_emission_slurry <- function(vs_total, vs_d, vs_nd, temp_c, enclosed) {

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

  manure_ch4_emission_slurry <- dplyr::if_else(enclosed == "no", 0.024 * vs_total * (vs_d * b_1 + vs_nd * b_2) * exp(ln_A - (E / (R * temp_k))),
                                     (0.024 * vs_total * (vs_d * b_1 + vs_nd * b_2) * exp(ln_A - (E / (R * temp_k)))) * (1 - n_eff))

  return(manure_ch4_emission_slurry)

}

#' Estimates the daily methane emissions from manure storage (solid, raw, and separated).
#'
#' @param vs Manure volatile solids (kg).
#' @param temp_c Temperature (°C).
#'
#' @return Methane daily emissions from manure storage (kg).
#' @export
#'
#' @examples
#' manure_ch4_emission_solid(vs = 1, temp_c = 25)
#'
manure_ch4_emission_solid <- function(vs, temp_c) {

  # Fixed parameters description
  #
  # B_0 is the achievable emission of CH4 during anaerobic digestion (g kg-1 VS) = 0.23
  #
  B_0 <- 0.23
  #
  # MCF is the CH4 conversion factor

  MCF <- (0.201 * temp_c) - 0.29

  manure_ch4_emission_solid <- dplyr::if_else(MCF < 0, (vs * B_0 * 0.68 * 0) / 100,
                                              (vs * B_0 * 0.68 * MCF) / 100)

  return(manure_ch4_emission_solid)

}
