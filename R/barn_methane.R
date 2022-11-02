#' Estimates the daily methane emissions from the barn floor (free-stalls or tie-stalls).
#'
#' @param temp_c Average ambient temperature of the barn (°C).
#' @param manure_area Area of the barn floor covered with manure (m²).
#'
#' @note Free-stalls = 3.5 m²/animal; tie-stalls = 1.5 m²/animal; growing = animals 2.5 m²/animal.
#' @return Methane emissions from the barn floor (kg).
#' @export
#'
#' @examples
#' barn_ch4_emission_floor(temp_c = 25, manure_area = 300)
#'
barn_ch4_emission_floor <- function(temp_c, manure_area) {

  barn_ch4_emission_floor <- dplyr::if_else(temp_c < 0, 0, (0.13 * temp_c) * manure_area / 1000)

  return(barn_ch4_emission_floor)

}
