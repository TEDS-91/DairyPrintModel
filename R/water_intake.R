
#' Daily lactating cows water intake.
#'
#' @param temp_c Temperature (°C).
#' @param milk_yield_kg Milk yield corrected for fat and protein (kg).
#' @param bw_kg Body weight (kg).
#' @seealso The equations used here are also available in \href{https://www.sciencedirect.com/science/article/pii/S0378377414002145}{Krauß et al. (2015)}.
#'
#' @return Daily water intake (l).
#' @export
#'
#' @examples
#' lactating_water_intake(temp_c = 35, milk_yield_kg = 45, bw_kg = 650)
lactating_water_intake <- function(temp_c, milk_yield_kg, bw_kg) {

  water_intake <- -26.12 + 1.516 * temp_c + 1.538 * milk_yield_kg + 0.063 * bw_kg

  return(water_intake)

}

#' Daily heifer and dry cows water intake.
#'
#' @param bw_kg Body weight (kg).
#'
#' @return Daily water intake (l).
#' @export
#'
#' @examples
#' heifer_dry_cows_water_intake(bw_kg = 250)
heifer_dry_cows_water_intake <- function(bw_kg) {

  water_intake <- 6.46 + (0.0728 * bw_kg)

  return(water_intake)

}
