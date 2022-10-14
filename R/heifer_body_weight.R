#' Estimates the heifer mean or final body weight at given month.
#'
#' @param birth_weight Calf birth weight (kg).
#' @param weaning_weight Weaning weight (kg).
#' @param age_first_calving Age at first calving (months).
#' @param mature_body_weight Mature body weight (kg)
#' @param month Month to be simulated.
#' @param type Weight type to be estimated: average weight in the period or final weight (kg).
#'
#' @return Heifer body weight (kg)
#' @export
#'
#' @examples
#' heifer_body_weight(birth_weight = 40,
#'                    weaning_weight = 80,
#'                    age_first_calving = 24,
#'                    mature_body_weight = 680,
#'                    month = 6,
#'                    type = "mean")
#'
#' heifer_body_weight(birth_weight = 40,
#'                    weaning_weight = 80,
#'                    age_first_calving = 24,
#'                    mature_body_weight = 680,
#'                    month = 6,
#'                    type = "final")
#'
heifer_body_weight <- function(birth_weight,
                               weaning_weight,
                               age_first_calving,
                               mature_body_weight,
                               month,
                               type){

  # Assuming first calving weight as 82% of the mature body weight - kg

  weight_first_calving <- 0.82 * mature_body_weight

  adg_heifers <- (weight_first_calving - weaning_weight) / ((age_first_calving - 2) * 30.4)

  if (type == "final") {
    heifer_body_weight <- 30.4 * adg_heifers * (month - 2) + weaning_weight
  } else if (type == "mean") {
    heifer_body_weight <- 30.4 * adg_heifers * (month - 2) + weaning_weight - (30.4 / 2 * adg_heifers)
  }

  return(heifer_body_weight)

}

