#' Body composition of adult cows.
#'
#' @param body_weight Body weight (kg).
#' @param BCS Body condition score (1 to 5).
#'
#' @return Proportion of protein, fat, water, and ash in the empty body weight of adult cows.
#' @seealso The equation used here is also available in \href{https://nap.nationalacademies.org/catalog/9825/nutrient-requirements-of-dairy-cattle-seventh-revised-edition-2001}{NRC (2001)}.
#' @export
#'
#' @examples
#' adult_cows_body_composition(body_weight = 680, BCS = 3)
adult_cows_body_composition <- function(body_weight, BCS) {

  # converting BCS from scale 1 to 5 to 1 to 9

  bcs_eq <- ((BCS - 1 ) * 2) + 1

  # empty body weight

  EBW <- 0.817 * body_weight

  # body fat - as proportion of EBW

  body_fat <- 0.037683 * bcs_eq

  # body protein - as proportion of EBW

  body_protein <- 0.200886 - 0.0066762 * bcs_eq

  # Water - considering 72.8% of the fat-free EBW

  body_water <- 0.7228 * (1 - body_fat)

  # Ash - obtained by difference

  body_ash <- 1 - body_fat - body_protein - body_water

  body_composition <- list(
    empty_body_weight_kg = round(EBW, 2),
    water                = round(body_water, 4),
    protein              = round(body_protein, 4),
    fat                  = round(body_fat, 4),
    ash                  = round(body_ash, 4),
    nitrogen_kg          = round(body_protein * EBW / 6.25, 1)
  )

  return(body_composition)

}




