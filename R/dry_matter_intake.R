#' Estimates the daily dry matter intake of lactating cows.
#'
#' @param days_milk days in milk.
#' @param body_weight Body weight (kg).
#' @param milk_yield Milk yield (kg).
#'
#' @return Lactating cow dry matter intake (kg).
#' @seealso The equation used here is also available in \href{https://nap.nationalacademies.org/catalog/9825/nutrient-requirements-of-dairy-cattle-seventh-revised-edition-2001}{NRC (2001)}.
#' @export
#'
#' @examples
#'lactating_dry_matter_intake(days_milk = 60, body_weight = 680, milk_yield = 40)
#'
lactating_dry_matter_intake <- function(days_milk, body_weight, milk_yield) {

  lactating_dry_matter_intake <- (0.372 * milk_yield + 0.0968 * body_weight ^ 0.75) * (1 - exp(-0.192 * (days_milk / 7 + 3.67)))

  return(lactating_dry_matter_intake)

}

#' Estimates the daily dry matter intake of heifers.
#'
#' @param mature_body_weight Mature body weight (kg).
#' @param body_weight Body weight (kg).
#'
#' @return Heifer dry matter intake (kg).
#' @export
#'
#' @examples
#' heifer_dry_matter_intake(mature_body_weight = 680, body_weight = 300)
#'
heifer_dry_matter_intake <- function(mature_body_weight, body_weight) {

  heifer_dry_matter_intake <- 0.022 * mature_body_weight * (1 - exp(-1.54 *( body_weight / mature_body_weight)))

  return(heifer_dry_matter_intake)

}

#' Estimates the daily dry matter intake of dry cows.
#'
#' @param body_weight Body weight (kg).
#'
#' @return Dry cow dry matter intake (kg).
#' @export
#'
#' @examples
#' dry_cow_dry_matter_intake(680)
#'
dry_cow_dry_matter_intake <- function(body_weight) {

  dry_cow_dry_matter_intake <- body_weight * 0.02

  return(dry_cow_dry_matter_intake)

}
