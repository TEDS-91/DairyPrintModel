#' estimates the daily milk potassium excretion.
#'
#' @param milk_yield Milk yield (kg).
#'
#' @return Daily milk potassium excretion (g).
#' @export
#'
#' @examples
#' milk_k_excretion(milk_yield = 45)
#'
milk_k_excretion <- function(milk_yield) {

  milk_k_excretion <- 1.5 * milk_yield

  return(milk_k_excretion)

}

#' Estimates the daily total potassium excretion of heifers.
#'
#' @param body_weight Body weight (kg).
#' @param dry_matter_intake Dry matter intake (kg).
#'
#' @return Daily total potassium excretion (g).
#' @export
#'
#' @examples
#' heifer_k_excretion(body_weight = 250, dry_matter_intake = 5.5)
#'
heifer_k_excretion <- function(body_weight, dry_matter_intake) {

  heifer_k_excretion <- 0.038 * body_weight + 6.1 * dry_matter_intake

  return(heifer_k_excretion)

}





