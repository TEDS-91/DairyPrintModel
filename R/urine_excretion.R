#' Estimates the urine excretion for lactating cows.
#'
#' @param dry_matter_intake Dry matter intake (kg).
#' @param crude_protein Diet crude protein (\%).
#' @param milk_protein Milk protein content(\%).
#'
#' @return Daily urine excretion for lactating cows (kg).
#' @export
#'
#' @examples
#' lactating_urine_excretion(dry_matter_intake = 25, crude_protein = 16, milk_protein = 3.2)
#'
lactating_urine_excretion <- function(dry_matter_intake, crude_protein, milk_protein) {

  lactating_urine_excretion <- -7.742 + 0.388 * dry_matter_intake + 0.726 * crude_protein + 2.066 * milk_protein

  return(lactating_urine_excretion)

}

#' Estimates the urine excretion for dry cows and heifers.
#'
#' @param k_intake Potassium intake (g/d)
#'
#' @return Urine excretion (l/d).
#' @export
#'
#' @examples
#' dry_and_heifer_urine_excretion(k_intake = 200)
dry_and_heifer_urine_excretion <- function(k_intake) {

  urine_excretion <- 2.7 + 0.053 * k_intake

  return(urine_excretion)

}
