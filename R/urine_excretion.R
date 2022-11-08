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
