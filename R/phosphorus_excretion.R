#' Estimates the daily fecal phosphorous excretion of lactating cows.
#'
#' @param dry_matter_intake Dry matter intake (kg).
#' @param phosphorous_content Diet phosphorous content (\%).
#'
#' @return Daily fecal phosphorous excretion (g).
#' @export
#'
#' @examples
#' lactating_p_total_excretion(dry_matter_intake = 25, phosphorous_content = 1.2)
#'
lactating_p_total_excretion <- function(dry_matter_intake, phosphorous_content) {

  lactating_p_total_excretion <- (dry_matter_intake * phosphorous_content / 100 * 560.7) + 21.1

  return(lactating_p_total_excretion)

}
