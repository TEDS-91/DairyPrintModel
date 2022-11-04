#' Estimates the daily fecal phosphorous excretion of lactating cows.
#'
#' @param dry_matter_intake Dry matter intake (kg).
#' @param phosphorous_content Diet phosphorous content (\%).
#' @param milk_p_excretion Milk phosphorous excretion (g).
#' @seealso The equation used here is also available in \href{https://pubmed.ncbi.nlm.nih.gov/16162547/}{Nennich et al. (2005)}.
#' @note This function assumes no tissue mobilization or retention of phosphorous by the cows.
#' @return Daily fecal phosphorous excretion (g).
#' @export
#'
#' @examples
#' lactating_p_total_excretion(dry_matter_intake = 25,
#'                             phosphorous_content = 0.4,
#'                             milk_p_excretion = 45)
#'
lactating_p_total_excretion <- function(dry_matter_intake,
                                        phosphorous_content,
                                        milk_p_excretion) {

  lactating_p_total_excretion <- (dry_matter_intake * phosphorous_content / 100 * 1000) - milk_p_excretion

  return(lactating_p_total_excretion)

}
