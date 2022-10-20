#' Estimates the body weight losses daily over the lactation.
#'
#' @param days_milk Days in milk.
#' @param parity Parity: primiparous or multiparous.
#'
#' @return Body weight loss at given day in milk (kg).
#' @seealso The equations used here are also available in \href{https://pubmed.ncbi.nlm.nih.gov/23415521/}{Galvão et al. (2013)}.
#' @export
#'
#' @examples
#' body_weight_loss_lactation(days_milk = 30, parity = "multiparous")
body_weight_loss_lactation <- function(days_milk, parity) {

  if (parity == "multiparous") {
    body_weight_change <- (- 40 / 75 * exp(1 - days_milk / 75) + 40 / 75 ^ 2 * days_milk * exp(1 - days_milk / 75))
  } else if (parity != "multiparous") {
    body_weight_change <- (- 20 / 65 * exp(1 - days_milk / 65) + 20 / 65 ^ 2 * days_milk * exp(1 - days_milk / 65))
  }

  return(body_weight_change)

}

#' Estimates the accumulate body weight losses in an user defined range of days in milk.
#'
#' @param min_value First day for weight estimation.
#' @param max_value Day limit for weight estimation.
#' @param parity Parity: primiparous or multiparous.
#'
#' @return Accumulate body weight losses (kg).
#' @export
#'
#' @examples
#' cow_body_weight(min_value = 1, max_value = 30, parity = "multiparous")
cow_body_weight <- function(min_value, max_value, parity) {

  cow_body_weight <- seq(min_value, max_value) %>%
    purrr::map_dbl(body_weight_loss_lactation, parity) %>%
    sum()

  return(cow_body_weight)

}
