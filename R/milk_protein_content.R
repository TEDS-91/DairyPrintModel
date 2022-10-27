#' Estimates the daily protein content in the milk.
#'
#' @param parity Parity: primiparous, secondiparous, or multiparous.
#' @param days_milk Days in milk.
#'
#' @return Daily milk protein content (\%).
#' @export
#'
#' @examples
#' milk_protein_content(parity = "multiparous", days_milk = 60)
#'
milk_protein_content <- function(parity, days_milk) {

  if (parity == "primiparous") {
    milk_protein_content <- 4.616 + 0.003161 * days_milk + -0.000005535 * days_milk ^ 2 + -0.8853 * log(days_milk) + 0.0993 * (log(days_milk) ^ 2 )
  } else if (parity == "secondiparous") {
    milk_protein_content <- 4.8851 + 0.003733 * days_milk + -0.000005359 * days_milk ^ 2 + -0.9781 * log(days_milk) + 0.104 * (log(days_milk) ^ 2 )
  } else {
    milk_protein_content <- 4.9812 + 0.001792 * days_milk + -0.000002778 * days_milk ^ 2 + -1.0986 * log(days_milk) + 0.1315 * (log(days_milk) ^ 2 )
  }

  return(milk_protein_content)

}

#' Estimates the average milk protein between two days in milk.
#'
#' @param parity  Parity: primiparous, secondiparous, or multiparous.
#' @param day_min Initial day in milk.
#' @param day_max Final day in milk.
#'
#' @return Average milk protein between two days in milk (\%).
#' @export
#'
#' @examples
#' average_milk_protein(parity = "multiparous", day_min = 30, day_max = 60)
#'
average_milk_protein <- function(parity, day_min, day_max) {

  average_milk_protein <- seq(day_min, day_max) %>%
    purrr::map_dbl(milk_protein_content, parity = parity) %>%
    sum() / (day_max - day_min + 1)

  return(average_milk_protein)

}
