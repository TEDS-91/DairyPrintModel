#' Estimates the daily protein content in the milk.
#'
#' @param parity Parity: primiparous, secondiparous, or multiparous.
#' @param days_milk Days in milk.
#' @param lambda_prot Hyperparameter to adjust the milk protein content for a specific average value.
#'
#' @return Daily milk protein content (\%).
#' @export
#'
#' @examples
#' milk_protein_content(parity = "multiparous", days_milk = 60, lambda_prot = 1)
#'
milk_protein_content <- function(parity, days_milk, lambda_prot) {

  if (parity == "primiparous") {
    milk_protein_content <- 4.616 * lambda_prot + 0.003161 * days_milk + -0.000005535 * days_milk ^ 2 + -0.8853 * log(days_milk) + 0.0993 * (log(days_milk) ^ 2 )
  } else if (parity == "secondiparous") {
    milk_protein_content <- 4.8851 * lambda_prot + 0.003733 * days_milk + -0.000005359 * days_milk ^ 2 + -0.9781 * log(days_milk) + 0.104 * (log(days_milk) ^ 2 )
  } else {
    milk_protein_content <- 4.9812 * lambda_prot + 0.001792 * days_milk + -0.000002778 * days_milk ^ 2 + -1.0986 * log(days_milk) + 0.1315 * (log(days_milk) ^ 2 )
  }

  return(milk_protein_content)

}

#' Estimates the average milk protein between two days in milk.
#'
#' @param parity  Parity: primiparous, secondiparous, or multiparous.
#' @param day_min Initial day in milk.
#' @param day_max Final day in milk.
#' @param lambda_prot Hyperparameter to adjust the milk protein content for a specific average value.
#'
#' @return Average milk protein between two days in milk (\%).
#' @export
#'
#' @examples
#' average_milk_protein(parity = "multiparous", day_min = 30, day_max = 60, lambda_prot = 1)
#'
average_milk_protein <- function(parity, day_min, day_max, lambda_prot) {

  average_milk_protein <- seq(day_min, day_max) %>%
    purrr::map_dbl(milk_protein_content, parity = parity, lambda_prot = lambda_prot) %>%
    sum() / (day_max - day_min + 1)

  return(average_milk_protein)

}

#' Estimates the lambda hyper parameter to reach the average milk protein defined by the user.
#'
#' @param obs_average_milk_prot Observed average milk protein (\%).
#' @param prop_primiparous Herd proportion of primiparous cows (\%).
#' @param prop_secondiparous Herd proportion of secondiparous cows (\%).
#' @param cow_calving_interval Calving interval (months).
#'
#' @return Lambda hyper parameter (dimensionless).
#' @export
#'
#' @examples
#' lambda_milk_prot(obs_average_milk_prot = 3.5,
#'                  prop_primiparous = 35,
#'                  prop_secondiparous = 30,
#'                  cow_calving_interval = 14)
#'
lambda_milk_prot <- function(obs_average_milk_prot,
                             prop_primiparous,
                             prop_secondiparous,
                             cow_calving_interval) {

  opt_function <- function(x) {

    # maximum days in milk to calculate lambda
    #
    max_days_milk <- round((cow_calving_interval - 2) * 30)

    coef_opt <- obs_average_milk_prot - (prop_primiparous / 100 * ((seq(1, max_days_milk) %>%
                                                                       purrr::map_dbl(milk_protein_content, parity = 'primiparous', lambda = x) %>%
                                                                       sum() ) / max_days_milk) +
                                            (prop_secondiparous / 100 * (seq(1, max_days_milk) %>%
                                                                           purrr::map_dbl(milk_protein_content, parity = 'secondiparous', lambda = x) %>%
                                                                           sum() / max_days_milk)) +
                                            ((100 - prop_primiparous - prop_secondiparous) / 100 * (seq(1, max_days_milk) %>%
                                                                                                      purrr::map_dbl(milk_protein_content, parity = 'multiparous', lambda = x) %>%
                                                                                                      sum() / max_days_milk)))

    return(coef_opt)

  }

  lambda_milk_prot <- stats::uniroot(f = opt_function, interval = c(-1, 3))[[1]]

  return(lambda_milk_prot)

}

