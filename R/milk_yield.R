#' Estimates the milk yield for lactating cows in the first, second, and third or more lactation being milked two or three times per day.
#'
#' @param days_milk Days in milk.
#' @param parity Parity: primiparous, secondiparous or multiparous.
#' @param milk_freq Milking frequency (2 or 3).
#' @param lambda_milk Hyper parameter to adjust the milk yield for a specific average value.
#'
#' @return Daily milk yield (kg).
#' @seealso The equations used here are also available in \href{https://www.sciencedirect.com/science/article/pii/S0022030222004362}{Li et al. (2022)}.
#' @export
#'
#' @examples
#' milk_yield(days_milk = 35, parity = "primiparous", milk_freq = 2, lambda_milk = 1)
#'
milk_yield <- function(days_milk, parity, milk_freq, lambda_milk) {

  # first lactation prms a, b, c

  lac1_a <- -4.18 + 1.4           # 1.4 due to Wisconsin deviation
  lac1_b <- -0.0037 - 0.0083      # -0.0083 due to Wisconsin deviation
  lac1_c <- -0.000931 - 0.00002   # -0.00002 due to Wisconsin deviation

  # second lactation prms a, b, c

  lac2_a <- 2.16 + 1.4            # 1.4 due to Wisconsin deviation
  lac2_b <- -0.012 - 0.0083       # -0.0083 due to Wisconsin deviation
  lac2_c <- 0.000266 - 0.00002    # -0.00002 due to Wisconsin deviation

  # third lactation prms a, b, c

  lac3_a <- 2.02 + 1.4            # 1.4 due to Wisconsin deviation
  lac3_b <- 0.0157 - 0.0083       # -0.0083 due to Wisconsin deviation
  lac3_c <- 0.000665 - 0.00002    # -0.00002 due to Wisconsin deviation

  # milk_freq = 2

  milk_freq2_a <- -0.74
  milk_freq2_b <- 0.0009
  milk_freq2_c <- 0.000015

  if (parity == 'primiparous' & milk_freq == 2) {
    milk_yield <- ((19.9 + lac1_a + milk_freq2_a) * lambda_milk * days_milk ^ (0.247 + lac1_b + milk_freq2_b) * exp(- (0.003376 + lac1_c + milk_freq2_c) * days_milk))
  } else if (parity == 'secondiparous'  & milk_freq == 2) {
    milk_yield <- ((19.9 + lac2_a + milk_freq2_a) * lambda_milk * days_milk ^ (0.247 + lac2_b + milk_freq2_b) * exp(- (0.003376 + lac2_c + milk_freq2_c) * days_milk))
  } else if (parity == 'multiparous' & milk_freq == 2) {
    milk_yield <- ((19.9 + lac3_a + milk_freq2_a) * lambda_milk * days_milk ^ (0.247 + lac3_b + milk_freq2_b) * exp(- (0.003376 + lac3_c + milk_freq2_c) * days_milk))
  }

  # milk_freq  = 3

  milk_freq3_a <- 0.74
  milk_freq3_b <- -0.0009
  milk_freq3_c <- -0.000015

  if (parity == 'primiparous' & milk_freq == 3) {
    milk_yield <- ((19.9 + lac1_a + milk_freq3_a) * lambda_milk * days_milk ^ (0.247 + lac1_b + milk_freq3_b) * exp(- (0.003376 + lac1_c + milk_freq3_c) * days_milk))
  } else if (parity == 'secondiparous' & milk_freq == 3) {
    milk_yield <- ((19.9 + lac2_a + milk_freq3_a) * lambda_milk * days_milk ^ (0.247 + lac2_b + milk_freq3_b) * exp(- (0.003376 + lac2_c + milk_freq3_c) * days_milk))
  } else if (parity == 'multiparous' & milk_freq == 3) {
    milk_yield <- ((19.9 + lac3_a + milk_freq3_a) * lambda_milk * days_milk ^ (0.247 + lac3_b + milk_freq3_b) * exp(- (0.003376 + lac3_c + milk_freq3_c) * days_milk))
  }

  return(milk_yield)

}

#' Estimates the total milk yield between two days in milk.
#'
#' @param min_value Initial day in milk.
#' @param max_value Final day in milk.
#' @param parity Parity: primiparous, secondiparous or multiparous.
#' @param milk_freq Observed milk yield (kg).
#' @param lambda_milk Hyper parameter to adjust the milk yield for a specific average value.
#'
#' @return Accumulated milk yield between two days in milk (kg).
#' @export
#'
#' @examples
#' milk_yield_acum(min_value = 30,
#'                 max_value = 60,
#'                 parity = "multiparous",
#'                 milk_freq = 2,
#'                 lambda_milk = 2)
milk_yield_acum <- function(min_value, max_value, parity, milk_freq, lambda_milk){

  milk_yield_acum <- seq(min_value, max_value) %>%
    purrr::map_dbl(milk_yield, parity, milk_freq, lambda_milk) %>%
    sum()

  return(milk_yield_acum)

}

#' Estimates the lambda hyper parameter to reach the average milk yield defined by the user.
#'
#' @param obs_average_milk_yield Observed average milk yield (kg).
#' @param milk_freq Milking frequency (2 or 3).
#' @param prop_primiparous Herd proportion of primiparous cows (\%).
#' @param prop_secondiparous Herd proportion of secondiparous cows (\%).
#' @param cow_calving_interval Calving interval (months).
#'
#' @return Lambda hyper parameter (dimensionless).
#' @export
#'
#' @examples
#' lambda_milk_yield(obs_average_milk_yield = 43,
#'                   milk_freq = 2,
#'                   prop_primiparous = 35,
#'                   prop_secondiparous = 30,
#'                   cow_calving_interval = 14)
#'
lambda_milk_yield <- function(obs_average_milk_yield,
                              milk_freq,
                              prop_primiparous,
                              prop_secondiparous,
                              cow_calving_interval) {

  opt_function <- function(x) {

  # maximum days in milk to calculate lambda
  #
  max_days_milk <- round((cow_calving_interval - 2) * 30.4)

  coef_opt <- obs_average_milk_yield - (prop_primiparous / 100 * ((seq(1, max_days_milk) %>%
                                                                     purrr::map_dbl(milk_yield, 'primiparous', milk_freq, lambda = x) %>%
                                                                     sum() ) / max_days_milk) +
                                       (prop_secondiparous / 100 * (seq(1, max_days_milk) %>%
                                                                      purrr::map_dbl(milk_yield, 'secondiparous', milk_freq, lambda = x) %>%
                                                                      sum() / max_days_milk)) +
                                       ((100 - prop_primiparous - prop_secondiparous) / 100 * (seq(1, max_days_milk) %>%
                                                                                                 purrr::map_dbl(milk_yield, 'multiparous', milk_freq, lambda = x) %>%
                                                                                                 sum() / max_days_milk)))

  return(coef_opt)

  }

  lambda_milk <- stats::uniroot(f = opt_function, interval = c(-1, 3))[[1]]

  return(lambda_milk)

}
