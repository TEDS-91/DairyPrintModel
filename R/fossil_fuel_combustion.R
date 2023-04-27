
#' Estimates the CO2eq emissions from gasoline combustion.
#'
#' @param gasoline_consumption Gasoline consumption (L).
#'
#' @return CO2eq emissions (kg).
#' @export
#'
#' @examples
#' gasoline_co2eq(gasoline_consumption = 10)

gasoline_co2eq <- function(gasoline_consumption) {

  gasoline_co2eq <- 2.17 * gasoline_consumption

  return(gasoline_co2eq)

}

#' Estimates the CO2eq emissions from gas natural combustion.
#'
#' @param natural_gas_consumption Gas natural consumption (L).
#'
#' @return CO2eq emissions (kg).
#' @export
#'
#' @examples
#' natural_gas_co2eq(natural_gas_consumption = 10)
natural_gas_co2eq <- function(natural_gas_consumption) {

  natural_gas_co2eq <- 0.002 * natural_gas_consumption

  return(natural_gas_co2eq)

}

#' Estimates the CO2eq emissions from gas diesel combustion.
#'
#' @param diesel_consumption Diesel consumption (L).
#'
#' @return CO2eq emissions (kg).
#' @export
#'
#' @examples
#' diesel_co2eq(diesel_consumption = 10)
diesel_co2eq <- function(diesel_consumption) {

  diesel_consumption <- 2.73 * diesel_consumption

  return(diesel_consumption)

}

#' Estimates the CO2eq emissions from electricity use.
#'
#' @param electricity_consumption Electricity consumption (kWh).
#'
#' @return CO2eq emissions (kg).
#' @export
#'
#' @examples
#' electricity_co2eq(electricity_consumption = 10)
electricity_co2eq <- function(electricity_consumption) {

  electricity_consumption <- 0.66 * electricity_consumption

  return(electricity_consumption)

}
