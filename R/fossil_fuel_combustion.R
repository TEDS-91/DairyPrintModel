

#' Title
#'
#' @param gasoline_consumption
#'
#' @return
#' @export
#'
#' @examples
gasoline_co2eq <- function(gasoline_consumption) {

  gasoline_co2eq <- 2.17 * gasoline_consumption

  return(gasoline_co2eq)

}

#' Title
#'
#' @param natural_gas_consumption
#'
#' @return
#' @export
#'
#' @examples
natural_gas_co2eq <- function(natural_gas_consumption) {

  natural_gas_co2eq <- 0.002 * natural_gas_consumption

  return(natural_gas_co2eq)

}

#' Title
#'
#' @param diesel_consumption
#'
#' @return
#' @export
#'
#' @examples
diesel_co2eq <- function(diesel_consumption) {

  diesel_consumption <- 2.73 * diesel_consumption

  return(diesel_consumption)

}




