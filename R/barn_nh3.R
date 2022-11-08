#' Estimates the equilibrium coefficient by the Henry's law.
#'
#' @param temp_c Temperature (°C).
#' @param pH pH.
#' @seealso The equation used here is also available in \href{https://elibrary.asabe.org/abstract.asp?aid=21731}{Rotz and Oenenma (2005)}.
#'
#' @return Equilibrium coefficient by the Henry's law
#' @export
#'
#' @examples
#' eq_coeff(temp_c = 25, pH = 7.7)
#'
eq_coeff <- function(temp_c, pH) {

  Kh <- 10 ^ (1478 / (temp_c + 273) - 1.69)

  Ka <- 1 + 10 ^ (0.09018 + 2729.9 / (temp_c + 273) - pH)

  eq_coeff <- Kh * Ka

  return(eq_coeff)

}

#' Estimates the resistance of NH3 transport from the manure surface to the free atmosphere.
#'
#' @param hsc Housing-specific constant (s/m).
#' @param temp_c Temperature (°C).
#' @seealso The equation used here is also available in \href{https://elibrary.asabe.org/abstract.asp?aid=21731}{Rotz and Oenenma (2005)}.
#'
#' @return Resistance of NH3 transport from the manure surface to the free atmosphere (s/m).
#' @export
#'
#' @examples
#' resistence_nh3(hsc = 260, temp_c = 25)
#'
resistence_nh3 <- function(hsc, temp_c) {

  resistence_nh3 <- hsc * (1 - 0.027 * (20 - temp_c))

  return(resistence_nh3)

}
