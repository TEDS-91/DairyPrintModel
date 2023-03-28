

#' Estimates the daily methane emissions from manure application (kg).
#'
#' @param Fvfa Daily concentration of VFAs in the slurry (mmol/kg slurry).
#' @param area_crop Land area (ha).
#' @seealso The equation used here is also available in \href{https://elibrary.asabe.org/abstract.asp?aid=27781}{Chianese et al. (2009)}.
#'
#' @return Daily methane emissions from manure application (kg).
#' @export
#'
#' @examples
#' manure_application_ch4_emission(Fvfa = 1, area_crop = 10)
#'
manure_application_ch4_emission <- function(Fvfa, area_crop) {

  manure_application_ch4_emission <- (0.17 * Fvfa + 0.026) * 0.032 * area_crop

  return(manure_application_ch4_emission)

}

#' Estimates the daily concentration of VFA in the field-applied slurry.
#'
#' @param Fvfa_ini Initial VFA concentration in the slurry at application (mmol /kg slurry).
#'
#' @return Daily concentration of VFA in the field-applied slurry (mmmol/kg).
#' @export
#'
#' @examples
#'Fvfa_conc(Fvfa_ini = 0.91)
#'
Fvfa_conc <- function(Fvfa_ini) {

  time_day <- seq(1, 11)

  Fvfa <- Fvfa_ini * exp(-0.6939 * time_day)

  return(Fvfa)

}

#' Estimates the initial concentration of volatile fatty acids in the slurry.
#'
#' @param Ftan concentration of TAN (total amoniacal nitrogen; NH4 + NH3) in the slurry (mmol/kg slurry).
#' @param ph pH of the manure slurry (dimensionless).
#'
#' @return concentration of volatile fatty acids in the slurry (mmol/kg slurry).
#' @export
#'
#' @examples
#' Fvfa_init_conc(Ftan = 2500, ph = 7)
#'
Fvfa_init_conc <- function(Ftan, ph) {

  Fvfa_init_conc <- (Ftan / 2.02) * (9.43 - ph)

  return(Fvfa_init_conc)

}
