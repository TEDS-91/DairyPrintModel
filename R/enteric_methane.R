
#' Methane Emissions from Lactating Cows.
#'
#' @description Bla Bla Bla
#'
#' @param dry_matter_intake_kg Dry matter intake (kg/day).
#' @param ndf_perc Diet neutral detergent Fiber percentage.
#' @param milk_fat_perc Milk fat content percentage.
#' @param bw_kg Cow body weight (kg).
#'
#' @return g of methane (cow/day).
#' @export
#'
enteric_methane_lactating <- function(dry_matter_intake_kg, ndf_perc, milk_fat_perc, bw_kg){

  enteric_methane_lac_cows_g_day <- -126 + 11.3 * dry_matter_intake_kg + 2.3 * ndf_perc + 28.8 * milk_fat_perc + 0.148 * bw_kg

  return(enteric_methane_lac_cows_g_day)

}
