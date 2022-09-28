
#' Methane Emissions from lactating cows.
#'
#' @param dry_matter_intake_kg dry matter inatake.
#' @param ndf_perc Neutral detergent Fiber.
#' @param milk_fat_perc Milk Fat.
#' @param bw_kg Body Weight.
#'
#' @return g of methane per day.
#' @export
#'
enteric_methane_lactating <- function(dry_matter_intake_kg, ndf_perc, milk_fat_perc, bw_kg){

  enteric_methane_lac_cows_g_day <- -126 + 11.3 * dry_matter_intake_kg + 2.3 * ndf_perc + 28.8 * milk_fat_perc + 0.148 * bw_kg

  return(enteric_methane_lac_cows_g_day)

}
