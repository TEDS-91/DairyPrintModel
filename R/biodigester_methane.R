#' Estimates the methane yield from the biodigester.
#'
#' @param volatile_solids Total volatile solids in the afluent (kg).
#' @param biodigester_efficiency Biodigester efficiency (values vary from 20\% to 45\%)
#' @seealso The equation used here is also available in \href{https://www.ars.usda.gov/northeast-area/up-pa/pswmru/docs/integrated-farm-system-model/#Reference}{Rotz et al. (2022)}.
#'
#' @return Total methane yield (kg).
#' @export
#'
#' @examples
#' biodigester_ch4_yield(volatile_solids = 2088, biodigester_efficiency = 30)
#'
biodigester_ch4_yield <- function(volatile_solids, biodigester_efficiency) {

  biodigester_ch4_yield <- volatile_solids * 0.35 * biodigester_efficiency / 100

  return(biodigester_ch4_yield)

}
