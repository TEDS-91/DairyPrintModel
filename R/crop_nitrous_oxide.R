
#' Nitrous oxide emissions from fertilizer application.
#'
#' @param nitrogen_applied Nitrogen applied (kg).
#' @seealso The equation used here is also available in \href{https://www.ipcc-nggip.iges.or.jp/public/2006gl/pdf/4_Volume4/V4_10_Ch10_Livestock.pdf}{IPCC (2006)}.
#'
#' @return Nitrous oxide emitted per kg N applied from fertilizers and manure (kg).
#' @export
#'
#' @examples
#'fert_manure_nitrous_oxide(nitrogen_applied = 100)
#
fert_manure_nitrous_oxide <- function(nitrogen_applied) {

  fert_manure_nitrous_oxide <- 1.6 * nitrogen_applied

  return(fert_manure_nitrous_oxide)

}
