
#' Ammonia emissions from fertilizer application.
#'
#' @param nitrogen_applied Nitrogen applied (kg).
#' @seealso The equation used here is also available in \href{https://www.ipcc-nggip.iges.or.jp/public/2006gl/pdf/4_Volume4/V4_10_Ch10_Livestock.pdf}{IPCC (2006)}.
#'
#' @return Ammonia emitted per kg N applied (kg).
#' @export
#'
#' @examples
#'fertilizer_nh3(nitrogen_applied = 100)
#
fertilizer_nh3 <- function(nitrogen_applied) {

  fertilizer_nh3 <- 0.1 * nitrogen_applied

  return(fertilizer_nh3)

}
