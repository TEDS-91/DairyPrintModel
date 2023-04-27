#' Manure density (kg/m3).
#'
#' @param manure_dm manure dry matter content (\%)
#'
#' @return Manure density (kg/m3).
#' @seealso The equation used here is also available in \href{https://www.mdpi.com/2076-3417/9/13/2703}{Wang et al. (2019)}.
#' @export
#'
#' @examples
#' manure_density(manure_dm = 12)
manure_density <- function(manure_dm) {

  manure_density <- 0.013 * manure_dm^3 - 1.28 * manure_dm^2 + 18.47 * manure_dm + 958.1

  return(manure_density)

}
