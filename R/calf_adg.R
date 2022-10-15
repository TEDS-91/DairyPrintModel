#' Estimates the average calf daily gain.
#'
#' @param milk_intake Calf milk intake (kg).
#' @param starter_intake Average starter intake (kg).
#'
#' @return Calf average daily gain in a given day (kg).
#' @seealso The equation used here is also available in \href{https://www.simleite.com.br/arquivosAnais/arquivo118}{Silva et al. (2015)}.
#' @export
#'
#' @examples
#' # Estimate the average daily gain of a calf consuming 6 l/day of milk and 0.2 kg/day of starter
#' calf_adg(milk_intake = 6, starter_intake = 0.2)
calf_adg <- function(milk_intake, starter_intake){

  calf_adg <- 0.0952 * milk_intake + 0.0425 * starter_intake

  return(calf_adg)

}

#' Estimates the calf body weight.
#'
#'@param birth_weight Birth weight (kg).
#'@param calf_adg Calf average daily gain (kg).
#' @param day_max Day limit for weight estimation.
#' @param type Weight type to be estimated: average weight in the period or final weight (kg).
#'
#' @return Average calf weight or final calf weight (kg).
#' @export
#'
#' @examples
#' calf_body_weight(birth_weight = 40, calf_adg = 0.5, day_max = 50, type = "final")
#' calf_body_weight(birth_weight = 40, calf_adg = 0.5, day_max = 50, type = "mean")
#'
calf_body_weight <- function(birth_weight, calf_adg, day_max, type) {

  if (type == "final") {
    calf_body_weight <- birth_weight + calf_adg * day_max
  } else if (type == "mean") {
    calf_body_weight <- birth_weight + calf_adg * day_max - 0.5 * calf_adg
  }

  return(calf_body_weight)

}















