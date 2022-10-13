#' Estimates the average calf starter intake in a given day.
#'
#' @param milk_intake Calf milk intake in liters.
#' @param age Calf age in days.
#'
#' @return Starter intake (kg).
#' @seealso The equations used here are also available in \href{https://pubmed.ncbi.nlm.nih.gov/29656719/}{Silva et al. (2019)}.
#' @export
#'
#' @examples
#' # Estimate the average starter intake of a group of calves 50 days old calf consuming 6 l of milk
#'starter_intake_calves(milk_intake = 6, age = 50)
starter_intake_calves <- function(milk_intake, age) {

  starter_intake <- ifelse(milk_intake <= 5,
                           0.1839 * milk_intake * exp((0.0333 - 0.0040 * milk_intake) * (age - (0.8302 + 6.0332 * milk_intake))) - (0.12 * milk_intake),
                           0.1225 * milk_intake * exp((0.0217 - 0.0015 * milk_intake) * (age - (3.5382 + 1.9508 * milk_intake))) - (0.12 * milk_intake))

  starter_intake <- ifelse(starter_intake < 0, 0, starter_intake)

  return(starter_intake)

}
