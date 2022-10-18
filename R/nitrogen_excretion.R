#' Estimates the daily fecal nitrogen excretion of lactating cows.
#'
#' @param nitrogen_intake Nitrogen intake (g/day).
#'
#' @return Daily fecal nitrogen excretion (g).
#' @seealso The equation used here is also available in \href{https://www.journalofdairyscience.org/article/S0022-0302(22)00420-9/fulltext}{Bougouin et al. (2022)}.
#' @export
#'
#' @examples
#' lactating_n_fecal_excretion(nitrogen_intake = 640)
#'
lactating_n_fecal_excretion <- function(nitrogen_intake) {

  lactating_n_fecal_excretion <- 24.5 + 0.29 * nitrogen_intake

  return(lactating_n_fecal_excretion)

}

#' Estimates the daily urinary nitrogen excretion of lactating cows.
#'
#' @param nitrogen_intake Nitrogen intake (g/day).
#'
#' @return Daily urinary nitrogen excretion (g).
#' @seealso The equation used here is also available in \href{https://www.journalofdairyscience.org/article/S0022-0302(22)00420-9/fulltext}{Bougouin et al. (2022)}.
#' @export
#'
#' @examples
#' lactating_n_urinary_excretion(nitrogen_intake = 640)
#'
lactating_n_urinary_excretion <- function(nitrogen_intake) {

  lactating_n_urinary_excretion <- 63.5 + 0.2 * nitrogen_intake

  return(lactating_n_urinary_excretion)

}

#' Estimates the daily total nitrogen excretion of lactating cows.
#'
#' @param nitrogen_intake Nitrogen intake (g/day).
#'
#' @return Daily total nitrogen excretion (g).
#' @seealso The equation used here is also available in \href{https://www.journalofdairyscience.org/article/S0022-0302(22)00420-9/fulltext}{Bougouin et al. (2022)}.
#' @export
#'
#' @examples
#' lactating_n_total_excretion(nitrogen_intake = 640)
#'
lactating_n_total_excretion <- function(nitrogen_intake) {

  lactating_n_total_excretion <- 68.8 + 0.53 * nitrogen_intake

  return(lactating_n_total_excretion)

}

#' Estimates the daily fecal nitrogen excretion of dry cows.
#'
#' @param nitrogen_intake Nitrogen intake (g/day).
#'
#' @return Daily fecal nitrogen excretion (g).
#' @export
#'
#' @examples
#' dry_fecal_n_excretion(nitrogen_intake = 230)
#'
dry_fecal_n_excretion <- function(nitrogen_intake) {

  dry_fecal_n_excretion <- 0.345 + 0.317 * nitrogen_intake

  return(dry_fecal_n_excretion)

}

#' Estimates the daily total nitrogen excretion of dry cows.
#'
#' @param nitrogen_intake Nitrogen intake (g/day).
#'
#' @return Daily total nitrogen excretion (g).
#' @export
#'
#' @examples
#' dry_n_total_excretion(nitrogen_intake = 230)
#'
dry_n_total_excretion <- function(nitrogen_intake) {

  dry_n_total_excretion <- 0.345 + 0.317 * nitrogen_intake

  return(dry_n_total_excretion)

}







