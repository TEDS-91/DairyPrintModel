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

#' Estimates the milk urea nitrogen (MUN).
#'
#' @param nitrogen_intake Nitrogen intake (g).
#'
#' @return Milk urea nitrogen (MUN) (g).
#' @seealso The equation used here is also available in \href{https://reader.elsevier.com/reader/sd/pii/S0022030215000259?token=7ECD53F63381FD64DFC3874D7B4B1BA42FF5330C206BC6F9EB59882C56F3093C1D8D1DB57FE3D563070977EDBFA8F7CC&originRegion=us-east-1&originCreation=20221102190050}{Aguirre-Villegas et al. (2015)}.
#' @export
#'
#' @examples
#' milk_urea_nitrogen(nitrogen_intake = 640)
#'
milk_urea_nitrogen <- function(nitrogen_intake) {

  milk_urea_nitrogen <- 0.0148 * nitrogen_intake + 2.16

  return(milk_urea_nitrogen)

}

#' Estimates the daily fecal nitrogen excretion of dry cows.
#'
#' @param nitrogen_intake Nitrogen intake (g/day).
#'
#' @return Daily fecal nitrogen excretion (g).
#' @seealso The equations used here are also available in \href{https://www.journalofdairyscience.org/article/S0022-0302(15)00147-2/fulltext}{Reed et al. (2015)}.
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
#' @seealso The equations used here are also available in \href{https://www.journalofdairyscience.org/article/S0022-0302(15)00147-2/fulltext}{Reed et al. (2015)}.
#' @export
#'
#' @examples
#' dry_n_total_excretion(nitrogen_intake = 230)
#'
dry_n_total_excretion <- function(nitrogen_intake) {

  dry_n_total_excretion <- 15.1 + 0.828 * nitrogen_intake

  return(dry_n_total_excretion)

}

#' Estimates the daily fecal nitrogen excretion of heifers.
#'
#' @param nitrogen_intake Nitrogen intake (g/day).
#'
#' @return Daily fecal nitrogen excretion (g).
#' @seealso The equations used here are also available in \href{https://www.journalofdairyscience.org/article/S0022-0302(15)00147-2/fulltext}{Reed et al. (2015)}.
#' @export
#'
#' @examples
#' heifer_n_fecal_excretion(nitrogen_intake = 120)
#'
heifer_n_fecal_excretion <- function(nitrogen_intake) {

  heifer_n_fecal_excretion <- 0.345 + 0.317 * nitrogen_intake

  return(heifer_n_fecal_excretion)

}

#' Estimates the daily total nitrogen excretion of heifers.
#'
#' @param nitrogen_intake Nitrogen intake (g/day).
#'
#' @return Daily total nitrogen excretion (g).
#' @seealso The equations used here are also available in \href{https://www.journalofdairyscience.org/article/S0022-0302(15)00147-2/fulltext}{Reed et al. (2015)}.
#' @export
#'
#' @examples
#' heifer_n_total_excretion(nitrogen_intake = 230)
#'
heifer_n_total_excretion <- function(nitrogen_intake) {

  heifer_n_total_excretion <- 15.1 + 0.828 * nitrogen_intake

  return(heifer_n_total_excretion)

}








