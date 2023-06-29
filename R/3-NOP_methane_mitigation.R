#' 3-NOP methane mitigation.
#'
#' @param nop_dose 3-NOP dose (mg/DM).
#' @param diet_ndf Diet NDF content (\%).
#' @param diet_crude_fat Diet ether extract content (\%).
#'
#' @return Expected CH4 mitigated after 3-NOP inclusion in the diet (\%).
#' @seealso The equation used here is also available in \href{https://www.journalofdairyscience.org/article/S0022-0302(22)00710-X/pdf}{Kebreab et al. (2022)}.
#' @export
#'
#' @examples
#'enteric_ch4_reduction_3nop(nop_dose = 70, diet_ndf = 33, diet_crude_fat = 4.2)
#'

enteric_ch4_reduction_3nop <- function(nop_dose, diet_ndf, diet_crude_fat) {

  enteric_ch4_reduction_3nop <- -32.4 - 0.282 * (nop_dose - 70.5) + 0.915 * (diet_ndf - 32.9) + 3.08 * (diet_crude_fat - 4.2)

  return(enteric_ch4_reduction_3nop)

}

# expand.grid(ndf = seq(25, 40, 1), crude_fat = seq(3, 7, 1)) %>%
#   dplyr::mutate(methane_red = enteric_ch4_reduction_3nop(70, ndf, crude_fat)) %>%
#   ggplot2::ggplot( ggplot2::aes(x = ndf, methane_red, col = as.factor(crude_fat))) +
#   ggplot2::geom_line() +
#   ggplot2::facet_wrap(~paste("Crude fat = ", crude_fat, "%")) +
#   ggplot2::xlab("NDF (%)") +
#   ggplot2::ylab("Methane reduction (%)") +
#   ggplot2::theme_bw() +
#   ggplot2::theme(legend.position = "none")







