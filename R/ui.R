#' Diet UI parameters.
#'
#' @param category Animal categories: lactation (lac), dry (dry), and heifers(hei).
#' @param cp Diet crude protein (\%).
#' @param ndf Diet neutral detergent fiber (\%).
#' @param adf Diet acid detergent fiber (\%).
#' @param ee Diet ether extract (\%).
#' @param p Diet phosphorous content (\%).
#' @param k Diet potassium content (\%).
#'
#' @return Diet UI parameters.
#'
diet_ui_prms <- function(category = "diet_lac", cp = 16, ndf = 25, adf = 15, ee = 4, p = 0.4, k = 1.4) {

  diet_ui_prms <- list(
    column(2,
           numericInput(inputId = paste0(category, "_cp"),  label = "Crude Protein (%):", value = cp,    min = 0,   max = 60)),
    column(2,
           numericInput(inputId = paste0(category, "_ndf"), label = "NDF (%):",           value = ndf,   min = 0,   max = 60)),
    column(2,
           numericInput(inputId = paste0(category, "_adf"), label = "ADF (%):",           value = adf,   min = 0,   max = 60)),
    column(2,
           numericInput(inputId = paste0(category, "_ee"),  label = "EE (%):",            value = ee,    min = 0,   max = 60)),
    column(2,
           numericInput(inputId = paste0(category, "_p"),   label = "Phosphorous (%):",   value = p,     min = 0,   max = 60)),
    column(2,
           numericInput(inputId = paste0(category, "_k"),   label = "Potassium (%):",     value = p,     min = 0,   max = 60))
  )

  return(diet_ui_prms)

}
