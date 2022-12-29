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


#' Crop UI parameters.
#'
#' @param id Crop id.
#'
#' @return Crop UI parameters.
#'
crop_ui_prms <- function(id) {

  crop_list <- list("Corn silage (ton)",
                    "Corn grain (bu)",
                    "Alfalfa silage (ton)",
                    "Soybean seed (bu)",
                    "Barley rolled (bu)",
                    "Wheat rolled (bu)",
                    "Sorghum grain (bu)",
                    "Oat grain (bu)")

  crop_ui_prms <- tagList(

    wellPanel(

      fluidRow(

        h4(strong("Crop types and Yield"), align = "center"),

        column(4,
               selectInput(inputId  = paste0(id, "_crop_type"), label = paste0("Crop : ", id), choices = crop_list, selected = crop_list[[1]])),
        column(4,
               numericInput(inputId = paste0(id, "_area"), label = "Area (ha):", value = 15, min = 0, max = 1000)),
        column(4,
               numericInput(inputId = paste0(id, "_yield"), label = "Yield (ton./hectare):", value = 62, min = 50, max = 80))),

      fluidRow(

        h4(strong("fertilizers"), align = "center"),

        column(2,
               numericInput(inputId = paste0(id, "_total_n_applied"), label = "Total Nitrogen Applied (kg/ha):", value  = 60, min = 0, max = 500)),
        column(2,
               numericInput(inputId = paste0(id, "_urea_pct_n"), label = "Urea Applied (% of Total Nitrogen):", value = 50, min = 0, max  = 100)),
        column(3,
               numericInput(inputId = paste0(id, "_p2o5_applied"), label = "Total P2O5 Applied (kg/ha):", value = 100, min = 0, max = 500)),
        column(3,
               numericInput(inputId = paste0(id, "_k2o_applied"), label = "Total K2O Applied (kg/ha):", value = 100, min = 0, max = 500)),
        column(2,
               numericInput(inputId = paste0(id, "_lime_applied"), label = "Lime applied (ton./hectare):", value = 2, min = 0, max = 10))),

      fluidRow(

        h4(strong("Manure"), align = "center"),

        column(4,
               selectInput(inputId  = paste0(id, "_application_method"), label = "Application method:", choices = c("Direct injection", "Irrigation", "Broadcast spreading"), selected = "Broadcast spreading")),
        column(4,
               numericInput(inputId = paste0(id, "_manure_pct"), label = "Manure Applied (% of Total Manure):", value = 20, min = 0, max = 100)
        )

      )

    ))

  return(crop_ui_prms)

}
