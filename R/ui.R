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

#' general parameters used over the application by other modules.
#'
#' @return UI elements.
#' @export
#'
general_ui_prms <- function() {

  general_ui_prms <- list(

    fluidRow(

      column(3,
             selectInput("facilitie",       label = "Facilitie",                 choices = c("freestall", "tie-stall"), selected = "tie-stall")),
      column(3,
             selectInput("county",          label = "County",                    choices = unique(wi_weather$county), selected = "Adams")),
      column(3,
             selectInput("bedding_type",    label = "Bedding type",              choices = c("Sand", "Sawdust", "Chopped straw"), selected = "Sawdust")),
      column(3,
             selectInput("biodigester",     label = "Biodigester",               choices = c("yes", "no"), selected = "yes"))),

    fluidRow(

      column(2,
             numericInput("biod_ef",        label = "Biodigester efficiency:",  value = 30)),
      column(2,
             selectInput("solid_liquid",    label = "Solid-liquid separation:",  choices = c("yes", "no"), selected = "yes")),
      column(2,
             selectInput("type_manure",     label = "Manure",                    choices = c("slurry", "solid"), selected = "solid")),
      column(2,
             selectInput("enclosed_manure", label = "Enclosed manure store:",    choices = c("yes", "no"), selected = "no")),
      column(2,
             selectInput("empty",           label = "Empty time:",               choices = c("Fall", "Spring", "Fall and Spring"), selected = "Fall")),
      column(2,
             selectInput("crust",           label = "Crust formation:",          choices = c("yes", "no"), selected = "no")),
      column(2,
             numericInput("storage_area",   label = "Manure Storage Area (m2):", value = 200))
    )

  )

  return(general_ui_prms)

}

#' Animal UI parameters.
#'
#' @param prm Parameter to be concatenated in the module.
#'
#' @return Animal UI components.
#'
animal_ui_prms <- function(prm) {

  animal_ui_prms <- list(

    fluidRow(

      column(3,
             numericInput(paste0(prm, "_n_cows"),              "Number of cows:",            value = 120, min = 0,  max = 30000)),
      column(3,
             numericInput(paste0(prm, "_cow_calving_int"),     "Calving Interval (mo):",     value = 14,  min = 10, max = 20)),
      column(3,
             numericInput(paste0(prm, "_cow_rep_rate"),        "Cow Culling Rate (%):",      value = 30,  min = 20, max = 50)),
      column(3,
             numericInput(paste0(prm, "_time_first_calv"),     "First Calving (mo):",        value = 24,  min = 18, max = 36))),

    fluidRow(

      column(3,
             numericInput(paste0(prm, "_calves_heifers_cul"),  "Heifers Culling Rate (%):",  value = 20,  min = 10, max = 50)),
      column(2,
             numericInput(paste0(prm, "_stillbirth_rate"),     "Stillbirth Rate (%):",       value = 7,   min = 0,  max = 15)),
      column(2,
             numericInput(paste0(prm, "_heifer_calf_born"),    "Percentage Heifers (%):",    value = 50,  min = 45, max = 55)),
      column(2,
             numericInput(paste0(prm, "_average_milk_yield"),  "Average Milk Yield (kg):",   value = 40,  min = 30, max = 60)),
      column(3,
             selectInput(paste0(prm, "_milk_freq"), "Milk Frequence:", choices = c(2, 3))))
  )

  return(animal_ui_prms)

}

#' Calf UI parameters.
#'
#' @param prm Parameter to be concatenated in the module.
#'
#' @return Calf UI components.
#'
calf_ui_prms <- function(prm) {

  calf_ui_prms <- list(

    fluidRow(

      column(3,
             numericInput(paste0(prm, "_milk_sup"),   "Milk Suply (l/d):", value = 6,   min = 2, max = 15)),
      column(2,
             numericInput(paste0(prm, "_starter_cp"),  "Starter Crude Protein (%):", value = 20,   min = 15, max = 30)),
      column(2,
             numericInput(paste0(prm, "_starter_ndf"), "Starter P (%):", value = 20,   min = 15, max = 30)),
      column(2,
             numericInput(paste0(prm, "_forage_cp"),   "Forage Crude Protein (%):", value = 20,   min = 15, max = 30)),
      column(3,
             numericInput(paste0(prm, "_forage_ndf"),  "Forage P (%):", value = 20,   min = 15, max = 30))
    )

  )

  return(calf_ui_prms)

}

#' Milk UI parameters.
#'
#' @param prm Parameter to be concatenated in the module.
#'
#' @return Milk UI components.
#'
milk_ui_prms <- function(prm) {

  milk_ui_prms <- list(

    fluidRow(

      column(12,
             h3(strong("Milk composition"), align = "center")),
      column(6,
             numericInput(paste0(prm, "_protein"),  "Milk Protein (%):",         value = 3.25, min = 2, max = 5)),
      column(6,
             numericInput(paste0(prm, "_fat"),  "Milk Fat (%):",                 value = 3.50,  min = 2, max = 5)))
  )

  return(milk_ui_prms)
}

#' Crop UI parameters.
#'
#' @param id Crop id.
#'
#' @return Crop UI components.
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
