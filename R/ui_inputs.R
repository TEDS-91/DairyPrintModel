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

    fluidRow(

    column(4,
           numericInput(inputId = paste0(category, "_cp"),  label = "CP (%):",  value = cp,    min = 0,   max = 60)),
    column(4,
           numericInput(inputId = paste0(category, "_ndf"), label = "NDF (%):", value = ndf,   min = 0,   max = 60)),
    column(4,
           numericInput(inputId = paste0(category, "_adf"), label = "ADF (%):", value = adf,   min = 0,   max = 60)),
    column(4,
           numericInput(inputId = paste0(category, "_ee"),  label = "EE (%):",  value = ee,    min = 0,   max = 60)),
    column(4,
           numericInput(inputId = paste0(category, "_p"),   label = "P (%):",   value = p,     min = 0,   max = 60)),
    column(4,
           numericInput(inputId = paste0(category, "_k"),   label = "K (%):",   value = k,     min = 0,   max = 60)))
  )

  return(diet_ui_prms)

}

#' Manure parameters used over the application by other modules.
#'
#' @return UI elements.
#' @export
#'
manure_ui_prms <- function() {

  manure_ui_prms <- list(

      column(2,
             selectInput("county",          label = "County:",                   choices = unique(wisconsin_weather_data$county), selected = "Dane")),

      column(2,
             selectInput("facilitie",       label = "Facility:",                 choices = c("freestall",
                                                                                             "tie-stall"), selected = "freestall")),
      column(2,
             selectInput("bedding_type",    label = "Bedding Type:",             choices = c("Sand",
                                                                                             "Sawdust",
                                                                                             "Chopped straw"), selected = "Chopped straw")),
      column(2,
             selectInput("type_manure",     label = "Manure:",                   choices = c("Solid",
                                                                                             "Semi-solid",
                                                                                             "Slurry",
                                                                                             "Liquid"), selected = "Slurry")),
      column(3,
             selectInput("manure_manag",    label = "Manure Management:",        choices = c("Daily Hauling",
                                                                                             "Pond or Tank Storage",
                                                                                             "Biodigester",
                                                                                             "Biodigester + Solid-liquid Separator"), selected = "Pond or Tank Storage"))

  )

  return(manure_ui_prms)

}

#' Manure management parameters used over the application by other modules.
#'
#' @param manure_managment Type of manure management.
#' @param type_manure Type of manure (solid, semi-solid, slurry or liquid).
#' @return UI elements.
#' @export
#'

manure_manag_ui <- function(manure_managment, type_manure) {

  manure_manag_ui <- list(

    fluidRow(

    if (manure_managment == "Biodigester + Solid-liquid Separator") {

      fluidRow(
        column(3,
               numericInput("biod_ef",        label = "Biodigester Efficiency:",  value = 25)),
        column(3,
               numericInput("storage_area",   label = "Manure Storage Area:",     value = 1360)),
        column(3,
               selectInput("empty",           label = "Emptying Time:",           choices = c("Fall", "Spring", "Fall and Spring"), selected = "Fall and Spring")),
        column(3,
               selectInput("application",     label = "Manure Application:",      choices = c("Broadcast spreading",
                                                                                              "Injected",
                                                                                              "Irrigation"), selected = "Broadcast spreading")))

    } else if (manure_managment == "Biodigester") {

      fluidRow(
        column(3,
               numericInput("biod_ef",        label = "Biodigester Efficiency:",  value = 25)),
        column(3,
               numericInput("storage_area",   label = "Manure Storage Area:",     value = 1360)),
        column(3,
               selectInput("empty",           label = "Emptying Time:",           choices = c("Fall",
                                                                                              "Spring",
                                                                                              "Fall and Spring"), selected = "Fall and Spring")),
        column(3,
               selectInput("application",     label = "Manure Application:",      choices = c("Broadcast spreading",
                                                                                              "Injected",
                                                                                              "Irrigation"), selected = "Broadcast spreading")))

    } else if (manure_managment == "Daily Hauling") {

    } else if (manure_managment == "Pond or Tank Storage") {

      if (type_manure == "Liquid" | type_manure == "Slurry") {

        fluidRow(
          column(3,
                 selectInput("empty",           label = "Emptying Time:",           choices = c("Fall",
                                                                                                "Spring",
                                                                                                "Fall and Spring"), selected = "Fall and Spring")),
          column(3,
                 numericInput("storage_area",   label = "Manure Storage Area:",     value = 1360)),
          column(3,
                 selectInput("crust",           label = "Crust Formation:",         choices = c("yes", "no"), selected = "no")),
          column(3,
                 selectInput("enclosed_manure", label = "Enclosed Manure:",         choices = c("yes", "no"), selected = "no")),
          column(3,
                 selectInput("application",     label = "Manure Application:",      choices = c("Broadcast spreading",
                                                                                                "Injected",
                                                                                                "Irrigation"), selected = "Broadcast spreading")))

      } else {

        fluidRow(
          column(6,
                 selectInput("empty",           label = "Emptying Time:",           choices = c("Fall",
                                                                                                "Spring",
                                                                                                "Fall and Spring"), selected = "Fall and Spring")),

          column(6,
                 selectInput("application",     label = "Manure Application:",      choices = c("Broadcast spreading",
                                                                                                "Injected",
                                                                                                "Irrigation"), selected = "Broadcast spreading")))

      }

    }

  )
)

}

#' Animal UI parameters.
#'
#' @param prm Parameter to be concatenated in the module.
#'
#' @return Animal UI components.
#'
animal_ui_prms <- function(prm) {

  animal_ui_prms <- list(

      column(3,
             numericInput(paste0(prm, "_n_cows"),               label = "Total Cows:",                value = 150, min = 0,  max = 30000)),
      column(3,
             numericInput(paste0(prm, "_cow_calving_int"),      label = "Calving Interval (mo):",     value = 15, min = 12, max = 20)),
      column(3,
             numericInput(paste0(prm, "_cow_rep_rate"),         label = "Cow Culling Rate (%):",      value = 35,  min = 20, max = 50)),
      column(3,
             numericInput(paste0(prm, "_time_first_calv"),      label = "First Calving (mo):",        value = 24, min = 0, max = 36)),
      column(3,
             numericInput(paste0(prm, "_calves_heifers_cul"),   label = "Heifers Culling Rate (%):",  value = 5,  min = 10, max = 50)),
      column(3,
             numericInput(paste0(prm, "_heifer_calf_born"),     label = "Female Calf Born (%):",      value = 50,  min = 45, max = 55)),
      column(3,
             numericInput(paste0(prm, "_average_milk_yield"),   label = "Milk Yield (kg/cow):",       value = 40,  min = 30, max = 60)),
      column(3,
             numericInput(paste0(prm, "_mature_weight"),        label = "Mature Weight (kg):",        value = 680.0,  min = 500, max = 1000)),
      column(3,
             selectInput(paste0(prm, "_milk_freq"),             label = "Milking Freq.:",             choices = c(2, 3), selected = 3))
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

      column(3,
             numericInput(paste0(prm, "_birth_weight"),  label = "Birth Weight (kg):",     value = 40,   min = 20, max = 65)),
      column(3,
             numericInput(paste0(prm, "_milk_sup"),      label = "Milk Suply (l/d):",      value = 6,    min = 2,  max = 15)),
      column(3,
             numericInput(paste0(prm, "_protein"),       label = "Milk Protein (%):",      value = 3.25, min = 2,  max = 5)),
      column(3,
             numericInput(paste0(prm, "_fat"),           label = "Milk Fat (%):",          value = 3.50, min = 2,  max = 5)),
      column(3,
             numericInput(paste0(prm, "_starter_cp"),    label = "Starter CP (%):",        value = 20,   min = 15, max = 30)),
      column(4,
             numericInput(paste0(prm, "_starter_p"),     label = "Starter P (%):",         value = 1,   min = 15, max = 30)),
      column(4,
             numericInput(paste0(prm, "_forage_cp"),     label = "Forage CP (%):",         value = 15,   min = 15, max = 30)),
      column(4,
             numericInput(paste0(prm, "_forage_p"),      label = "Forage P (%):",          value = 0.3,   min = 15, max = 30))
    )

  return(calf_ui_prms)

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

      h5(strong("Crop Type, Yield, and Manure Applied"), align = "center"),

      fluidRow(

        column(3,
               selectInput(inputId  = paste0(id, "_crop_type"),  label = paste0("Crop : "),             choices = crop_list, selected = crop_list[[1]])),
        column(3,
               numericInput(inputId = paste0(id, "_area"),       label = "Area (ha):",                  value = 35, min = 0, max = 1000)),
        column(3,
               numericInput(inputId = paste0(id, "_yield"),      label = "Yield (ton./hectare):",       value = 62, min = 50, max = 80)),
        column(3,
               numericInput(inputId = paste0(id, "_manure_pct"), label = "Manure (% of Total Manure):", value = 25, min = 0,  max = 100)
        )),

      h5(strong("Fertilizers and Lime Applied"), align = "center"),

      fluidRow(

        column(2,
               numericInput(inputId = paste0(id, "_total_n_applied"), label = "N (kg/ha):",           value  = 20, min = 0, max = 500)),
        column(3,
               numericInput(inputId = paste0(id, "_urea_pct_n"),      label = "Urea (% of Total N):", value = 50,  min = 0, max = 100)),
        column(3,
               numericInput(inputId = paste0(id, "_p2o5_applied"),    label = "P2O5 (kg/ha):",        value = 100, min = 0, max = 500)),
        column(2,
               numericInput(inputId = paste0(id, "_k2o_applied"),     label = "K2O (kg/ha):",         value = 100, min = 0, max = 500)),
        column(2,
               numericInput(inputId = paste0(id, "_lime_applied"),    label = "Lime (ton./ha):",      value = 0.3, min = 0, max = 10)))

      )

    )

  return(crop_ui_prms)

}
