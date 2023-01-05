#' crop UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_crop_ui <- function(id){
  ns <- NS(id)
  tagList(

    h3(strong("Crops"), align = "center"),

    selectInput(ns("number_crops"), label = "Number of Crops:", choices = seq(1, 10, 1), selected = 5),

    uiOutput(ns("crop_types")),

    tableOutput(ns("df")),

    tableOutput(ns("teste"))


  )
}

#' crop Server Functions
#'
#' @noRd
mod_crop_server <- function(id, animal_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$crop_types <- renderUI({

      purrr::map(1:input$number_crops, ~ crop_ui_prms(ns(.x)))

    })

    output$df <- renderTable({
      animal_data()
    })


    valores <- reactive({

      req(input[["1_crop_type"]])

      valores <- data.frame()

      for ( i in 1:input$number_crops) {

        crop_values <- cbind(i,
                             input[[paste0(i, "_crop_type")]],
                             input[[paste0(i, "_area")]],
                             input[[paste0(i, "_yield")]],
                             input[[paste0(i, "_total_n_applied")]],
                             input[[paste0(i, "_urea_pct_n")]],
                             input[[paste0(i, "_p2o5_applied")]],
                             input[[paste0(i, "_k2o_applied")]],
                             input[[paste0(i, "_lime_applied")]],
                             input[[paste0(i, "_application_method")]],
                             input[[paste0(i, "_manure_pct")]])

         valores <- rbind(valores, crop_values)

       }

        colnames(valores) <- c("crop_id", "crop_type", "area", "yield", "total_n_applied",
                               "urea_pct_applied", "phosphate_applied", "potash_applied",
                               "lime_applied", "application_method", "manure_pct")

        valores

    })


    output$teste <- renderTable({

      crop_list <- list("Corn silage (ton)",
                        "Corn grain (bu)",
                        "Alfalfa silage (ton)",
                        "Soybean seed (bu)",
                        "Barley rolled (bu)",
                        "Wheat rolled (bu)",
                        "Sorghum grain (bu)",
                        "Oat grain (bu)")


      crop_nutrient_removal_n_fix <- tibble::tribble(
        ~crop,                  ~n_removal,  ~p_removal, ~k_removal, ~nitrogen_fix,
        "Corn silage (ton)",     4.4,         1.41,        3.31,     0,
        "Corn grain (bu)",       0.3,         0.16,        0.11,     0,
        "Alfalfa silage (ton)",  23.13,       5.44,        22.22,    16.16,
        "Soybean seed (bu)",     1.5,         0.33,        0.54,     0.89,
        "Barley rolled (bu)",    0.45,        0.18,        0.15,     0,
        "Wheat rolled (bu)",     0.63,        0.23,        0.14,     0,
        "Sorghum grain (bu)",    0.30,        0.18,        0.12,     0,
        "Oat grain (bu)",        0.35,        0.13,        0.09,     0
      )


      valores <- valores()

       valores <- valores %>%
         dplyr::mutate_at(c(3:9, 11), as.numeric) %>%
         dplyr::mutate(
           co2_lime = lime_decomposition_co2(lime_applied = lime_applied * 1000),
           co2_urea = urea_decomposition_co2(urea_applied = (urea_pct_applied / 100 * total_n_applied))
          )

       valores %>%
         dplyr::inner_join(crop_nutrient_removal_n_fix, by = c("crop_type" = "crop")) %>%
         dplyr::mutate(
           total_n_removal = n_removal * yield,
           total_p_removal = p_removal * yield / 2.29,
           total_k_removal = k_removal * yield / 1.21,
           total_n_fixed   = nitrogen_fix * yield,
           crop_n_balance  = total_n_applied - total_n_removal + total_n_fixed
         )




    })

  })
}
