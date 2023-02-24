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

    fluidRow(
    bs4Dash::bs4Card(
      width = 12,
      title = "Crops ",
      elevation = 2,
      solidHeader = TRUE,
      status = "teal",
      collapsed = FALSE,
      fluidRow(
        column(3,
               selectInput(ns("number_crops"), label = "Homegrown Crops:", choices = seq(1, 10, 1), selected = 4))),
      fluidRow(
        column(12,
               uiOutput(ns("crop_types")))))),

    bs4Dash::bs4Card(
      width = 12,
      title = "Crops ",
      elevation = 2,
      solidHeader = TRUE,
      status = "teal",
      collapsed = FALSE,
      fluidRow(
        bs4Dash::valueBoxOutput(ns("lime_urea_co2")),
        bs4Dash::valueBoxOutput(ns("fert_nh3")),
        bs4Dash::valueBoxOutput(ns("total_nitrous_oxide")),
        tableOutput(ns("teste"))
      )

    #tableOutput(ns("df")),

    )


  )
}

#' crop Server Functions
#'
#' @noRd
mod_crop_server <- function(id, animal_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$crop_types <- renderUI({

      evens <- function(x) subset(x, x %% 2 == 0)

      vector_crops <- seq(1, input$number_crops)

      fluidRow(
        column(6,
               purrr::map(vector_crops[!vector_crops %in% evens(vector_crops)],
                          ~bs4Dash::bs4Card(width = 12,
                                            title = paste0("Crop: ", .x),
                                            elevation = 2,
                                            solidHeader = TRUE,
                                            status = "teal",
                                            collapsed = FALSE,
                                            crop_ui_prms(ns(.x))))),
        column(6,
               purrr::map(evens(vector_crops),
                          ~bs4Dash::bs4Card(width = 12,
                                            title = paste0("Crop: ", .x),
                                            elevation = 2,
                                            solidHeader = TRUE,
                                            status = "teal",
                                            collapsed = FALSE,
                                            crop_ui_prms(ns(.x)))))
      )

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
                             input[[paste0(i, "_manure_pct")]],
                             input[[paste0(i, "_total_n_applied")]],
                             input[[paste0(i, "_urea_pct_n")]],
                             input[[paste0(i, "_p2o5_applied")]],
                             input[[paste0(i, "_k2o_applied")]],
                             input[[paste0(i, "_lime_applied")]])

         valores <- rbind(valores, crop_values)

       }

        colnames(valores) <- c("crop_id", "crop_type", "area", "yield",  "manure_pct", "total_n_applied",
                               "urea_pct_applied", "phosphate_applied", "potash_applied",
                               "lime_applied")

        valores

    })

    crop_calculations <- reactive({

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
        dplyr::mutate_at(c(3:10), as.numeric) %>%
        dplyr::mutate(
          co2_lime = lime_decomposition_co2(lime_applied = lime_applied * 1000) * area,
          co2_urea = urea_decomposition_co2(urea_applied = (urea_pct_applied / 100 * total_n_applied)) * area,
          nh3_n_fertilizer = fertilizer_nh3(nitrogen_applied = total_n_applied) * area,
          nitrous_oxide = fert_manure_nitrous_oxide(nitrogen_applied = total_n_applied) * area # TODO! missing manure N
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

    output$teste <- renderTable({

    crop_calculations()

    })

    # Total CO2 emitted from lime and urea

    output$lime_urea_co2 <- bs4Dash::renderValueBox({

      total_co2 <- crop_calculations() %>%
        dplyr::mutate(
          total_co2 = co2_urea + co2_lime
        ) %>%
        dplyr::summarise(
          total_co2 = sum(total_co2)
        ) %>%
        dplyr::pull(total_co2)

      value_box_spark(
        value    = round(total_co2, 1),
        title    = "Total CO2 Emissions from Lime and Urea (kg/year)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = icon("fa-solid fa-fire-flame-simple", verify_fa = FALSE),
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    # Total Ammonia emitted from N fertilzer

    output$fert_nh3 <- bs4Dash::renderValueBox({

      total_nh3 <- crop_calculations() %>%
        dplyr::summarise(
          total_nh3 = sum(nh3_n_fertilizer)
        ) %>%
        dplyr::pull(total_nh3)

      value_box_spark(
        value    = round(total_nh3, 1),
        title    = "Total Ammonia emissions from N Fertizers (kg/year)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = icon("fa-solid fa-fire-flame-simple", verify_fa = FALSE),
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    # Total Nitrous oxide emitted from N fertilzer + Manure

    output$total_nitrous_oxide <- bs4Dash::renderValueBox({

      total_nitrous_oxide <- crop_calculations() %>%
        dplyr::summarise(
          total_nh3 = sum(nh3_n_fertilizer),
          total_nitrous_oxide = sum(nitrous_oxide)
        ) %>%
        dplyr::mutate(
          total_nitrous_oxide = total_nitrous_oxide + 0.01 * total_nh3
        ) %>%
        dplyr::pull(total_nitrous_oxide)

      value_box_spark(
        value    = round(total_nitrous_oxide, 1),
        title    = "Total Nitrous Oxide emissions from N Fertizers (Missing Manure) (kg/year)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = icon("fa-solid fa-fire-flame-simple", verify_fa = FALSE),
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })


  })
}
