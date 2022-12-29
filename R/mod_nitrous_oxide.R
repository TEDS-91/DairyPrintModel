#' nitrous_oxide_emissions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_nitrous_oxide_emissions_ui <- function(id){
  ns <- NS(id)
  tagList(

    h2(textOutput(ns("tabela")))

  )
}

#' nitrous_oxide Server Functions
#'
#' @noRd
mod_nitrous_oxide_emissions_server <- function(id,
                                               manure_storage_area,
                                               enclosed_manure){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    nitrous_oxide <- reactive({

      manure_storage_area <- manure_storage_area()

      enclosed_manure <- enclosed_manure()

      daily_emissions <- dplyr::if_else(enclosed_manure == "yes", 0.16 * manure_storage_area / 1000, 0.8 * manure_storage_area / 1000)

      yday <- seq(1, 365)

      cum_emissions <- yday * daily_emissions


      nitrous_oxide <- tibble::tibble(

        yday,

        daily_emissions,

        cum_emissions


      )



    })

    output$tabela <- renderText({

      # paste("The total anual emissions of Nitrous Oxide from manure storage is", nitrous_oxide()[3] %>%
      #  sum(), "kg")

    })




  })
}

## To be copied in the UI
# mod_nitrous_oxide_emissions_ui("nitrous_oxide")

## To be copied in the server
# mod_nitrous_oxide_emissions_server("nitrous_oxide")
