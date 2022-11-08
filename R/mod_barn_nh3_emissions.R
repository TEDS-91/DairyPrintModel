#' barn_nh3_emissions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_barn_nh3_emissions_ui <- function(id){
  ns <- NS(id)
  tagList(

    selectInput(ns("county"), label = "Select the county", choices = unique(wi_wheather$county), selected = "Adams"),

    #selectInput(ns("facilitie"), label = "Select the facilitie", choices = c("freestall", "tie-stall"), selected = "tie-stall"),

    tableOutput(ns("tabela"))

  )
}

#' barn_nh3_amissions Server Functions
#'
#' @noRd
mod_barn_nh3_emissions_server <- function(id, facilitie){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    nh3_emissions <- reactive({

      facilitie <- facilitie()

       urine_vol <- 21

       facilitie1 <- ifelse(facilitie == "freestall", 3.5, 1.5)

       manure_solution_mass <- urine_vol / facilitie1

       manure_solution_pH <- 7.7

       gamma_densi <- 1000 # manure density

       const <- 86400 # time conversion


      nh3_emissions <- wi_wheather %>%
        dplyr::filter(county == input$county) %>%
        dplyr::mutate(
          Const = const,
          gama_densi = gamma_densi,
          M = manure_solution_mass,
          Q = eq_coeff(temp_c = aver_tempC, pH = manure_solution_pH),
          r = resistence_nh3(hsc = 260, temp_c = aver_tempC), # TODO
          tan_kg_m2 = 0.209 / facilitie1 , # Or calculate TAN TODO
          loss_kg_m2 = tan_kg_m2 * Const * gama_densi / (r * M * Q),
          loss_animal_kg = loss_kg_m2 * facilitie1,
          cum_loss = cumsum(loss_animal_kg)
         )

     nh3_emissions


    })

    output$tabela <- renderTable({

      nh3_emissions()

    })


  })
}

## To be copied in the UI
# mod_barn_nh3_emissions_ui("barn_nh3_emissions")

## To be copied in the server
# mod_barn_nh3_emissions_server("barn_nh3_emissions")
