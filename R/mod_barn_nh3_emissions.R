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

    tableOutput(ns("tabela"))#,
    #plotOutput(ns("grafico"))

  )
}

#' barn_nh3_amissions Server Functions
#'
#' @noRd
mod_barn_nh3_emissions_server <- function(id, county, facilitie){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    nh3_emissions <- reactive({

      facilitie <- facilitie()

      county <- county()

       urine_vol <- 21

       facilitie1 <- ifelse(facilitie == "freestall", 3.5, 1.5)

       manure_solution_mass <- urine_vol / facilitie1

       manure_solution_pH <- 7.7

       gamma_densi <- 1000 # manure density

       const <- 86400 # time conversion

      nh3_emissions <- wi_weather %>%
        dplyr::filter(county == county()) %>%
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

      #nh3_emissions()

    })

 #   output$grafico <- renderPlot({
#
#       nh3 <- tibble::as_tibble(nh3_emissions())
#
#       nh3 %>%
#       ggplot2::ggplot( ggplot2::aes(x = yday, y = loss_animal_kg)) +
#         ggplot2::theme_bw() +
#         ggplot2::geom_point() +
#         ggplot2::xlab("Yaer Days") +
#         ggplot2::ylab("Daily Amonia Emissions")

   # })

  })
}

## To be copied in the UI
# mod_barn_nh3_emissions_ui("barn_nh3_emissions")

## To be copied in the server
# mod_barn_nh3_emissions_server("barn_nh3_emissions")
