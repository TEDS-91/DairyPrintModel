#' fuel_combustion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fuel_combustion_ui <- function(id){
  ns <- NS(id)
  tagList(

    h3(strong("Fuel Combustion"), align = "center"),

    fluidPage(

      column(4,
             numericInput(ns("gasoline"), label = "Gasoline Consumption (L/day):", value = 5, min = 0, max = 1000)),
      column(4,
             numericInput(ns("natural_gas"), label = "Natural Gas Consumption (L/day):", value = 5, min = 0, max = 1000)),
      column(4,
             numericInput(ns("diesel"), label = "Diesel Consumption (L/day):", value = 5, min = 0, max = 1000))
    ),

    tableOutput(ns("tabela"))

  )
}

#' fuel_combustion Server Functions
#'
#' @noRd
mod_fuel_combustion_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    tabela_calc <- reactive({

      tabela <- tibble::tibble(

        gasoline_co2eq = gasoline_co2eq(input$gasoline),
        natural_gas_co2eq = natural_gas_co2eq(input$natural_gas),
        diesel_co2eq = diesel_co2eq(input$diesel)
      )

      tabela

    })

    output$tabela <- renderTable({

      tabela_calc()

    })

  })
}
