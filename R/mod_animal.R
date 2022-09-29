#' animal UI Function
#'
#' @description This is the animal module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_animal_ui <- function(id){
  ns <- NS(id)
  tagList(

    numericInput(ns("n_cows"),              "Number of cows:",            value = 120, min = 0,  max = 30000),
    numericInput(ns("cow_calving_int"),     "Calving Interval (mo):",     value = 14,  min = 10, max = 20),
    numericInput(ns("cow_rep_rate"),        "Cow Culling Rate (%):",      value = 30,  min = 20, max = 50),
    numericInput(ns("calves_heifers_cul"),  "Heifers Culling Rate (%):",  value = 20,  min = 10, max = 50),
    numericInput(ns("stillbirth_rate"),     "Stillbirth Rate (%):",       value = 7,   min = 0,  max = 15),
    numericInput(ns("heifer_calf_born"),    "Percentage Heifers (%):",    value = 50,  min = 45, max = 55),
    numericInput(ns("time_first_calv"),     "First Calving (mo):",        value = 24,  min = 22, max = 36),

    plotOutput(ns("grafico"))

  )
}

#' animal Server Functions
#'
#' @noRd
mod_animal_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$grafico <- renderPlot({

      graphics::hist(stats::rnorm(input$n_cows))

    })
  })
}

## To be copied in the UI
# mod_animal_ui("animal_1")

## To be copied in the server
# mod_animal_server("animal_1")
