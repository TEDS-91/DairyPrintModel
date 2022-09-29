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

    plotOutput(ns("grafico")),
    echarts4r::echarts4rOutput(ns("grafico2"))

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

    output$grafico2 <- echarts4r::renderEcharts4r({

      df <- data.frame(
        KPI = c("KPI_1", "KPI_2", "KPI_3", "KPI_4", "KPI_5"),
        FARM_1 = runif(5, 4, 6),
        FARM_2 = runif(5, 3, 7)
      )

      df |>
        echarts4r::e_charts(KPI) |>
        echarts4r::e_radar(FARM_1, max = 7, name = "Farm 2") |>
        echarts4r::e_radar(FARM_2, max = 7, name = "Farm 1") |>
        echarts4r::e_tooltip(trigger = "item")
    })

  })
}

## To be copied in the UI
# mod_animal_ui("animal_1")

## To be copied in the server
# mod_animal_server("animal_1")
