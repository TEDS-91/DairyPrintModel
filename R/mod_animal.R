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
    numericInput(ns("time_first_calv"),     "First Calving (mo):",        value = 24,  min = 18, max = 36),

    tableOutput(ns("herd_stab")),
    tableOutput(ns("herd_stab2"))

  )
}

#' animal Server Functions
#'
#' @noRd
mod_animal_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    herd_matrix <- reactive({

      herd_matrix <- herd_stabilization(time_first_calv    = input$time_first_calv,
                                        heifer_calf_born   = input$heifer_calf_born,
                                        stillbirth_rate    = input$stillbirth_rate,
                                        calves_heifers_cul = input$calves_heifers_cul,
                                        cow_rep_rate       = input$cow_rep_rate,
                                        cow_calving_int    = input$cow_calving_int,
                                        n_adult_cows       = input$n_cows)

    })

    herd_matrix2 <- reactive({

      herd_matrix2 <- herd_projection(time_first_calv    = input$time_first_calv,
                                      heifer_calf_born   = input$heifer_calf_born,
                                      stillbirth_rate    = input$stillbirth_rate,
                                      calves_heifers_cul = input$calves_heifers_cul,
                                      cow_rep_rate       = input$cow_rep_rate,
                                      cow_calving_int    = input$cow_calving_int,
                                      n_adult_cows       = input$n_cows,
                                      months_to_project  = 12)

    })

    output$herd_stab <- renderTable({

      length(herd_matrix())

    })

    output$herd_stab2 <- renderTable({

      herd_matrix2()[1]

    })



  })
}

## To be copied in the UI
# mod_animal_ui("animal_1")

## To be copied in the server
# mod_animal_server("animal_1")
