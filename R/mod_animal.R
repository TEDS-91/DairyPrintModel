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

    wellPanel(

    h3(icon("fa-duotone fa-cow", verify_fa = FALSE), strong("Herd Information"), align = "center"),

    style = "background-color:#E2EBF4",

    fluidRow(
      column(3,
    numericInput(ns("n_cows"),              "Number of cows:",            value = 120, min = 0,  max = 30000)),
      column(3,
    numericInput(ns("cow_calving_int"),     "Calving Interval (mo):",     value = 14,  min = 10, max = 20)),
      column(3,
    numericInput(ns("cow_rep_rate"),        "Cow Culling Rate (%):",      value = 30,  min = 20, max = 50)),
      column(3,
    numericInput(ns("time_first_calv"),     "First Calving (mo):",        value = 24,  min = 18, max = 36))),

    fluidRow(
      column(3,
    numericInput(ns("calves_heifers_cul"),  "Heifers Culling Rate (%):",  value = 20,  min = 10, max = 50)),
      column(3,
    numericInput(ns("stillbirth_rate"),     "Stillbirth Rate (%):",       value = 7,   min = 0,  max = 15)),
      column(3,
    numericInput(ns("heifer_calf_born"),    "Percentage Heifers (%):",    value = 50,  min = 45, max = 55)),
      column(3,
    numericInput(ns("average_milk_yield"),  "Average Milk Yield (kg):",   value = 40,  min = 30, max = 60))),

    br(),

    fluidRow(
    column(6,
    h3(strong("Milk composition"), align = "center")),

    column(6,
    h3(strong("Diet Information"), align = "center")),

    br(),

    column(3,
    actionButton(ns("button"),
                 "Run!",
                 style = "color: #fff; background-color: #007582; border-color: #007582; height:40px; width:80px")))),

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

    herd_stab_matrix <- reactive({

      herd_stab_matrix <- herd_stabilization(time_first_calv    = input$time_first_calv,
                                             heifer_calf_born   = input$heifer_calf_born,
                                             stillbirth_rate    = input$stillbirth_rate,
                                             calves_heifers_cul = input$calves_heifers_cul,
                                             cow_rep_rate       = input$cow_rep_rate,
                                             cow_calving_int    = input$cow_calving_int,
                                             n_adult_cows       = input$n_cows)

    })

    herd_matrix <- reactive({

      herd_matrix <- herd_projection(time_first_calv    = input$time_first_calv,
                                     heifer_calf_born   = input$heifer_calf_born,
                                     stillbirth_rate    = input$stillbirth_rate,
                                     calves_heifers_cul = input$calves_heifers_cul,
                                     cow_rep_rate       = input$cow_rep_rate,
                                     cow_calving_int    = input$cow_calving_int,
                                     n_adult_cows       = input$n_cows,
                                     months_to_project  = 12)

    })

    df <- eventReactive(input$button, {

      # converting the herd_matrix to tibble
      herd_demographics_raw <- herd_matrix()[[1]] %>%
        tibble::as_tibble()

      df <- herd_demographics_raw %>%
        tibble::as_tibble() %>%
        dplyr::mutate(MonthSimulated = seq(1, dim(herd_demographics_raw)[1])) %>%
        dplyr::relocate(MonthSimulated, .before = HeiOpenGrowMonth1) %>%
        tidyr::pivot_longer(-MonthSimulated,
                            names_to = "GeneralCategories",
                            values_to = "NumberAnimals") %>%
        tidyr::separate(GeneralCategories, into = c("Categories", "Others"), 3) %>%
        tidyr::separate(Others, into = c("ReproductiveStatus", "Others"), 4) %>%
        tidyr::separate(Others, into = c("Phase", "MonthLac"), 4) %>%
        tidyr::separate(MonthLac, into = c("Garbage", "Month"), 5) %>%
        dplyr::select(-Garbage)

      df

    })



    output$herd_stab2 <- renderTable({


      df()

    })

    })

}

## To be copied in the UI
# mod_animal_ui("animal")

## To be copied in the server
# mod_animal_server("animal")
