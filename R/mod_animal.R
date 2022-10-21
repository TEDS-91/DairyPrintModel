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
             #mod_milk_yield_optimization_ui(ns("milk_yield_optimization")),
    numericInput(ns("average_milk_yield"),  "Average Milk Yield (kg):",   value = 40,  min = 30, max = 60),
    selectInput(ns("milk_freq"), "Milk Frequence:", choices = c(2, 3))


             )),

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
                 style = "color: #fff; background-color: #007582; border-color: #007582; height:40px; width:80px")),

    column(9,
    downloadButton("rmd_report", "Report.html",
                   style = "color: #fff; background-color: #007582; border-color: #007582")))),

    textOutput(ns("lambda")),
    tableOutput(ns("herd_stab2")),
    plotOutput(ns("graphic"))

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

    lambda_milk_calc <- reactive({

      age_first_calv <- input$time_first_calv

      cow_calving_int <- input$cow_calving_int

      n_cows <- input$n_cows

      prop_primiparous <- sum(herd_matrix()[[1]][1, (age_first_calv + 1):((age_first_calv + 1) + cow_calving_int - 1)]) / n_cows

      prop_secondiparous <- sum(herd_matrix()[[1]][1, (age_first_calv + 1 + cow_calving_int):(age_first_calv + 1 + 2 * cow_calving_int - 1)]) / n_cows


      lambda_milk_calc <- lambda_milk_yield(obs_average_milk_yield = input$average_milk_yield,
                                            milk_freq              = input$milk_freq,
                                            prop_primiparous       = prop_primiparous * 100,
                                            prop_secondiparous     = prop_secondiparous * 100,
                                            cow_calving_interval   = input$cow_calving_int) - 0.02
      lambda_milk_calc

      })


    output$lambda <- renderPrint({

      paste("the lambda value is:", lambda_milk_calc())

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
        dplyr::select(-Garbage) %>%
        dplyr::mutate(Categories = dplyr::if_else(Categories == "Hei" & Month == "1", "Cal",
                                           dplyr::if_else(Categories == "Hei" & Month == "2", "Cal", Categories)))

      # additional inputs

      birth_weight_kg <- 40
      milk_sup_l <- 6
      mature_body_weight <- 680

      calf_adg_kg <- seq(1, 60) %>%
        purrr::map_dbl(starter_intake_calves, milk_intake = milk_sup_l) %>%
        purrr::map_dbl(calf_adg, milk_intake = milk_sup_l) %>%
        sum() / 60

      weaning_weight_kg <- birth_weight_kg + calf_adg_kg * 60

      df <- df %>%
        dplyr::mutate(
          Month   = as.numeric(Month),
          day_min = Month * 30 - 29,
          day_max = Month * 30,
          mean_body_weight =
            dplyr::if_else(Categories == "Hei",
                           purrr::map_dbl(Month,
                                          heifer_body_weight,
                                          birth_weight       = birth_weight_kg,
                                          weaning_weight     = weaning_weight_kg,
                                          age_first_calving  = input$time_first_calv,
                                          mature_body_weight = mature_body_weight,
                                          type               = "mean"),
                           dplyr::if_else(Categories == "Cal",
                                          purrr::map_dbl(day_max,
                                                         calf_body_weight,
                                                         birth_weight       = birth_weight_kg,
                                                         calf_adg           = calf_adg_kg,
                                                         type               = "mean"), 0)),
          final_body_weight =
            dplyr::if_else(Categories == "Hei",
                           purrr::map_dbl(Month,
                                          heifer_body_weight,
                                          birth_weight       = birth_weight_kg,
                                          weaning_weight     = weaning_weight_kg,
                                          age_first_calving  = input$time_first_calv,
                                          mature_body_weight = mature_body_weight,
                                          type               = "final"),
                           dplyr::if_else(Categories == "Cal",
                                          purrr::map_dbl(day_max,
                                                         calf_body_weight,
                                                         birth_weight       = birth_weight_kg,
                                                         calf_adg           = calf_adg_kg,
                                                         type               = "final"), 0))
        ) %>%
        dplyr::group_by(MonthSimulated) %>%
        dplyr::mutate(
          cow_bw_parturium_kg = dplyr::if_else(Phase == "Grow", 0,
                                               dplyr::if_else(Phase == 'Lac1', mature_body_weight * 0.82,
                                                              dplyr::if_else(Phase == 'Lac2', mature_body_weight * 0.92, mature_body_weight))),
          bw_change_total_kg  = dplyr::if_else(Phase == "Grow", 0,
                                               dplyr::if_else(Phase == "Lac1",
                                                              purrr::map2_dbl(day_min,
                                                                              day_max,
                                                                              cow_body_weight,
                                                                              parity = "primiparous"),
                                                              purrr::map2_dbl(day_min,
                                                                              day_max,
                                                                              cow_body_weight,
                                                                              parity = "multiparous"))),
          bw_change_acum_kg    = cumsum(bw_change_total_kg),
          mean_cow_weight_kg   = cow_bw_parturium_kg + bw_change_acum_kg - 0.5 * bw_change_total_kg,
          mean_body_weight_kg  = mean_body_weight + mean_cow_weight_kg,
          final_body_weight_kg = final_body_weight + bw_change_total_kg + cow_bw_parturium_kg,

          milk_yield_kg_2 = dplyr::if_else(Phase == "Grow" | Categories == "Dry", 0,
                                           dplyr::if_else(Phase == "Lac1",
                                                          purrr::map2_dbl(day_min,
                                                                          day_max,
                                                                          milk_yield_acum,
                                                                          parity      = "primiparous",
                                                                          milk_freq   = input$milk_freq,
                                                                          lambda_milk = lambda_milk_calc()),
                                                          dplyr::if_else(Phase == "Lac2",
                                                                         purrr::map2_dbl(day_min,
                                                                                         day_max,
                                                                                         milk_yield_acum,
                                                                                         parity      = "secondiparous",
                                                                                         milk_freq   = input$milk_freq,
                                                                                         lambda_milk = lambda_milk_calc()),
                                                                         purrr::map2_dbl(day_min,
                                                                                         day_max,
                                                                                         milk_yield_acum,
                                                                                         parity = "multiparous",
                                                                                         milk_freq = input$milk_freq,
                                                                                         lambda_milk = lambda_milk_calc())))),
          milk_yield_kg_cow2 = milk_yield_kg_2 / 30
        ) #%>%
        #dplyr::filter(MonthSimulated == 1 & Categories == "Cow") %>%
        #dplyr::ungroup() %>%
        #dplyr::summarise(
        #  average_milk_yield = (weighted.mean(milk_yield_kg_2, NumberAnimals))) / 30





    })

    output$herd_stab2 <- renderTable({

      df()

    })

      observeEvent(input$button, {

        output$graphic <- renderPlot({

          df() %>%
          ggplot2::ggplot(ggplot2::aes(x = Month, mean_body_weight_kg, col = Phase)) +
            ggplot2::geom_line() +
            ggplot2::facet_wrap(~Phase, scales = "free") +
            ggplot2::theme(legend.position = "bottom")

      })

    })

 })

}

## To be copied in the UI
# mod_animal_ui("animal")

## To be copied in the server
# mod_animal_server("animal")
