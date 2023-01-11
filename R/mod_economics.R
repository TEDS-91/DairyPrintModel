#' economics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_economics_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      bs4Dash::bs4Card(
        title = h4(strong("Economics"), align = "center"),
        width = 12,
        fluidRow(

          column(3,
                 numericInput(ns("lact_diet_cost"),   label = "Lactation Cows ($/DM)", value = 0.41)),
          column(3,
                 numericInput(ns("dry_diet_cost"),    label = "Dry ($/DM)", value = 0.35)),
          column(3,
                 numericInput(ns("heifer_diet_cost"), label = "Heifers ($/DM)", value = 0.28)),
          column(3,
                 numericInput(ns("milk_price"),       label = "Milk Price ($/wt)", value = 21))))),

    #h4(strong("Economics"), align = "center"),

    fluidRow(
      bs4Dash::bs4Card(
        title = h4(strong("Economics"), align = "center"),
        width = 12,
        fluidRow(
          tableOutput(ns("tabela")),
          bs4Dash::valueBoxOutput(ns("vbox")),
          bs4Dash::valueBoxOutput(ns("vbox2"))

        )
      )
    )
  )
}

#' economics Server Functions
#'
#' @noRd
mod_economics_server <- function(id,
                                 animal_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    economics <- reactive({

      animal_data <- animal_data()

      # animal prmts - they will come from the animal

      milking_cows <- 230
      dry_cows <- 50
      heifers <- 134

      #dmi_milking_cows <- 28.5
      dmi_milking_cows <- animal_data %>%
        dplyr::filter(Categories == "Cow") %>%
        dplyr::pull("dmi_kg")

      #dmi_dry_cows <- 12
      dmi_dry_cows <- animal_data %>%
        dplyr::filter(Categories == "Dry") %>%
        dplyr::pull("dmi_kg")

      #dmi_heifers <- 7.7
      dmi_heifers <- animal_data %>%
        dplyr::filter(Categories == "Hei") %>%
        dplyr::pull("dmi_kg")


      #my_lactating <- input$milk_yield
      my_lactating <- animal_data %>%
        dplyr::filter(Categories == "Cow") %>%
        dplyr::pull("milk_yield_kg")

      feed_efic <- my_lactating / dmi_milking_cows

      total_income <- input$milk_price / 100 / 0.46 * my_lactating

      feed_cost_lac <- dmi_milking_cows * input$lact_diet_cost

      iofc_dry <- 0 - input$dry_diet_cost * dmi_dry_cows

      feed_cost_dry <- (dry_cows * input$dry_diet_cost * dmi_dry_cows) / milking_cows

      feed_cost_kg_milk <- feed_cost_lac / my_lactating

      iofc_lac <- total_income - feed_cost_lac

      iofc_lac_dry <- total_income - feed_cost_lac - feed_cost_dry

      economics <- tibble::tibble(

        "Feed Efficiency (kg/kg)"                 = feed_efic,
        "Total Milk Income ($/cow)"               = total_income,
        "Feed Cost ($/cow)"                       = feed_cost_lac,
        "Income Over Feed Cost Lac ($/cow)"       = iofc_lac,
        "Income Over Feed Cost Lac + Dry ($/cow)" = iofc_lac_dry,
        "Income Over feed Cost Dry ($/cow)"       = iofc_dry,
        "Feed Cost per Kg Milk ($)"               = feed_cost_kg_milk

      )


    })

    output$tabela <- renderTable({

      economics <- tibble::as_tibble(economics())

      economics

    })

    # testing value box

    output$vbox <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        value = tags$p(round(economics()[1], 2), style = "font-size: 200%;"),
        subtitle = "Feed Efficiency",
        color = "lightblue",
        icon = icon("fa-thin fa-leaf", verify_fa = FALSE),
        elevation = c(1)
      )
    })

    output$vbox2 <- bs4Dash::renderValueBox({
      bs4Dash::valueBox(
        value = tags$p(round(economics()[4], 2), style = "font-size: 200%;"),
        subtitle = tags$p("Income Over Feed Cost ($/Cow)", style = "font-size: 100%;"),
        color = "lightblue",
        icon = icon("fa-solid fa-dollar-sign", verify_fa = FALSE),
        elevation = c(1)
      )
    })



  })
}

