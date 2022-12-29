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

    h3(strong("Economics - Diet Costs"), align = "center"),

    fluidRow(
      column(2,
             numericInput(ns("lact_diet_cost"),   label = "Lactation Cows ($/DM)", value = 0.41)),
      column(2,
             numericInput(ns("dry_diet_cost"),    label = "Dry ($/DM)", value = 0.35)),
      column(2,
             numericInput(ns("heifer_diet_cost"), label = "Heifers ($/DM)", value = 0.28)),
      column(2,
             numericInput(ns("milk_yield"),       label = "Milk Yield (kg/d)", value = 43)),
      column(2,
             numericInput(ns("milk_price"),       label = "Milk Price ($/wt)", value = 21))),

    #h4(strong("Economics"), align = "center"),

    tableOutput(ns("tabela"))

  )
}

#' economics Server Functions
#'
#' @noRd
mod_economics_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    economics <- reactive({

      # animal prmts - they will come from the animal

      milking_cows <- 230
      dry_cows <- 50
      heifers <- 134

      dmi_milking_cows <- 28.5
      dmi_dry_cows <- 12
      dmi_heifers <- 7.7

      my_lactating <- input$milk_yield

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

  })
}

## To be copied in the UI
# mod_economics_ui("economics")

## To be copied in the server
# mod_economics_server("economics")
