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
        title = "Diet Costs and Milk Price",
        elevation = 1,
        width = 12,
        solidHeader = TRUE,
        status = "teal",
        collapsible = TRUE,
        fluidRow(
          column(3,
                 numericInput(ns("lact_diet_cost"),   label = "Milking Cows ($/kgDM):",   value = 0.3)),
          column(3,
                 numericInput(ns("dry_diet_cost"),    label = "Dry Cows ($/kgDM):",       value = 0.12)),
          column(3,
                 numericInput(ns("heifer_diet_cost"), label = "Heifers ($/kgDM):",        value = 0.12)),
          column(3,
                 numericInput(ns("milk_price"),       label = "Milk Price ($/cwt):",     value = 21))))),

    fluidRow(
      bs4Dash::bs4Card(
        title = "Economic analysis",
        elevation = 1,
        width = 12,
        solidHeader = TRUE,
        status = "teal",
        collapsible = TRUE,
        maximizable = TRUE,
        fluidRow(
          reactable::reactableOutput(ns("tabela"))))
    )
  )
}

#' economics Server Functions
#'
#' @noRd
mod_economics_server <- function(id,
                                 animal_data,
                                 calf_milk_intake){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    economics <- reactive({

      animal_data <- animal_data()

      calf_milk_intake <- calf_milk_intake()

      # animal prmts - they will come from the animal

      milking_cows <- animal_data$total_animals[2]
      dry_cows <- animal_data$total_animals[3]
      heifers <- animal_data$total_animals[4]

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
        dplyr::pull("milk_yield_kg_fpc")

      feed_efic <- my_lactating / dmi_milking_cows

      # total income discounting milk supplied to calves

      total_income <- input$milk_price / (100 / 2.2) * (my_lactating - (animal_data$total_animals[1] * calf_milk_intake) / milking_cows)

      feed_cost_lac <- dmi_milking_cows * input$lact_diet_cost

      iofc_dry <- 0 - input$dry_diet_cost * dmi_dry_cows

      feed_cost_dry <- (dry_cows * input$dry_diet_cost * dmi_dry_cows) / milking_cows

      feed_cost_kg_milk <- feed_cost_lac / my_lactating

      iofc_lac <- total_income - feed_cost_lac

      iofc_lac_dry <- total_income - feed_cost_lac - feed_cost_dry

      economics <- tibble::tibble(

        #"Feed Efficiency (kg/kg)"                 = feed_efic,
        "Total Milk Income ($/cow)"               = total_income,
        "Feed Cost ($/cow)"                       = feed_cost_lac,
        "Income Over Feed Cost Lac ($/cow)"       = iofc_lac,
        "Income Over Feed Cost Lac + Dry ($/cow)" = iofc_lac_dry,
        "Income Over feed Cost Dry ($/cow)"       = iofc_dry,
        "Feed Cost per Kg Milk ($)"               = feed_cost_kg_milk
        ) %>%
        dplyr::mutate_if(is.numeric, round, 2)

    })

    output$tabela <- reactable::renderReactable({

      economics <- tibble::as_tibble(economics()) %>%
        reactable::reactable(
          defaultColDef = reactable::colDef(
            header = function(value) gsub(".", " ", value, fixed = TRUE),
            cell = function(value) format(value, nsmall = 1),
            align = "center",
            minWidth = 200,
            headerStyle = list(background = "#f7f7f8")
          ),
          columns = list(
            Species = reactable::colDef(minWidth = 300)  # overrides the default
          ),
          bordered = TRUE,
          highlight = TRUE
        )

      economics

    })

  })
}

