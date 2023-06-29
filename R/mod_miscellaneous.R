#' economics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_miscellaneous_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      bs4Dash::bs4Card(
        title = "Diet Costs and Milk Price",
        elevation = 1,
        width = 6,
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
                 numericInput(ns("milk_price"),       label = "Milk Price ($/cwt):",     value = 21)))),

      bs4Dash::bs4Card(
        title = "Fuel Consumption",
        elevation = 1,
        width = 6,
        solidHeader = TRUE,
        status = "teal",
        collapsible = TRUE,
        fluidRow(
          column(3,
                 numericInput(ns("gasoline"),    label = "Gasoline Consumption (L/year):",       value = 0, min = 0, max = 1000)),
          column(3,
                 numericInput(ns("natural_gas"), label = "Natural Gas Consumption (L/year):",     value = 0, min = 0, max = 1000)),
          column(3,
                 numericInput(ns("diesel"),      label = "Diesel Consumption (L/year):",          value = 0, min = 0, max = 1000)),
          column(3,
                 numericInput(ns("electricity"), label = "Electricity Consumption (kWh/year):",   value = 150000, min = 0, max = 1000)))))

  )
}

#' economics Server Functions
#'
#' @noRd
mod_miscellaneous_server <- function(id,
                                     animal_data,
                                     calf_milk_intake){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


# -------------------------------------------------------------------------
# User's feedback to inappropriate inputs
# -------------------------------------------------------------------------

    user_feedback <- function(input_id) {

      observeEvent(input[[input_id]], {

        req(input[[input_id]])

        if (input[[input_id]] < 0 | is.null(input[[input_id]]) | !isTruthy(input[[input_id]])) {

          shinyFeedback::showFeedbackWarning(inputId = input_id, text = "Can't be negative or null")

        } else {

          shinyFeedback::hideFeedback(input_id)

        }
      }
      )
    }

    list(
      "gasoline",
      "natural_gas",
      "diesel",
      "electricity"
    ) %>%
      purrr::map(user_feedback)

    economics <- reactive({

      print(paste("O verdadeiro e:", isTruthy(input[["gasoline"]]) ))

      animal_data <- animal_data()

      calf_milk_intake <- calf_milk_intake()

      # animal prmts - they will come from the animal

      milking_cows <- animal_data$total_animals[2]
      dry_cows <- animal_data$total_animals[3]
      heifers <- animal_data$total_animals[4]

      # dmi_milking_cows <- 28.5
      dmi_milking_cows <- animal_data %>%
        dplyr::filter(Categories == "Cow") %>%
        dplyr::pull("dmi_kg")

      # dmi_dry_cows <- 12
      dmi_dry_cows <- animal_data %>%
        dplyr::filter(Categories == "Dry") %>%
        dplyr::pull("dmi_kg")

      # dmi_heifers <- 7.7
      dmi_heifers <- animal_data %>%
        dplyr::filter(Categories == "Hei") %>%
        dplyr::pull("dmi_kg")


      # my_lactating <- input$milk_yield
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

        "Total Milk Income ($/cow)"               = total_income,
        "Feed Cost ($/cow)"                       = feed_cost_lac,
        "Income Over Feed Cost Lac ($/cow)"       = iofc_lac,
        "Income Over Feed Cost Lac + Dry ($/cow)" = iofc_lac_dry,
        "Income Over feed Cost Dry ($/cow)"       = iofc_dry,
        "Feed Cost per Kg Milk ($)"               = feed_cost_kg_milk
        ) %>%
        dplyr::mutate_if(is.numeric, round, 2)

    })

# -------------------------------------------------------------------------
# CO2 from fuels ----------------------------------------------------------
# -------------------------------------------------------------------------

    tabela_calc <- reactive({

      tabela <- tibble::tibble(

        gasoline_co2eq    = gasoline_co2eq(input$gasoline),
        natural_gas_co2eq = natural_gas_co2eq(input$natural_gas),
        diesel_co2eq      = diesel_co2eq(input$diesel),
        electricity_co2eq = electricity_co2eq(input$electricity)
      )

      tabela %>%
        dplyr::summarise(
          total_co2_eq_fuel = gasoline_co2eq + natural_gas_co2eq + diesel_co2eq + electricity_co2eq
        ) %>%
        dplyr::pull(total_co2_eq_fuel)

    })

# -------------------------------------------------------------------------
# Report outcomes - inputs ------------------------------------------------
# -------------------------------------------------------------------------

    fuel_inputs <- reactive({

      df <- tibble::tibble(
        "Gasoline (l/year)"      = input$gasoline,
        "Natural Gas (l/year)"   = input$natural_gas,
        "Diesel (l/year)"        = input$diesel,
        "Electricity (kWh/year)" = input$electricity
      )

      df

    })


# -------------------------------------------------------------------------
# Outcomes from this module to populate others ----------------------------
# -------------------------------------------------------------------------

    return(
      list(
        co2_eq_fuel    = reactive(tabela_calc()),
        fuel_inputs    = reactive(fuel_inputs()),
        milk_price     = reactive(input$milk_price),
        dry_diet_cost  = reactive(input$dry_diet_cost),
        lact_diet_cost = reactive(input$lact_diet_cost)
      )
    )

  })
}

