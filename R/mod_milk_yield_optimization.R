#' milk_yield_optimization UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_milk_yield_optimization_ui <- function(id){
  ns <- NS(id)
  tagList(

  )
}

#' milk_yield_optimization Server Functions
#'
#' @noRd
mod_milk_yield_optimization_server <- function(id, obs_average_milk_yield, milk_freq, prop_primiparous, prop_secondiparous, cow_calving_interval){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

      lambda <- reactive({

        lambda <- lambda_milk_yield(obs_average_milk_yield = obs_average_milk_yield,
                                    milk_freq              = milk_freq,
                                    prop_primiparous       = prop_primiparous,
                                    prop_secondiparous     = prop_secondiparous,
                                    cow_calving_interval   = cow_calving_interval)
      })

    return(
      lambda = lambda()
      )

  })

}

## To be copied in the UI
# mod_milk_yield_optimization_ui("milk_yield_optimization")

## To be copied in the server
# mod_milk_yield_optimization_server("milk_yield_optimization")
