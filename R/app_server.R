#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  #telemetry <- shiny.telemetry::Telemetry$new() # 1. Initialize telemetry with default options) # add parameters here (if any)

  #telemetry$start_session(track_values = TRUE) # 3. Minimal setup to track events

  # Your application server logic

  shinyalert::shinyalert(title = "Welcome to DairyPrint Model! ",
                         text =
                         "To run the model, first you have to calibrate it according to your own farm inputs.

                         After filling up all inputs in the herd calibration page, you're ready to run the model:
                         just hit 'Run!' and have fun!

                         The other tabs will respond imediately after input changes without the need to hit any button.
                         ",
                         type = "info",
                         size = "m")

  facilitie <- reactive(input$facilitie)

  county <- reactive(input$county)

  bedding <- reactive(input$bedding_type)

  manure_management <- reactive(input$manure_manag)

  biodigester <- reactive(input$biodigester)

  biod_ef <- reactive(input$biod_ef)

  solid_liquid_separation <- reactive(input$solid_liquid)

  enclosed_manure <- reactive(input$enclosed_manure)

  empty_time <- reactive(input$empty)

  type_manure <- reactive(input$type_manure)

  crust <- reactive(input$crust)

  manure_storage_area <- reactive(input$storage_area)

  manure_application_method <- reactive(input$application)

  # Calling the modules

  animal_data <- mod_animal_server("animal")

  # calf milk ingestion input

  calf_milk_sup <- animal_data[[2]]

  miscellaneous <- mod_miscellaneous_server("miscellaneous",
                                            animal_data      = animal_data[[1]],
                                            calf_milk_intake = calf_milk_sup)

  ghg_emissions <- mod_manure_ghg_emissions_server("ch4_emissions",
                                                   animal_data         = animal_data[[1]]
                                                   )

  co2eq_purchased <- mod_purchased_feeds_server("purchased_feeds_1")

  # crops

  ghg_crop <- mod_crop_server("crop",
                              animal_data               = animal_data[[1]],
                              manure_management         = manure_management,
                              type_manure               = type_manure,
                              manure_inputs             = ghg_emissions[["manure_inputs"]],
                              manure_application_method = manure_application_method,
                              co2eq_purchased           = co2eq_purchased[[1]],
                              manure_data               = ghg_emissions[["nh3_emissions"]],
                              phosphorous_from_manure   = animal_data[["phosphorous_from_manure"]],
                              potassium_from_manure     =  animal_data[["potassium_from_manure"]],
                              county                    = ghg_emissions[["county"]])


  mod_dashboard_server("dashboard",

                       # All the inputs bellow are coming from the herd module

                       animal_data             = animal_data[[1]],
                       calf_milk_intake        = calf_milk_sup,
                       herd_inputs             = animal_data[["herd_inputs"]],
                       raw_animal_df           = animal_data[["raw_animal_df"]],
                       diet_inputs             = animal_data[["diet_inputs"]],
                       farm_area               = animal_data[["farm_area"]],

                       # All the inputs bellow are coming from the manure module

                       manure_inputs      = ghg_emissions[["manure_inputs"]],
                       nh3_emissions      = ghg_emissions[["nh3_emissions"]],
                       herd_methane       = ghg_emissions[["herd_methane"]],
                       fac_methane        = ghg_emissions[["fac_methane"]],
                       storage_methane    = ghg_emissions[["storage_methane"]],
                       fac_ammonia        = ghg_emissions[["fac_ammonia"]],
                       storage_ammonia    = ghg_emissions[["storage_ammonia"]],
                       bedding_qnt        = ghg_emissions[["bedding_quant"]],
                       direct_storage_n2o = ghg_emissions[["direct_storage_n2o"]],

                       # All the inputs bellow are coming from the miscellaneous module

                       co2_eq_fuel_col_spread = miscellaneous[["co2_eq_fuel"]],
                       fuel_inputs = miscellaneous[["fuel_inputs"]],
                       milk_price  = miscellaneous[["milk_price"]],
                       dry_diet_cost = miscellaneous[["dry_diet_cost"]],
                       lact_diet_cost = miscellaneous[["lact_diet_cost"]],

                       # All the inputs bellow are coming from the crop and purchased feeds module

                       co2eq_purchased           = co2eq_purchased[[1]],
                       purchased_feeds           = co2eq_purchased[["purchased_feeds"]],
                       n_purchased_feeds         = co2eq_purchased[["n_purchased_feeds"]],
                       k_purchased_feeds         = co2eq_purchased[["k_purchased_feeds"]],
                       p_purchased_feeds         = co2eq_purchased[["p_purchased_feeds"]],

                       # All the inputs bellow are coming from the crop and purchased feeds module

                       crop_inputs        = ghg_crop[["crop_inputs"]],
                       total_co2          = ghg_crop[["total_co2"]],
                       total_nh3          = ghg_crop[["total_nh3"]],
                       total_n2o          = ghg_crop[["total_n2o"]],
                       total_ch4          = ghg_crop[["total_ch4"]],

                       n_fixed            = ghg_crop[["n_fixed"]],
                       n_leached          = ghg_crop[["n_leached"]],
                       k_from_fertilizers = ghg_crop[["k_from_fertilizers"]],
                       p_from_fertilizers = ghg_crop[["p_from_fertilizers"]],
                       p_losses           = ghg_crop[["p_losses"]],
                       k_losses           = ghg_crop[["k_losses"]],
                       n_extracted_crops  = ghg_crop[["n_extracted_crops"]],
                       direct_n2o_storage = ghg_emissions[["direct_storage_n2o"]])

  mod_info_server("info_1")


}

