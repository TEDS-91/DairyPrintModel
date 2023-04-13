#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Your application server logic

  # General prmts from UI

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

  mod_fuel_combustion_server("fuel_combustion")

  # calf milk ingestion input

  calf_milk_sup <- animal_data[[2]]

  miscellaneous <- mod_miscellaneous_server("miscellaneous",
                                            animal_data      = animal_data[[1]],
                                            calf_milk_intake = calf_milk_sup)

  ghg_emissions <- mod_manure_ghg_emissions_server("ch4_emissions",
                                                   animal_data         = animal_data[[1]],
                                                   county              = county,
                                                   facilitie           = facilitie,
                                                   bedding             = bedding,
                                                   manure_management   = manure_management,
                                                   biodigester         = biodigester,
                                                   biodigester_ef      = biod_ef,
                                                   type_manure         = type_manure,
                                                   solid_liquid        = solid_liquid_separation,
                                                   enclosed_manure     = enclosed_manure,
                                                   empty_time          = empty_time,
                                                   crust               = crust,
                                                   manure_storage_area = manure_storage_area)


  mod_nh3_emissions_server("nh3_emissions",
                           county              = county,
                           facilitie           = facilitie,
                           biodigester         = biodigester,
                           type_manure         = type_manure,
                           crust               = crust,
                           empty_time          = empty_time,
                           manure_storage_area = manure_storage_area,
                           dataset             = ghg_emissions[[1]])

  mod_nitrous_oxide_emissions_server("nitrous_oxide",
                                     enclosed_manure     = enclosed_manure,
                                     manure_storage_area = manure_storage_area)

  co2eq_purchased <- mod_purchased_feeds_server("purchased_feeds_1")

  # crops

  ghg_crop <- mod_crop_server("crop",
                              animal_data               = animal_data[[1]],
                              manure_management         = manure_management,
                              type_manure               = type_manure,
                              manure_application_method = manure_application_method,
                              co2eq_purchased           = co2eq_purchased[[1]],
                              manure_data               = ghg_emissions[["nh3_emissions"]])


  mod_dashboard_server("dashboard",
                       animal_data        = animal_data[[1]],
                       calf_milk_intake   = calf_milk_sup,
                       herd_inputs        = animal_data[["herd_inputs"]],
                       raw_animal_df      = animal_data[["raw_animal_df"]],
                       diet_inputs        = animal_data[["diet_inputs"]],

                       manure_inputs      = ghg_emissions[["manure_inputs"]],
                       nh3_emissions      = ghg_emissions[["nh3_emissions"]],
                       herd_methane       = ghg_emissions[["herd_methane"]],
                       fac_methane        = ghg_emissions[["fac_methane"]],
                       storage_methane    = ghg_emissions[["storage_methane"]],
                       fac_ammonia        = ghg_emissions[["fac_ammonia"]],
                       storage_ammonia    = ghg_emissions[["storage_ammonia"]],

                       co2_eq_fuel_col_spread = miscellaneous[["co2_eq_fuel"]],

                       crop_inputs               = ghg_crop[["crop_inputs"]],
                       co2eq_purchased           = co2eq_purchased[[1]],
                       purchased_feeds           = co2eq_purchased[["purchased_feeds"]],

                       total_co2          = ghg_crop[["total_co2"]],
                       total_nh3          = ghg_crop[["total_nh3"]],
                       total_n2o          = ghg_crop[["total_n2o"]],
                       total_ch4          = ghg_crop[["total_ch4"]],
                       direct_n2o_storage = ghg_emissions[["direct_storage_n2o"]])

  mod_info_server("info_1")



}

