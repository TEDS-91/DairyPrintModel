#' ch4_emissions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_manure_ghg_emissions_ui <- function(id){
  ns <- NS(id)
  tagList(

      bs4Dash::bs4Card(
        title = "Manure Handling and Management",
        elevation = 2,
        width = 12,
        solidHeader = TRUE,
        status = "teal",
        collapsible = TRUE,
        fluidRow(
          manure_ui_prms(),
          uiOutput(ns("sls")))
        ),

    fluidRow(
      bs4Dash::bs4Card(
        title = "Manure and GHG Emissions Dashboard",
        elevation = 2,
        width = 12,
        solidHeader = TRUE,
        status = "teal",
        collapsible = TRUE,
        maximizable = TRUE,
        fluidRow(

          list(
            "total_manure_managed",
            "herd_methane",
            "facilitie_methane",
            "manure_storage_methane",
            "barn_nh3",
            "storage_nh3",
            "total_n2o"
          ) %>%
            purrr::map(\(x) bs4Dash::valueBoxOutput(ns(x), width = 3)),

          br(),

          bs4Dash::tabBox(
            id = "tabcard",
            status = "primary",
            solidHeader = FALSE,
            type = "tabs",
            width = 12,
            tabPanel(
              title = "Methane Emissions",
              fluidRow(
                bs4Dash::bs4Card(
                  title = "Barn",
                  width = 6,
                  footer = NULL,
                  plotly::plotlyOutput(ns("barn_ch4_chart"))),
                bs4Dash::bs4Card(
                  title = "Manure Storage",
                  width = 6,
                  footer = NULL,
                  plotly::plotlyOutput(ns("storage_ch4_chart"))))),

            tabPanel(
              title = "Ammonia Emissions",
              fluidRow(
                bs4Dash::bs4Card(
                  title = "Barn",
                  width = 6,
                  footer = NULL,
                  plotly::plotlyOutput(ns("barn_nh3_chart")),
                  tableOutput(ns("ammonia_teste"))),
                bs4Dash::bs4Card(
                  title = "Sorage",
                  width = 6,
                  footer = NULL,
                  plotly::plotlyOutput(ns("storage_nh3_chart"))))),

            tabPanel(
              title = "Weather Data",
              fluidRow(
                bs4Dash::bs4Card(
                  title = "Temperature (C) and precipitation (mm).",
                  width = 12,
                  footer = "Weather data is a summary of temperature (mean, minimum, and maximum; C) and
                            precipitation (mm) values from year 2001 to 2021. Data were obtained from
                            the Daymet website.",
                plotly::plotlyOutput(ns("weather_chart")),

                tableOutput(ns('show_inputs'))

                )
              )
            )
          )
        )
      )
    )
  )
}

#' ch4_emissions Server Functions
#'
#' @noRd
mod_manure_ghg_emissions_server <- function(id,
                                            animal_data,
                                            county,
                                            facilitie,
                                            bedding,
                                            manure_management,
                                            biodigester,
                                            biodigester_ef,
                                            type_manure,
                                            solid_liquid,
                                            enclosed_manure,
                                            empty_time,
                                            crust,
                                            manure_storage_area){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

     output$sls <- renderUI({

       req(c(manure_management(), type_manure()))

       fluidRow(column(12,
                       manure_manag_ui(manure_managment = manure_management(), type_manure = type_manure())))

       })

# -------------------------------------------------------------------------
# Methane emissions from manure storage -----------------------------------
# -------------------------------------------------------------------------

     manure_dm <- reactive({

       type_manure <- ifelse(is.null(type_manure()), "Slurry", type_manure())

       manure_dm <- ifelse(type_manure == "Semi-solid", 13,
                                   ifelse(type_manure == "Slurry", 8,
                                                  ifelse(type_manure == "Solid", 20, 5)))
       manure_dm

     })

    # all logic without connect with the animal part

    emissions <- reactive({

# -------------------------------------------------------------------------
# conditioning the inputs to NULL values. This is necessary because they
# depend on inputs from other parts oh the App - they are not direct inputs
# from this module. Otherwise the function 'req()' would work.
# -------------------------------------------------------------------------

      animal_data <- animal_data()

      county <- ifelse(is.null(county()), "Dane", county())

      manure_management <- ifelse(is.null(manure_management()), "Pond or Tank Storage", manure_management())

      crust <- ifelse(manure_management == "Biodigester + Solid-liquid Separator" | manure_management == "Biodigester", "no",
                      ifelse(manure_management == "Pond or Tank Storage", "no", crust()))

      facilitie <- ifelse(is.null(facilitie()), "freestall", facilitie())

      bedding <- ifelse(is.null(bedding()), "Sawdust", bedding())

      biodigester <- biodigester()

      biodigester_ef <- ifelse(is.null(biodigester_ef()), 25, biodigester_ef())

      type_manure <- ifelse(is.null(type_manure()), "Slurry", type_manure())

      solid_liquid <- solid_liquid()

      enclosed_manure <- ifelse(is.null(enclosed_manure()), "no", enclosed_manure())

      empty_time <- ifelse(is.null(empty_time()), "Fall and Spring", empty_time())


# -------------------------------------------------------------------------

      # animal prmts - they will come from the animal

      milking_cows <- animal_data %>%
        dplyr::filter(Categories == "Cow") %>%
        dplyr::pull(total_animals)

      dry_cows <- animal_data %>%
        dplyr::filter(Categories == "Dry") %>%
        dplyr::pull(total_animals)

      heifers <- animal_data %>%
        dplyr::filter(Categories == "Hei") %>%
        dplyr::pull(total_animals)

      total_manure_kg <- animal_data %>%
        dplyr::filter(Categories!= "Cal") %>%
        dplyr::summarise(
          total_manure = sum(total_manure_kg)
        ) %>%
        dplyr::pull(total_manure)

      #total_ts_manure_kg <- milking_cows * milking_cows_manure * milking_cows_ts + dry_cows * dry_cows_manure * dry_cows_ts + heifers * heifers_manure * heifers_ts
      total_ts_manure_kg <- animal_data %>%
        dplyr::summarise(
          ts_manure = sum(total_solids_kg)
        ) %>%
        dplyr::pull(ts_manure)

      #total_vs_manure_kg <- (milking_cows * milking_cows_manure * milking_cows_ts + dry_cows * dry_cows_manure * dry_cows_ts + heifers * heifers_manure * heifers_ts) * 0.8

      total_vs_manure_kg <- animal_data %>%
        dplyr::summarise(
          vs_manure = sum(total_volatile_solids_kg)
        ) %>%
        dplyr::pull(vs_manure)

      # Total Urine

      total_urine_kg <- animal_data %>%
        dplyr::summarise(
          total_urine_kg = sum(total_urine_kg)
        ) %>%
        dplyr::pull(total_urine_kg)

      # Total TAN

      #browser()

      total_tan_kg <- animal_data %>%
        dplyr::summarise(
          total_tan_kg = sum(total_tan_excreted_g) / 1000
        ) %>%
        dplyr::pull(total_tan_kg)

      # N in feces

      total_fecal_n_kg <- animal_data %>%
        dplyr::summarise(
          total_fecal_n_kg = sum(total_fecal_n_escreted_g) / 1000
        ) %>%
        dplyr::pull(total_fecal_n_kg)

      yday <- seq(1, 730, 1)

      req(county())

      temp_c <- rep( wisconsin_weather_data %>%
                      dplyr::filter(county == county()) %>%
                      dplyr::pull(aver_tempC), 2)

      #herd_ch4_emissions_kg <- milking_cows * 0.464 + dry_cows * 0.261 + heifers * 0.156
      herd_ch4_emissions_kg <-  animal_data %>%
        dplyr::summarise(
          total_ch4_kg = sum(total_ch4_kg)
        ) %>%
        dplyr::pull(total_ch4_kg)

      area_exposed_m2 <- ifelse(facilitie == "freestall", milking_cows * 3.5 + dry_cows * 3.5 + heifers * 2,
                                        milking_cows * 1.2 + dry_cows * 1.2 + heifers * 1)

      barn_ch4_emissions_kg <- barn_ch4_emission_floor(temp_c = temp_c, manure_area = area_exposed_m2)


      # determining bedding quantity
      bedding_quantity_kg <- ifelse(bedding == "Sawdust", 2 * (milking_cows + dry_cows + heifers),
                                            ifelse(bedding == "Sand", 1.5 * (milking_cows + dry_cows + heifers),
                                            3 * (milking_cows + dry_cows + heifers)))

      bedding_ts_kg <- bedding_quantity_kg * 0.9

      bedding_vs_kg <- ifelse(bedding == "Sawdust", bedding_ts_kg * 0.8,
                                      ifelse(bedding == "Sand", bedding_ts_kg * 0,
                                                     bedding_ts_kg * 0.8))

      total_mass_managed_kg <- total_manure_kg + bedding_quantity_kg

      total_ts_managed_kg <- total_ts_manure_kg + bedding_ts_kg

      total_vs_managed_kg <- total_vs_manure_kg + bedding_vs_kg

      total_mass_managed_corSS_kg <- ifelse(bedding == "Sand", total_mass_managed_kg - 1.5 * (milking_cows + dry_cows + heifers),
                                                    total_mass_managed_kg)

      if (manure_management == "Daily Hauling") {

        # regardless of the type of manure, emissions are considered zero!

        empty_day <- 0

        manure_dm <- manure_dm()

        h2o_to_add <- (total_ts_managed_kg / (manure_dm / 100)) - total_mass_managed_corSS_kg

        total_manure_manag_kg <- total_mass_managed_kg + ifelse(h2o_to_add > 0, h2o_to_add, 0)

        vs_solid_loaded_kg_day <- total_vs_managed_kg

        total_ch4_kg <- 0

        tabela_outputs <- tibble::tibble(
          year_day                 = yday,
          temp_c                   = temp_c,
          teste = total_ts_managed_kg/total_manure_manag_kg,
          facilitie                = facilitie,
          area_exposed_m2          = area_exposed_m2,
          type_manure              = type_manure,
          manure_management        = manure_management,
          empty_day                = empty_day,
          manure_dm                = manure_dm,
          crust                    = crust,
          total_urine_kg           = total_urine_kg,
          total_tan_kg             = total_tan_kg,
          fecal_n_kg               = total_fecal_n_kg,
          total_manure_manag_kg    = total_manure_manag_kg,
          vs_solid_loaded_kg_day   = vs_solid_loaded_kg_day,
          herd_ch4_emissions_kg    = herd_ch4_emissions_kg,
          barn_ch4_emissions_kg    = barn_ch4_emissions_kg,
          storage_ch4_emissions_kg = rep(0, 730)
        )

        tabela_outputs

      } else if (manure_management == "Pond or Tank Storage") {

        if (type_manure == "Semi-solid" | type_manure == "Solid") {

          req(empty_time)

          req(enclosed_manure)

          #empty_day <- empty_days()

          empty_day <- empty_day(empty_time = empty_time)

          manure_dm <- manure_dm()

          h2o_to_add <- (total_ts_managed_kg / (manure_dm / 100)) - total_mass_managed_corSS_kg

          total_manure_manag_kg <- total_mass_managed_kg + ifelse(h2o_to_add > 0, h2o_to_add, 0)

          vs_solid_loaded_kg_day <- ifelse(empty_day == 1, 0, total_vs_managed_kg)

          total_ch4_kg <- manure_ch4_emission_solid(volatile_solids = vs_solid_loaded_kg_day, temp_c = temp_c)

          tabela_outputs <- tibble::tibble(
            year_day                 = yday,
            temp_c                   = temp_c,
            facilitie                = facilitie,
            area_exposed_m2          = area_exposed_m2,
            type_manure              = type_manure,
            manure_management        = manure_management,
            empty_day                = empty_day,
            manure_dm                = manure_dm,
            crust                    = crust,
            total_urine_kg           = total_urine_kg,
            total_tan_kg             = total_tan_kg,
            fecal_n_kg               = total_fecal_n_kg,
            total_manure_manag_kg    = total_manure_manag_kg,
            vs_solid_loaded_kg_day   = vs_solid_loaded_kg_day,
            herd_ch4_emissions_kg    = herd_ch4_emissions_kg,
            barn_ch4_emissions_kg    = barn_ch4_emissions_kg,
            storage_ch4_emissions_kg = total_ch4_kg
          )

          tabela_outputs

        } else if (type_manure == "Liquid" | type_manure == "Slurry") {

          req(empty_time)

          empty_day <- empty_day(empty_time = empty_time)

          manure_dm <- manure_dm()

          h2o_to_add <- (total_ts_managed_kg / (manure_dm / 100)) - total_mass_managed_corSS_kg

          total_manure_manag_kg <- total_mass_managed_kg + ifelse(h2o_to_add > 0, h2o_to_add, 0)

          vs_liq_loaded_kg_day <- ifelse(empty_day == 1, 0, total_vs_managed_kg)

          #print(vs_liq_loaded_kg_day)

          remaining_vs_tank_pct <- 0.05

          tank_capacity <- 365 * vs_liq_loaded_kg_day[1]

          vs_liq_loaded_cum_kg <- vector(length = 730)

          vs_liq_loaded_cum_kg[1] <- vs_liq_loaded_kg_day[1]

          vs_liq_deg <- rep(NA, 730); vs_liq_ndeg <- rep(NA, 730); vs_liq_loss_kg_day <- rep(NA, 730); vs_liq_loss_cum_kg <- rep(NA, 730);
          vs_liq_total_cum_kg <- rep(NA, 730); ch4_liq_emission_kg_day <- rep(NA, 730); co2_liq_emission_kg <- rep(NA, 730)


         # temp_c <- temp_c()

          enclosed_manure <- enclosed_manure

          empty_days <- empty_day(empty_time = empty_time)


          for (i in 2:730) {

            # required parameters

            B_o <- 0.2

            ch4_pot <- 0.48

            # first row calculations to initialize all calculations

            vs_liq_loaded_cum_kg[1] <- vs_liq_loaded_kg_day[1]

            vs_liq_total_cum_kg[1] <- vs_liq_loaded_kg_day[1]

            vs_liq_loss_kg_day[1] <- 0

            vs_liq_loss_cum_kg[1] <- 0

            vs_liq_deg[1] <- (vs_liq_loaded_cum_kg[1] * (B_o / ch4_pot) - vs_liq_loss_cum_kg[1]) / vs_liq_total_cum_kg[1]

            vs_liq_ndeg[1] <- 1 - vs_liq_deg[1]

            ch4_liq_emission_kg_day[1] <- manure_ch4_emission_slurry(volatile_solids_total = vs_liq_total_cum_kg[1],
                                                                     volatile_solids_d     = vs_liq_deg[1],
                                                                     volatile_solids_nd    = vs_liq_ndeg[1],
                                                                     temp_c                = temp_c[1],
                                                                     enclosed              = enclosed_manure)

            co2_liq_emission_kg[1] <- ifelse(enclosed_manure == "no", 0, ch4_liq_emission_kg_day[1] * 2.75)

            # calculations for the rest of the vectors

            vs_liq_loaded_cum_kg[i] <- ifelse(empty_days[i] == 0, vs_liq_loaded_kg_day[i] + vs_liq_loaded_cum_kg[i - 1],
                                                      (remaining_vs_tank_pct / 100 * tank_capacity))

            vs_liq_loss_kg_day[i] <- ch4_liq_emission_kg_day[i - 1] * 3

            vs_liq_loss_cum_kg[i] <- ifelse(vs_liq_loaded_cum_kg[i] <= (remaining_vs_tank_pct / 100 * tank_capacity), 0, vs_liq_loss_kg_day[i] + vs_liq_loss_cum_kg[i - 1])

            vs_liq_total_cum_kg[i] <- ifelse(vs_liq_loaded_cum_kg[i] <= (remaining_vs_tank_pct / 100 * tank_capacity), vs_liq_loaded_cum_kg[i], vs_liq_loaded_cum_kg[i] - vs_liq_loss_cum_kg[i - 1])

            vs_liq_deg[i] <- ifelse(vs_liq_loaded_cum_kg[i] <= (remaining_vs_tank_pct / 100 * tank_capacity), (vs_liq_loaded_cum_kg[i] * (B_o / ch4_pot) - 0) / vs_liq_total_cum_kg[i],
                                            (vs_liq_loaded_cum_kg[i] * (B_o / ch4_pot) - vs_liq_loss_cum_kg[i]) / vs_liq_total_cum_kg[i])

            vs_liq_ndeg[i] <- 1 - vs_liq_deg[i]

            ch4_liq_emission_kg_day[i] <- manure_ch4_emission_slurry(volatile_solids_total = vs_liq_total_cum_kg[i],
                                                                     volatile_solids_d     = vs_liq_deg[i],
                                                                     volatile_solids_nd    = vs_liq_ndeg[i],
                                                                     temp_c                = temp_c[i],
                                                                     enclosed              = enclosed_manure)

            co2_liq_emission_kg[i] <- ifelse(enclosed_manure == "no", 0, ch4_liq_emission_kg_day[i] * 2.75)

          }

          tabela_outputs <- tibble::tibble(
            year_day                 = yday,
            temp_c                   = temp_c,
            facilitie                = facilitie,
            area_exposed_m2          = area_exposed_m2,
            type_manure              = type_manure,
            manure_management        = manure_management,
            empty_day                = empty_days,#empty_day,
            manure_dm                = manure_dm,
            crust                    = crust,
            total_urine_kg           = total_urine_kg,
            total_tan_kg             = total_tan_kg,
            fecal_n_kg               = total_fecal_n_kg,
            total_manure_manag_kg    = total_manure_manag_kg,
            vs_liq_loaded_kg_day     = vs_liq_loaded_kg_day,
            total_cum = vs_liq_total_cum_kg,
            herd_ch4_emissions_kg    = herd_ch4_emissions_kg,
            barn_ch4_emissions_kg    = barn_ch4_emissions_kg,
            storage_ch4_emissions_kg = ch4_liq_emission_kg_day
          )

          tabela_outputs

        }

      } else if (manure_management == "Biodigester") {

        req(biodigester_ef)

        req(empty_time)

        empty_day <- empty_day(empty_time = empty_time)

        manure_dm <- manure_dm()

        h2o_to_add <- (total_ts_managed_kg / (manure_dm / 100)) - total_mass_managed_corSS_kg

        total_manure_manag_kg <- total_mass_managed_kg + ifelse(h2o_to_add > 0, h2o_to_add, 0)

        vs_solid_loaded_kg_day <- ifelse(empty_day == 1, 0, total_vs_managed_kg)


        # missing the bedding

        total_mass_managed_kg <- total_manure_manag_kg

        total_ts_managed_kg <- total_ts_managed_kg

        total_vs_managed_kg <- vs_solid_loaded_kg_day



        total_mass_managed_corSS_kg <- total_mass_managed_kg #ifelse( bedding == "Sand", total_mass_managed_kg - 1.5 * (milking_cows + dry_cows + heifers),
        #total_mass_managed_kg)

        biod_ch4_yield_kg <- biodigester_ch4_yield(volatile_solids = total_vs_managed_kg, biodigester_efficiency = biodigester_ef)


        biod_ch4_vol_m3 <- biod_ch4_yield_kg / 0.657

        biod_co2_vol_m3 <- biod_ch4_vol_m3 / 60 * 40

        biod_co2_yield_kg <- biod_co2_vol_m3 * 1.87

        total_biogas_m3 <- biod_ch4_vol_m3 + biod_co2_vol_m3

        biogas_vs_ratio <- total_biogas_m3 / total_vs_managed_kg


        total_mass_digested_kg <- total_mass_managed_corSS_kg - biod_ch4_yield_kg - biod_co2_yield_kg

        digested_ts_kg <- total_ts_managed_kg - biod_ch4_yield_kg - biod_co2_yield_kg

        digested_vs_kg <- total_vs_managed_kg - biod_ch4_yield_kg - biod_co2_yield_kg

        # it's already no

        # solid storage

        #empty_days <- empty_day(empty_time = empty_time)


        empty_day <- empty_day(empty_time = empty_time)

        #temp_c <- temp_c

        enclosed_manure <- "no"


        # no solids

        #vs_solid_loaded_kg <- digested_vs_kg#ifelse(empty_day == 0, manure_vs_solids_after_sep_kg,
        #ifelse(type_manure == "solid" & empty_days == 0, digested_vs_kg, 0))

        #ch4_emissions_solid_storage_kg <- manure_ch4_emission_solid(volatile_solids = vs_solid_loaded_kg, temp_c = temp_c)

        # liquid storage

        vs_liq_loaded_kg_day <- digested_vs_kg #ifelse(solid_liquid == "yes", manure_vs_liquids_after_sep_kg,
        #ifelse(type_manure == "Slurry", digested_vs_kg, 0))

        #vs_liq_loaded_kg_day <- rep(vs_liq_loaded_kg_day, 730)

        remaining_vs_tank_pct <- 0.05

        tank_capacity <- 365 * vs_liq_loaded_kg_day[1]

        vs_liq_loaded_cum_kg <- vector(length = 730)

        vs_liq_loaded_cum_kg[1] <- vs_liq_loaded_kg_day[1]

        vs_liq_deg <- rep(NA, 730); vs_liq_ndeg <- rep(NA, 730); vs_liq_loss_kg_day <- rep(NA, 730); vs_liq_loss_cum_kg <- rep(NA, 730);
        vs_liq_total_cum_kg <- rep(NA, 730); ch4_liq_emission_kg_day <- rep(NA, 730); co2_liq_emission_kg <- rep(NA, 730)


        for (i in 2:730) {

          # required parameters

          B_o <- 0.2

          ch4_pot <- 0.48

          # first row calculations to initialize all calculations

          vs_liq_loaded_cum_kg[1] <- vs_liq_loaded_kg_day[1]

          vs_liq_total_cum_kg[1] <- vs_liq_loaded_kg_day[1]

          vs_liq_loss_kg_day[1] <- 0

          vs_liq_loss_cum_kg[1] <- 0

          vs_liq_deg[1] <- (vs_liq_loaded_cum_kg[1] * (B_o / ch4_pot) - vs_liq_loss_cum_kg[1]) / vs_liq_total_cum_kg[1]

          vs_liq_ndeg[1] <- 1 - vs_liq_deg[1]

          ch4_liq_emission_kg_day[1] <- manure_ch4_emission_slurry(volatile_solids_total = vs_liq_total_cum_kg[1],
                                                                   volatile_solids_d     = vs_liq_deg[1],
                                                                   volatile_solids_nd    = vs_liq_ndeg[1],
                                                                   temp_c                = temp_c[1],
                                                                   enclosed              = enclosed_manure)

          co2_liq_emission_kg[1] <- ifelse(enclosed_manure == "no", 0, ch4_liq_emission_kg_day[1] * 2.75)

          # calculations for the rest of the vectors

          # cuidado empty days --> empty_day
          vs_liq_loaded_cum_kg[i] <- ifelse(empty_day[i] == 0, vs_liq_loaded_kg_day[i] + vs_liq_loaded_cum_kg[i - 1],
                                                    (remaining_vs_tank_pct / 100 * tank_capacity))

          vs_liq_loss_kg_day[i] <- ch4_liq_emission_kg_day[i - 1] * 3

          vs_liq_loss_cum_kg[i] <- ifelse(vs_liq_loaded_cum_kg[i] <= (remaining_vs_tank_pct / 100 * tank_capacity), 0, vs_liq_loss_kg_day[i] + vs_liq_loss_cum_kg[i - 1])

          vs_liq_total_cum_kg[i] <- ifelse(vs_liq_loaded_cum_kg[i] <= (remaining_vs_tank_pct / 100 * tank_capacity), vs_liq_loaded_cum_kg[i], vs_liq_loaded_cum_kg[i] - vs_liq_loss_cum_kg[i - 1])

          vs_liq_deg[i] <- ifelse(vs_liq_loaded_cum_kg[i] <= (remaining_vs_tank_pct / 100 * tank_capacity), (vs_liq_loaded_cum_kg[i] * (B_o / ch4_pot) - 0) / vs_liq_total_cum_kg[i],
                                          (vs_liq_loaded_cum_kg[i] * (B_o / ch4_pot) - vs_liq_loss_cum_kg[i]) / vs_liq_total_cum_kg[i])

          vs_liq_ndeg[i] <- 1 - vs_liq_deg[i]

          ch4_liq_emission_kg_day[i] <- manure_ch4_emission_slurry(volatile_solids_total = vs_liq_total_cum_kg[i],
                                                                   volatile_solids_d     = vs_liq_deg[i],
                                                                   volatile_solids_nd    = vs_liq_ndeg[i],
                                                                   temp_c                = temp_c[i],
                                                                   enclosed              = enclosed_manure)

          co2_liq_emission_kg[i] <- ifelse(enclosed_manure == "no", 0, ch4_liq_emission_kg_day[i] * 2.75)

        }

        tabela_outputs <- tibble::tibble(
          year_day                 = yday,
          temp_c                   = temp_c,
          facilitie                = facilitie,
          area_exposed_m2          = area_exposed_m2,
          type_manure              = type_manure,
          manure_management        = manure_management,
          empty_day                = empty_day,#empty_days,
          manure_dm                = manure_dm,
          crust                    = crust,
          total_urine_kg           = total_urine_kg,
          total_tan_kg             = total_tan_kg,
          fecal_n_kg               = total_fecal_n_kg,
          total_manure_manag_kg    = total_manure_manag_kg,
          vs_liq_loaded_kg_day     = vs_liq_loaded_kg_day,
          herd_ch4_emissions_kg    = herd_ch4_emissions_kg,
          barn_ch4_emissions_kg    = barn_ch4_emissions_kg,
          storage_ch4_emissions_kg = ch4_liq_emission_kg_day

        )

        tabela_outputs

      } else if (manure_management == "Biodigester + Solid-liquid Separator") {

        req(biodigester_ef)

        req(empty_time)

        empty_day <- empty_day(empty_time = empty_time)

        manure_dm <- manure_dm()

        h2o_to_add <- (total_ts_managed_kg / (manure_dm / 100)) - total_mass_managed_corSS_kg


        total_manure_manag_kg <- total_mass_managed_kg + ifelse(h2o_to_add > 0, h2o_to_add, 0)

        vs_solid_loaded_kg_day <- ifelse(empty_day == 1, 0, total_vs_managed_kg)


        # missing the bedding

        total_mass_managed_kg <- total_manure_manag_kg

        total_ts_managed_kg <- total_ts_managed_kg

        total_vs_managed_kg <- vs_solid_loaded_kg_day


        total_mass_managed_corSS_kg <- total_mass_managed_kg #ifelse( bedding == "Sand", total_mass_managed_kg - 1.5 * (milking_cows + dry_cows + heifers),
        #total_mass_managed_kg)

        biod_ch4_yield_kg <- biodigester_ch4_yield(volatile_solids = total_vs_managed_kg, biodigester_efficiency = biodigester_ef)


        biod_ch4_vol_m3 <- biod_ch4_yield_kg / 0.657

        biod_co2_vol_m3 <- biod_ch4_vol_m3 / 60 * 40

        biod_co2_yield_kg <- biod_co2_vol_m3 * 1.87

        total_biogas_m3 <- biod_ch4_vol_m3 + biod_co2_vol_m3

        biogas_vs_ratio <- total_biogas_m3 / total_vs_managed_kg


        total_mass_digested_kg <- total_mass_managed_corSS_kg - biod_ch4_yield_kg - biod_co2_yield_kg

        digested_ts_kg <- total_ts_managed_kg - biod_ch4_yield_kg - biod_co2_yield_kg

        digested_vs_kg <- total_vs_managed_kg - biod_ch4_yield_kg - biod_co2_yield_kg

        # it's already yes
        manure_solids_after_sep_pct <- 10.5 #ifelse(solid_liquid == "no", 0, 10.5)

        manure_solids_after_sep_kg <- manure_solids_after_sep_pct / 100 * total_mass_digested_kg

        manure_liquids_after_sep_pct <- 89.5#ifelse(solid_liquid == "no", 0, 89.5)

        manure_liquids_after_sep_kg <- manure_liquids_after_sep_pct / 100 * total_mass_digested_kg

        manure_ts_solids_after_sep_pct <- 42#ifelse(solid_liquid == "no", 0, 42)

        manure_ts_solids_after_sep_kg <- manure_ts_solids_after_sep_pct / 100 * digested_ts_kg

        manure_ts_liquids_after_sep_pct <- 58#ifelse(solid_liquid == "no", 0, 58)

        manure_ts_liquids_after_sep_kg <- manure_ts_liquids_after_sep_pct / 100 * digested_ts_kg

        manure_vs_solids_after_sep_pct <- 47#ifelse(solid_liquid == "no", 0, 47)

        manure_vs_solids_after_sep_kg <- manure_vs_solids_after_sep_pct / 100 * digested_vs_kg

        manure_vs_liquids_after_sep_pct <- 53#ifelse(solid_liquid == "no", 0, 53)

        manure_vs_liquids_after_sep_kg <- manure_vs_liquids_after_sep_pct / 100 * digested_vs_kg


        ts_solids_final_after_sep_pct <- manure_ts_solids_after_sep_kg / manure_solids_after_sep_kg * 100#ifelse(solid_liquid == "no", 0, manure_ts_solids_after_sep_kg / manure_solids_after_sep_kg * 100)

        ts_liquids_final_after_sep_pct <- manure_ts_liquids_after_sep_kg / manure_liquids_after_sep_kg * 100#ifelse(solid_liquid == "no", 0, manure_ts_liquids_after_sep_kg / manure_liquids_after_sep_kg * 100)

        vs_solids_final_after_sep_pct <- manure_vs_solids_after_sep_kg / manure_solids_after_sep_kg * 100#ifelse(solid_liquid == "no", 0, manure_vs_solids_after_sep_kg / manure_solids_after_sep_kg * 100)

        vs_liquids_final_after_sep_pct <- manure_vs_liquids_after_sep_kg / manure_liquids_after_sep_kg * 100#ifelse(solid_liquid == "no", 0, manure_vs_liquids_after_sep_kg / manure_liquids_after_sep_kg * 100)

        # solid storage

        #empty_days <- empty_day(empty_time = empty_time)


        #empty_day <- empty_days()

        #temp_c <- temp_c()

        enclosed_manure <- "no"

        vs_solid_loaded_kg <- manure_vs_solids_after_sep_kg#ifelse(empty_day == 0, manure_vs_solids_after_sep_kg,
        #ifelse(type_manure == "solid" & empty_days == 0, digested_vs_kg, 0))

        ch4_emissions_solid_storage_kg <- manure_ch4_emission_solid(volatile_solids = vs_solid_loaded_kg, temp_c = temp_c)

        # liquid storage

        vs_liq_loaded_kg_day <- manure_vs_liquids_after_sep_kg #ifelse(solid_liquid == "yes", manure_vs_liquids_after_sep_kg,
        #ifelse(type_manure == "Slurry", digested_vs_kg, 0))

        #vs_liq_loaded_kg_day <- rep(vs_liq_loaded_kg_day, 730)

        remaining_vs_tank_pct <- 0.05

        tank_capacity <- 365 * vs_liq_loaded_kg_day[1]

        vs_liq_loaded_cum_kg <- vector(length = 730)

        vs_liq_loaded_cum_kg[1] <- vs_liq_loaded_kg_day[1]

        vs_liq_deg <- rep(NA, 730); vs_liq_ndeg <- rep(NA, 730); vs_liq_loss_kg_day <- rep(NA, 730); vs_liq_loss_cum_kg <- rep(NA, 730);
        vs_liq_total_cum_kg <- rep(NA, 730); ch4_liq_emission_kg_day <- rep(NA, 730); co2_liq_emission_kg <- rep(NA, 730)


        for (i in 2:730) {

          # required parameters

          B_o <- 0.2

          ch4_pot <- 0.48

          # first row calculations to initialize all calculations

          vs_liq_loaded_cum_kg[1] <- vs_liq_loaded_kg_day[1]

          vs_liq_total_cum_kg[1] <- vs_liq_loaded_kg_day[1]

          vs_liq_loss_kg_day[1] <- 0

          vs_liq_loss_cum_kg[1] <- 0

          vs_liq_deg[1] <- (vs_liq_loaded_cum_kg[1] * (B_o / ch4_pot) - vs_liq_loss_cum_kg[1]) / vs_liq_total_cum_kg[1]

          vs_liq_ndeg[1] <- 1 - vs_liq_deg[1]

          ch4_liq_emission_kg_day[1] <- manure_ch4_emission_slurry(volatile_solids_total = vs_liq_total_cum_kg[1],
                                                                   volatile_solids_d     = vs_liq_deg[1],
                                                                   volatile_solids_nd    = vs_liq_ndeg[1],
                                                                   temp_c                = temp_c[1],
                                                                   enclosed              = enclosed_manure)

          co2_liq_emission_kg[1] <- ifelse(enclosed_manure == "no", 0, ch4_liq_emission_kg_day[1] * 2.75)

          # calculations for the rest of the vectors

          # cuidado empty days --> empty_day
          vs_liq_loaded_cum_kg[i] <- ifelse(empty_day[i] == 0, vs_liq_loaded_kg_day[i] + vs_liq_loaded_cum_kg[i - 1],
                                                    (remaining_vs_tank_pct / 100 * tank_capacity))

          vs_liq_loss_kg_day[i] <- ch4_liq_emission_kg_day[i - 1] * 3

          vs_liq_loss_cum_kg[i] <- ifelse(vs_liq_loaded_cum_kg[i] <= (remaining_vs_tank_pct / 100 * tank_capacity), 0, vs_liq_loss_kg_day[i] + vs_liq_loss_cum_kg[i - 1])

          vs_liq_total_cum_kg[i] <- ifelse(vs_liq_loaded_cum_kg[i] <= (remaining_vs_tank_pct / 100 * tank_capacity), vs_liq_loaded_cum_kg[i], vs_liq_loaded_cum_kg[i] - vs_liq_loss_cum_kg[i - 1])

          vs_liq_deg[i] <- ifelse(vs_liq_loaded_cum_kg[i] <= (remaining_vs_tank_pct / 100 * tank_capacity), (vs_liq_loaded_cum_kg[i] * (B_o / ch4_pot) - 0) / vs_liq_total_cum_kg[i],
                                          (vs_liq_loaded_cum_kg[i] * (B_o / ch4_pot) - vs_liq_loss_cum_kg[i]) / vs_liq_total_cum_kg[i])

          vs_liq_ndeg[i] <- 1 - vs_liq_deg[i]

          ch4_liq_emission_kg_day[i] <- manure_ch4_emission_slurry(volatile_solids_total = vs_liq_total_cum_kg[i],
                                                                   volatile_solids_d     = vs_liq_deg[i],
                                                                   volatile_solids_nd    = vs_liq_ndeg[i],
                                                                   temp_c                = temp_c[i],
                                                                   enclosed              = enclosed_manure)

          co2_liq_emission_kg[i] <- ifelse(enclosed_manure == "no", 0, ch4_liq_emission_kg_day[i] * 2.75)

        }


        tabela_outputs <- tibble::tibble(
          year_day                 = yday,
          temp_c                   = temp_c,
          facilitie                = facilitie,
          area_exposed_m2          = area_exposed_m2,
          type_manure              = type_manure,
          manure_management        = manure_management,
          empty_day                = empty_day,#empty_days,
          manure_dm                = manure_dm,
          crust                    = crust,
          total_urine_kg           = total_urine_kg,
          total_tan_kg             = total_tan_kg,
          fecal_n_kg               = total_fecal_n_kg,
          total_manure_manag_kg    = total_manure_manag_kg,
          vs_liq_loaded_kg_day     = vs_liq_loaded_kg_day,
          herd_ch4_emissions_kg    = herd_ch4_emissions_kg,
          barn_ch4_emissions_kg    = barn_ch4_emissions_kg,
          storage_ch4_emissions_kg = ch4_liq_emission_kg_day + ch4_emissions_solid_storage_kg
        )

        tabela_outputs

      }

    })

    summarized_data <- reactive({

      emissions() %>%
        dplyr::filter(year_day > 365) %>%
        dplyr::summarise(
          total_ch4_herd     = sum(herd_ch4_emissions_kg),
          total_ch4_fac      = sum(barn_ch4_emissions_kg),
          total_ch4_storage  = sum(storage_ch4_emissions_kg)

        )
    })

    output$teste2 <- renderTable({

      #emissions()[1:10, ]

    })

# -------------------------------------------------------------------------
# Total Manure Managed ----------------------------------------------------
# -------------------------------------------------------------------------

    output$total_manure_managed <- bs4Dash::renderValueBox({

      total_manure <- ifelse(is.null(emissions()$total_manure_manag_kg[1]) | is.na(emissions()$total_manure_manag_kg[1]), 29, emissions()$total_manure_manag_kg[1])

      value_box_spark(
        value    = round(total_manure / 1000, 2),
        title    = "Total Manure Produced Daily (Ton.)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

# -------------------------------------------------------------------------
# Methane Emissions -------------------------------------------------------
# -------------------------------------------------------------------------

    output$herd_methane <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(summarized_data()[["total_ch4_herd"]] / 1000, 2),
        title    = "Total Herd Methane Emissions (Ton./year)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    output$facilitie_methane <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(summarized_data()[["total_ch4_fac"]] / 1000, 2),
        title    = "Total Barn Methane Emissions (Ton./year)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    output$manure_storage_methane <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(sum(summarized_data()[["total_ch4_storage"]] / 1000, na.rm = TRUE), 2),
        title    = "Total Storage Methane Emissions (Ton./year)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

# -------------------------------------------------------------------------
# Methane plots -----------------------------------------------------------
# -------------------------------------------------------------------------

    # Barn CH4

    output$barn_ch4_chart <- plotly::renderPlotly({

     emissions <- emissions() %>%
       tibble::as_tibble() %>%
       dplyr::filter(year_day > 365) %>%
       dplyr::mutate(
         yday = year_day - 365
       )

      fig <- emissions %>%
        plotly::plot_ly(x = ~ yday,
                        y = ~ round(barn_ch4_emissions_kg, 2),
                        name = "CH4",
                        type = "scatter",
                        mode = "lines+markers") %>%
        plotly::config(displayModeBar = FALSE)


      ay <- list(
        tickfont = list(color = "black"),
        overlaying = "y",
        side = "right",
        title = "Temp. (C)")

      fig <- fig %>%
        plotly::add_trace(x     = ~1:365,
                          y     = ~round(temp_c[1:365], 1),
                          name  = "Temp.",
                          yaxis = "y2",
                          mode  = "lines+markers",
                          type  = "scatter")

      fig <- fig %>%
        plotly::layout(
        title  = " ",
        yaxis2 = ay,
        xaxis  = list(title = "Year Days"),
        yaxis  = list(title = "Daily Methane Emissions From Barn (kg)")
      ) %>%
        plotly::layout(plot_bgcolor = 'white',
               xaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor     = 'ffff'),
               yaxis = list(
                 zerolinecolor = '#ffff',
                 zerolinewidth = 2,
                 gridcolor     = 'ffff',
                 range = c(0, 5)
        )) %>%
        plotly::layout(legend = list(itemsizing = 'constant'))

    })

    # Manure Storage

    output$storage_ch4_chart <- plotly::renderPlotly({

      emissions <- emissions() %>%
        tibble::as_tibble() %>%
        dplyr::filter(year_day > 365) %>%
        dplyr::mutate(
          yday = year_day - 365,
          total_storage_kg = storage_ch4_emissions_kg
        )

      fig <- emissions %>%
        plotly::plot_ly(x    = ~ yday,
                        y    = ~ round(total_storage_kg, 2),
                        name = "CH4",
                        type = "scatter",
                        mode = "lines+markers") %>%
        plotly::config(displayModeBar = FALSE)

      ay <- list(
        tickfont   = list(color = "black"),
        overlaying = "y",
        side       = "right",
        title      = "Temp. (C)")

      fig <- fig %>%
        plotly::add_trace(x     = ~1:365,
                          y     = ~round(temp_c[1:365], 1),
                          name  = "Temp.",
                          yaxis = "y2",
                          mode  = "lines+markers",
                          type  = "scatter")

      fig <- fig %>%
        plotly::layout(
          title  = " ",
          yaxis2 = ay,
          xaxis  = list(title = "Year Days"),
          yaxis  = list(title = "Daily Methane Emissions From Storage (kg)")
        ) %>%
        plotly::layout(plot_bgcolor = 'white',
                       xaxis = list(
                         zerolinecolor = '#ffff',
                         zerolinewidth = 2,
                         gridcolor = 'ffff'),
                       yaxis = list(
                         zerolinecolor = '#ffff',
                         zerolinewidth = 2,
                         gridcolor = 'ffff')
        ) %>%
        plotly::layout(legend = list(itemsizing = 'constant'))

    })

# -------------------------------------------------------------------------
# Ammonia emissions -------------------------------------------------------
# -------------------------------------------------------------------------

    nh3_emissions <- reactive({

      # Barn NH3 emissions

      manure_density <- manure_density(manure_dm())

      emissions() %>%
        dplyr::mutate(

          manure_solution_mass = total_urine_kg / area_exposed_m2,
          manure_solution_pH   = 7.7,
          gamma_densi          = manure_density,
          const                = 86400,
          Q                    = eq_coeff(temp_c = temp_c, pH = manure_solution_pH),
          r                    = resistence_nh3(hsc = 260, temp_c = temp_c),
          tan_kg_m2            = total_tan_kg / area_exposed_m2,
          loss_kg_m2           = tan_kg_m2 * const * gamma_densi / (r * manure_solution_mass * Q),
          loss_animal_kg       = loss_kg_m2 * area_exposed_m2,
          cum_loss             = cumsum(loss_animal_kg)
        )

    })


    # Ammonia emissions

    storage <- reactive({

# -------------------------------------------------------------------------
# conditioning the inputs to NULL values. This is necessary because they
# depend on inputs from other parts oh the App - they are not direct inputs
# from this module. Otherwise the function 'req()' would work.
# -------------------------------------------------------------------------

      empty_time <- ifelse(is.null(empty_time()), "Fall and Spring", empty_time())

      manure_management <- ifelse(is.null(manure_management()), "Pond or Tank Storage", manure_management())

      type_manure <- ifelse(is.null(type_manure()), "Slurry", type_manure())

      mineralization_pct <- ifelse(empty_time == "Fall" | empty_time == "Spring", 0.25, 0.12)

      crust <- ifelse(manure_management == "Biodigester + Solid-liquid Separator" | manure_management == "Biodigester", "no",
                      ifelse(manure_management == "Pond or Tank Storage", "no", crust()))

# -------------------------------------------------------------------------

      teste <- nh3_emissions() %>%
        dplyr::mutate(

          mineralization_pct = mineralization_pct,

          n_mineralized_feces = ifelse(manure_management == "Biodigester" | manure_management == "Biodigester + Solid-liquid Separator",
                                               mineralization_pct * fecal_n_kg + 0.16 * fecal_n_kg, mineralization_pct * fecal_n_kg),

          remaining_tan_kg = total_tan_kg - loss_animal_kg + n_mineralized_feces,

          manure_sol_loaded = (manure_dm) * total_manure_manag_kg / 100, #TODO check manure mass

          total_nitrogen_storage_kg = (fecal_n_kg - n_mineralized_feces)  + remaining_tan_kg
        )

      tank_capacity <- 365 * teste$manure_sol_loaded[1]

      volume_diario <- teste$remaining_tan_kg[1]


      acumulado <- vector(length =  730)

      acumulado[1] <- volume_diario

      tank_cap <- 365 * volume_diario

      empty_days <- empty_day(empty_time = empty_time)

      for (i in 2:length(empty_days)) {

        if(empty_days[i] == 0) {

          acumulado[i] <- acumulado[i - 1] + volume_diario

        } else {

          acumulado[i] <- tank_cap * 0 + volume_diario

        }


      }

      acumulado


      # logic for cumul manure

      #volume_diario_man <- teste$manure_sol_loaded[1]
      volume_diario_man <- teste$total_manure_manag_kg[1]

      acumulado_manure <- vector(length =  730)

      acumulado_manure[1] <- volume_diario_man

      tank_cap2 <- 365 * volume_diario_man

      for (i in 2:length(empty_days)) {

        if(empty_days[i] == 0) {

          acumulado_manure[i] <- acumulado_manure[i - 1] + volume_diario_man

        } else {

          acumulado_manure[i] <- tank_cap2 * 0.01 + volume_diario_man

        }

      }

      acumulado_manure


       temp_c <- rep( wisconsin_weather_data %>%
                       dplyr::filter(county == county()) %>%
                       dplyr::pull(aver_tempC), 2)

      # r for storage

       if (crust == "no" & type_manure == "Slurry") {

         r_storage <- 19

       } else if (crust == "yes" & type_manure == "Slurry") {

         r_storage <- 75

       } else if (crust == "yes" & type_manure == "Liquid") {

         r_storage <- 75

       } else if (crust == "no" & type_manure == "Liquid") {

         r_storage <- 19

       } else {

         r_storage <- 10

       }

       r_storage

      ph_storage <- 7.5

      Q_storage <- eq_coeff(temp_c = temp_c, pH = ph_storage)

      storage_area <- ifelse(is.null(manure_storage_area()), 1360, manure_storage_area())

      gamma_densi <- 1000

      const <- 86400

      storage_N_loss_m2 <- acumulado * const * gamma_densi / (r_storage * acumulado_manure * Q_storage)

      cum_N_loss_m2 <- cumsum(storage_N_loss_m2)

      teste %>%
        dplyr::mutate(
          temp_c = temp_c,
          acumulado = acumulado,
          const = const,
          gamma_densi = gamma_densi,
          r_storage = r_storage,
          acumulado_manure = acumulado_manure,
          Q_storage = Q_storage,
          storage_N_loss_m2 = storage_N_loss_m2,
          cum_tan_kg = cumsum(remaining_tan_kg),
          total_storage_N_loss = ifelse(manure_management == "Daily Hauling", 0, storage_N_loss_m2 * storage_area)
        )

    })


  output$teste2 <- renderTable({

    storage()

  })

  n2o_from_storage <- reactive({

    if (manure_management() == "Daily Hauling") {

      0

    } else {

      sum(storage()$total_nitrogen_storage_kg[366:730]) * 0.005 / 0.64

    }

  })

  barn_nh3 <- reactive({

    total_nh3 <- nh3_emissions() %>%
      dplyr::filter(year_day <=365) %>%
      dplyr::summarise(
        total_nh3 = round(sum(loss_animal_kg), 1)
      ) %>%
      dplyr::pull(total_nh3)

    total_nh3

  })

  storage_nh3 <- reactive({

    total_nh3 <- storage() %>%
      dplyr::filter(year_day > 365) %>%
      dplyr::summarise(
        total_nh3 = round(sum(total_storage_N_loss), 1)
      ) %>%
      dplyr::pull(total_nh3)

    total_nh3

  })

    output$barn_nh3 <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(barn_nh3() / 0.82 / 1000, 2),
        title    = "Barn Ammonia Emissions (Ton./year)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    output$storage_nh3 <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(storage_nh3() / 0.82 / 1000, 2),
        title    = "Storage Ammonia Emissions (Ton./year)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    output$total_n2o <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(((storage_nh3() + barn_nh3()) / 100 / 0.64 + n2o_from_storage()) / 1000, 2),
        title    = "Nitrous Oxide Emissions (Ton./year)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    # nitrogen curves

    output$barn_nh3_chart <- plotly::renderPlotly({

      emissions <- nh3_emissions() %>%
        tibble::as_tibble() %>%
        dplyr::filter(year_day <= 365)

      fig <- emissions %>%
        plotly::plot_ly(x = ~ year_day,
                        y = ~ round(loss_animal_kg, 5),
                        name = "NH3",
                        type = "scatter",
                        mode = "lines+markers") %>%
        plotly::config(displayModeBar = FALSE)


      ay <- list(
        tickfont = list(color = "black"),
        overlaying = "y",
        side = "right",
        title = "Temp. (C)")

      fig <- fig %>%
        plotly::add_trace(x     = ~1:365,
                          y     = ~round(temp_c[1:365], 1),
                          name  = "Temp.",
                          yaxis = "y2",
                          mode  = "lines+markers",
                          type  = "scatter")

      fig <- fig %>%
        plotly::layout(
          title  = " ",
          yaxis2 = ay,
          xaxis  = list(title = "Year Days"),
          yaxis  = list(title = "Daily Ammonia Emissions From Barn (kg)")
        ) %>%
        plotly::layout(plot_bgcolor = 'white',
                       xaxis = list(
                         zerolinecolor = '#ffff',
                         zerolinewidth = 2,
                         gridcolor     = 'ffff'),
                       yaxis = list(
                         zerolinecolor = '#ffff',
                         zerolinewidth = 2,
                         gridcolor     = 'ffff')
        ) %>%
        plotly::layout(legend = list(itemsizing = 'constant'))


      })

    output$storage_nh3_chart <- plotly::renderPlotly({

      emissions <- storage() %>%
        tibble::as_tibble() %>%
        dplyr::filter(year_day > 365) %>%
        dplyr::mutate(
          year_day = year_day - 365
        )

      fig <- emissions %>%
        plotly::plot_ly(x = ~ year_day,
                        y = ~ round(total_storage_N_loss, 5),
                        name = "NH3",
                        type = "scatter",
                        mode = "lines+markers") %>%
        plotly::config(displayModeBar = FALSE)


      ay <- list(
        tickfont = list(color = "black"),
        overlaying = "y",
        side = "right",
        title = "Temp. (C)")

      fig <- fig %>%
        plotly::add_trace(x     = ~1:365,
                          y     = ~round(temp_c[1:365], 1),
                          name  = "Temp.",
                          yaxis = "y2",
                          mode  = "lines+markers",
                          type  = "scatter")

      fig <- fig %>%
        plotly::layout(
          title  = " ",
          yaxis2 = ay,
          xaxis  = list(title = "Year Days"),
          yaxis  = list(title = "Daily Ammonia Emissions From Storage (kg)")
        ) %>%
        plotly::layout(plot_bgcolor = 'white',
                       xaxis = list(
                         zerolinecolor = '#ffff',
                         zerolinewidth = 2,
                         gridcolor     = 'ffff'),
                       yaxis = list(
                         zerolinecolor = '#ffff',
                         zerolinewidth = 2,
                         gridcolor     = 'ffff')
        ) %>%
        plotly::layout(legend = list(itemsizing = 'constant'))


    })


# -------------------------------------------------------------------------
# Weather data plot
# -------------------------------------------------------------------------

    output$weather_chart <- plotly::renderPlotly({

      weather <- wisconsin_weather_data %>%
        dplyr::filter(county == county())

      # Mean temperature

      fig <- weather %>%
        plotly::plot_ly(x    = ~ yday,
                        y    = ~ round(aver_tempC, 1),
                        name = "Mean Temper. (C)",
                        type = "scatter",
                        mode = "lines+markers") %>%
        plotly::config(displayModeBar = FALSE)

      # Min temperature

      fig <- fig %>%
        plotly::add_trace(x     = ~yday,
                          y     = ~round(min_tempC, 1),
                          name  = "Min. Temper. (C)",
                          yaxis = "y1",
                          mode  = "lines+markers",
                          type  = "scatter")
      #  Max temperature

      fig <- fig %>%
        plotly::add_trace(x     = ~yday,
                          y     = ~round(max_tempC, 1),
                          name  = "Max. Temper. (C)",
                          yaxis = "y1",
                          mode  = "lines+markers",
                          type  = "scatter")
      # Precipitation

      ay <- list(
        tickfont   = list(color = "black"),
        overlaying = "y",
        side       = "right",
        title      = "Precip. (mm)")

      fig <- fig %>%
        plotly::add_trace(x     = ~yday,
                          y     = ~round(precip_mm, 1),
                          name  = "Precip. (mm)",
                          yaxis = "y2",
                          mode  = "lines+markers",
                          type  = "scatter")

      fig <- fig %>%
        plotly::layout(
          title  = " ",
          yaxis2 = ay,
          xaxis  = list(title = "Year Days"),
          yaxis  = list(title = "Average Temperature (C)")
        ) %>%
        plotly::layout(plot_bgcolor = 'white',
                       xaxis = list(
                         zerolinecolor = '#ffff',
                         zerolinewidth = 2,
                         gridcolor = 'ffff'),
                       yaxis = list(
                         zerolinecolor = '#ffff',
                         zerolinewidth = 2,
                         gridcolor = 'ffff')
        ) %>%
        plotly::layout(legend = list(itemsizing = 'constant'))

    })

# -------------------------------------------------------------------------
# Report outcomes - inputs ------------------------------------------------
# -------------------------------------------------------------------------

    manure_inputs <- reactive({

      manure_inputs <- tibble::tibble(

        "County"            = county(),
        "Facility"          = facilitie(),
        "Bedding Type"      = bedding(),
        "Manure Management" = manure_management(),
        "Manure Type"       = type_manure(),
        "Empty Time"        = empty_time()
      )

      manure_inputs

    })

# -------------------------------------------------------------------------
# Outputs from this module to populate others -----------------------------
# -------------------------------------------------------------------------

    return(
      list(
        manure_inputs      = reactive(manure_inputs()),
        nh3_emissions      = reactive(storage()),
        herd_methane       = reactive(summarized_data()[["total_ch4_herd"]]),
        fac_methane        = reactive(summarized_data()[["total_ch4_fac"]]),
        storage_methane    = reactive(summarized_data()[["total_ch4_storage"]]),
        fac_ammonia        = reactive(barn_nh3()),
        storage_ammonia    = reactive(storage_nh3()),
        direct_storage_n2o = reactive(n2o_from_storage())
      )
    )

  })
}
