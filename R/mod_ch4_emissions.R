#' ch4_emissions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ch4_emissions_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      bs4Dash::bs4Card(
        title = "Manure Handling and Management",
        elevation = 2,
        width = 12,
        solidHeader = TRUE,
        status = "teal",
        collapsible = TRUE,
        fluidRow(
          general_ui_prms()))),

    fluidRow(
      bs4Dash::bs4Card(
        title = "Manure and GHG Emissions Dashboard",
        elevation = 2,
        width = 12,
        solidHeader = TRUE,
        status = "teal",
        collapsible = TRUE,
        fluidRow(
          bs4Dash::valueBoxOutput(ns("herd_methane")),
          bs4Dash::valueBoxOutput(ns("facilitie_methane")),
          bs4Dash::valueBoxOutput(ns("manure_storage_methane")),
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
                  plotly::plotlyOutput(ns("storage_ch4_chart")))

              )),
            tabPanel(
              title = "Ammonia Emissions",
              fluidRow(
                "Add the content"
                #plotly::plotlyOutput(ns("storage_ch4_chart"))
              )),
            tabPanel(
              title = "Distribution of Emissions",
              fluidRow(
                bs4Dash::bs4Card(
                  title = "Methane",
                  width = 6,
                  footer = NULL,
                plotly::plotlyOutput(ns("pie_chart")))
              ))
            )
          #tableOutput(ns("tabela"))
          ))),

    # fluidRow(
    #   bs4Dash::bs4Card(
    #     title = "Plots",
    #     elevation = 4,
    #     width = 12,
    #     solidHeader = TRUE,
    #     status = "navy",
    #     collapsible = TRUE,
    #     fluidRow(
    #       plotOutput(ns("plot")))))


  )
}

#' ch4_emissions Server Functions
#'
#' @noRd
mod_ch4_emissions_server <- function(id,
                                     animal_data,
                                     county,
                                     facilitie,
                                     bedding,
                                     biodigester,
                                     biodigester_ef,
                                     type_manure,
                                     solid_liquid,
                                     enclosed_manure,
                                     empty_time){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # all logic without connect with the animal part

    emissions <- reactive({

      animal_data <- animal_data()

      county <- county()

      facilitie <- facilitie()

      bedding <- bedding()

      biodigester <- biodigester()

      biodigester_ef <- biodigester_ef()

      type_manure <- type_manure()

      solid_liquid <- solid_liquid()

      enclosed_manure <- enclosed_manure()

      empty_time <- empty_time()

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

      #milking_cows_manure <- 65
      #dry_cows_manure <- 25
      #heifers_manure <- 21

      #milking_cows_ts <- 0.0875
      #dry_cows_ts <- 0.0443
      #heifers_ts <- 0.0459

      #total_manure_kg <- milking_cows * milking_cows_manure + dry_cows * dry_cows_manure + heifers * heifers_manure
      total_manure_kg <- animal_data %>%
        dplyr::summarise(
          total_manure = sum(total_manure_kg)
        ) %>%
        dplyr::pull(total_manure)

      print(total_manure_kg)

      #total_ts_manure_kg <- milking_cows * milking_cows_manure * milking_cows_ts + dry_cows * dry_cows_manure * dry_cows_ts + heifers * heifers_manure * heifers_ts
      total_ts_manure_kg <- animal_data %>%
        dplyr::summarise(
          ts_manure = sum(total_solids_kg)
        ) %>%
        dplyr::pull(ts_manure)

      print(total_ts_manure_kg)

      #total_vs_manure_kg <- (milking_cows * milking_cows_manure * milking_cows_ts + dry_cows * dry_cows_manure * dry_cows_ts + heifers * heifers_manure * heifers_ts) * 0.8

      total_vs_manure_kg <- animal_data %>%
        dplyr::summarise(
          vs_manure = sum(total_volatile_solids_kg)
        ) %>%
        dplyr::pull(vs_manure)

      print(total_vs_manure_kg)



      yday <- seq(1, 730, 1)

      temp_c <- rep(wi_weather %>%
                      dplyr::filter(county == county()) %>%
                      dplyr::pull(aver_tempC), 2)

      #herd_ch4_emissions_kg <- milking_cows * 0.464 + dry_cows * 0.261 + heifers * 0.156
      herd_ch4_emissions_kg <-  animal_data %>%
        dplyr::summarise(
          total_ch4_kg = sum(total_ch4_kg)
        ) %>%
        dplyr::pull(total_ch4_kg)

      print(herd_ch4_emissions_kg)

      area_exposed_m2 <- dplyr::if_else(facilitie == "freestall", milking_cows * 3.5 + dry_cows * 3.5 + heifers * 2.5,
                                        milking_cows * 1.5 + dry_cows * 1.5 + heifers * 2.5)

      barn_ch4_emissions_kg <- barn_ch4_emission_floor(temp_c = temp_c, manure_area = area_exposed_m2)


      # determining bedding quantity
      bedding_quantity_kg <- dplyr::if_else(bedding == "Sawdust", 2 * (milking_cows + dry_cows + heifers),
                                            dplyr::if_else(bedding == "Sand", 1.5 * (milking_cows + dry_cows + heifers),
                                            3 * (milking_cows + dry_cows + heifers)))

      bedding_ts_kg <- bedding_quantity_kg * 0.9

      bedding_vs_kg <- dplyr::if_else(bedding == "Sawdust", bedding_ts_kg * 0.8,
                                      dplyr::if_else(bedding == "Sand", bedding_ts_kg * 0,
                                                     bedding_ts_kg * 0.8))

      total_mass_managed_kg <- total_manure_kg + bedding_quantity_kg

      total_ts_managed_kg <- total_ts_manure_kg + bedding_ts_kg

      total_vs_managed_kg <- total_vs_manure_kg + bedding_vs_kg

      total_mass_managed_corSS_kg <- dplyr::if_else(bedding == "Sand", total_mass_managed_kg - 1.5 * (milking_cows + dry_cows + heifers),
                                                    total_mass_managed_kg)

      biod_ch4_yield_kg <- dplyr::if_else(biodigester == "yes", biodigester_ch4_yield(volatile_solids = total_vs_managed_kg, biodigester_efficiency = biodigester_ef),
                                          0)

      biod_ch4_vol_m3 <- biod_ch4_yield_kg / 0.657

      biod_co2_vol_m3 <- biod_ch4_vol_m3 / 60 * 40

      biod_co2_yield_kg <- biod_co2_vol_m3 * 1.87

      total_biogas_m3 <- biod_ch4_vol_m3 + biod_co2_vol_m3

      biogas_vs_ratio <- total_biogas_m3 / total_vs_managed_kg


      total_mass_digested_kg <- total_mass_managed_corSS_kg - biod_ch4_yield_kg - biod_co2_yield_kg

      digested_ts_kg <- total_ts_managed_kg - biod_ch4_yield_kg - biod_co2_yield_kg

      digested_vs_kg <- total_vs_managed_kg - biod_ch4_yield_kg - biod_co2_yield_kg


      manure_solids_after_sep_pct <- dplyr::if_else(solid_liquid == "no", 0, 10.5)

      manure_solids_after_sep_kg <- manure_solids_after_sep_pct / 100 * total_mass_digested_kg

      manure_liquids_after_sep_pct <- dplyr::if_else(solid_liquid == "no", 0, 89.5)

      manure_liquids_after_sep_kg <- manure_liquids_after_sep_pct / 100 * total_mass_digested_kg

      manure_ts_solids_after_sep_pct <- dplyr::if_else(solid_liquid == "no", 0, 42)

      manure_ts_solids_after_sep_kg <- manure_ts_solids_after_sep_pct / 100 * digested_ts_kg

      manure_ts_liquids_after_sep_pct <- dplyr::if_else(solid_liquid == "no", 0, 58)

      manure_ts_liquids_after_sep_kg <- manure_ts_liquids_after_sep_pct / 100 * digested_ts_kg

      manure_vs_solids_after_sep_pct <- dplyr::if_else(solid_liquid == "no", 0, 47)

      manure_vs_solids_after_sep_kg <- manure_vs_solids_after_sep_pct / 100 * digested_vs_kg

      manure_vs_liquids_after_sep_pct <- dplyr::if_else(solid_liquid == "no", 0, 53)

      manure_vs_liquids_after_sep_kg <- manure_vs_liquids_after_sep_pct / 100 * digested_vs_kg


      ts_solids_final_after_sep_pct <- dplyr::if_else(solid_liquid == "no", 0, manure_ts_solids_after_sep_kg / manure_solids_after_sep_kg * 100)

      ts_liquids_final_after_sep_pct <- dplyr::if_else(solid_liquid == "no", 0, manure_ts_liquids_after_sep_kg / manure_liquids_after_sep_kg * 100)

      vs_solids_final_after_sep_pct <- dplyr::if_else(solid_liquid == "no", 0, manure_vs_solids_after_sep_kg / manure_solids_after_sep_kg * 100)

      vs_liquids_final_after_sep_pct <- dplyr::if_else(solid_liquid == "no", 0, manure_vs_liquids_after_sep_kg / manure_liquids_after_sep_kg * 100)

      # solid storage
      #
      empty_days <- empty_day(empty_time = empty_time)

      vs_solid_loaded_kg <- dplyr::if_else(solid_liquid == "yes" & empty_days == 0, manure_vs_solids_after_sep_kg,
                                           dplyr::if_else(type_manure == "solid" & empty_days == 0, digested_vs_kg, 0))

      ch4_emissions_solid_storage_kg <- manure_ch4_emission_solid(volatile_solids = vs_solid_loaded_kg, temp_c = temp_c)

      # liquid storage

      vs_liq_loaded_kg_day <- dplyr::if_else(solid_liquid == "yes", manure_vs_liquids_after_sep_kg,
                                             dplyr::if_else(type_manure == "Slurry", digested_vs_kg, 0))

      vs_liq_loaded_kg_day <- rep(vs_liq_loaded_kg_day, 730)

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

        co2_liq_emission_kg[1] <- dplyr::if_else(enclosed_manure == "no", 0, ch4_liq_emission_kg_day[1] * 2.75)

        # calculations for the rest of the vectors

        vs_liq_loaded_cum_kg[i] <- dplyr::if_else(empty_days[i] == 0, vs_liq_loaded_kg_day[i] + vs_liq_loaded_cum_kg[i - 1],
                                                  (remaining_vs_tank_pct / 100 * tank_capacity))

        vs_liq_loss_kg_day[i] <- ch4_liq_emission_kg_day[i - 1] * 3

        vs_liq_loss_cum_kg[i] <- dplyr::if_else(vs_liq_loaded_cum_kg[i] <= (remaining_vs_tank_pct / 100 * tank_capacity), 0, vs_liq_loss_kg_day[i] + vs_liq_loss_cum_kg[i - 1])

        vs_liq_total_cum_kg[i] <- dplyr::if_else(vs_liq_loaded_cum_kg[i] <= (remaining_vs_tank_pct / 100 * tank_capacity), vs_liq_loaded_cum_kg[i], vs_liq_loaded_cum_kg[i] - vs_liq_loss_cum_kg[i - 1])

        vs_liq_deg[i] <- dplyr::if_else(vs_liq_loaded_cum_kg[i] <= (remaining_vs_tank_pct / 100 * tank_capacity), (vs_liq_loaded_cum_kg[i] * (B_o / ch4_pot) - 0) / vs_liq_total_cum_kg[i],
                                        (vs_liq_loaded_cum_kg[i] * (B_o / ch4_pot) - vs_liq_loss_cum_kg[i]) / vs_liq_total_cum_kg[i])

        vs_liq_ndeg[i] <- 1 - vs_liq_deg[i]

        ch4_liq_emission_kg_day[i] <- manure_ch4_emission_slurry(volatile_solids_total = vs_liq_total_cum_kg[i],
                                                                 volatile_solids_d     = vs_liq_deg[i],
                                                                 volatile_solids_nd    = vs_liq_ndeg[i],
                                                                 temp_c                = temp_c[i],
                                                                 enclosed              = enclosed_manure)

        co2_liq_emission_kg[i] <- dplyr::if_else(enclosed_manure == "no", 0, ch4_liq_emission_kg_day[i] * 2.75)

      }

      total_ts_remaing_kg <- total_ts_managed_kg - ch4_liq_emission_kg_day - ch4_emissions_solid_storage_kg



      emissions <- tibble::tibble(
        yday,
        temp_c,
        total_ts_remaing_kg,
        # herd_ch4_emissions_kg,
        # area_exposed_m2,
        # total_manure_kg,
        # total_ts_manure_kg,
        # bedding_quantity_kg,
        # bedding_ts_kg,
        # bedding_vs_kg,
        # total_vs_manure_kg,
        # total_mass_managed_kg,
        # total_ts_managed_kg,
        # total_vs_managed_kg,
        # total_mass_managed_corSS_kg,
        # biodigester,
        # biodigester_ef,
        # biod_ch4_yield_kg,
        # biod_ch4_vol_m3,
        # biod_co2_yield_kg,
        # biod_co2_vol_m3,
        # total_biogas_m3,
        # biogas_vs_ratio,
        # total_mass_digested_kg,
        # digested_ts_kg,
        # digested_vs_kg,
        #
        # manure_solids_after_sep_pct,
        # manure_solids_after_sep_kg,
        # manure_liquids_after_sep_pct,
        # manure_liquids_after_sep_kg,
        # manure_ts_solids_after_sep_pct,
        # manure_ts_solids_after_sep_kg,
        # manure_ts_liquids_after_sep_pct,
        # manure_ts_liquids_after_sep_kg,
        # manure_vs_solids_after_sep_pct,
        # manure_vs_solids_after_sep_kg,
        # manure_vs_liquids_after_sep_pct,
        # manure_vs_liquids_after_sep_kg,
        #
        # ts_solids_final_after_sep_pct,
        # ts_liquids_final_after_sep_pct,
        # vs_solids_final_after_sep_pct,
        # vs_liquids_final_after_sep_pct,
        #
        empty_days,
        # vs_solid_loaded_kg,
        #
        # vs_liq_loaded_kg_day,
        # vs_liq_total_cum_kg,
        # vs_liq_loss_kg_day,
        # vs_liq_loss_cum_kg,
        # vs_liq_deg,
        # vs_liq_ndeg,
        #
        herd_ch4_emissions_kg,
        barn_ch4_emissions_kg,
        ch4_emissions_solid_storage_kg,
        ch4_liq_emission_kg_day




      )
    })

    summarized_data <- reactive({

      emissions() %>%
        dplyr::filter(yday > 365) %>%
        dplyr::summarise(
          total_ch4_herd = sum(herd_ch4_emissions_kg),
          total_ch4_fac  = sum(barn_ch4_emissions_kg),
          total_ch4_liq  = sum(ch4_liq_emission_kg_day),
          total_ch4_soli = sum(ch4_emissions_solid_storage_kg)

        )
    })


    output$tabela <- renderTable({

      #summarized_data()

    })


# -------------------------------------------------------------------------
# Methane Emissions -------------------------------------------------------
# -------------------------------------------------------------------------

    output$herd_methane <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(summarized_data()[1], 1),
        title    = "Total Herd Methane Emissions (kg/year)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = icon("fa-solid fa-fire-flame-simple", verify_fa = FALSE),
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    output$facilitie_methane <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(summarized_data()[2], 2),
        title    = "Total Barn Methane Emissions (kg/year)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = icon("fa-solid fa-fire-flame-simple", verify_fa = FALSE),
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    output$manure_storage_methane <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(sum(summarized_data()[4], summarized_data()[3], na.rm = TRUE), 1),
        title    = "Total Storage Methane Emissions (kg/year)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = icon("fa-solid fa-fire-flame-simple", verify_fa = FALSE),
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
       dplyr::filter(yday > 365) %>%
       dplyr::mutate(
         yday = yday - 365
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
                 gridcolor     = 'ffff')
        ) %>%
        plotly::layout(legend = list(itemsizing = 'constant'))

    })

    # Manure Storage

    output$storage_ch4_chart <- plotly::renderPlotly({

      emissions <- emissions() %>%
        tibble::as_tibble() %>%
        dplyr::filter(yday > 365) %>%
        dplyr::mutate(
          yday = yday - 365,
          total_storage_kg = ch4_emissions_solid_storage_kg + ch4_liq_emission_kg_day
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


    output$pie_chart <- plotly::renderPlotly({

      herd <- summarized_data()[1]$total_ch4_herd

      fac <- summarized_data()[2]$total_ch4_fac

      storage <- summarized_data()[4]$total_ch4_soli + summarized_data()[3]$total_ch4_liq

      values = c(herd, fac, storage)

      labels = c("Herd", "Barn", "Manure Storage")

      plotly::plot_ly(type = 'pie',
                      labels = labels,
                      values = values,
              textinfo = 'label+percent',
              insidetextorientation = 'radial')

    })



    # output$plot <- renderPlot({
    #
    # emissoes <- tibble::tibble(emissions())
    #
    # if(solid_liquid() == "no" & type_manure() == "slurry") {
    #
    #   graphics::par(mar = c(5, 4, 4, 4) + 0.3)
    #
    #   emissoes$ch4_liq_emission_kg_day %>%
    #     plot(xlab = "Year days",
    #          ylab = "Methane Emission (kg)",
    #          type = "b",
    #          col = "red",
    #          lwd = 5,
    #          pch = 17,
    #          main = "Daily and Cumulative CH4 Emissions from Manure Storage")
    #
    #   graphics::par(new = TRUE) # Add new plot
    #
    #   plot(emissoes$yday,
    #        cumsum(emissoes$ch4_liq_emission_kg_day),
    #        type = "b",
    #        col = "blue",
    #        lwd = 5,
    #        pch = 15,
    #        axes = FALSE, xlab = "", ylab = "") # Create second plot without axes
    #
    #   graphics::axis(side = 4, at = pretty(range(cumsum(emissoes$ch4_liq_emission_kg_day))))      # Add second axis
    #   graphics::mtext("Cumulative Methane Emission (kg)", side = 4, line = 3)
    #   graphics::legend("topleft", legend = c("Cum. CH4 (kg)", "Daily CH4 (kg)"),
    #             col = c("blue", "red"), pch = 17, cex = 0.9)
    #
    #
    #
    # } else if (solid_liquid() == "no" & type_manure() == "solid") {
    #
    #   print(
    #   ggplot2::ggplot(emissions(), ggplot2::aes(x = yday, y = ch4_emissions_solid_storage_kg)) +
    #     ggplot2::geom_point())
    #
    # } else {
    #
    #   liq_plot <- function() {
    #
    #   graphics::par(mar = c(5, 4, 4, 4) + 0.3)
    #
    #   emissoes$ch4_liq_emission_kg_day %>%
    #     plot(xlab = "Year days",
    #          ylab = "Methane Emission (kg)",
    #          type = "b",
    #          col = "red",
    #          lwd = 5,
    #          pch = 17,
    #          main = "Daily and Cumulative CH4 Emissions from Manure Storage")
    #
    #   graphics::par(new = TRUE) # Add new plot
    #
    #   plot(emissoes$yday,
    #        cumsum(emissoes$ch4_liq_emission_kg_day),
    #        type = "b",
    #        col = "blue",
    #        lwd = 5,
    #        pch = 15,
    #        axes = FALSE, xlab = "", ylab = "") # Create second plot without axes
    #
    #   graphics::axis(side = 4, at = pretty(range(cumsum(emissoes$ch4_liq_emission_kg_day))))      # Add second axis
    #   graphics::mtext("Cumulative Methane Emission (kg)", side = 4, line = 3)
    #   graphics::legend("topleft", legend = c("Cum. CH4 (kg)", "Daily CH4 (kg)"),
    #                    col = c("blue", "red"), pch = 17, cex = 0.9)
    #   }
    #
    #
    #
    #   graphics::par(mfrow = c(1, 2))
    #
    #   liq_plot()
    #   plot(emissoes$ch4_emissions_solid_storage_kg, ylab = "Methane emissios (kg/day)", xlab = "Year day")
    #
    # }
    # })

  })
}
