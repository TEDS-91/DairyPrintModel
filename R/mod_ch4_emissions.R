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

    tableOutput(ns("tabela"))

  )
}

#' ch4_emissions Server Functions
#'
#' @noRd
mod_ch4_emissions_server <- function(id, county, facilitie, bedding){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # all logic without connect with the animal part

    emissions <- reactive({

      county <- county()

      facilitie <- facilitie()

      bedding <- bedding()

      # animal prmts

      milking_cows <- 230
      dry_cows <- 50
      heifers <- 134

      milking_cows_manure <- 65
      dry_cows_manure <- 25
      heifers_manure <- 21

      milking_cows_ts <- 0.0875
      dry_cows_ts <- 0.0443
      heifers_ts <- 0.0459

      total_manure_kg <- milking_cows * milking_cows_manure + dry_cows * dry_cows_manure + heifers * heifers_manure

      total_ts_manure_kg <- milking_cows * milking_cows_manure * milking_cows_ts + dry_cows * dry_cows_manure * dry_cows_ts + heifers * heifers_manure * heifers_ts

      total_vs_manure_kg <- (milking_cows * milking_cows_manure * milking_cows_ts + dry_cows * dry_cows_manure * dry_cows_ts + heifers * heifers_manure * heifers_ts) * 0.8


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


      yday <- seq(1, 730, 1)

      temp_c <- rep(wi_wheather %>%
        dplyr::filter(county == county()) %>%
        dplyr::pull(aver_tempC), 2)

      herd_ch4_emissions_kg <- milking_cows * 0.464 + dry_cows * 0.261 + heifers * 0.156

      area_exposed_m2 <- dplyr::if_else(facilitie == "freestall", milking_cows * 3.5 + dry_cows * 3.5 + heifers * 2.5,
                                     milking_cows * 1.5 + dry_cows * 1.5 + heifers * 2.5)

      barn_ch4_emissions_kg <- barn_ch4_emission_floor(temp_c = temp_c, manure_area = area_exposed_m2)




      emissions <- tibble::tibble(
        yday,
        temp_c,
        herd_ch4_emissions_kg,
        area_exposed_m2,
        barn_ch4_emissions_kg,
        total_manure_kg,
        total_ts_manure_kg,
        bedding_quantity_kg,
        bedding_ts_kg,
        bedding_vs_kg,
        total_vs_manure_kg,
        total_mass_managed_kg,
        total_ts_managed_kg,
        total_vs_managed_kg,
        total_mass_managed_corSS_kg


      )



    })

    output$tabela <- renderTable({

      emissions()

    })






  })
}

## To be copied in the UI
# mod_ch4_emissions_ui("ch4_emissions")

## To be copied in the server
# mod_ch4_emissions_server("ch4_emissions")
