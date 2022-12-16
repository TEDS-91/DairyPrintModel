#' nh3_emissions UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_nh3_emissions_ui <- function(id){
  ns <- NS(id)
  tagList(

    tableOutput(ns("tabela")),

    plotOutput(ns("grafico")),

    tableOutput(ns("tabela2"))

  )
}

#' barn_nh3_emissions Server Functions
#'
#' @noRd
mod_nh3_emissions_server <- function(id,
                                     county,
                                     facilitie,
                                     biodigester,
                                     type_manure,
                                     crust,
                                     empty_time){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    nh3_emissions <- reactive({

      facilitie <- facilitie()

      county <- county()

      biodigester <- biodigester()

      type_manure <- type_manure()

      empty_time <- empty_time()

      crust <- crust()

      urine_vol <- 21

      TAN <- 0.209

      Nfeces <- 0.232

      ts_manure_kg <- 9.64

      fresh_manure_kg <- 74.7

      facilitie <- ifelse(facilitie == "freestall", 3.5, 1.5)

      manure_solution_mass <- urine_vol / facilitie

      manure_solution_pH <- 7.7

      gamma_densi <- 1000 # manure density

      const <- 86400 # time conversion

      yday <- seq(1, 730, 1)

      temp_c <- rep(wi_weather %>%
                      dplyr::filter(county == county()) %>%
                      dplyr::pull(aver_tempC), 2)

      Q <- eq_coeff(temp_c = temp_c, pH = manure_solution_pH)

      r <- resistence_nh3(hsc = 260, temp_c = temp_c) #TODO

      tan_kg_m2 <- TAN / facilitie #TODO

      loss_kg_m2 <- tan_kg_m2 * const * gamma_densi / (r * manure_solution_mass * Q)

      loss_animal_kg <- loss_kg_m2 * facilitie

      cum_loss <- cumsum(loss_animal_kg)

      # storage emissions

      remaining_tan <- TAN - loss_animal_kg

      empty_days <- empty_day(empty_time = empty_time)

      mineralization_pct <- dplyr::if_else(empty_time == "Fall" | empty_time == "Spring", 0.25, 0.12)

      n_mineralized_feces <- dplyr::if_else(biodigester == "no", mineralization_pct * Nfeces, mineralization_pct * Nfeces + 0.16 * Nfeces)

      total_tan <- cumsum(remaining_tan + n_mineralized_feces)

      manure_sol_loaded <- rep(fresh_manure_kg * ( 1 - ts_manure_kg / fresh_manure_kg), 730)

      cum_manure_sol_loaded <- cumsum(manure_sol_loaded)

      tank_capacity <- 365 * manure_sol_loaded[1]

      # logic for emptying - TAN

      volume_diario <- (remaining_tan + n_mineralized_feces)[1]

      acumulado <- vector(length =  730)

      acumulado[1] <- volume_diario

      tank_cap <- 365 * volume_diario

      for (i in 2:length(empty_days)) {

        if(empty_days[i] == 0) {
          acumulado[i] <- acumulado[i - 1] + volume_diario
        } else {
          acumulado[i] <- tank_cap * 0 + volume_diario
        }


      }

      # logic for cumul manure

      volume_diario_man <- manure_sol_loaded[1]

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



      #


      # r for storage

      if (crust == "no" & type_manure == "slurry") {

        r_storage <- resistence_nh3(hsc = 19, temp_c = temp_c)

      } else if (crust == "yes" & type_manure == "slurry") {

        r_storage <- resistence_nh3(hsc = 75, temp_c = temp_c)

      } else {

        r_storage <- resistence_nh3(hsc = 10, temp_c = temp_c)

      }

      ph_storage <- 7.5

      Q_storage <- eq_coeff(temp_c = temp_c, pH = ph_storage)

      storage_area <- 200

      storage_N_loss_m2 <- total_tan * const * gamma_densi / (r_storage * cum_manure_sol_loaded * Q_storage) * storage_area

      storage_N_loss_m22 <- acumulado * const * gamma_densi / (r_storage * acumulado_manure * Q_storage) * storage_area

      cum_N_loss_m2 <- cumsum(storage_N_loss_m2)

      cum_N_loss_m22 <- cumsum(storage_N_loss_m22)

      nh3_emissions <- tibble::tibble(

        # barn emissions

        yday,
        temp_c,
        empty_days,
        # const,
        # gamma_densi,
        # r,
        # manure_solution_mass,
        # manure_solution_pH,
        # Q,
        # tan_kg_m2,
        # loss_kg_m2,
        # loss_animal_kg,
        # cum_loss,
        #
        # remaining_tan,
        # empty_days,
        # mineralization_pct,
        # n_mineralized_feces,
        # total_tan,
        # manure_sol_loaded,
        # cum_manure_sol_loaded,
        # crust,
        # r_storage,
        # ph_storage,
        # Q_storage,
        volume_diario,
        storage_N_loss_m2,
        storage_N_loss_m22,
        #cum_N_loss_m2,

        acumulado,
        volume_diario_man,
        cum_manure_sol_loaded,
        acumulado_manure,
        cum_N_loss_m22,
        cum_N_loss_m2



      )

     nh3_emissions

    })

    output$tabela <- renderTable({

      #nh3_emissions()

    })

    output$grafico <- renderPlot({

       nh3 <- tibble::as_tibble(nh3_emissions())

       nh3 %>%
       ggplot2::ggplot( ggplot2::aes(x = yday, y = storage_N_loss_m2), col = "blue") +
         ggplot2::theme_bw() +
         ggplot2::geom_point() +
         ggplot2::geom_point( ggplot2::aes(x = yday, y = storage_N_loss_m22), col = "blue") +
         ggplot2::xlab("Yaer Days") +
         ggplot2::ylab("Daily Amonia Emissions")

    })

    output$tabela2 <- renderTable({

      nh3 <- tibble::as_tibble(nh3_emissions())


      nh3 %>%
        dplyr::summarise(
          total_nh3_emp = sum(cum_N_loss_m22),
          total_nh3_without_emp = sum(cum_N_loss_m2)
        )



    })



  })
}

## To be copied in the UI
# mod_nh3_emissions_ui("nh3_emissions")

## To be copied in the server
# mod_nh3_emissions_server("nh3_emissions")
