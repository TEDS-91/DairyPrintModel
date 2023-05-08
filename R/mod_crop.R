#' crop UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_crop_ui <- function(id){
  ns <- NS(id)

  tagList(

    fluidRow(
    bs4Dash::bs4Card(
      width = 12,
      title = "Homegrown crops ",
      elevation = 2,
      solidHeader = TRUE,
      status = "teal",
      collapsed = FALSE,
      fluidRow(
        column(3,
               selectInput(ns("number_crops"), label = "Homegrown Crops:", choices = seq(1, 10, 1), selected = 4))),
      fluidRow(
        column(12,
               uiOutput(ns("crop_types"))))),

    bs4Dash::bs4Card(
      width = 12,
      title = "Crop emissions ",
      elevation = 2,
      solidHeader = TRUE,
      status = "teal",
      collapsed = FALSE,
      fluidRow(

        list(
          "lime_urea_co2",
          "fert_nh3",
          "total_nitrous_oxide",
          "ch4_field"
        ) %>%
          purrr::map(\(x) bs4Dash::valueBoxOutput(ns(x), width = 3))
      )),

    bs4Dash::box(
      width = 12,
      title = "Manure applied",
      elevation = 2,
      solidHeader = TRUE,
      status = "teal",
      collapsed = FALSE,
      fluidRow(
        textOutput(ns("manure_spreaded"))
      ))


    )

  )
}

#' crop Server Functions
#'
#' @noRd
mod_crop_server <- function(id,
                            animal_data,
                            type_manure,
                            manure_management,
                            manure_application_method,
                            manure_data,
                            co2eq_purchased){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

# -------------------------------------------------------------------------
# UI for crop according to the number of crops
# -------------------------------------------------------------------------

    output$crop_types <- renderUI({

      evens <- function(x) subset(x, x %% 2 == 0)

      vector_crops <- seq(1, input$number_crops)

      fluidRow(
        column(6,
               purrr::map(vector_crops[!vector_crops %in% evens(vector_crops)],
                          ~bs4Dash::bs4Card(width = 12,
                                            title = paste0("Crop: ", .x),
                                            elevation = 2,
                                            solidHeader = TRUE,
                                            status = "teal",
                                            collapsed = FALSE,
                                            crop_ui_prms(ns(.x))))),
        column(6,
               purrr::map(evens(vector_crops),
                          ~bs4Dash::bs4Card(width = 12,
                                            title = paste0("Crop: ", .x),
                                            elevation = 2,
                                            solidHeader = TRUE,
                                            status = "teal",
                                            collapsed = FALSE,
                                            crop_ui_prms(ns(.x)))))
      )

    })

# -------------------------------------------------------------------------
# Nitrogen from manure ----------------------------------------------------
# -------------------------------------------------------------------------

    # correcting for the area available

    total_area <- reactive({

      sum(valores()$manure_pct)

    })

    output$manure_spreaded <- renderText({

      dplyr::if_else(total_area() < 100, paste("Total manure spreaded is lower than 100%. The N, P, and K in the manure is considered exported."),
                     paste("Total manure spreaded is", total_area(), "%."
                     ))

    })



    nitrogen_from_manure <- reactive({

      # correcting the TAN losses for the type of application method

      tan_losses <- dplyr::if_else(manure_application_method() == "Broadcast spreading", 0.99,
                                   dplyr::if_else(manure_application_method() == "Irrigation", 0.90, 1))


      print(paste("The total area is (%):", total_area()))

      if( manure_management() == "Daily Hauling") {

        # total nitrogen applied in to the field - daily hauling

        manure_data() %>%
          dplyr::slice(366:730) %>%
          dplyr::mutate(
            total_nitrogen_field = total_tan_kg + fecal_n_kg - loss_animal_kg,
            total_tan_field      = total_tan_kg - loss_animal_kg
          ) %>%
          dplyr::summarise(
            totalN   = sum(total_nitrogen_field) * total_area() / 100,
            totalTAN = sum(total_tan_field) * total_area() / 100
          ) %>%
          dplyr::mutate(
            nh3_application_method = totalTAN - (totalTAN * tan_losses),
            totalTAN               = totalTAN * tan_losses,
            totalN                 = totalN - (totalTAN - (totalTAN * tan_losses))
          )

      } else {

        # total nitrogen applied in to the field - daily hauling

        manure_data() %>%
          dplyr::slice(366:730) %>%
          dplyr::mutate(
            total_nitrogen_field = total_tan_kg + fecal_n_kg - loss_animal_kg - total_storage_N_loss,
            total_tan_field      = total_tan_kg - loss_animal_kg - total_storage_N_loss
          ) %>%
          dplyr::summarise(

            totaTAN1 = sum(total_tan_kg),
            fecal_n = sum(fecal_n_kg),
            storage_loss = sum(total_storage_N_loss),
            loss_animal_kg = sum(loss_animal_kg),

            totalN   = sum(total_nitrogen_field) * total_area() / 100,
            totalTAN = sum(total_tan_field) * total_area() / 100
          ) %>%
          dplyr::mutate(
            nh3_application_method = totalTAN - (totalTAN * tan_losses),
            totalTAN               = totalTAN * tan_losses,
            totalN                 = totalN - (totalTAN - (totalTAN * tan_losses))
          )

      }

    })


    output$df <- renderPrint({

     #print( co2eq_purchased() )

      print(paste("The manure method is ", manure_application_method()))

    })


    valores <- reactive({

      req(input[[paste0(input$number_crops, "_crop_type")]])

      crop_values <- function(input_id) {

        unlist(purrr::map(1:input$number_crops, function(i) { input[[paste0(i, input_id)]] } ))

      }

      crop_prms <- tibble::tibble(
        "crop_id"           = seq(1:input$number_crops),
        "crop_type"         = crop_values("_crop_type"),
        "area"              = crop_values("_area"),
        "yield"             = crop_values("_yield"),
        "manure_pct"        = crop_values("_manure_pct"),
        "total_n_applied"   = crop_values("_total_n_applied"),
        "urea_pct_applied"  = crop_values("_urea_pct_n"),
        "phosphate_applied" = crop_values("_p2o5_applied"),
        "potash_applied"    = crop_values("_k2o_applied"),
        "lime_applied"      = crop_values("_lime_applied")

      )

     crop_prms

    })

    crop_calculations <- reactive({

      crop_list <- list("Corn silage (ton)",
                        "Corn grain (bu)",
                        "Alfalfa silage (ton)",
                        "Soybean seed (bu)",
                        "Barley rolled (bu)",
                        "Wheat rolled (bu)",
                        "Sorghum grain (bu)",
                        "Oat grain (bu)")


      crop_nutrient_removal_n_fix <- tibble::tribble(
        ~crop,                  ~n_removal,  ~p_removal, ~k_removal, ~nitrogen_fix,
        "Corn silage (ton)",     4.4,         1.41,        3.31,     0,
        "Corn grain (bu)",       0.3,         0.16,        0.11,     0,
        "Alfalfa silage (ton)",  23.13,       5.44,        22.22,    16.16,
        "Soybean seed (bu)",     1.5,         0.33,        0.54,     0.89,
        "Barley rolled (bu)",    0.45,        0.18,        0.15,     0,
        "Wheat rolled (bu)",     0.63,        0.23,        0.14,     0,
        "Sorghum grain (bu)",    0.30,        0.18,        0.12,     0,
        "Oat grain (bu)",        0.35,        0.13,        0.09,     0
      )


      valores <- valores()

      valores <- valores %>%
        dplyr::mutate_at(c(3:10), as.numeric) %>%
        dplyr::mutate(
          co2_lime         = lime_decomposition_co2(lime_applied = lime_applied * 1000) * area,
          co2_urea         = urea_decomposition_co2(urea_applied = (urea_pct_applied / 100 * total_n_applied)) * area,
          nh3_n_fertilizer = fertilizer_nh3(nitrogen_applied = total_n_applied) * area,
          # calculating the total nitrous oxide from fert application after leaching
          nitrous_oxide    = fert_manure_nitrous_oxide(nitrogen_applied = total_n_applied * 0.7) * area # TODO! missing manure N
        )



      print(valores)

      valores %>%
        dplyr::inner_join(crop_nutrient_removal_n_fix, by = c("crop_type" = "crop")) %>%
        dplyr::mutate(
          total_n_removal = n_removal * yield,
          total_p_removal = p_removal * yield / 2.29,
          total_k_removal = k_removal * yield / 1.21,
          total_n_fixed   = nitrogen_fix * yield,
          crop_n_balance  = total_n_applied - total_n_removal + total_n_fixed
        )


    })

    # n fixed

    nitrogen_fixed <- reactive({

      crop_calculations() %>%
        dplyr::mutate(
          n_fixed_kg = total_n_fixed * yield
        ) %>%
        dplyr::summarise(
          n_fixed_ton = sum(n_fixed_kg) / 1000
        )

    })

    # Nitrogen leached calculation - 30% of the N applied in fields is leached in WI - IPCC 2006!

    n_leached <- reactive({

      print(paste("fixado e", nitrogen_fixed()))

      leached_from_fert <- valores() %>%
        dplyr::mutate(
          total_n_fert = total_n_applied * area * 0.7
        ) %>%
        dplyr::pull(total_n_fert) %>%
        sum()

      leached_from_manure <- nitrogen_from_manure()$totalN * 0.7

      (leached_from_fert + leached_from_manure) * 0.0075


    })


    nh3_from_manure_application <- reactive({

      #print(paste("Leached", n_leached))

      (nitrogen_from_manure()$totalTAN * ((20 + 5 * manure_data()$manure_dm[1]) * (7 / 7.3)) * 17 / 14) / 100

    })


# -------------------------------------------------------------------------
# Methane calculations from field application -----------------------------
# -------------------------------------------------------------------------

    ch4_field_kg <- reactive({

      # total area in hectares

      total_area <- sum(valores()$area)

      # initial volatile fatty acids in manure

      Fvfa_ini <- Fvfa_init_conc(Ftan = 43, ph = 7) # TODO

      # Total annual emissions

      if(manure_application_method() != "Injected") {

        # the models to predict ch4 from field application were developed for slurry and liquid manure
        # solid and semi-solid manure will be considered 0
        # For injected manure, the emissions will be considered 0

        if (type_manure() == "Slurry" | type_manure() == "Liquid") {

          Fvfa_conc(Fvfa_ini = Fvfa_ini) %>%
            purrr::map_dbl(manure_application_ch4_emission, area_crop = total_area) %>%
            sum()

        } else {

          0

        }

      } else {

        0

      }

    })

# -------------------------------------------------------------------------
# End of calculations -----------------------------------------------------
# -------------------------------------------------------------------------

    total_co2 <- reactive({

      total_co2 <- crop_calculations() %>%
        dplyr::mutate(
          total_co2 = co2_urea + co2_lime
        ) %>%
        dplyr::summarise(
          total_co2 = sum(total_co2)
        ) %>%
        dplyr::pull(total_co2)

      total_co2 + co2eq_purchased()

    })

    total_nh3 <- reactive({

      total_nh3 <- crop_calculations() %>%
        dplyr::summarise(
          total_nh3 = sum(nh3_n_fertilizer)
        ) %>%
        dplyr::pull(total_nh3)

      total_nh3 + nh3_from_manure_application() + nitrogen_from_manure()$nh3_application_method


    })

    total_n2o <- reactive({

      # calculating the total N applied from manure and discounting the leached fraction and NH3

      n2o_from_field_man <- (nitrogen_from_manure()$totalN * 0.7 - nh3_from_manure_application() ) * 0.01 + nitrogen_from_manure()$nh3_application_method * 0.01

      print(paste("N2o man appl", nitrogen_from_manure()$nh3_application_method * 0.01))

      # calculation of nitrous oxide from N leached

      n2o_from_n_leached <- n_leached() / 0.64

      total_nitrous_oxide <- crop_calculations() %>%
        dplyr::summarise(
          total_nh3 = sum(nh3_n_fertilizer),
          total_nitrous_oxide = sum(nitrous_oxide)
        ) %>%
        dplyr::mutate(
          total_nitrous_oxide = total_nitrous_oxide / 0.64 + total_nh3 / 0.64 * 0.01
        ) %>%
        dplyr::pull(total_nitrous_oxide)

      print(paste(" n2o fert", total_nitrous_oxide, " field", n2o_from_field_man, "leached", n2o_from_n_leached))

      total_nitrous_oxide + n2o_from_field_man + n2o_from_n_leached


    })

    # Total CH4 emmited from field

    output$ch4_field <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(ch4_field_kg() / 1000, 2),
        title    = "CH4 emitted (Ton./year)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 6,
        color    = "white",
        href     = NULL
      )

    })

    # Total CO2 emitted from lime and urea

    output$lime_urea_co2 <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(total_co2() / 1000, 1),
        title    = "CO2 emitted from Lime and Urea (Ton./year)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 6,
        color    = "white",
        href     = NULL
      )

    })

    # Total Ammonia emitted from N fertilizer

    output$fert_nh3 <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(total_nh3() / 0.82 / 1000, 1),
        title    = "NH3 emitted from N (Ton./year)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    # Total Nitrous oxide emitted from N fertilizer + Manure

    output$total_nitrous_oxide <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(total_n2o() / 1000, 1),
        title    = "N2O emitted from N (Ton./year)",
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
# Outputs from this module to populate others -----------------------------
# -------------------------------------------------------------------------

    return(
      list(
        crop_inputs = reactive(valores()),
        total_co2   = reactive(total_co2()),
        total_nh3   = reactive(total_nh3()),
        total_n2o   = reactive(total_n2o()),
        total_ch4   = reactive(ch4_field_kg()),
        n_fixed     = reactive( nitrogen_fixed() ),
        n_leached   = reactive( n_leached() )
      )
    )


  })
}
