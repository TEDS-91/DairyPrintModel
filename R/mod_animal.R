#' animal UI Function
#'
#' @description This is the animal module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_animal_ui <- function(id){
  ns <- NS(id)
  tagList(

# -------------------------------------------------------------------------
# Including in head, but I should create a CSS file -----------------------
# -------------------------------------------------------------------------

    tags$head(tags$style('body {color:#696969;}')),
    tags$head(tags$style(HTML(".small-box {height: 92px}"))),
    tags$head(tags$style(HTML(".fa{font-size: 20px;}"))),

# -------------------------------------------------------------------------
      #h1("Herd calibration"),


      fluidRow(
        bs4Dash::bs4Card(
          title = "Herd Management",
          elevation = 1,
          width = 12,
          solidHeader = TRUE,
          status = "teal",
          collapsible = TRUE,
          fluidRow(

        bs4Dash::bs4Card(
          title = "Herd Information",
          elevation = 1,
          width = 7,
          solidHeader = TRUE,
          status = "teal",
          collapsible = TRUE,
          fluidRow(
            animal_ui_prms(ns("animal")))),

        bs4Dash::bs4Card(
          title = "Calf Feed Management and Milk Composition",
          elevation = 1,
          width = 5,
          solidHeader = TRUE,
          status = "teal",
          collapsible = TRUE,
          fluidRow(
            calf_ui_prms(ns("calf"))))),

      fluidRow(
        bs4Dash::bs4Card(
          title = "Diet Information",
          elevation = 1,
          width = 12,
          solidHeader = TRUE,
          status = "teal",
          collapsible = TRUE,
          fluidRow(
            bs4Dash::bs4Card(
              title = "Lactating Cows",
              width = 4,
              fluidRow(
                diet_ui_prms(ns("diet_lac"), cp = 17, ndf = 30, adf = 22, ee = 5, p = 0.4, k = 1.1))),
            bs4Dash::bs4Card(
              title = "Dry Cows",
              width = 4,
              fluidRow(
                diet_ui_prms(ns("diet_dry"), cp = 13, ndf = 40, adf = 33, ee = 4, p = 0.23, k = 0.6))),
            bs4Dash::bs4Card(
              title = "Heifers",
              width = 4,
              fluidRow(
                diet_ui_prms(ns("diet_hei"), cp = 12, ndf = 35, adf = 23, ee = 4, p = 0.23, k = 0.45))))),

        bs4Dash::bs4Card(
          title = "Nutritional Strategy for Enteric Methane Mitigation",
          elevation = 1,
          width = 12,
          solidHeader = TRUE,
          status = "teal",
          collapsible = TRUE,
          fluidRow(
            column(6,
                   radioButtons(ns("radio"), label = "Nutritional strategy for reducing enteric methane?",
                                choices = list("yes" = "yes",
                                               "no" = "no"),
                                selected = "no")),
            column(3,
                   uiOutput(ns("nut_aditive"))))))),

        bs4Dash::bs4Card(
          title = p("Herd Dashboard",
                    actionButton(ns("button"), "Run!", tags$i(fontawesome::fa("person-running")),
                                 class = "btn-xs",
                                 style = "position: absolute; right: 120px;
                                          color: #fff; background-color: #20C997; padding:4px; font-size:100%;
                                          border-color: white; height:35px; width:130px;")),
          elevation = 1,
          width = 12,
          solidHeader = TRUE,
          status = "teal",
          collapsible = TRUE,
          maximizable = TRUE,
          hr(),

          bs4Dash::tabBox(
            id = "tabcard",
            status = "primary",
            solidHeader = FALSE,
            type = "tabs",
            width = 12,

            tabPanel(
              title = "Herd Inventory",
              fluidRow(
                bs4Dash::bs4Card(
                  title = "Number of Animals by Category",
                  collapsible = FALSE,
                  elevation = 1,
                  width = 12,
                  solidHeader = TRUE,
                  fluidRow(

                    list(
                      "number_lac",
                      "number_dry",
                      "number_hei",
                      "number_cal"
                    ) %>%
                      purrr::map(\(x) bs4Dash::valueBoxOutput(ns(x), width = 3)))),

                bs4Dash::box(
                  title = "Herd Stability",
                  collapsible = FALSE,
                  #elevation = 1,
                  width = 12,
                  #solidHeader = TRUE,
                  fluidRow(
                    textOutput(ns("herd_message"))))
                )),

            tabPanel(
              title = "Performance Metrics",
              fluidRow(

                # metric cards
                list(
                  # DMI
                 "hei_dmi",
                 "dry_dmi",
                 "lac_dmi",
                 # water
                 "hei_water",
                 "dry_water",
                 "lac_water",
                 # Milk yield
                 "lac_my",
                 "lac_my_fpc",
                 "feed_ef"
                ) %>%
                  purrr::map(\(x) bs4Dash::valueBoxOutput(ns(x))),

                # Plots
                bs4Dash::bs4Card(
                  title = "Milk Yield",
                  width = 6,
                  footer = "Milk yield calculated according to the Wood's model (1967).",
                  plotly::plotlyOutput(ns("lactation_curves_plotly"))),
                bs4Dash::bs4Card(
                  title = "Dry Matter Intake",
                  width = 6,
                  footer = "Dry matter intake calculated according to the NRC (2001) equation.",
                  plotly::plotlyOutput(ns("dmi_curves_plotly")))
                )),
            tabPanel(
              title = "Manure Excretion and GHG Emissions",
              fluidRow(

                list(
                  "hei_manure",
                  "dry_manure",
                  "lac_manure",
                  "hei_ch4",
                  "dry_ch4",
                  "lac_ch4",
                  "methane_yield",
                  "methane_intensity"
                ) %>%
                  purrr::map(\(x) bs4Dash::valueBoxOutput(ns(x), width = 3)),

                # Plots
                bs4Dash::bs4Card(
                  title = "Manure Excretion",
                  width = 6,
                  footer = NULL,
                  plotly::plotlyOutput(ns("manure_curves_plotly"))),
                bs4Dash::bs4Card(
                  title = "Methane Emissions",
                  width = 6,
                  footer = NULL,
                  plotly::plotlyOutput(ns("methane_curves_plotly"))))),
            tabPanel(
              title = "Nutrient Excretion",
              fluidRow(

                list(
                  "hei_nit",
                  "dry_nit",
                  "lac_nit",
                  "hei_pho",
                  "dry_pho",
                  "lac_pho",
                  "hei_pot",
                  "dry_pot",
                  "lac_pot"
                ) %>%
                  purrr::map(\(x) bs4Dash::valueBoxOutput(ns(x))),

                # Plots
                bs4Dash::bs4Card(
                  title = "Nitrogen",
                  width = 4,
                  footer = NULL,
                  plotly::plotlyOutput(ns("nit_curves_plotly"))),
                bs4Dash::bs4Card(
                  title = "Phosphorous",
                  width = 4,
                  footer = NULL,
                  plotly::plotlyOutput(ns("pho_curves_plotly"))),
                bs4Dash::bs4Card(
                  title = "Potassium",
                  width = 4,
                  footer = NULL,
                  plotly::plotlyOutput(ns("pot_curves_plotly")))
                )
              )
            )
          )
        )
      )
   #)
}

#' animal Server Functions
#'
#' @noRd
mod_animal_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

# -------------------------------------------------------------------------
# User's feedback to inappropriate inputs
# -------------------------------------------------------------------------

     user_feedback <- function(input_id, decimal) {

       observeEvent(input[[input_id]], {

         req(input[[input_id]])

         if (input[[input_id]] < 0 & is.integer(input[[input_id]]) == TRUE & decimal == FALSE) {

           shinyFeedback::showFeedbackWarning(inputId = input_id, text = "Can't be negative")

         } else if (input[[input_id]] < 0 | is.integer(input[[input_id]]) == FALSE & decimal == FALSE) {

           shinyFeedback::showFeedbackWarning(inputId = input_id, text = "Can't be negative or decimal")

         } else {

           shinyFeedback::hideFeedback(input_id)

         }
       }
     )
   }

    tibble::tribble(
      ~input_id,                        ~decimal,
      "animal_n_cows",                  FALSE,
      "animal_cow_calving_int",         FALSE,
      "animal_cow_rep_rate",            TRUE,
      "animal_time_first_calv",         FALSE,
      "animal_calves_heifers_cul",      TRUE,
      "animal_heifer_calf_born",        TRUE,
      "animal_average_milk_yield",      TRUE,
      "animal_mature_weight",           TRUE
    ) %>%
      purrr::pmap(user_feedback)

# -------------------------------------------------------------------------

    # Methane nutritional additive to reduce emissions

    output$nut_aditive <- renderUI({

      if(input$radio == "yes") {

        numericInput(ns("methane_red"), label = "Expected reduction (%): ", value = 0, min = 0, max = 100)

      } else {
        NULL
      }

    })


    herd_matrix <- reactive({

      herd_matrix <- herd_projection(time_first_calv    = input$animal_time_first_calv,
                                     heifer_calf_born   = input$animal_heifer_calf_born,
                                     stillbirth_rate    = 3, # Fixed value
                                     calves_heifers_cul = input$animal_calves_heifers_cul,
                                     cow_rep_rate       = input$animal_cow_rep_rate,
                                     cow_calving_int    = input$animal_cow_calving_int,
                                     n_adult_cows       = input$animal_n_cows,
                                     months_to_project  = 2)

    })

    lambda_milk_calc <- eventReactive(input$button, {

      age_first_calv <- input$animal_time_first_calv

      cow_calving_int <- input$animal_cow_calving_int

      n_cows <- input$animal_n_cows

      prop_primiparous <- sum(herd_matrix()[[1]][1, (age_first_calv + 1):((age_first_calv + 1) + cow_calving_int - 1)]) / n_cows

      prop_secondiparous <- sum(herd_matrix()[[1]][1, (age_first_calv + 1 + cow_calving_int):(age_first_calv + 1 + 2 * cow_calving_int - 1)]) / n_cows


      lambda_milk_calc <- lambda_milk_yield(obs_average_milk_yield = input$animal_average_milk_yield,
                                            milk_freq              = input$animal_milk_freq,
                                            prop_primiparous       = prop_primiparous * 100,
                                            prop_secondiparous     = prop_secondiparous * 100,
                                            cow_calving_interval   = input$animal_cow_calving_int) - 0.02
      lambda_milk_calc

      })


    lambda_milk_prot_calc <- eventReactive(input$button, {

      age_first_calv <- input$animal_time_first_calv

      cow_calving_int <- input$animal_cow_calving_int

      n_cows <- input$animal_n_cows

      prop_primiparous <- sum(herd_matrix()[[1]][1, (age_first_calv + 1):((age_first_calv + 1) + cow_calving_int - 1)]) / n_cows

      prop_secondiparous <- sum(herd_matrix()[[1]][1, (age_first_calv + 1 + cow_calving_int):(age_first_calv + 1 + 2 * cow_calving_int - 1)]) / n_cows


      lambda_milk_prot_calc <- lambda_milk_prot(obs_average_milk_prot = input$calf_protein,
                                             prop_primiparous       = prop_primiparous * 100,
                                             prop_secondiparous     = prop_secondiparous * 100,
                                             cow_calving_interval   = input$animal_cow_calving_int)

      lambda_milk_prot_calc

    })


    #output$lambda <- renderPrint({

      #paste("the lambda value is:", lambda_milk_prot_calc())

    #})


    df <- eventReactive(input$button, {

     # status("Inputs are up to date!")

      withProgress(message = "Running the model...", detail = ' ', value = 0.5, {


      # converting the herd_matrix to tibble
      herd_demographics_raw <- herd_matrix()[[1]] %>%
        tibble::as_tibble()

      df <- herd_demographics_raw %>%
        tibble::as_tibble() %>%
        dplyr::mutate(MonthSimulated = seq(1, dim(herd_demographics_raw)[1])) %>%
        dplyr::relocate(MonthSimulated, .before = HeiOpenGrowMonth1) %>%
        tidyr::pivot_longer(-MonthSimulated,
                            names_to = "GeneralCategories",
                            values_to = "NumberAnimals") %>%
        tidyr::separate(GeneralCategories, into = c("Categories", "Others"), 3) %>%
        tidyr::separate(Others, into = c("ReproductiveStatus", "Others"), 4) %>%
        tidyr::separate(Others, into = c("Phase", "MonthLac"), 4) %>%
        tidyr::separate(MonthLac, into = c("Garbage", "Month"), 5) %>%
        dplyr::select(-Garbage) %>%
        dplyr::mutate(Categories = dplyr::if_else(Categories == "Hei" & Month == "1", "Cal",
                                           dplyr::if_else(Categories == "Hei" & Month == "2", "Cal", Categories)))

      ###################################################################################################################

      # additional inputs

      birth_weight_kg <- input$calf_birth_weight
      milk_sup_l <- input$calf_milk_sup
      mature_body_weight <- as.numeric(input$animal_mature_weight)

      milk_fat <- input$calf_fat
      milk_prot <- input$calf_protein
      milk_p_g_l <- 0.996
      milk_k_g_l <- 1.5
      milk_solids <- (1 - (100 - milk_fat - milk_prot - 4.6 - 0.8) / 100) * 100
      starter_cp <- input$calf_starter_cp
      starter_p <- 0.5
      starter_k <- 1
      forage_cp <- input$calf_forage_cp
      forage_intake_1 <- 0.05
      forage_intake_2 <- 0.25
      forage_p <- 0.5
      forage_k <- 1
      methane_aditive <- ifelse(input$radio == "no", 0, input$methane_red)

      dmi_dry <- 0.02

      diet_cp_lac  <- input$diet_lac_cp
      diet_ee_lac  <- input$diet_lac_ee
      diet_ndf_lac <- input$diet_lac_ndf
      diet_nda_lac <- input$diet_lac_adf
      diet_p_lac   <- input$diet_lac_p
      diet_k_lac   <- input$diet_lac_k

      diet_cp_hei  <- input$diet_hei_cp
      diet_ee_hei  <- input$diet_hei_ee
      diet_ndf_hei <- input$diet_hei_ndf
      diet_nda_hei <- input$diet_hei_adf
      diet_p_hei   <- input$diet_hei_p
      diet_k_hei   <- input$diet_hei_k

      diet_cp_dry  <- input$diet_dry_cp
      diet_ee_dry  <- input$diet_dry_ee
      diet_ndf_dry <- input$diet_dry_ndf
      diet_nda_dry <- input$diet_dry_adf
      diet_p_dry   <- input$diet_dry_p
      diet_k_dry   <- input$diet_dry_k

      ##################################################################################################################

      calf_adg_kg <- seq(1, 60) %>%
        purrr::map_dbl(starter_intake_calves, milk_intake = milk_sup_l) %>%
        purrr::map_dbl(calf_adg, milk_intake = milk_sup_l) %>%
        sum() / 60

      weaning_weight_kg <- birth_weight_kg + calf_adg_kg * 60

      ##################################################################################################################

      # BW calculations

      df_bw <- df %>%
        dplyr::mutate(
          Month   = as.numeric(Month),
          day_min = Month * 30 - 29,
          day_max = Month * 30,
          mean_body_weight =
            dplyr::if_else(Categories == "Hei",
                           purrr::map_dbl(Month,
                                          heifer_body_weight,
                                          birth_weight       = birth_weight_kg,
                                          weaning_weight     = weaning_weight_kg,
                                          age_first_calving  = input$animal_time_first_calv,
                                          mature_body_weight = mature_body_weight,
                                          type               = "mean"),
                           dplyr::if_else(Categories == "Cal",
                                          purrr::map_dbl(day_max,
                                                         calf_body_weight,
                                                         birth_weight       = birth_weight_kg,
                                                         calf_adg           = calf_adg_kg,
                                                         type               = "mean"), 0)),
          final_body_weight =
            dplyr::if_else(Categories == "Hei",
                           purrr::map_dbl(Month,
                                          heifer_body_weight,
                                          birth_weight       = birth_weight_kg,
                                          weaning_weight     = weaning_weight_kg,
                                          age_first_calving  = input$animal_time_first_calv,
                                          mature_body_weight = mature_body_weight,
                                          type               = "final"),
                           dplyr::if_else(Categories == "Cal",
                                          purrr::map_dbl(day_max,
                                                         calf_body_weight,
                                                         birth_weight       = birth_weight_kg,
                                                         calf_adg           = calf_adg_kg,
                                                         type               = "final"), 0))
        ) %>%
        dplyr::group_by(MonthSimulated) %>%
        dplyr::mutate(
          cow_bw_parturium_kg = dplyr::if_else(Phase == "Grow", 0,
                                               dplyr::if_else(Phase == 'Lac1', mature_body_weight * 0.82,
                                                              dplyr::if_else(Phase == 'Lac2', mature_body_weight * 0.92, mature_body_weight))),
          bw_change_total_kg  = dplyr::if_else(Phase == "Grow", 0,
                                               dplyr::if_else(Phase == "Lac1",
                                                              purrr::map2_dbl(day_min,
                                                                              day_max,
                                                                              cow_body_weight,
                                                                              parity = "primiparous"),
                                                              purrr::map2_dbl(day_min,
                                                                              day_max,
                                                                              cow_body_weight,
                                                                              parity = "multiparous"))),
          bw_change_acum_kg    = cumsum(bw_change_total_kg),
          mean_cow_weight_kg   = cow_bw_parturium_kg + bw_change_acum_kg - 0.5 * bw_change_total_kg,
          mean_body_weight_kg  = mean_body_weight + mean_cow_weight_kg,
          final_body_weight_kg = final_body_weight + bw_change_total_kg + cow_bw_parturium_kg)

          # Milk yield calculations

          df_my <- df_bw %>%
            dplyr::mutate(
              milk_yield_kg_2 = dplyr::if_else(Phase == "Grow" | Categories == "Dry", 0,
                                               dplyr::if_else(Phase == "Lac1",
                                                              purrr::map2_dbl(day_min,
                                                                              day_max,
                                                                              milk_yield_acum,
                                                                              parity      = "primiparous",
                                                                              milk_freq   = input$animal_milk_freq,
                                                                              lambda_milk = lambda_milk_calc()),
                                                              dplyr::if_else(Phase == "Lac2",
                                                                             purrr::map2_dbl(day_min,
                                                                                             day_max,
                                                                                             milk_yield_acum,
                                                                                             parity      = "secondiparous",
                                                                                             milk_freq   = input$animal_milk_freq,
                                                                                             lambda_milk = lambda_milk_calc()),
                                                                             purrr::map2_dbl(day_min,
                                                                                             day_max,
                                                                                             milk_yield_acum,
                                                                                             parity      = "multiparous",
                                                                                             milk_freq   = input$animal_milk_freq,
                                                                                             lambda_milk = lambda_milk_calc())))),
              milk_yield_kg_cow2 = milk_yield_kg_2 / 30,

              milk_protein = dplyr::if_else(Phase == "Grow" | Categories == "Dry", 0,
                                            dplyr::if_else(Categories == "Lac1", purrr::pmap_dbl(list(day_min, day_max), average_milk_protein, parity = "primiparous", lambda_prot = lambda_milk_prot_calc()),
                                                           dplyr::if_else(Categories == "Lac2",
                                                                          purrr::pmap_dbl(list(day_min, day_max), average_milk_protein, parity = "secondiparous", lambda_prot = lambda_milk_prot_calc()),
                                                                          purrr::pmap_dbl(list(day_min, day_max), average_milk_protein, parity = "multiparous", lambda_prot = lambda_milk_prot_calc())))),

              # milk corrected for 4% fat and 3.3% of protein
              milk_yield_kg_fpc = energy_corrected_milk(milk_yield = milk_yield_kg_cow2, milk_fat = milk_fat, milk_prot = milk_protein))

          # Dry matter intake calculations

          df_dmi <- df_my %>%
            dplyr::mutate(
              starter_intake_kg = dplyr::if_else(Categories == "Cal",
                                                 purrr::map2_dbl(day_min,
                                                                 day_max,
                                                                 starter_intake_calves_interval_days,
                                                                 milk_intake = milk_sup_l), 0),
              milk_solids_intake_kg = dplyr::if_else(Categories == "Cal",
                                                     (milk_sup_l * milk_solids / 100), 0),
              forage_intake_kg = dplyr::if_else(Categories == "Cal" & Month == 1, forage_intake_1,
                                                dplyr::if_else(Categories == "Cal" & Month == 2, forage_intake_2, 0)),

              dry_matter_intake_kg_animal = dplyr::if_else(Categories == "Cow",
                                                           lactating_dry_matter_intake(((day_min + day_max) / 2), body_weight = mean_body_weight_kg, milk_yield = milk_yield_kg_fpc),
                                                           dplyr::if_else(Categories == "Hei",
                                                                          heifer_dry_matter_intake(mature_body_weight = mature_body_weight, body_weight = mean_body_weight_kg),
                                                                          dplyr::if_else(Categories == "Cal",
                                                                                         starter_intake_kg + milk_solids_intake_kg + forage_intake_kg,
                                                                                         dry_cow_dry_matter_intake(body_weight = mean_body_weight_kg)))),
              dry_matter_intake_kg = round(dry_matter_intake_kg_animal * 30, 2),
              dry_matter_intake_bw = dplyr::if_else(Categories != "Cow", round(dry_matter_intake_kg_animal / mean_body_weight_kg * 100, 2), round(dry_matter_intake_kg_animal / mean_cow_weight_kg * 100, 2)),
              feed_efficiency = round(milk_yield_kg_fpc / dry_matter_intake_kg_animal, 2)
            ) %>%
            dplyr::ungroup() %>%
            dplyr::group_by(Phase)

        # Water intake calculations

          df_water <- df_dmi %>%
            dplyr::mutate(
              water_intake_l_animal = dplyr::if_else(Categories == "Cow",
                                                     lactating_water_intake(temp_c = 7.67, milk_yield_kg = milk_yield_kg_fpc, bw_kg = mean_body_weight_kg),
                                                           dplyr::if_else(Categories == "Hei",
                                                                          heifer_dry_cows_water_intake(bw_kg = mean_body_weight_kg),
                                                                          dplyr::if_else(Categories == "Cal",
                                                                                         0,
                                                                                         heifer_dry_cows_water_intake(bw_kg = mean_body_weight_kg)))))

        # GHG emissions

        df_ghg <- df_water %>%
          dplyr::mutate(
            gei_mj_day = dplyr::if_else(Categories == "Cow",
                                        gross_energy_intake(dry_matter_intake       = dry_matter_intake_kg_animal,
                                                            crude_protein           = diet_cp_lac,
                                                            ether_extract           = diet_ee_lac,
                                                            neutral_detergent_fiber = diet_ndf_lac),
                                        dplyr::if_else(Categories == "Hei",
                                                       gross_energy_intake(dry_matter_intake       = dry_matter_intake_kg_animal,
                                                                           crude_protein           = diet_cp_hei,
                                                                           ether_extract           = diet_ee_hei,
                                                                           neutral_detergent_fiber = diet_ndf_hei),
                                                       gross_energy_intake(dry_matter_intake       = dry_matter_intake_kg_animal,
                                                                           crude_protein           = diet_cp_dry,
                                                                           ether_extract           = diet_ee_dry,
                                                                           neutral_detergent_fiber = diet_ndf_dry))),
            enteric_methane_g_animal_day = dplyr::if_else(Categories == "Cow",
                                                          lactating_enteric_methane(gross_energy_intake     = gei_mj_day,
                                                                                    neutral_detergent_fiber = diet_ndf_lac,
                                                                                    ether_extract           = diet_ee_lac,
                                                                                    body_weight             = mean_body_weight_kg,
                                                                                    milk_fat                = milk_fat) * (1 - methane_aditive / 100),
                                                          dplyr::if_else(Categories == "Hei",
                                                                         heifer_enteric_methane(gross_energy_intake     = gei_mj_day,
                                                                                                neutral_detergent_fiber = diet_ndf_hei,
                                                                                                body_weight             = mean_body_weight_kg),
                                                                         dry_enteric_methane(gross_energy_intake = gei_mj_day,
                                                                                              ether_extract      = diet_ee_dry))),
            total_methane_g_animal = enteric_methane_g_animal_day * 30,
            methane_yield = dplyr::if_else(Categories == "Cow", enteric_methane_g_animal_day / dry_matter_intake_kg_animal, 0),
            methane_intensity = dplyr::if_else(Categories == "Cow", enteric_methane_g_animal_day / milk_yield_kg_fpc, 0),
            methane_acum_animal = cumsum(enteric_methane_g_animal_day * 30),

            co2_emissions_kg = animal_co2_emissions(dry_matter_intake = dry_matter_intake_kg_animal, body_weight = mean_body_weight_kg),

            n2o_emissions_g = dplyr::if_else(Categories == "Hei",
                                              animal_n2o_emissions(dry_matter_intake = dry_matter_intake_kg_animal,
                                                                   crude_protein     = diet_cp_hei),
                                              dplyr::if_else(Categories == "Cow",
                                                             animal_n2o_emissions(dry_matter_intake = dry_matter_intake_kg_animal,
                                                                                  crude_protein     = diet_cp_lac),
                                                             dplyr::if_else(Categories == "Dry",
                                                                            animal_n2o_emissions(dry_matter_intake = dry_matter_intake_kg_animal,
                                                                                                 crude_protein     = diet_cp_dry),
                                                                            animal_n2o_emissions(dry_matter_intake = dry_matter_intake_kg_animal,
                                                                                                 crude_protein = 24))))) #TODO

          # Manure calculations

        df_manure <- df_ghg %>%
          dplyr::mutate(
            fresh_manure_output_kg_day = dplyr::if_else(Categories == "Hei",
                                                        heifer_manure_excretion(dry_matter_intake = dry_matter_intake_kg_animal,
                                                                                body_weight       = mean_body_weight_kg),
                                                                       dplyr::if_else(Categories == "Cow",
                                                                                      lactation_manure_excretion(dry_matter_intake = dry_matter_intake_kg_animal),
                                                                                      dplyr::if_else(Categories == "Cal",
                                                                                                     heifer_manure_excretion(dry_matter_intake = dry_matter_intake_kg_animal,
                                                                                                                             body_weight       = mean_body_weight_kg),
                                                                                                     dry_manure_excretion(body_weight          = mean_body_weight_kg,
                                                                                                                          crude_protein           = diet_cp_dry,
                                                                                                                          neutral_detergent_fiber = diet_ndf_dry)))),
            dm_manure_output_kg_day = dplyr::if_else(Categories == "Cow",
                                                     lactation_dm_manure_excretion(dry_matter_intake = dry_matter_intake_kg_animal),
                                                     dplyr::if_else(Categories == "Hei" & Month > 8,
                                                                    15.26 * fresh_manure_output_kg_day / 100,
                                                                    dplyr::if_else(Categories == "Hei" & Month <= 8, # TODO
                                                                                   heifer_dm_manure_excretion(dry_matter_intake = dry_matter_intake_kg_animal),
                                                                                   dplyr::if_else(Categories == "Cal",
                                                                                                  starter_intake_kg * (1 - 0.6) + milk_solids_intake_kg * (1 - 0.9) + forage_intake_kg * (1 - 0.4), #TODO
                                                                                                  11.76 * fresh_manure_output_kg_day / 100)))),
            manure_dm_content = dm_manure_output_kg_day / fresh_manure_output_kg_day  * 100)

          # Volatile solids

          df_volatile_solids <- df_manure %>%
            dplyr::mutate(
              volatile_solids_kg_d = dplyr::if_else(Categories == "Hei",
                                             volatile_solids(dry_matter_intake       = dry_matter_intake_kg_animal,
                                                             neutral_detergent_fiber = diet_ndf_hei,
                                                             crude_protein           = diet_cp_hei),
                                             dplyr::if_else(Categories == "Dry",
                                                     volatile_solids(dry_matter_intake       = dry_matter_intake_kg_animal,
                                                                     neutral_detergent_fiber = diet_ndf_dry,
                                                                     crude_protein           = diet_cp_dry),
                                                     dplyr::if_else(Categories == "Cow",
                                                             volatile_solids(dry_matter_intake       = dry_matter_intake_kg_animal,
                                                                             neutral_detergent_fiber = diet_ndf_lac,
                                                                             crude_protein           = diet_cp_lac),
                                                             dry_matter_intake_kg_animal * 0.1 * 0.9))), # TODO,
              digestible_volatile_solids_kg_d = dplyr::if_else(Categories == "Hei",
                                                               digestible_volatile_solids(dry_matter_intake = dry_matter_intake_kg_animal,
                                                                                          neutral_detergent_fiber = diet_ndf_hei,
                                                                                          acid_detergent_fiber = diet_nda_hei,
                                                                                          crude_protein = diet_cp_hei),
                                                               dplyr::if_else(Categories == "Dry",
                                                                              digestible_volatile_solids(dry_matter_intake = dry_matter_intake_kg_animal,
                                                                                                         neutral_detergent_fiber = diet_ndf_dry,
                                                                                                         acid_detergent_fiber = diet_nda_dry,
                                                                                                         crude_protein = diet_cp_dry),
                                                                              dplyr::if_else(Categories == "Cow",
                                                                                             digestible_volatile_solids(dry_matter_intake = dry_matter_intake_kg_animal,
                                                                                                                        neutral_detergent_fiber = diet_ndf_lac,
                                                                                                                        acid_detergent_fiber = diet_nda_lac,
                                                                                                                        crude_protein = diet_cp_lac),
                                                                                             digestible_volatile_solids(dry_matter_intake = dry_matter_intake_kg_animal,
                                                                                                                        neutral_detergent_fiber = 0,
                                                                                                                        acid_detergent_fiber = 0,
                                                                                                                        crude_protein = 24)))), # TODO
              volatile_solids_content_dm = volatile_solids_kg_d / fresh_manure_output_kg_day * 100,

              volatile_solids_digestibility = digestible_volatile_solids_kg_d / volatile_solids_kg_d)

          incProgress(1/15, message = "We are almost there...")

          # Nitrogen calculations

          df_nitrogen <- df_volatile_solids %>%
            dplyr::mutate(

              # urine calculations

              total_urine_excretion_kg = dplyr::if_else(Categories == "Hei",
                                                        7,#0.025*mean_body_weight_kg,#lactating_urine_excretion(dry_matter_intake_kg_animal, diet_cp_hei, 0),
                                                         dplyr::if_else(Categories == "Dry",
                                                                        17,#0.025*mean_body_weight_kg,#lactating_urine_excretion(dry_matter_intake_kg_animal, diet_cp_dry, 0),
                                                                        dplyr::if_else(Categories == "Cow",
                                                                                       lactating_urine_excretion(dry_matter_intake_kg_animal, diet_cp_lac, milk_protein),
                                                                                       0))), #TODO

              total_nitrogen_ingested_g = dplyr::if_else(Categories == "Hei",
                                                  (dry_matter_intake_kg_animal * 1000) * (diet_cp_hei / 100) / 6.25,
                                                  dplyr::if_else(Categories == "Dry",
                                                          (dry_matter_intake_kg_animal * 1000) * (diet_cp_dry / 100) / 6.25,
                                                          dplyr::if_else(Categories == "Cow",
                                                                  dry_matter_intake_kg_animal * 1000 * (diet_cp_lac / 100) / 6.25,
                                                                  ((milk_solids_intake_kg * milk_prot / milk_solids) / 6.25 + (starter_intake_kg * starter_cp / 100) / 6.25 + (forage_intake_kg * forage_cp / 100) / 6.25) * 1000))),
              total_nitrogen_excreted_g = dplyr::if_else(Categories == "Cow",
                                                         lactating_n_total_excretion(nitrogen_intake = total_nitrogen_ingested_g),
                                                  dplyr::if_else(Categories == "Dry",
                                                                 dry_n_total_excretion(nitrogen_intake = total_nitrogen_ingested_g),
                                                          dplyr::if_else(Categories == "Hei",
                                                                  heifer_n_total_excretion(nitrogen_intake = total_nitrogen_ingested_g),
                                                                          ((milk_solids_intake_kg * milk_prot / milk_solids) * (1 - 0.75) + starter_intake_kg * starter_cp / 100 * (1 - 0.65) + forage_intake_kg * forage_cp / 100 * (1 - 0.50) * 1000) ))), # TODO
              fecal_nitrogen_excreted_d = dplyr::if_else(Categories == "Cow",
                                                         lactating_n_fecal_excretion(nitrogen_intake = total_nitrogen_ingested_g),
                                                         dplyr::if_else(Categories == "Dry",
                                                                        dry_n_fecal_excretion(nitrogen_intake = total_nitrogen_ingested_g),
                                                                        dplyr::if_else(Categories == "Cal",
                                                                                       total_nitrogen_excreted_g * 0.2,
                                                                                       heifer_n_fecal_excretion(nitrogen_intake = total_nitrogen_ingested_g)))),
              urine_nitrogen_excreted_g = total_nitrogen_excreted_g - fecal_nitrogen_excreted_d,

              milk_n_output_g = dplyr::if_else(Categories == "Cow", milk_yield_kg_fpc * milk_protein / 100 / 6.25 * 1000, 0),

              nitrogen_balance_g = total_nitrogen_ingested_g - total_nitrogen_excreted_g - milk_n_output_g,

              n_ef = milk_n_output_g / total_nitrogen_ingested_g * 100)

          # P calculations

          df_phosphorous <- df_nitrogen %>%
            dplyr::mutate(
              total_phosphorus_ingested_g = dplyr::if_else(Categories == "Hei",
                                                           (dry_matter_intake_kg_animal * 1000) * (diet_p_hei / 100),
                                                           dplyr::if_else(Categories == "Dry",
                                                                          (dry_matter_intake_kg_animal * 1000) * (diet_p_dry / 100),
                                                                          dplyr::if_else(Categories == "Cal",
                                                                                         milk_sup_l * milk_p_g_l + (starter_intake_kg * starter_p / 100 * 1000) + (forage_intake_kg * forage_p / 100 * 1000),
                                                                                         (dry_matter_intake_kg_animal * 1000) * (diet_p_lac / 100)))),

              total_phosphorus_excretion_milk = dplyr::if_else(Categories == "Cow", milk_yield_kg_fpc * milk_p_g_l, 0),

              total_phosphorus_excretion_g = dplyr::if_else(Categories == "Hei",
                                                            total_phosphorus_ingested_g, #TODO
                                                            dplyr::if_else(Categories == "Dry",
                                                                           total_phosphorus_ingested_g, #TODO
                                                                           dplyr::if_else(Categories == "Cal",
                                                                                          (milk_sup_l * milk_p_g_l * (1 - 0.85) + starter_intake_kg * starter_p / 100 * (1 - 0.85) + forage_intake_kg * forage_p / 100 * (1 - 0.70) * 1000),
                                                                                          #total_phosphorus_ingested_g - total_phosphorus_excretion_milk
                                                                                           lactating_p_total_excretion(dry_matter_intake       = dry_matter_intake_kg_animal,
                                                                                                                       phosphorous_content     = diet_p_lac,
                                                                                                                       milk_p_excretion        = total_phosphorus_excretion_milk)
                                                                                          ))),


              phosphorus_balance_g = total_phosphorus_ingested_g - total_phosphorus_excretion_g - total_phosphorus_excretion_milk)

          # K calculations

          df_potassium <- df_phosphorous %>%
            dplyr::mutate(
              total_potassium_ingested_g = dplyr::if_else(Categories == "Hei",
                                                          (dry_matter_intake_kg_animal * 1000) * (diet_k_hei / 100),
                                                          dplyr::if_else(Categories == "Dry",
                                                                         (dry_matter_intake_kg_animal * 1000) * (diet_k_dry / 100),
                                                                         dplyr::if_else(Categories == "Cal",
                                                                                        milk_sup_l * milk_k_g_l + (starter_intake_kg * starter_k / 100 * 1000) + (forage_intake_kg * forage_k / 100 * 1000),
                                                                                        (dry_matter_intake_kg_animal * 1000) * (diet_k_lac / 100)))),

              total_potassium_excretion_milk = dplyr::if_else(Categories == "Cow", milk_k_excretion(milk_yield_kg_fpc), 0),

              total_potassium_excretion_g = dplyr::if_else(Categories == "Hei",
                                                           total_potassium_ingested_g,
                                                           dplyr::if_else(Categories == "Dry",
                                                                           total_potassium_ingested_g,
                                                                           dplyr::if_else(Categories == "Cal",
                                                                                          (milk_sup_l * milk_k_g_l * (1 - 0.85) + starter_intake_kg * starter_k / 100 * (1 - 0.85) + forage_intake_kg * forage_k / 100 * (1 - 0.70) * 1000), #TODO
                                                                                          total_potassium_ingested_g - total_potassium_excretion_milk
                                                                                          ))))

          df_potassium %>%
            dplyr::ungroup()


        }

      )

    })

    df_sum <- reactive({

      df_sum <- df() %>%
        dplyr::group_by(MonthSimulated, Categories) %>%
        dplyr::summarise(
          total_animals            = round(sum(NumberAnimals)),
          milk_yield_kg            = stats::weighted.mean(milk_yield_kg_cow2, NumberAnimals),
          milk_cp_pct              = stats::weighted.mean(milk_protein, NumberAnimals),
          milk_fat_pct             = input$calf_fat,
          milk_yield_kg_fpc        = stats::weighted.mean(milk_yield_kg_fpc, NumberAnimals),
          dmi_kg                   = stats::weighted.mean(dry_matter_intake_kg_animal, NumberAnimals),
          water_l                  = stats::weighted.mean(water_intake_l_animal, NumberAnimals),
          total_ch4_kg             = sum(NumberAnimals * enteric_methane_g_animal_day / 1000),
          total_n2o_kg             = sum(NumberAnimals * n2o_emissions_g / 1000),
          total_manure_kg          = sum(NumberAnimals * fresh_manure_output_kg_day),
          total_urine_kg           = sum(NumberAnimals * total_urine_excretion_kg),
          total_solids_kg          = sum(NumberAnimals * dm_manure_output_kg_day),
          total_volatile_solids_kg = sum(NumberAnimals * volatile_solids_kg_d),
          total_tan_excreted_g     = sum(NumberAnimals * urine_nitrogen_excreted_g),
          total_fecal_n_escreted_g = sum(NumberAnimals * fecal_nitrogen_excreted_d),
          # correct the names below
          # TODO
          total_n_excreted_g       = stats::weighted.mean(total_nitrogen_excreted_g, NumberAnimals),
          total_p_excreted_g       = stats::weighted.mean(total_phosphorus_excretion_g, NumberAnimals),
          total_k_excreted_g       = stats::weighted.mean(total_potassium_excretion_g, NumberAnimals),
          .groups = "drop",
        ) %>%
        dplyr::filter(MonthSimulated == 1) %>%
        dplyr::select(-MonthSimulated) %>%
        dplyr::mutate_if(is.numeric, round, 7)

      df_sum

    })

# -------------------------------------------------------------------------
# Herd Inventory ----------------------------------------------------------
# -------------------------------------------------------------------------

    # lactating cows

    output$number_lac <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$total_animals[2], 2),
        title    = "Lactating cows",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 1,
        color    = "white",
        href     = NULL
      )
    })

    # dry cows

    output$number_dry <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$total_animals[3], 2),
        title    = "Dry cows",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 1,
        color    = "white",
        href     = NULL
      )
    })

    # heifers

    output$number_hei <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$total_animals[4], 2),
        title    = "Heifers",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 1,
        color    = "white",
        href     = NULL
      )
    })

    # calves

    output$number_cal <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$total_animals[1], 2),
        title    = "Calves",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 1,
        color    = "white",
        href     = NULL
      )
    })




    # Message about Herd stability

    message <- eventReactive(input$button, {

      if(round(herd_matrix()[[3]], 0) > round(herd_matrix()[[2]], 0)) {

        paste0("The average number of female calves born monthly is ", round(herd_matrix()[[2]], 0), ". The average number of female calves needed to make up the replacement herd is ",
               round(herd_matrix()[[3]], 0), " .")

      } else if (round(herd_matrix()[[3]], 0) < round(herd_matrix()[[2]], 0)) {

        paste0("The average number of female calves born monthly is ", round(herd_matrix()[[2]], 0), ". The average number of female calves needed to make up the replacement herd is ",
               round(herd_matrix()[[3]], 0), " .")

      } else {

        paste0("The average number of female calves born monthly is ", round(herd_matrix()[[2]], 0), ". The average number of female calves needed to make up the replacement herd is ",
               round(herd_matrix()[[3]], 0), " . Then, your herd is in equilibrium.")

      }


    })

    # Outputting the message

    output$herd_message <- renderText({

      message()

    })

    observeEvent({input$button}, showNotification("You're ready to explore the outcomes!",
                                                  duration = 2,
                                                  type = "message"))

    observeEvent({list(input$animal_time_first_calv,
                       input$animal_heifer_calf_born,
                       input$animal_calves_heifers_cul,
                       input$animal_average_milk_yield,
                       input$animal_cow_rep_rate,
                       input$animal_cow_calving_int,
                       input$animal_milk_freq,
                       input$animal_n_cows,
                       input$animal_mature_weight,

                       input$calf_birth_weight,
                       input$calf_milk_sup,
                       input$calf_fat,
                       input$calf_protein,
                       input$calf_starter_cp,
                       input$calf_forage_cp,
                       input$calf_forage_p,
                       input$calf_starter_p,

                       input$diet_lac_cp,
                       input$diet_lac_ee,
                       input$diet_lac_ndf,
                       input$diet_lac_adf,
                       input$diet_lac_p,
                       input$diet_lac_k,
                       input$diet_hei_cp,
                       input$diet_hei_ee,
                       input$diet_hei_ndf,
                       input$diet_hei_adf,
                       input$diet_hei_p,
                       input$diet_hei_k,
                       input$diet_dry_cp,
                       input$diet_dry_ee,
                       input$diet_dry_ndf,
                       input$diet_dry_adf,
                       input$diet_dry_p,
                       input$diet_dry_k,
                       input$radio,
                       input$methane_red


                       )},
                 {showNotification("The model hasn't yet ran or you've changed some input!
                                   Please remember to click Run again!",
                                   duration = 10,
                                   type = "error")

                   })

# -------------------------------------------------------------------------
# Performance metrics -----------------------------------------------------
# -------------------------------------------------------------------------

    # DMI

    output$hei_dmi <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$dmi_kg[4], 2),
        title    = "Heifer DMI (kg/d)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 1,
        color    = "white",
        href     = NULL
      )
    })

    output$dry_dmi <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$dmi_kg[3], 2),
        title    = "Dry Cows DMI (kg/d)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 1,
        color    = "white",
        href     = NULL
      )

    })

    output$lac_dmi <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$dmi_kg[2], 2),
        title    = "Milking Cows DMI (kg/d)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    # Water intake

    output$hei_water <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$water_l[4], 2),
        title    = "Heifer Water Intake (l/d)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 1,
        color    = "white",
        href     = NULL
      )
    })

    output$dry_water <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$water_l[3], 2),
        title    = "Dry Cows Water Intake (l/d)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 1,
        color    = "white",
        href     = NULL
      )

    })

    output$lac_water <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$water_l[2], 2),
        title    = "Milking Cows Water Intake (l/d)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     =  " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    # Milk Yield - non corrected for fat and Protein

    output$lac_my <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$milk_yield_kg[2], 1),
        title    = "Milk Yield (kg/d)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     =  " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    # Milk Yield - corrected for 4% fat and 3.3% protein

    output$lac_my_fpc <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$milk_yield_kg_fpc[2], 1),
        title    = "Milk Yield Corrected for 4% Fat and 3.3% Protein (kg/d)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     =  " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    # Feed Efficiency

    output$feed_ef <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$milk_yield_kg_fpc[2] / df_sum()$dmi_kg[2], 2),
        title    = "Feed Efficiency",
        sparkobj = NULL,
        subtitle = tagList(" "),
        info     = " ",
        icon     =  " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

# -------------------------------------------------------------------------
# Plots performance metrics -----------------------------------------------
# -------------------------------------------------------------------------

    # lactation curves

    output$lactation_curves_plotly <- plotly::renderPlotly({

      df() %>%
        dplyr::filter(Categories == "Cow" & MonthSimulated == 1 & Phase %in% c("Lac1", "Lac2", "Lac3")) %>%
        dplyr::mutate(
          Phase = dplyr::if_else(Phase == "Lac1", "Primip", dplyr::if_else(Phase == "Lac2", "Second", "Multi"))
        ) %>%
        plotly::plot_ly(x = ~ Month,
                        y = ~ milk_yield_kg_cow2,
                        color = ~ Phase,
                        type = "scatter",
                        mode = "lines+markers") %>%
        plotly::layout(yaxis = list(title = "Milk Yield (kg/day)"),
                       xaxis = list(title = "Months in Lactation")) %>%
        plotly::config(displayModeBar = FALSE)

    })

    # DMI curves

    output$dmi_curves_plotly <- plotly::renderPlotly({

      df() %>%
        dplyr::filter(Categories == "Cow" & MonthSimulated == 1 & Phase %in% c("Lac1", "Lac2", "Lac3")) %>%
        dplyr::mutate(
          Phase = dplyr::if_else(Phase == "Lac1", "Primip", dplyr::if_else(Phase == "Lac2", "Second", "Multi"))
        ) %>%
        plotly::plot_ly(x = ~ Month,
                        y = ~ dry_matter_intake_kg_animal,
                        color = ~ Phase,
                        type = "scatter",
                        mode = "lines+markers") %>%
        plotly::layout(yaxis = list(title = "Dry Matter Intake (kg/day)"),
                       xaxis = list(title = "Months in Lactation")) %>%
        plotly::config(displayModeBar = FALSE)

    })

# -------------------------------------------------------------------------
# Methane metrics ---------------------------------------------------------
# -------------------------------------------------------------------------

    output$hei_ch4 <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$total_ch4_kg[4] / df_sum()$total_animals[4], 3),
        title    = "Heifers Methane Emissions (kg/d)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     =  " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    output$dry_ch4 <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$total_ch4_kg[3] / df_sum()$total_animals[3], 3),
        title    = "Dry Cows Methane Emissions (kg/d)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     =  " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    output$lac_ch4 <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$total_ch4_kg[2] / df_sum()$total_animals[2], 3),
        title    = "Milking Cows Methane Emissions (kg/d)",
        sparkobj = NULL,
        subtitle = tagList(" "),
        info     = " ",
        icon     =  " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    output$methane_yield <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$total_ch4_kg[2] / df_sum()$total_animals[2] / df_sum()$dmi_kg[2] * 1000, 1),
        title    = "Methane Yield (g/kg Dry Matter)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    output$methane_intensity <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$total_ch4_kg[2] / df_sum()$total_animals[2] / df_sum()$milk_yield_kg_fpc[2] * 1000, 1),
        title    = "Methane Intensity (g/kg Milk)",
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
# Manure ------------------------------------------------------------------
# -------------------------------------------------------------------------

    output$hei_manure <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$total_manure_kg[4] / df_sum()$total_animals[4], 2),
        title    = "Heifer Manure Excretion (kg/d)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     =  " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    output$dry_manure <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$total_manure_kg[3] / df_sum()$total_animals[3], 2),
        title    = "Dry Cows Manure Excretion (kg/d)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     =  " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    output$lac_manure <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$total_manure_kg[2] / df_sum()$total_animals[2], 2),
        title    = "Milking Cows Manure Excretion (kg/d)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     =  " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

# -------------------------------------------------------------------------
# Methane and Manure plots ------------------------------------------------
# -------------------------------------------------------------------------

    # Methane curves

    output$methane_curves_plotly <- plotly::renderPlotly({

      df() %>%
        dplyr::filter(Categories == "Cow" & MonthSimulated == 1 & Phase %in% c("Lac1", "Lac2", "Lac3")) %>%
        dplyr::mutate(
          Phase = dplyr::if_else(Phase == "Lac1", "Primip", dplyr::if_else(Phase == "Lac2", "Second", "Multi"))
        ) %>%
        plotly::plot_ly(x = ~ Month,
                        y = ~ enteric_methane_g_animal_day / 1000,
                        color = ~ Phase,
                        type = "scatter",
                        mode = "lines+markers") %>%
        plotly::layout(yaxis = list(title = "Methane Emissions (kg/day)"),
                       xaxis = list(title = "Months in Lactation")) %>%
        plotly::config(displayModeBar = FALSE)

    })

    # Manure curves

    output$manure_curves_plotly <- plotly::renderPlotly({

      df() %>%
        dplyr::filter(Categories == "Cow" & MonthSimulated == 1 & Phase %in% c("Lac1", "Lac2", "Lac3")) %>%
        dplyr::mutate(
          Phase = dplyr::if_else(Phase == "Lac1", "Primip", dplyr::if_else(Phase == "Lac2", "Second", "Multi"))
        ) %>%
        plotly::plot_ly(x = ~ Month,
                        y = ~ fresh_manure_output_kg_day,
                        color = ~ Phase,
                        type = "scatter",
                        mode = "lines+markers") %>%
        plotly::layout(yaxis = list(title = "Manure Excretion (kg/day)"),
                       xaxis = list(title = "Months in Lactation")) %>%
        plotly::config(displayModeBar = FALSE)

    })

# -------------------------------------------------------------------------
# Nutrient balances -------------------------------------------------------
# -------------------------------------------------------------------------

    # nitrogen

    output$hei_nit <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$total_n_excreted_g[4] / 1000, 2),
        title    = "Heifers Nitrogen Excretion (kg/d)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    output$dry_nit <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$total_n_excreted_g[3] / 1000, 2),
        title    = "Dry Cows Nitrogen Excretion (kg/d)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    output$lac_nit <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$total_n_excreted_g[2] / 1000, 2),
        title    = "Milking Cows Nitrogen Excretion (kg/d)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    # Phosphorous

    output$hei_pho <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$total_p_excreted_g[4] / 1000, 2),
        title    = "Heifers Phosphorous Excretion (kg/d)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    output$dry_pho <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$total_p_excreted_g[3] / 1000, 2),
        title    = "Dry Cows Phosphorous Excretion (kg/d)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    output$lac_pho <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$total_p_excreted_g[2] / 1000, 2),
        title    = "Milking Cows Phosphorous Excretion (kg/d)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    # Potassium

    output$hei_pot <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$total_k_excreted_g[4] / 1000, 2),
        title    = "Heifers Potassium Excretion (kg/d)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    output$dry_pot <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$total_k_excreted_g[3] / 1000, 2),
        title    = "Dry Cows Potassium Excretion (kg/d)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    output$lac_pot <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(df_sum()$total_k_excreted_g[2] / 1000, 2),
        title    = "Milking Cows Potassium Excretion (kg/d)",
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
# N, P, and K plots -------------------------------------------------------
# -------------------------------------------------------------------------

    # nitrogen curves

    output$nit_curves_plotly <- plotly::renderPlotly({

      df() %>%
        dplyr::filter(Categories == "Cow" & MonthSimulated == 1 & Phase %in% c("Lac1", "Lac2", "Lac3")) %>%
        dplyr::mutate(
          Phase = dplyr::if_else(Phase == "Lac1", "Primip", dplyr::if_else(Phase == "Lac2", "Second", "Multi"))
        ) %>%
        plotly::plot_ly(x = ~ Month,
                        y = ~ round(total_nitrogen_excreted_g / 1000, 3),
                        color = ~ Phase,
                        type = "scatter",
                        mode = "lines+markers") %>%
        plotly::layout(yaxis = list(title = "Total Nitrogen Excretion (kg/day)"),
                       xaxis = list(title = "Months in Lactation")) %>%
        plotly::config(displayModeBar = FALSE)

    })

    # phosphorous curves

    output$pho_curves_plotly <- plotly::renderPlotly({

      df() %>%
        dplyr::filter(Categories == "Cow" & MonthSimulated == 1 & Phase %in% c("Lac1", "Lac2", "Lac3")) %>%
        dplyr::mutate(
          Phase = dplyr::if_else(Phase == "Lac1", "Primip", dplyr::if_else(Phase == "Lac2", "Second", "Multi"))
        ) %>%
        plotly::plot_ly(x = ~ Month,
                        y = ~ round(total_phosphorus_excretion_g / 1000, 4),
                        color = ~ Phase,
                        type = "scatter",
                        mode = "lines+markers") %>%
        plotly::layout(yaxis = list(title = "Total Phosphorous Excretion (kg/day)"),
                       xaxis = list(title = "Months in Lactation")) %>%
        plotly::config(displayModeBar = FALSE)

    })

    # potassium curves

    output$pot_curves_plotly <- plotly::renderPlotly({

      df() %>%
        dplyr::filter(Categories == "Cow" & MonthSimulated == 1 & Phase %in% c("Lac1", "Lac2", "Lac3")) %>%
        dplyr::mutate(
          Phase = dplyr::if_else(Phase == "Lac1", "Primip", dplyr::if_else(Phase == "Lac2", "Second", "Multi"))
        ) %>%
        plotly::plot_ly(x = ~ Month,
                        y = ~ round(total_potassium_excretion_g / 1000, 4),
                        color = ~ Phase,
                        type = "scatter",
                        mode = "lines+markers") %>%
        plotly::layout(yaxis = list(title = "Total Potassium Excretion (kg/day)"),
                       xaxis = list(title = "Months in Lactation")) %>%
        plotly::config(displayModeBar = FALSE)

    })

# -------------------------------------------------------------------------
# Report outcomes - inputs ------------------------------------------------
# -------------------------------------------------------------------------

    herd_inputs <- reactive({

      df <- tibble::tibble(
        "Total Cows"                = input$animal_n_cows,
        "Calving Interval (mo)"     = input$animal_cow_calving_int,
        "Cow Culling Rate (%)"      = input$animal_cow_rep_rate,
        "Age at First Calving (mo)" = input$animal_time_first_calv,
        "Heifers Culling Rate (%)"  = input$animal_calves_heifers_cul,
        "Milk Yield (kg/day)"       = input$animal_average_milk_yield,
        "Milking Frequency"         = input$animal_milk_freq
      )

      df

    })

    diet_inputs <- reactive({

      df <- tibble::tibble(
        "Categories" = c("Milking Cows", "Dry Cows", "Heifers"),
        "CP (%)"     = c(input$diet_lac_cp, input$diet_dry_cp, input$diet_hei_cp),
        "NDF (%)"    = c(input$diet_lac_ndf, input$diet_dry_ndf, input$diet_hei_ndf),
        "ADF (%)"    = c(input$diet_lac_adf, input$diet_dry_adf, input$diet_hei_adf),
        "EE (%)"     = c(input$diet_lac_ee, input$diet_dry_ee, input$diet_hei_ee),
        "P (%)"      = c(input$diet_lac_p, input$diet_dry_p, input$diet_hei_p),
        "K (%)"      = c(input$diet_lac_k, input$diet_dry_k, input$diet_hei_k)

      )

      df

    })

# -------------------------------------------------------------------------
# Outputs from this module to populate others -----------------------------
# -------------------------------------------------------------------------

    milk_supply <- reactive({

      milk_supply <- input$calf_milk_sup

      milk_supply

      })

    return(
      list(
        df = reactive(df_sum()),
        milk_intake = reactive(milk_supply()),

        herd_inputs = reactive(herd_inputs()),
        diet_inputs = reactive(diet_inputs()),
        raw_animal_df = reactive({ df() })

      )
    )

  })
}
