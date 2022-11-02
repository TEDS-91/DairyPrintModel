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

    wellPanel(

    h3(icon("fa-duotone fa-cow", verify_fa = FALSE), strong("Herd Information"), align = "center"),

    style = "background-color:#E2EBF4",

    #waiter::use_waiter(),

    fluidRow(
      column(3,
    numericInput(ns("n_cows"),              "Number of cows:",            value = 120, min = 0,  max = 30000)),
      column(3,
    numericInput(ns("cow_calving_int"),     "Calving Interval (mo):",     value = 14,  min = 10, max = 20)),
      column(3,
    numericInput(ns("cow_rep_rate"),        "Cow Culling Rate (%):",      value = 30,  min = 20, max = 50)),
      column(3,
    numericInput(ns("time_first_calv"),     "First Calving (mo):",        value = 24,  min = 18, max = 36))),

    fluidRow(
      column(3,
    numericInput(ns("calves_heifers_cul"),  "Heifers Culling Rate (%):",  value = 20,  min = 10, max = 50)),
      column(2,
    numericInput(ns("stillbirth_rate"),     "Stillbirth Rate (%):",       value = 7,   min = 0,  max = 15)),
      column(2,
    numericInput(ns("heifer_calf_born"),    "Percentage Heifers (%):",    value = 50,  min = 45, max = 55)),
      column(2,
    numericInput(ns("average_milk_yield"),  "Average Milk Yield (kg):",   value = 40,  min = 30, max = 60)),
      column(3,
    selectInput(ns("milk_freq"), "Milk Frequence:", choices = c(2, 3))))),

    wellPanel(

    style = "background-color:#E2EBF4",

    fluidRow(
    column(12,
    h3(strong("Milk composition"), align = "center")),

    column(6,
           numericInput(ns("milk_protein"),  "Milk Protein (%):",         value = 3.25, min = 2, max = 5)),
    column(6,
           numericInput(ns("milk_fat"),  "Milk Fat (%):",                 value = 3.5,  min = 2, max = 5))
    )),

    wellPanel(

    style = "background-color:#E2EBF4",

    fluidRow(
    column(12,
    h3(strong("Diet Information"), align = "center")),

    h4(strong("Lactation Cows"), align = "left"),

    diet_ui_prms(ns("diet_lac")),

    h4(strong("Dry Cows"), align = "left"),

    diet_ui_prms(ns("diet_dry")),

    h4(strong("Heifers"), align = "left"),

    diet_ui_prms(ns("diet_hei")),

    h4(strong("Calves"), align = "left"),

    column(3,
           numericInput(ns("milk_sup"), "Milk Suply (l/d):", value = 6,   min = 2, max = 15)),
    column(2,
           numericInput(ns("starter_cp"), "Starter Crude Protein (%):", value = 20,   min = 15, max = 30)),
    column(2,
           numericInput(ns("starter_ndf"), "Starter P (%):", value = 20,   min = 15, max = 30)),
    column(2,
           numericInput(ns("forage_cp"), "Forage Crude Protein (%):", value = 20,   min = 15, max = 30)),
    column(3,
           numericInput(ns("forage_ndf"), "Forage P (%):", value = 20,   min = 15, max = 30))),


    br(),

    fluidRow(
    column(3,
    actionButton(ns("button"),
                 "Run!",
                 style = "color: #fff; background-color: #007582; border-color: #007582; height:40px; width:80px")),

    column(9,
    downloadButton("rmd_report", "Report.html",
                   style = "color: #fff; background-color: #007582; border-color: #007582")))),

    textOutput(ns("lambda")),
    shinycssloaders::withSpinner(tableOutput(ns("herd_stab2")), type = 1, color = "#0dc5c1", size = 5),
    plotOutput(ns("graphic"))

  )

}

#' animal Server Functions
#'
#' @noRd
mod_animal_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    herd_stab_matrix <- reactive({

      herd_stab_matrix <- herd_stabilization(time_first_calv    = input$time_first_calv,
                                             heifer_calf_born   = input$heifer_calf_born,
                                             stillbirth_rate    = input$stillbirth_rate,
                                             calves_heifers_cul = input$calves_heifers_cul,
                                             cow_rep_rate       = input$cow_rep_rate,
                                             cow_calving_int    = input$cow_calving_int,
                                             n_adult_cows       = input$n_cows)

    })

    herd_matrix <- reactive({

      herd_matrix <- herd_projection(time_first_calv    = input$time_first_calv,
                                     heifer_calf_born   = input$heifer_calf_born,
                                     stillbirth_rate    = input$stillbirth_rate,
                                     calves_heifers_cul = input$calves_heifers_cul,
                                     cow_rep_rate       = input$cow_rep_rate,
                                     cow_calving_int    = input$cow_calving_int,
                                     n_adult_cows       = input$n_cows,
                                     months_to_project  = 12)

    })

    lambda_milk_calc <- eventReactive(input$button, {

      age_first_calv <- input$time_first_calv

      cow_calving_int <- input$cow_calving_int

      n_cows <- input$n_cows

      prop_primiparous <- sum(herd_matrix()[[1]][1, (age_first_calv + 1):((age_first_calv + 1) + cow_calving_int - 1)]) / n_cows

      prop_secondiparous <- sum(herd_matrix()[[1]][1, (age_first_calv + 1 + cow_calving_int):(age_first_calv + 1 + 2 * cow_calving_int - 1)]) / n_cows


      lambda_milk_calc <- lambda_milk_yield(obs_average_milk_yield = input$average_milk_yield,
                                            milk_freq              = input$milk_freq,
                                            prop_primiparous       = prop_primiparous * 100,
                                            prop_secondiparous     = prop_secondiparous * 100,
                                            cow_calving_interval   = input$cow_calving_int) - 0.02
      lambda_milk_calc

      })


    output$lambda <- renderPrint({

     # paste("the lambda value is:", lambda_milk_calc())
     #
      input$diet_lac_cp


    })


    df <- eventReactive(input$button, {

      withProgress(message = "Running the model...", {


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

      birth_weight_kg <- 40
      milk_sup_l <- 6
      mature_body_weight <- 680
      milk_fat <- 3.67
      milk_prot <- 3.25
      milk_p_g_l <- 0.996
      milk_solids <- (1 - (100 - milk_fat - milk_prot - 4.6 - 0.8) / 100) * 100
      starter_cp <- 20
      starter_p <- 0.5
      forage_cp <- 12
      forage_intake_1 <- 0.05
      forage_intake_2 <- 0.25
      forage_p <- 0.5

      dmi_dry <- 0.02

      diet_cp_lac <- input$diet_lac_cp
      diet_ee_lac <- input$diet_lac_ee
      diet_ndf_lac <- input$diet_lac_ndf
      diet_nda_lac <- input$diet_lac_adf
      diet_p_lac <- input$diet_lac_p

      diet_cp_hei <- 12
      diet_ee_hei <- 3
      diet_ndf_hei <- 45
      diet_nda_hei <- 26
      diet_p_hei <- 0.5

      diet_cp_dry <- 12
      diet_ee_dry <- 3
      diet_ndf_dry <- 45
      diet_nda_dry <- 22
      diet_p_dry <- 0.5




      ##################################################################################################################

      calf_adg_kg <- seq(1, 60) %>%
        purrr::map_dbl(starter_intake_calves, milk_intake = milk_sup_l) %>%
        purrr::map_dbl(calf_adg, milk_intake = milk_sup_l) %>%
        sum() / 60

      weaning_weight_kg <- birth_weight_kg + calf_adg_kg * 60

      df <- df %>%
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
                                          age_first_calving  = input$time_first_calv,
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
                                          age_first_calving  = input$time_first_calv,
                                          mature_body_weight = mature_body_weight,
                                          type               = "final"),
                           dplyr::if_else(Categories == "Cal",
                                          purrr::map_dbl(day_max,
                                                         calf_body_weight,
                                                         birth_weight       = birth_weight_kg,
                                                         calf_adg           = calf_adg_kg,
                                                         type               = "final"), 0))
        ) %>%

        # BW calculations
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
          final_body_weight_kg = final_body_weight + bw_change_total_kg + cow_bw_parturium_kg,

          # Milk yield calculations
          milk_yield_kg_2 = dplyr::if_else(Phase == "Grow" | Categories == "Dry", 0,
                                           dplyr::if_else(Phase == "Lac1",
                                                          purrr::map2_dbl(day_min,
                                                                          day_max,
                                                                          milk_yield_acum,
                                                                          parity      = "primiparous",
                                                                          milk_freq   = input$milk_freq,
                                                                          lambda_milk = lambda_milk_calc()),
                                                          dplyr::if_else(Phase == "Lac2",
                                                                         purrr::map2_dbl(day_min,
                                                                                         day_max,
                                                                                         milk_yield_acum,
                                                                                         parity      = "secondiparous",
                                                                                         milk_freq   = input$milk_freq,
                                                                                         lambda_milk = lambda_milk_calc()),
                                                                         purrr::map2_dbl(day_min,
                                                                                         day_max,
                                                                                         milk_yield_acum,
                                                                                         parity = "multiparous",
                                                                                         milk_freq = input$milk_freq,
                                                                                         lambda_milk = lambda_milk_calc())))),
          milk_yield_kg_cow2 = milk_yield_kg_2 / 30,


          # Dry matter intake calculations
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
                                                       lactating_dry_matter_intake(((day_min + day_max) / 2), body_weight = mean_body_weight_kg, milk_yield = milk_yield_kg_cow2),
                                                       dplyr::if_else(Categories == "Hei",
                                                                      heifer_dry_matter_intake(mature_body_weight = mature_body_weight, body_weight = mean_body_weight_kg),
                                                        dplyr::if_else(Categories == "Cal",
                                                                       starter_intake_kg + milk_solids_intake_kg + forage_intake_kg,
                                                                       dry_cow_dry_matter_intake(body_weight = mean_body_weight_kg)))),
          dry_matter_intake_kg = round(dry_matter_intake_kg_animal * 30, 2),
          dry_matter_intake_bw = dplyr::if_else(Categories != "Cow", round(dry_matter_intake_kg_animal / mean_body_weight_kg * 100, 2), round(dry_matter_intake_kg_animal / mean_cow_weight_kg * 100, 2)),
          feed_efficiency = round(milk_yield_kg_cow2 / dry_matter_intake_kg_animal, 2)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::group_by(Phase) %>%

        # GHG emissions
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
                                                                                  milk_fat                = milk_fat),
                                                        dplyr::if_else(Categories == "Hei",
                                                                       heifer_enteric_methane(gross_energy_intake     = gei_mj_day,
                                                                                              neutral_detergent_fiber = diet_ndf_hei,
                                                                                              body_weight             = mean_body_weight_kg),
                                                                       dry_enteric_methane(gross_energy_intake = gei_mj_day,
                                                                                            ether_extract      = diet_ee_dry))),
          total_methane_g_animal = enteric_methane_g_animal_day * 30,
          methane_yield = dplyr::if_else(Categories == "Cow", enteric_methane_g_animal_day / dry_matter_intake_kg_animal, 0),
          methane_intensity = dplyr::if_else(Categories == "Cow", enteric_methane_g_animal_day / milk_yield_kg_cow2, 0),
          methane_acum_animal = cumsum(enteric_methane_g_animal_day * 30),

          co2_emissions_kg = animal_co2_emissions(dry_matter_intake = dry_matter_intake_kg_animal, body_weight = mean_body_weight_kg),

          n2o_emissions_kg = dplyr::if_else(Categories == "Hei",
                                            animal_n2o_emissions(dry_matter_intake = dry_matter_intake_kg_animal,
                                                                 crude_protein     = diet_cp_hei) / 1000,
                                            dplyr::if_else(Categories == "Cow",
                                                           animal_n2o_emissions(dry_matter_intake = dry_matter_intake_kg_animal,
                                                                                crude_protein     = diet_cp_lac) / 1000,
                                                           dplyr::if_else(Categories == "Dry",
                                                                          animal_n2o_emissions(dry_matter_intake = dry_matter_intake_kg_animal,
                                                                                               crude_protein     = diet_cp_dry) / 1000,
                                                                          animal_n2o_emissions(dry_matter_intake = dry_matter_intake_kg_animal,
                                                                                               crude_protein = 24) / 1000))), #TODO

          # Manure calculations
          fresh_manure_output_kg_day = dplyr::if_else(Categories == "Hei",
                                                      heifer_manure_excretion(dry_matter_intake = dry_matter_intake_kg_animal,
                                                                              body_weight       = mean_body_weight_kg),
                                                                     dplyr::if_else(Categories == "Cow",
                                                                                    lactation_manure_excretion(dry_matter_intake = dry_matter_intake_kg_animal),
                                                                                    dplyr::if_else(Categories == "Cal",
                                                                                                   heifer_manure_excretion(dry_matter_intake = dry_matter_intake_kg_animal,
                                                                                                                           body_weight       = mean_body_weight_kg),
                                                                                                   dry_manure_excretion(body_weight = mean_body_weight_kg,
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
          manure_dm_content = dm_manure_output_kg_day / fresh_manure_output_kg_day  * 100,

          # Volatile solids
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
                                                         volatile_solids(dry_matter_intake       = dry_matter_intake_kg_animal,
                                                                         neutral_detergent_fiber = 0,
                                                                         crude_protein           = 24)))), # TODO,
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

          volatile_solids_digestibility = digestible_volatile_solids_kg_d / volatile_solids_kg_d,

          # Nitrogen calculations
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
                                                                    dry_n_total_excretion(nitrogen_intake = total_nitrogen_ingested_g),
                                                                    dplyr::if_else(Categories == "Cal",
                                                                                   total_nitrogen_excreted_g * 0.2,
                                                                                   heifer_n_fecal_excretion(nitrogen_intake = total_nitrogen_ingested_g)))),
          urine_nitrogen_excreted_g = total_nitrogen_excreted_g - fecal_nitrogen_excreted_d,

          milk_protein = dplyr::if_else(Phase == "Grow" | Categories == "Dry", 0,
                                        dplyr::if_else(Categories == "Lac1", purrr::pmap_dbl(list(day_min, day_max), average_milk_protein, parity = "primiparous"),
                                                       dplyr::if_else(Categories == "Lac2", purrr::pmap_dbl(list(day_min, day_max), average_milk_protein, parity = "secondiparous"),
                                                                      purrr::pmap_dbl(list(day_min, day_max), average_milk_protein, parity = "multiparous")))),

          milk_n_output_g = dplyr::if_else(Categories == "Cow", milk_yield_kg_cow2 * milk_protein / 100 / 6.25 * 1000, 0),

          nitrogen_balance_g = total_nitrogen_ingested_g - total_nitrogen_excreted_g - milk_n_output_g,

          n_ef = milk_n_output_g / total_nitrogen_ingested_g * 100,

          # P calculations
          total_phosphorus_ingested_g = dplyr::if_else(Categories == "Hei",
                                                       (dry_matter_intake_kg_animal * 1000) * (diet_p_hei / 100),
                                                       dplyr::if_else(Categories == "Dry",
                                                                      (dry_matter_intake_kg_animal * 1000) * (diet_p_dry / 100),
                                                                      dplyr::if_else(Categories == "Cal",
                                                                                     milk_sup_l * milk_p_g_l + (starter_intake_kg * starter_p / 100 * 1000) + (forage_intake_kg * forage_p / 100 * 1000),
                                                                                     (dry_matter_intake_kg_animal * 1000) * (diet_p_lac / 100)))),

          total_phosphorus_excretion_milk = dplyr::if_else(Categories == "Cow", milk_yield_kg_cow2 * milk_p_g_l, 0),

          total_phosphorus_excretion_g = dplyr::if_else(Categories == "Hei",
                                                        total_phosphorus_ingested_g * (1 - 0.6), #TODO
                                                        dplyr::if_else(Categories == "Dry",
                                                                       total_phosphorus_ingested_g * (1 - 0.6), #TODO
                                                                       dplyr::if_else(Categories == "Cal",
                                                                                      (milk_sup_l * milk_p_g_l * (1 - 0.85) + starter_intake_kg * starter_p / 100 * (1 - 0.85) + forage_intake_kg * forage_p / 100 * (1 - 0.70) * 1000),
                                                                                      lactating_p_total_excretion(dry_matter_intake       = dry_matter_intake_kg_animal,
                                                                                                                  phosphorous_content     = diet_p_lac,
                                                                                                                  milk_p_excretion        = total_phosphorus_excretion_milk)))),


          phosphorus_balance_g = total_phosphorus_ingested_g - total_phosphorus_excretion_g - total_phosphorus_excretion_milk

        )


      #%>%
        #dplyr::filter(MonthSimulated == 1 & Categories == "Cow") %>%
        #dplyr::ungroup() %>%
        #dplyr::summarise(
        #  average_milk_yield = (weighted.mean(milk_yield_kg_2, NumberAnimals))) / 30

      }

      )

    })

    output$herd_stab2 <- renderTable({

        df()

    })

      observeEvent(input$button, {

        output$graphic <- renderPlot({

          df() %>%
          ggplot2::ggplot(ggplot2::aes(x = Month, mean_body_weight_kg, col = Phase)) +
            ggplot2::geom_line() +
            ggplot2::facet_wrap(~Phase, scales = "free") +
            ggplot2::theme(legend.position = "bottom")

      })

    })

 })

}

## To be copied in the UI
# mod_animal_ui("animal")

## To be copied in the server
# mod_animal_server("animal")
