#' dashboard UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_dashboard_ui <- function(id){
  ns <- NS(id)
  tagList(

    bs4Dash::bs4Card(
      title = "Carbon Footprint and Main GHG Sources",
      elevation = 1,
      width = 12,
      solidHeader = TRUE,
      status = "teal",
      collapsible = TRUE,
      maximizable = TRUE,
      footer = "Note: The total carbon dioxide equivalents were calculated considering global warming potential of 28 and 264 for methane and nitrous oxide, respectively (Myhre ey al., 2013).",
      fluidRow(
        list(
          "total_methane",
          "total_n2o",
          "total_co2") %>%
          purrr::map(\(x) bs4Dash::valueBoxOutput(ns(x)))),

      fluidRow(
        bs4Dash::bs4Card(
          title = "Carbon dioxide eq. by kg of Milk",
          width = 3,
          footer = NULL,
          numberColor = "#adb5bd",
          fluidRow(
            plotly::plotlyOutput(ns("gauge_co2_eq")))),

        bs4Dash::bs4Card(
          title = "Carbon dioxide eq./kgMilk by source of GHG",
          width = 3,
          footer = NULL,
          fluidRow(
            plotly::plotlyOutput(ns("bar_co2_eq"))
          )),

        bs4Dash::bs4Card(
          title = "Carbon dioxide eq. by farm sources",
          width = 3,
          footer = NULL,
          fluidRow(
            plotly::plotlyOutput(ns("pie_co2_eq"))
          )),
        bs4Dash::bs4Card(
          title = "Carbon dioxide eq. by type of GHG",
          width = 3,
          footer = NULL,
          fluidRow(
            plotly::plotlyOutput(ns("pie_co2_eq2")))))

      ),

    fluidRow(
      bs4Dash::bs4Card(
        title = "Nutrient Balances",
        elevation = 1,
        width = 12,
        solidHeader = TRUE,
        status = "teal",
        collapsible = TRUE,
        maximizable = TRUE,
        fluidRow(
          bs4Dash::bs4Card(
            title = "Nitrogen",
            elevation = 1,
            width = 4,
            solidHeader = TRUE,
            status = "white",
            collapsible = TRUE,
            maximizable = FALSE,
            footer = "Feasible N balance: 0-0.00785 kg/kg milk and 0-117.7 kg/ha.",
            fluidRow(
          plotly::plotlyOutput(ns("water_nitrogen"))),
          hr(),
          fluidRow(
            bs4Dash::valueBoxOutput(ns("n_per_kg_milk"), width = 6),
            bs4Dash::valueBoxOutput(ns("n_per_hect"), width = 6))),

          bs4Dash::bs4Card(
            title = "Phosphorous",
            elevation = 1,
            width = 4,
            solidHeader = TRUE,
            status = "white",
            collapsible = TRUE,
            maximizable = FALSE,
            footer = "Feasible P balance: 0-0.00098 kg/kg milk and 0-13.5 kg/ha.",
            fluidRow(
              plotly::plotlyOutput(ns("water_phosphorous"))),
            hr(),
            fluidRow(
              bs4Dash::valueBoxOutput(ns("p_per_kg_milk"), width = 6),
              bs4Dash::valueBoxOutput(ns("p_per_hect"), width = 6))),

          bs4Dash::bs4Card(
            title = "Potassium",
            elevation = 1,
            width = 4,
            solidHeader = TRUE,
            status = "white",
            collapsible = TRUE,
            maximizable = FALSE,
            footer = "Feasible K balance: 0-0.00268 kg/kg milk and 0-41.5 kg/ha.",
            fluidRow(
              plotly::plotlyOutput(ns("water_potassium"))),
            hr(),
            fluidRow(
              bs4Dash::valueBoxOutput(ns("k_per_kg_milk"), width = 6),
              bs4Dash::valueBoxOutput(ns("k_per_hect"), width = 6)) ))),

      bs4Dash::bs4Card(
        title = "Economic Analysis",
        elevation = 1,
        width = 12,
        solidHeader = TRUE,
        status = "teal",
        collapsible = TRUE,
        maximizable = TRUE,
        fluidRow(
          list(
            "milk_income",
            "feed_cost",
            "iofc_lac",
            "iofc_lac_dry",
            "iofc_dry",
            "feed_cost_milk") %>%
            purrr::map(\(x) bs4Dash::valueBoxOutput(ns(x)))),
        column(offset = 10,
               1,
               downloadButton(ns("rmd_report"),
                              "Report.html",
                              style = "color: #fff; background-color: #007582; border-color: #007582; height:50px; width:140px"))
      )
    )
  )
}

#' dashboard Server Functions
#'
#' @noRd
mod_dashboard_server <- function(id,

                                 animal_data,
                                 calf_milk_intake,
                                 herd_inputs,
                                 diet_inputs,
                                 raw_animal_df,
                                 farm_area,

                                 nh3_emissions,
                                 herd_methane,
                                 fac_methane,
                                 storage_methane,
                                 fac_ammonia,
                                 storage_ammonia,
                                 manure_inputs,
                                 bedding_qnt,

                                 co2_eq_fuel_col_spread,
                                 fuel_inputs,
                                 milk_price,
                                 dry_diet_cost,
                                 lact_diet_cost,

                                 crop_inputs,
                                 co2eq_purchased,
                                 purchased_feeds,
                                 n_purchased_feeds,
                                 k_purchased_feeds,
                                 p_purchased_feeds,

                                 total_co2,
                                 total_nh3,
                                 total_n2o,
                                 total_ch4,
                                 n_fixed,
                                 n_leached,
                                 k_from_fertilizers,
                                 p_from_fertilizers,
                                 p_losses,
                                 k_losses,
                                 n_extracted_crops,
                                 direct_n2o_storage){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$tabela_teste <- renderTable({

      print(paste("Area da fazenda", farm_area() ))

    })

    milk_yield_fpc <- reactive({

      print(paste("Area da fazenda", farm_area() ))

      animal_data() %>%
        dplyr::filter(Categories == "Cow") %>%
        dplyr::mutate(
          total_milk_kg = total_animals * milk_yield_kg_fpc * 365
        ) %>%
        dplyr::pull(total_milk_kg)

    })

    total_methane_emmited <- reactive({

      # herd           barn            storage             crop
      herd_methane() + fac_methane() + storage_methane() + total_ch4()

    })

    total_n2o_emmited <- reactive({

      (fac_ammonia() / 100 / 0.82) + direct_n2o_storage() + (storage_ammonia() / 100 / 0.82) + total_n2o() + sum(animal_data()$total_n2o_kg)

    })

    total_co2_emmited <- reactive({

      # co2 eq from organic bedding

      co2_eq_bedding <- bedding_qnt() * 0.2 * 365

      #print(paste("CO2eq bedding YY ", co2_eq_bedding))

      total_co2() + co2_eq_fuel_col_spread() + co2_eq_bedding

    })

    total_co2e_q_emitted <- reactive({

      herd <- herd_methane() * 28

      fac <- fac_methane() * 28 + fac_ammonia() / 100 / 0.82 * 264

      storage <- storage_methane() * 28 + storage_ammonia() / 100 / 0.82 * 264 + direct_n2o_storage() * 264

      crop <- total_n2o() * 264 + total_co2() + total_ch4() * 28

      herd + fac + storage + crop + co2_eq_fuel_col_spread()

    })

# -------------------------------------------------------------------------
# Herd Outcomes -----------------------------------------------------------
# -------------------------------------------------------------------------

    methane_yield_and_intens <- reactive({

      animal_data() %>%
        dplyr::filter(Categories == "Cow") %>%
        dplyr::mutate(
          methane_yield  = total_ch4_kg / dmi_kg / total_animals * 1000,
          methane_intens = total_ch4_kg / milk_yield_kg_fpc / total_animals * 1000,
        )

    })

    output$total_methane <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(total_methane_emmited() / 1000, 2),
        title    = "Total Methane (Ton./year)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 2,
        color    = "white",
        href     = NULL
      )

    })

    output$total_n2o <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(total_n2o_emmited() / 1000, 2),
        title    = "Total Nitrous Oxide (Ton./year)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 2,
        color    = "white",
        href     = NULL
      )

    })

    output$total_co2 <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(total_co2_emmited() / 1000, 2),
        title    = "Total Carbon Dioxide (Ton./year)",
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
# Bar chart with CO2eq by farm process
# -------------------------------------------------------------------------

    df_bar_plot <- reactive({

      df <- tibble::tibble(
        Source = "Co2 eq.",
        Herd = round(herd_methane() * 28 / (methane_yield_and_intens()$milk_yield_kg_fpc * 365 * methane_yield_and_intens()$total_animals), 3),
        Barn = round((fac_methane() * 28 + fac_ammonia() / 100 / 0.82 * 264) / (methane_yield_and_intens()$milk_yield_kg_fpc * 365 * methane_yield_and_intens()$total_animals), 3),
        Manure = round((storage_methane() * 28 + storage_ammonia() / 100 / 0.82 * 264 + direct_n2o_storage() * 264) / (methane_yield_and_intens()$milk_yield_kg_fpc * 365 * methane_yield_and_intens()$total_animals), 3),
        "Crop and purchased feeds" = round((total_n2o() * 264 + total_co2() + total_ch4() * 28) / (methane_yield_and_intens()$milk_yield_kg_fpc * 365 * methane_yield_and_intens()$total_animals), 3),
        "Fuel" = round(co2_eq_fuel_col_spread() / (methane_yield_and_intens()$milk_yield_kg_fpc * 365 * methane_yield_and_intens()$total_animals), 3)
      )

      df

    })

    output$bar_co2_eq <- plotly::renderPlotly({

      plotly::plot_ly(x = c(df_bar_plot()$Herd, df_bar_plot()$Barn, df_bar_plot()$Manure, df_bar_plot()$`Crop and purchased feeds`, df_bar_plot()$Fuel),
                      y = c("Herd", "Barn", "Manure Handling", "Crop/Purch. Feeds", "Fuel"),
                      text = c(df_bar_plot()$Herd, df_bar_plot()$Barn, df_bar_plot()$Manure, df_bar_plot()$`Crop and purchased feeds`, df_bar_plot()$Fuel),
                      type = 'bar',
                      orientation = 'h') %>%
        plotly::layout(xaxis = list(title = "Co2eq/kgMilk", tickangle = 45)) %>%
        plotly::layout(yaxis = list(categoryorder = "total ascending", tickangle = -45)) %>%
        plotly::config(displayModeBar = FALSE) %>%
        plotly::layout(showlegend = FALSE)

    })

# -------------------------------------------------------------------------
# Economics ---------------------------------------------------------------
# -------------------------------------------------------------------------

    economics <- reactive({

      animal_data <- animal_data()

      calf_milk_intake <- calf_milk_intake()

      # animal prmts - they will come from the herd module

      milking_cows <- animal_data$total_animals[2]
      dry_cows <- animal_data$total_animals[3]
      heifers <- animal_data$total_animals[4]

      # DMI milking cows
      dmi_milking_cows <- animal_data %>%
        dplyr::filter(Categories == "Cow") %>%
        dplyr::pull("dmi_kg")

      # DMI dry cows
      dmi_dry_cows <- animal_data %>%
        dplyr::filter(Categories == "Dry") %>%
        dplyr::pull("dmi_kg")

      #DMI heifers
      dmi_heifers <- animal_data %>%
        dplyr::filter(Categories == "Hei") %>%
        dplyr::pull("dmi_kg")

      # Milk Yield lactating
      my_lactating <- animal_data %>%
        dplyr::filter(Categories == "Cow") %>%
        dplyr::pull("milk_yield_kg_fpc")

      feed_efic <- my_lactating / dmi_milking_cows

      # total income discounting milk supplied to calves

      total_income <- milk_price() / (100 / 2.2) * (my_lactating - (animal_data$total_animals[1] * calf_milk_intake) / milking_cows)

      feed_cost_lac <- dmi_milking_cows * lact_diet_cost()

      iofc_dry <- 0 - dmi_dry_cows * dry_diet_cost()

      feed_cost_dry <- (dry_cows * dry_diet_cost() * dmi_dry_cows) / milking_cows

      feed_cost_kg_milk <- feed_cost_lac / my_lactating

      iofc_lac <- total_income - feed_cost_lac

      iofc_lac_dry <- total_income - feed_cost_lac - feed_cost_dry

      economics <- tibble::tibble(

        "Total Milk Income ($/cow)"               = total_income,
        "Feed Cost ($/cow)"                       = feed_cost_lac,
        "Income Over Feed Cost Lac ($/cow)"       = iofc_lac,
        "Income Over Feed Cost Lac + Dry ($/cow)" = iofc_lac_dry,
        "Income Over feed Cost Dry ($/cow)"       = iofc_dry,
        "Feed Cost per Kg Milk ($)"               = feed_cost_kg_milk
      ) %>%
        dplyr::mutate_if(is.numeric, round, 2)

    })

    # Economic Cards

    output$milk_income <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(economics()$`Total Milk Income ($/cow)`, 2),
        title    = "Milk Income ($/cow)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    output$feed_cost <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(economics()$`Feed Cost ($/cow)`, 2),
        title    = "Feed Cost ($/cow)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    output$iofc_lac <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(economics()$`Income Over Feed Cost Lac ($/cow)`, 2),
        title    = "IOFC Lactating Cows ($/cow)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    output$iofc_lac_dry <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(economics()$`Income Over Feed Cost Lac + Dry ($/cow)`, 2),
        title    = "IOFC Lactating and Dry Cows ($/cow)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    output$iofc_dry <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(economics()$`Income Over feed Cost Dry ($/cow)`, 2),
        title    = "IOFC Dry Cows ($/cow)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    output$feed_cost_milk <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(economics()$`Feed Cost per Kg Milk ($)`, 2),
        title    = "Feed Cost per Kg Milk ($)",
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
# Environmental outcomes --------------------------------------------------
# -------------------------------------------------------------------------

    output$co2eq <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(total_co2e_q_emitted() / 1000, 1),
        title    = "Total Carbon Eq. (ton./year)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    output$co2eq_milk <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(total_co2e_q_emitted() / milk_yield_fpc(), 3),
        title    = "Carbon Eq. per kg Milk",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 4,
        color    = "white",
        href     = NULL
      )

    })

    output$gauge_co2_eq <- plotly::renderPlotly({

      co2eq <- round(total_co2e_q_emitted() / milk_yield_fpc(), 3)

      built_gauge_plotly(carbon_equivalent = co2eq)

    })

    output$pie_co2_eq <- plotly::renderPlotly({

      herd <- herd_methane() * 28

      fac <- fac_methane() * 28 + fac_ammonia() / 100 / 0.82 * 264

      storage <- storage_methane() * 28 + storage_ammonia() / 0.82 / 100 * 264 + direct_n2o_storage() * 264

      crop <- total_n2o() * 264 + total_co2() + total_ch4() * 28

      values = c(herd, fac, storage, crop) / 1000

      labels = c("Herd", "Barn", "Man. Storage", "Crop/Purch. Feeds")

      plotly::plot_ly(values = ~round(values, 1),
                      labels = ~labels,
                      textinfo = 'label+percent') %>%
        plotly::add_pie(hole = 0.55) %>%
        plotly::config(displayModeBar = FALSE) %>%
        plotly::layout(showlegend = FALSE,
                       annotations = list(text = ~ paste(round(total_co2e_q_emitted() / 1000, 1), "ton./year", sep = "<br>"),
                                          "showarrow"  = FALSE, font = list(size = 25)))

    })

    output$pie_co2_eq2 <- plotly::renderPlotly({

      methane <- round(herd_methane() * 28 + storage_methane() * 28 + fac_methane() * 28 + total_ch4() * 28, 1)

      co2 <- round(total_co2(), 1)

      n2o <- round(fac_ammonia() / 100 / 0.82 * 264 + storage_ammonia() / 100 / 0.82 * 264 + total_n2o() * 264  + direct_n2o_storage() * 264 , 1)

      total_co2eq <- methane + co2 + n2o

      values = c(methane, co2, n2o)

      labels = c("Methane", "CO2", "N2O")

      plotly::plot_ly(values = ~round(values, 1),
                      labels = ~labels,
                      textinfo = 'label+percent') %>%
        plotly::add_pie(hole = 0.55) %>%
        plotly::config(displayModeBar = FALSE) %>%
        plotly::layout(showlegend = FALSE,
                       annotations = list(text = ~ paste(round(total_co2e_q_emitted() / 1000, 1), "ton./year", sep = "<br>"),
                                          "showarrow"  = FALSE, font = list(size = 25)))

    })

# -------------------------------------------------------------------------
# Nutrient balances
# -------------------------------------------------------------------------

    nitrogen_from_diet <- reactive({

      animal_data() %>%
        dplyr::mutate(
          total_n_ingested_g = total_animals * total_n_ingested_g  * 365
        ) %>%
        dplyr::summarise(
          total_n_ingested_ton = sum(total_n_ingested_g) / 1000000
        ) %>%
        dplyr::pull(total_n_ingested_ton) %>%
        round(3)

    })

    nitrogen_from_milk <- reactive({

      round(milk_yield_fpc() * 3.3 / 100 / 1000 / 6.25, 2)

    })

    nitrogen_from_culled_cows <- reactive({

      round(herd_inputs()$`Cow Culling Rate (%)` * herd_inputs()$`Total Cows` / 100 * adult_cows_body_composition(body_weight = herd_inputs()$`Mature Body Weight (kg)`, BCS = herd_inputs()$`BCS at Culling`)$nitrogen_kg / 1000, 3)

    })

    nitrogen_from_barn <- reactive({

      round(fac_ammonia() / 1000, 2)

      #(fac_ammonia() / 100 / 0.82) + direct_n2o_storage() + (storage_ammonia() / 100 / 0.82) + total_n2o() + sum(animal_data()$total_n2o_kg)

    })

    nitrogen_from_manure_storage <- reactive({

      round((direct_n2o_storage() * 0.64 + storage_ammonia()) / 1000, 2)

    })

    nitrogen_from_fertilizers <- reactive({

      print(paste("The n extracted from crops is", typeof(n_extracted_crops()[[1]])))

      crop_inputs() %>%
        dplyr::mutate(
          total_n_kg = total_n_applied * area
        ) %>%
        dplyr::summarise(
          total_n_ton = sum(total_n_kg) / 1000
        ) %>%
        dplyr::pull(total_n_ton) %>%
        round(2)

    })

    nitrogen_from_bedding <- reactive({

      round(bedding_qnt() * 1.4 / 100 / 1000 * 365, 2)

    })

    nitrogen_balance <- reactive({

      n_balance <- round(n_purchased_feeds()[[1]] + nitrogen_from_fertilizers() + n_fixed()[[1]] - nitrogen_from_barn() - nitrogen_from_manure_storage() - nitrogen_from_culled_cows() - nitrogen_from_milk() - (n_leached() / 0.0075 / 1000) + nitrogen_from_bedding(), 2)

      n_balance

    })

    # Nitrogen

    output$water_nitrogen <- plotly::renderPlotly({

      data <- tibble::tribble(
        ~source,            ~value,                                    ~measure,
        "Purchased Feeds",   round(n_purchased_feeds()[[1]], 2),        "input",
        "Fertilizers",       nitrogen_from_fertilizers(),               "input",
        "Bedding",           nitrogen_from_bedding(),                   "input",
        "Legumes",           round(n_fixed()[[1]],2),                   "input",
        "Milk sold",         -nitrogen_from_milk(),                     "output",
        "Culled Animals",    -nitrogen_from_culled_cows(),              "output",
        "Barn",              -nitrogen_from_barn(),                     "output",
        "Manure Storage",    -nitrogen_from_manure_storage(),           "output",
        "Leached",           -round(n_leached() / 0.0075 / 1000, 2),    "output",
        "Balance",           round(nitrogen_balance(), 2),              "total"
      )

      data <- tibble::tibble(x       = data$value,
                             y       = factor(data$source, levels = data$source),
                             measure = data$measure)

      built_waterfall_plot_plotly(data, nitrogen_balance(), "N (Ton./Year)")

      })

    # Nitrogen card

    output$n_per_kg_milk <- bs4Dash::renderValueBox({

      n_bal <- as.numeric(signif(nitrogen_balance() * 1000 / milk_yield_fpc(), 1), scientific = FALSE)

      color <- ifelse(n_bal <= 0.00785 & n_bal >= 0, "green", "red")

      value_box_spark(
        value    = format(n_bal, scientific = FALSE),
        title    = "Nitrogen balance (Kg N/Kg milk)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 12,
        color    = color,
        href     = NULL
      )

    })

    output$n_per_hect <- bs4Dash::renderValueBox({

      n_bal <- as.numeric(signif(nitrogen_balance() * 1000 / farm_area(), 1), scientific = FALSE)

      color <- ifelse(n_bal >= 0 & n_bal <= 117.7, "green", "red")

      value_box_spark(
        value    = n_bal,
        title    = "Nitrogen balance (Kg N/ha)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 12,
        color    = color,
        href     = NULL
      )

    })

    # Phosphorous

    phosphorous_from_milk <- reactive({

      round((0.996 * milk_yield_fpc())/ 1000000, 1)

    })

    phosphorous_from_culled_cows <- reactive({

      round(herd_inputs()$`Cow Culling Rate (%)` * herd_inputs()$`Total Cows` / 100 * adult_cows_body_composition(body_weight = herd_inputs()$`Mature Body Weight (kg)`,
                                                                                                                  BCS = herd_inputs()$`BCS at Culling`)$empty_body_weight_kg * 0.72 / 100 / 1000, 3)

    })

    phosphorous_from_bedding <- reactive({

      round(bedding_qnt() * 0.3 / 100 / 1000 * 365, 2)

    })

    phosphorous_balance <- reactive({

      p_balance <- p_purchased_feeds()[[1]] + p_from_fertilizers()[[1]] - phosphorous_from_milk() - phosphorous_from_culled_cows() - (p_losses() / 1000) + phosphorous_from_bedding()

      p_balance

    })

    output$water_phosphorous <- plotly::renderPlotly({

      data <- tibble::tribble(
        ~source,            ~value,                                    ~measure,
        "Purchased Feeds",   round(p_purchased_feeds()[[1]], 2),        "input",
        "Bedding",           phosphorous_from_bedding(),                "input",
        "Fertilizers",       round(p_from_fertilizers()[[1]], 2),       "input",
        "Milk sold",         -phosphorous_from_milk(),                  "output",
        "Culled Animals",    -phosphorous_from_culled_cows(),           "output",
        "Soil losses",       -round(p_losses() / 1000, 2),              "output",
        "Balance",           round(phosphorous_balance(), 2),           "total"
      )

      data <- tibble::tibble(x       = data$value,
                             y       = factor(data$source, levels = data$source),
                             measure = data$measure)


      built_waterfall_plot_plotly(data, phosphorous_balance(), "P (Ton./Year)")

    })

    # Phosphorous card

    output$p_per_kg_milk <- bs4Dash::renderValueBox({

      p_bal <- as.numeric(signif(phosphorous_balance() * 1000 / milk_yield_fpc(), 1), scientific = FALSE)

      color <- ifelse(p_bal >= 0 & p_bal <= 0.00098, "green", "red")

      value_box_spark(
        value    = format(p_bal, scientific = FALSE),
        title    = "Phosphorous balance (Kg N/Kg milk)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 12,
        color    = color,
        href     = NULL
      )

    })

    output$p_per_hect <- bs4Dash::renderValueBox({

      p_bal <- as.numeric(signif(phosphorous_balance() * 1000 / farm_area(), 1), scientific = FALSE)

      color <- ifelse(p_bal >= 0 & p_bal <= 13.45, "green", "red")

      value_box_spark(
        value    = p_bal,
        title    = "Phosphorous balance (Kg P/ha)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 12,
        color    = color,
        href     = NULL
      )

    })

    # Potassium

    potassium_from_milk <- reactive({

      round(milk_k_excretion(milk_yield = milk_yield_fpc())/ 1000000, 1)

    })

    potassium_from_culled_cows <- reactive({

      round(herd_inputs()$`Cow Culling Rate (%)` * herd_inputs()$`Total Cows` / 100 * adult_cows_body_composition(body_weight = herd_inputs()$`Mature Body Weight (kg)`,
                                                                                                                  BCS = herd_inputs()$`BCS at Culling`)$empty_body_weight_kg * 0.20 / 100 / 1000, 3)

    })

    potassium_from_bedding <- reactive({

      round(bedding_qnt() * 0.4 / 100 / 1000 * 365, 2)

    })

    potassium_balance <- reactive({

      k_balance <- k_purchased_feeds()[[1]] + k_from_fertilizers()[[1]] - potassium_from_culled_cows() - potassium_from_milk() - k_losses() + potassium_from_bedding()

      k_balance

    })

    output$water_potassium <- plotly::renderPlotly({

      data <- tibble::tribble(
        ~source,            ~value,                                    ~measure,
        "Purchased Feeds",   round(k_purchased_feeds()[[1]], 2),        "input",
        "bedding",           potassium_from_bedding(),                  "input",
        "Fertilizers",       k_from_fertilizers()[[1]],                 "input",
        "Milk sold",         -potassium_from_milk(),                    "output",
        "Culled Animals",    -potassium_from_culled_cows(),             "output",
        "Soil lossses",      -round(k_losses(), 2),                     "output",
        "Balance",           round(potassium_balance(), 2),             "total"
      )

      data <- tibble::tibble(x       = data$value,
                             y       = factor(data$source, levels = data$source),
                             measure = data$measure)

      built_waterfall_plot_plotly(data, potassium_balance(), "K (Ton./Year)")

    })

    # Potassium card

    output$k_per_kg_milk <- bs4Dash::renderValueBox({

      k_bal <- as.numeric(signif(potassium_balance() * 1000 / milk_yield_fpc(), 1))

      color <- ifelse(k_bal >= 0 & k_bal <= 0.002678, "green", "red")

      value_box_spark(
        value    = format(k_bal, scientific = FALSE),
        title    = "Potassium balance (Kg N/Kg milk)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 12,
        color    = color,
        href     = NULL
      )

    })

    output$k_per_hect <- bs4Dash::renderValueBox({

      k_bal <- as.numeric(signif(potassium_balance() * 1000 / farm_area(), 1), scientific = FALSE)

      color <- ifelse(k_bal >= 0 & k_bal <= 41.47, "green", "red")

      value_box_spark(
        value    = format(k_bal),
        title    = "Potassium balance (Kg K/ha)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = " ",
        width    = 12,
        color    = color,
        href     = NULL
      )

    })

# -------------------------------------------------------------------------
# Report generation -------------------------------------------------------
# -------------------------------------------------------------------------

    report <- reactive({

      methane_yield_and_intens()$methane_intens

    })

    output$rmd_report <- downloadHandler(

      "DairyPrintModelReport.html",

      content =

        function(file) {

          withProgress(message = "Rendering the report...", {

            path1 <- system.file("app", "report.Rmd", package = "DairyPrintModel")

            path2 <- system.file("app", "built_report.html", package = "DairyPrintModel")

            rmarkdown::render(
              input       = path1,
              output_file = path2,

              params = list(
                total_co2e_q_emitted = total_co2e_q_emitted(),
                co2eq_milk           = total_co2e_q_emitted() / milk_yield_fpc(),
                suma_table           = report(),
                crop_inputs          = crop_inputs(),
                co2eq_purchased      = co2eq_purchased(),
                fuel_inputs          = fuel_inputs(),
                purchased_feeds      = purchased_feeds(),
                nh3_emissions        = nh3_emissions(),
                manure_inputs        = manure_inputs(),
                herd_inputs          = herd_inputs(),
                diet_inputs          = diet_inputs(),
                animal_data          = animal_data(),
                raw_animal_df        = raw_animal_df(),

                # ghg emissions
                total_ch4_emmited    = round(total_methane_emmited() / 1000, 2),
                total_n2o_emmited    = round(total_n2o_emmited() / 1000, 2),
                total_co2_emmited    = round(total_co2_emmited() / 1000, 2),
                total_co2e_q_emitted = total_co2e_q_emitted() / 1000,
                co2eq                = round(total_co2e_q_emitted() / milk_yield_fpc(), 3),

                plot_by_sorce        = df_bar_plot(),

                # nutrient balances

                nitrogen_balance     = nitrogen_balance(),
                n_purchased_feeds    = n_purchased_feeds()[[1]],
                nitrogen_from_fertilizers = nitrogen_from_fertilizers(),
                n_fixed              = n_fixed()[[1]],
                nitrogen_from_barn   = nitrogen_from_barn(),
                nitrogen_from_manure_storage = nitrogen_from_manure_storage(),
                nitrogen_from_culled_cows = nitrogen_from_culled_cows(),
                nitrogen_from_milk   = nitrogen_from_milk(),
                n_leached            = n_leached() / 0.0075 / 1000,

                phosphorous_balance  = phosphorous_balance(),
                p_purchased_feeds    = p_purchased_feeds()[[1]],
                p_from_fertilizers   = p_from_fertilizers()[[1]],
                phosphorous_from_milk = phosphorous_from_milk(),
                phosphorous_from_culled_cows = phosphorous_from_culled_cows(),
                p_losses             = p_losses() / 1000,

                potassium_balance    = potassium_balance(),
                k_purchased_feeds = k_purchased_feeds()[[1]],
                k_from_fertilizers = k_from_fertilizers()[[1]],
                potassium_from_culled_cows = potassium_from_culled_cows(),
                potassium_from_milk = potassium_from_milk(),
                k_losses = k_losses(),

                # economics

                economics = economics()

              )
            )

            readBin(con  = path2,
                    what = "raw",
                    n    = file.info(path2)[ , "size"]) %>%

              writeBin(con = file)

          })

        }


      )
  })
}

