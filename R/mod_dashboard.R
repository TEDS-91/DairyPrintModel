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
        #bs4Dash::valueBoxOutput(ns("co2eq")),
        list(
          "total_methane",
          "total_n2o",
          "total_co2"
        ) %>%
          purrr::map(\(x) bs4Dash::valueBoxOutput(ns(x)))
      ),
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
            plotly::plotlyOutput(ns("pie_co2_eq2"))
          )),
      )
      ),

    # fluidRow(
    #
    #   column(offset = 10,
    #          1,
    #          downloadButton(ns("rmd_report"),
    #                         "Report.html",
    #                         style = "color: #fff; background-color: #007582; border-color: #007582; height:50px; width:140px"))
    #
    #
    # ),

    #tableOutput(ns("tabela_teste")),

    fluidRow(

      # bs4Dash::bs4Card(
      #   title = "Herd Outcomes",
      #   elevation = 1,
      #   width = 12,
      #   solidHeader = TRUE,
      #   status = "teal",
      #   collapsible = TRUE,
      #   maximizable = TRUE,
      #   fluidRow(
      #     bs4Dash::bs4Card(
      #       title = "Methane",
      #       width = 6,
      #       footer = NULL,
      #       fluidRow(
      #         DT::dataTableOutput(ns("methane_table"))
      #         #plotly::plotlyOutput(ns("pie_co2_eq2"))
      #       ))
      #   )),

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
            fluidRow(
          plotly::plotlyOutput(ns("water_nitrogen")))),

          bs4Dash::bs4Card(
            title = "Phosphorous",
            elevation = 1,
            width = 4,
            solidHeader = TRUE,
            status = "white",
            collapsible = TRUE,
            maximizable = FALSE,
            fluidRow(
              plotly::plotlyOutput(ns("water_phosphorous")))),

          bs4Dash::bs4Card(
            title = "Potassium",
            elevation = 1,
            width = 4,
            solidHeader = TRUE,
            status = "white",
            collapsible = TRUE,
            maximizable = FALSE,
            fluidRow(
              plotly::plotlyOutput(ns("water_potassium")))),



          )),

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
            "feed_cost_milk"
          ) %>%
            purrr::map(\(x) bs4Dash::valueBoxOutput(ns(x)))
          ),


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

                                 nh3_emissions,
                                 herd_methane,
                                 fac_methane,
                                 storage_methane,
                                 fac_ammonia,
                                 storage_ammonia,
                                 manure_inputs,

                                 co2_eq_fuel_col_spread,
                                 fuel_inputs,
                                 milk_price,
                                 dry_diet_cost,
                                 lact_diet_cost,

                                 crop_inputs,
                                 co2eq_purchased,
                                 purchased_feeds,
                                 total_co2,
                                 total_nh3,
                                 total_n2o,
                                 total_ch4,
                                 n_fixed,
                                 n_leached,
                                 direct_n2o_storage){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$tabela_teste <- renderTable({
      #nh3_emissions()
      #animal_data()
     # print(co2_eq_fuel_col_spread())
     #c(herd_methane(), fac_methane(), storage_methane(), fac_ammonia()/0.82, storage_ammonia() / 0.82, total_co2(), total_nh3() / 0.82, total_n2o())


      print(paste("fixad pode ser", n_fixed() ))

    })

    milk_yield_fpc <- reactive({

      print(paste("fixad pode ser", n_fixed() ))

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

      total_co2() + co2_eq_fuel_col_spread()

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


# -------------------------------------------------------------------------


    output$bar_co2_eq <- plotly::renderPlotly({

      df <- tibble::tibble(
        Source = "Co2 eq.",
        Herd = round(herd_methane() * 28 / (methane_yield_and_intens()$milk_yield_kg_fpc * 365 * methane_yield_and_intens()$total_animals), 3),
        Barn = round((fac_methane() * 28 + fac_ammonia() / 100 / 0.82 * 264) / (methane_yield_and_intens()$milk_yield_kg_fpc * 365 * methane_yield_and_intens()$total_animals), 3),
        Manure = round((storage_methane() * 28 + storage_ammonia() / 100 / 0.82 * 264 + direct_n2o_storage() * 264) / (methane_yield_and_intens()$milk_yield_kg_fpc * 365 * methane_yield_and_intens()$total_animals), 3),
        "Crop and purchased feeds" = round((total_n2o() * 264 + total_co2() + total_ch4() * 28) / (methane_yield_and_intens()$milk_yield_kg_fpc * 365 * methane_yield_and_intens()$total_animals), 3),
        "Fuel" = round(co2_eq_fuel_col_spread() / (methane_yield_and_intens()$milk_yield_kg_fpc * 365 * methane_yield_and_intens()$total_animals), 3)
      )

      plotly::plot_ly(x = c(df$Herd, df$Barn, df$Manure, df$`Crop and purchased feeds`, df$Fuel),
                      y = c("Herd", "Barn", "Manure Handling", "Crop/Purch. Feeds", "Fuel"),
                      text = c(df$Herd, df$Barn, df$Manure, df$`Crop and purchased feeds`, df$Fuel),
                      type = 'bar',
                      orientation = 'h') %>%
        plotly::layout(xaxis = list(title = "Co2eq/kgMilk")) %>%
        plotly::layout(yaxis = list(categoryorder = "total ascending")) %>%
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

      req(total_co2e_q_emitted())

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

      req(total_co2e_q_emitted())

      fig <- plotly::plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = round(total_co2e_q_emitted() / milk_yield_fpc(), 3),
        title = list(text = "CO2eq/kgMilk"),
        type = "indicator",
        mode = "gauge+number+delta",
        delta = list(reference = 0.99),
        gauge = list(
          axis = list(range = list(NULL, 2)),
          bar = list(color = "darkblue"),
          steps = list(
            list(range = c(0, 0.76),   color = "lightgreen"),
            list(range = c(0.76, 1.37), color = "orange"),
            list(range = c(1.37, 2), color = "red")),
          threshold = list(
            line = list(color = "black", width = 6),
            thickness = 0.75,
            value = 0.99))) %>%
        plotly::config(displayModeBar = FALSE) %>%
        plotly::layout(showlegend = FALSE)

      fig

    })

    output$pie_co2_eq <- plotly::renderPlotly({

      herd <- herd_methane() * 28

      fac <- fac_methane() * 28 + fac_ammonia() / 100 / 0.82 * 264

      storage <- storage_methane() * 28 + storage_ammonia() / 0.82 / 100 * 264 + direct_n2o_storage() * 264

      crop <- total_n2o() * 264 + total_co2() + total_ch4() * 28

      values = c(herd, fac, storage, crop) / 1000

      labels = c("Herd", "Barn", "Man. Storage", "Crop")

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

      req(fac_methane())

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

    methane_table <- reactive({

      data <- animal_data() %>%
        dplyr::group_by(Categories) %>%
        dplyr::summarise(
          "Methane" = (total_ch4_kg * 365),
          "Total Methane (ton.)" = round(Methane / 1000, 1)
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          total_methane = sum(Methane)
        ) %>%
        dplyr::group_by(Categories) %>%
        dplyr::mutate(
          "Methane (%)" = round(Methane / total_methane * 100, 1),
          Categories = ifelse(Categories == "Cow", "Milking cows",
                              ifelse(Categories == "Hei", "Heifers",
                                     ifelse(Categories == "Dry", "Dry cows", "Calves")))
        ) %>%
        dplyr::arrange(dplyr::desc(Methane)) %>%
        dplyr::select(-total_methane, -Methane)

      data

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

      round(milk_yield_fpc() * 3.3 / 100 / 1000 / 6.25, 3)

    })

    nitrogen_from_culled_cows <- reactive({

      round(herd_inputs()$`Cow Culling Rate (%)` * herd_inputs()$`Total Cows` / 100 * adult_cows_body_composition(body_weight = herd_inputs()$`Mature Body Weight (kg)`, BCS = herd_inputs()$`BCS at Culling`)$nitrogen_kg / 1000, 3)

    })


    nitrogen_from_barn <- reactive({

      round(fac_ammonia() / 1000, 3)

      #(fac_ammonia() / 100 / 0.82) + direct_n2o_storage() + (storage_ammonia() / 100 / 0.82) + total_n2o() + sum(animal_data()$total_n2o_kg)

    })

    nitrogen_from_manure_storage <- reactive({

      round((direct_n2o_storage() * 0.64 + storage_ammonia()) / 1000, 3)

    })

    nitrogen_from_fertilizers <- reactive({


      crop_inputs() %>%
        dplyr::mutate(
          total_n_kg = total_n_applied * area
        ) %>%
        dplyr::summarise(
          total_n_ton = sum(total_n_kg) / 1000
        ) %>%
        dplyr::pull(total_n_ton) %>%
        round(3)

    })


    # Nitrogen

    output$water_nitrogen <- plotly::renderPlotly({

      fixado <- ifelse(is.null(n_fixed()), 0, n_fixed())

      balance <- round(nitrogen_from_diet() + nitrogen_from_fertilizers() - nitrogen_from_barn() - nitrogen_from_manure_storage() - nitrogen_from_culled_cows() - nitrogen_from_milk() - (n_leached() / 0.0075 / 1000), 3)

      x = c(nitrogen_from_diet(),   nitrogen_from_fertilizers(), -nitrogen_from_barn(), -nitrogen_from_manure_storage(),  -nitrogen_from_culled_cows(),       -nitrogen_from_milk(),          fixado, -round(n_leached() / 0.0075 / 1000, 3),  balance)
      y = c("Diet",                               "Fertilizers", "barn",                               "Manure Storage",  "Culled Animals",                  "Milk Sold",                     "Legumes",           "Leached", "N balance")

      measure = c("input",          "input",                     "output",                                     "output",   "output",                         "output",                         "input",             "output", "total")

      data = data.frame(x, y = factor(y, levels = y), measure)

      fig <- plotly::plot_ly(data, x = ~x,
                             y = ~y,
                             measure = ~measure,
                             text = ~ x,
                             type = "waterfall",
                             orientation = "h",
                             connector = list(mode = "between", line = list(width = 4, color = "rgb(0, 0, 0)", dash = 0)))
      fig <- fig %>%
        plotly::layout(#title = "Profit and loss statement 2018<br>waterfall chart displaying positive and negative",
               xaxis = list(title = "N (Ton./year)", tickfont = "16", ticks = "outside"),
               yaxis = list(title = "", type = "category", autorange = "reversed"),
               xaxis = list(title = "", type = "linear"),
               #margin = c(l = 150),
               showlegend = FALSE) %>%
        plotly::config(displayModeBar = FALSE)

      })

    output$water_phosphorous <- plotly::renderPlotly({

      x = c(100,    20,           -5,         -30,    -20,       -15,        50)
      y = c("Diet", "fertilizers", "Animals", "Milk", "Ammonia", "Leaching", "N balance")

      measure = c("input", "input", "output", "output", "output", "output", "total")

      data = data.frame(x, y = factor(y, levels = y), measure)

      fig <- plotly::plot_ly(data, x = ~x,
                             y = ~y,
                             measure = ~measure,
                             text = ~ x,
                             type = "waterfall",
                             orientation = "h",
                             connector = list(mode = "between", line = list(width = 4, color = "rgb(0, 0, 0)", dash = 0)))
      fig <- fig %>%
        plotly::layout(#title = "Profit and loss statement 2018<br>waterfall chart displaying positive and negative",
          xaxis = list(title = "Phosphorous (Ton./year)", tickfont = "16", ticks = "outside"),
          yaxis = list(title = "", type = "category", autorange = "reversed"),
          xaxis = list(title = "", type = "linear"),
          #margin = c(l = 150),
          showlegend = FALSE) %>%
        plotly::config(displayModeBar = FALSE)

    })

    output$water_potassium <- plotly::renderPlotly({

      x = c(100,    20,           -5,         -30,    -20,       -15,        50)
      y = c("Diet", "fertilizers", "Animals", "Milk", "Ammonia", "Leaching", "N balance")

      measure = c("input", "input", "output", "output", "output", "output", "total")

      data = data.frame(x, y = factor(y, levels = y), measure)

      fig <- plotly::plot_ly(data, x = ~x,
                             y = ~y,
                             measure = ~measure,
                             text = ~ x,
                             type = "waterfall",
                             orientation = "h",
                             connector = list(mode = "between", line = list(width = 4, color = "rgb(0, 0, 0)", dash = 0)))
      fig <- fig %>%
        plotly::layout(#title = "Profit and loss statement 2018<br>waterfall chart displaying positive and negative",
          xaxis = list(title = "Potassium (Ton./year)", tickfont = "16", ticks = "outside"),
          yaxis = list(title = "", type = "category", autorange = "reversed"),
          xaxis = list(title = "", type = "linear"),
          #margin = c(l = 150),
          showlegend = FALSE) %>%
        plotly::config(displayModeBar = FALSE)

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
                methane_table        = methane_table(),
                suma_table           = report(),
                crop_inputs          = crop_inputs(),
                co2eq_purchased      = co2eq_purchased(),
                fuel_inputs          = fuel_inputs(),
                purchased_feeds      = purchased_feeds(),
                nh3_emissions        = nh3_emissions(),
                manure_inputs        = manure_inputs(),
                herd_inputs          = herd_inputs(),
                diet_inputs          = diet_inputs(),
                raw_animal_df        = raw_animal_df()
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

## To be copied in the UI
# mod_dashboard_ui("dashboard")

## To be copied in the server
# mod_dashboard_server("dashboard")
