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

    fluidRow(
      bs4Dash::bs4Card(
        title = "CO2eq Sources",
        width = 4,
        footer = NULL,
        fluidRow(
          plotly::plotlyOutput(ns("gauge_co2_eq")))),
      bs4Dash::bs4Card(
        title = "CO2eq Sources",
        width = 4,
        footer = NULL,
        fluidRow(
          plotly::plotlyOutput(ns("pie_co2_eq"))
        )),
      bs4Dash::bs4Card(
        title = "CO2eq Sources",
        width = 4,
        footer = NULL,
        fluidRow(
          echarts4r::echarts4rOutput(ns("pie_co2_eq2"))
          #plotly::plotlyOutput(ns("pie_co2_eq2"))
        ))


    ),








    tableOutput(ns("tabela_teste")),

    fluidRow(

      bs4Dash::bs4Card(
        title = "Herd Outcomes",
        elevation = 1,
        width = 12,
        solidHeader = TRUE,
        status = "teal",
        collapsible = TRUE,
        maximizable = TRUE,
        fluidRow(
          bs4Dash::bs4Card(
            title = "Methane",
            width = 6,
            footer = NULL,
            fluidRow(
              bs4Dash::valueBoxOutput(ns("methane_yield")),
              bs4Dash::valueBoxOutput(ns("methane_intensity")) )),

          bs4Dash::bs4Card(
            title = "Methane",
            width = 6,
            footer = NULL,
            fluidRow(
              DT::dataTableOutput(ns("methane_table"))
              #plotly::plotlyOutput(ns("pie_co2_eq2"))
            ))
        )),

        bs4Dash::bs4Card(
          title = "Diet Costs and Milk Price",
          elevation = 1,
          width = 12,
          solidHeader = TRUE,
          status = "teal",
          collapsible = TRUE,
          fluidRow(
            column(3,
                   numericInput(ns("lact_diet_cost"),   label = "Milking Cows ($/kgDM):",   value = 0.3)),
            column(3,
                   numericInput(ns("dry_diet_cost"),    label = "Dry Cows ($/kgDM):",       value = 0.12)),
            column(3,
                   numericInput(ns("heifer_diet_cost"), label = "Heifers ($/kgDM):",        value = 0.12)),
            column(3,
                   numericInput(ns("milk_price"),       label = "Milk Price ($/cwt):",     value = 21)))),

      bs4Dash::bs4Card(
        title = "Economic analysis",
        elevation = 1,
        width = 12,
        solidHeader = TRUE,
        status = "teal",
        collapsible = TRUE,
        maximizable = TRUE,
        fluidRow(
          reactable::reactableOutput(ns("tabela")))),


      bs4Dash::bs4Card(
        title = "Carbon Footprint",
        elevation = 1,
        width = 12,
        solidHeader = TRUE,
        status = "teal",
        collapsible = TRUE,
        maximizable = TRUE,
        fluidRow(
          bs4Dash::valueBoxOutput(ns("co2eq")),
          bs4Dash::valueBoxOutput(ns("co2eq_milk")),
          bs4Dash::valueBoxOutput(ns("manure_storage_methane"))
        ),




          column(offset = 11,
                 1,
                 downloadButton(ns("rmd_report"),
                                "Report.html",
                                style = "color: #fff; background-color: #007582; border-color: #007582; height:50px; width:140px"))))

  )
}

#' dashboard Server Functions
#'
#' @noRd
mod_dashboard_server <- function(id,
                                 animal_data,
                                 calf_milk_intake,
                                 animal_inputs,
                                 nh3_emissions,
                                 herd_methane,
                                 fac_methane,
                                 storage_methane,
                                 fac_ammonia,
                                 storage_ammonia,
                                 co2_eq_fuel_col_spread,
                                 crop_inputs,
                                 total_co2,
                                 total_nh3,
                                 total_n2o,
                                 total_ch4,
                                 direct_n2o_storage){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$tabela_teste <- renderTable({
      #nh3_emissions()
      #animal_data()
      print(co2_eq_fuel_col_spread())
     #c(herd_methane(), fac_methane(), storage_methane(), fac_ammonia()/0.82, storage_ammonia() / 0.82, total_co2(), total_nh3() / 0.82, total_n2o())


    })

    milk_yield_fpc <- reactive({

      animal_data() %>%
        dplyr::filter(Categories == "Cow") %>%
        dplyr::mutate(
          total_milk_kg = total_animals * milk_yield_kg_fpc * 365
        ) %>%
        dplyr::pull(total_milk_kg)

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


    output$methane_yield <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(methane_yield_and_intens()$methane_yield, 1),
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
        value    = round(methane_yield_and_intens()$methane_intens, 1),
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
# Economics ---------------------------------------------------------------
# -------------------------------------------------------------------------

    economics <- reactive({

      animal_data <- animal_data()

      calf_milk_intake <- calf_milk_intake()

      # animal prmts - they will come from the animal

      milking_cows <- animal_data$total_animals[2]
      dry_cows <- animal_data$total_animals[3]
      heifers <- animal_data$total_animals[4]

      #dmi_milking_cows <- 28.5
      dmi_milking_cows <- animal_data %>%
        dplyr::filter(Categories == "Cow") %>%
        dplyr::pull("dmi_kg")

      #dmi_dry_cows <- 12
      dmi_dry_cows <- animal_data %>%
        dplyr::filter(Categories == "Dry") %>%
        dplyr::pull("dmi_kg")

      #dmi_heifers <- 7.7
      dmi_heifers <- animal_data %>%
        dplyr::filter(Categories == "Hei") %>%
        dplyr::pull("dmi_kg")


      #my_lactating <- input$milk_yield
      my_lactating <- animal_data %>%
        dplyr::filter(Categories == "Cow") %>%
        dplyr::pull("milk_yield_kg_fpc")

      feed_efic <- my_lactating / dmi_milking_cows

      # total income discounting milk supplied to calves

      total_income <- input$milk_price / (100 / 2.2) * (my_lactating - (animal_data$total_animals[1] * calf_milk_intake) / milking_cows)

      feed_cost_lac <- dmi_milking_cows * input$lact_diet_cost

      iofc_dry <- 0 - input$dry_diet_cost * dmi_dry_cows

      feed_cost_dry <- (dry_cows * input$dry_diet_cost * dmi_dry_cows) / milking_cows

      feed_cost_kg_milk <- feed_cost_lac / my_lactating

      iofc_lac <- total_income - feed_cost_lac

      iofc_lac_dry <- total_income - feed_cost_lac - feed_cost_dry

      economics <- tibble::tibble(

        #"Feed Efficiency (kg/kg)"                 = feed_efic,
        "Total Milk Income ($/cow)"               = total_income,
        "Feed Cost ($/cow)"                       = feed_cost_lac,
        "Income Over Feed Cost Lac ($/cow)"       = iofc_lac,
        "Income Over Feed Cost Lac + Dry ($/cow)" = iofc_lac_dry,
        "Income Over feed Cost Dry ($/cow)"       = iofc_dry,
        "Feed Cost per Kg Milk ($)"               = feed_cost_kg_milk
      ) %>%
        dplyr::mutate_if(is.numeric, round, 2)

    })

    output$tabela <- reactable::renderReactable({

      economics <- tibble::as_tibble(economics()) %>%
        reactable::reactable(
          defaultColDef = reactable::colDef(
            header = function(value) gsub(".", " ", value, fixed = TRUE),
            cell = function(value) format(value, nsmall = 1),
            align = "center",
            minWidth = 200,
            headerStyle = list(background = "#f7f7f8")
          ),
          columns = list(
            Species = reactable::colDef(minWidth = 300)  # overrides the default
          ),
          bordered = TRUE,
          highlight = TRUE
        )

      economics

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

      fig <- plotly::plot_ly(
        domain = list(x = c(0, 1), y = c(0, 1)),
        value = round(total_co2e_q_emitted() / milk_yield_fpc(), 3),
        title = list(text = "CO2eq/kgMilk"),
        type = "indicator",
        mode = "gauge+number+delta",
        delta = list(reference = 1),
        gauge = list(
          axis = list(range = list(NULL, 2)),
          steps = list(
            list(range = c(0, 1), color = "lightgray"),
            list(range = c(1, 1.8), color = "orange")),
          threshold = list(
            line = list(color = "red", width = 6),
            thickness = 0.75,
            value = 1.8))) %>%
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
        plotly::layout(showlegend = FALSE)

    })

    output$pie_co2_eq2 <- echarts4r::renderEcharts4r({

      methane <- round(herd_methane() * 28 + storage_methane() * 28 + fac_methane() * 28 + total_ch4() * 28, 1)

      co2 <- round(total_co2(), 1)

      n2o <- round(fac_ammonia() / 100 / 0.82 * 264 + storage_ammonia() / 100 / 0.82 * 264 + total_n2o() * 264  + direct_n2o_storage() * 264 , 1)

      total_co2eq <- methane + co2 + n2o

      values = c(methane, co2, n2o)

      labels = c("Methane", "CO2", "N2O")

      echarts4r::e_chart(width = 1100, height = 550,
                         renderer = "svg") %>%
        echarts4r::e_list(list(
          tooltip = list(trigger = "item"),
          legend = list(top = "5%", left = "center"),
          series = list(
            list(
              name = "Access From",
              type = "pie",
              radius = c("40%", "70%"),
              avoidLabelOverlap = FALSE,
              itemStyle = list(
                borderRadius = 10,
                borderColor = "#fff",
                borderWidth = 2
              ),
              label = list(show = FALSE, position = "center"),
              emphasis = list(
                label = list(
                  show = TRUE,
                  fontSize = 40,
                  fontWeight = "bold"
                )
              ),
              labelLine = list(show = TRUE),
              data = list(
                list(value = round(methane / total_co2eq * 100, 1), name = "Methane"),
                list(value = round(co2 / total_co2eq * 100, 1),     name  = "CO2"),
                list(value = round(n2o / total_co2eq * 100, 1),     name  = "N2O")
              )
            )
          )
        ))

      # plotly::plot_ly(values = ~round(values, 1),
      #                 labels = ~labels,
      #                 textinfo = 'label+percent') %>%
      #   plotly::add_pie(hole = 0.55) %>%
      #   plotly::config(displayModeBar = FALSE) %>%
      #   plotly::layout(showlegend = FALSE)

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


    output$methane_table <- DT::renderDataTable({


      DT::datatable(methane_table(),
                    rownames= FALSE,
                    options = list(dom = 't',
                                   initComplete = DT::JS(
                                     "function(settings, json) {",
                                     "$(this.api().table().header()).css({'background-color': '#EEEEEE', 'color': 'black'});",
                                     "}"))) %>%
        DT::formatStyle(
          'Methane (%)',
          background = DT::styleColorBar(methane_table()$`Methane (%)`, "#3c8dbc"),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
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
              input       = path1, #"inst/app/report.Rmd",
              output_file = "built_report.html",

              params = list(
                total_co2e_q_emitted = total_co2e_q_emitted(),
                co2eq_milk           = total_co2e_q_emitted() / milk_yield_fpc(),
                methane_table        = methane_table(),
                suma_table           = report(),
                crop_inputs          = crop_inputs()
              )
            )

            readBin(con  = path2,#"inst/app/built_report.html",
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
