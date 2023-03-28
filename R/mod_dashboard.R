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

    tableOutput(ns("tabela_teste")),

    fluidRow(
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
            )),
          bs4Dash::bs4Card(
            title = "Methane",
            width = 4,
            footer = NULL,
            fluidRow(
              DT::dataTableOutput(ns("methane_table"))
              #plotly::plotlyOutput(ns("pie_co2_eq2"))
            ))


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
                                 nh3_emissions,
                                 herd_methane,
                                 fac_methane,
                                 storage_methane,
                                 fac_ammonia,
                                 storage_ammonia,
                                 total_co2,
                                 total_nh3,
                                 total_n2o,
                                 total_ch4,
                                 direct_n2o_storage){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$tabela_teste <- renderTable({
      #nh3_emissions()
      animal_data()
      print(total_co2())
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

      herd + fac + storage + crop


    })

    output$co2eq <- bs4Dash::renderValueBox({

      value_box_spark(
        value    = round(total_co2e_q_emitted() / 1000, 1),
        title    = "Total Carbon Eq. (ton./year)",
        sparkobj = NULL,
        subtitle = tagList(),
        info     = " ",
        icon     = icon("fa-solid fa-poop", verify_fa = FALSE),
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
        icon     = icon("fa-solid fa-poop", verify_fa = FALSE),
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

    output$methane_table <- DT::renderDataTable({

      data <- animal_data() %>%
        dplyr::group_by(Categories) %>%
        dplyr::summarise(
          "Methane" = (total_ch4_kg * total_animals)
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

      DT::datatable(data,
                    rownames= FALSE,
                    options = list(dom = 't',
                                   initComplete = DT::JS(
                                     "function(settings, json) {",
                                     "$(this.api().table().header()).css({'background-color': '#EEEEEE', 'color': 'black'});",
                                     "}"))) %>%
        DT::formatStyle(
          'Methane (%)',
          background = DT::styleColorBar(data$`Methane (%)`, "#3c8dbc"),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )

    })


  })
}

## To be copied in the UI
# mod_dashboard_ui("dashboard")

## To be copied in the server
# mod_dashboard_server("dashboard")
