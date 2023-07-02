#' purchased_feeds UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_purchased_feeds_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
    bs4Dash::bs4Card(
      width = 12,
      title = "Purchased Feeds",
      elevation = 2,
      solidHeader = TRUE,
      status = "teal",
      footer = "Carbon dioxide equivalents obtained from BASF's Eco-efficiency analysis tool representative of U.S. national values (BASF, Ludwigshafen, Germany) (Rotz et al, 2021).",
      collapsed = FALSE,
        fluidRow(
        column(4,
               actionButton(ns("add_btn"), "Add Feed",
                            style = "color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; ")),
        column(4, offset = 3,
               actionButton(ns("rm_btn"), "Remove Feed",
                            style = "color: #fff; background-color: #27ae60; border-color: #fff;padding: 5px 14px 5px 14px;margin: 5px 5px 5px 5px; "))),
      br(),
      #textOutput(ns("counter")),

      fluidRow(
        column(12,
               uiOutput(ns("textbox_ui"))),
       tableOutput(ns("teste"))
       )))

  )
}

#' purchased_feeds Server Functions
#'
#' @noRd
mod_purchased_feeds_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    carbon_footprint <- reactive({

      # carbon footprint

      carbon_footprint <- tibble::tribble(
        ~"feeds",                   ~"carbon_foot",   ~"DM",    ~"cp_content",   ~"Qnt",  ~"k_content", ~"p_content",    ~"source - co2eq",
        "Corn grain",                0.37,              88.1,    9.4,              350,            0.42,       0.30,      "Rotz et al. (2021)",
        "Distillers's grain",        0.60,              90.2,   29.7,               74,            1.10,       0.83,      "Rotz et al. (2021)",
        "Soybean meal",              0.50,              89.6,   46.3,              106,            2.12,       0.66,      "Rotz et al. (2021)",
        "Canola meal",               0.50,              90.3,   37.8,               58,            1.41,       1.10,      "Rotz et al. (2021)",
        "Grass hay",                 0.15,              88.1,   10.6,              148,            2.01,       0.23,      "Rotz et al. (2019)",
        "Fat",                       1.52,              99.8,   0,                 37,                0,          0,      "Rotz et al. (2021)",
        "Mineral and vitamins",      1.62,              99.0,   0,                 23,                0,          0,      "Rotz et al. (2021)",
        "Corn silage",               0.15,              35.1,    8.8,              1125,           0.50,       0.10,      "Rotz et al. (2021)",
        "Alfalfa hay",               0.18,              90.3,    19.2,             245,            2.37,       0.28,      "Rotz et al. (2021)",
        "Cotton seed meal",         0.517,              90.5,    44.9,             0,              1.13,       0.60,      "Ecoinvent",
        "Alfalfa grass silage",     0.364,              39.1,    20.0,             0,              2.01,       0.23,      "Ecoinvent",
        "Corn gluten feed",          0.30,              86.4,    65.0,             0,              1.46,       1.00,      "Rotz et al. (2021)",
        "Protein mix",               0.50,              5,      5,                 0,                 0,          0,      "Rotz et al. (2021)",
        "General by product feed",   0.20,              5,      5,                 0,                 0,          0,      "Rotz et al. (2021)",
        "Milk replacer",            12.10,             90.0,   25.0,               0,                 0,          0,      "Rotz et al. (2021)",
        "Urea",                      1.72,             99,     281.25,             0,                 0,          0,      "Ecoinvent"

      )
    })

    feed_list <- reactive({

      carbon_footprint()$feeds

    })

    qnt_list <- reactive(carbon_footprint()$Qnt)

    # Track the number of input boxes to render
    counter <- reactiveValues(n = 7)

    # Track all user inputs
    AllInputs <- reactive({
      x <- reactiveValuesToList(input)
    })

    observeEvent(input$add_btn, {counter$n <- counter$n + 1})

    observeEvent(input$rm_btn, {

      if (counter$n > 0) counter$n <- counter$n - 1

    })

    output$counter <- renderPrint(print(counter$n))

    textboxes <- reactive({

      n <- counter$n

      evens <- function(x) subset(x, x %% 2 == 0)

      vector_crops <- seq(n)

      print(vector_crops[!vector_crops %in% evens(vector_crops)])

      if (n > 0) {
        isolate({
          list(
            fluidRow(

            column(6,
              purrr::map(vector_crops[!vector_crops %in% evens(vector_crops)],
                         ~bs4Dash::bs4Card(width = 12,
                               title = paste0("Feed: ", .x),
                               elevation = 2,
                               solidHeader = TRUE,
                               status = "teal",
                               collapsed = FALSE,
                               fluidRow(
                column(6,
                       selectInput(inputId  = ns(paste0("feed_", .x)),
                                   label    = paste0("Feed ", .x),
                                   choices  = feed_list(),
                                   selected = ifelse(.x > length(feed_list()), feed_list()[[1]], feed_list()[[.x]]))),
                column(6,
                       numericInput(inputId = ns(paste0("qnt_", .x)),
                                    label   = paste0("Quantity  (ton./year)"),
                                    value   = qnt_list()[[.x]]))))
          )),

          column(6,
          purrr::map(evens(vector_crops),
                     ~bs4Dash::bs4Card(width = 12,
                               title = paste0("Feed: ", .x),
                               elevation = 2,
                               solidHeader = TRUE,
                               status = "teal",
                               collapsed = FALSE,
                               fluidRow(
                column(6,
                       selectInput(inputId  = ns(paste0("feed_", .x)),
                                   label    = paste0("Feed ", .x),
                                   choices  = feed_list(),
                                   selected = ifelse(.x > length(feed_list()), feed_list()[[1]], feed_list()[[.x]]))),
                column(6,
                       numericInput(inputId = ns(paste0("qnt_", .x)),
                                    label   = paste0("Quantity  (ton./year)"),
                                    value   = qnt_list()[[.x]]))))
            )
          )
        )
      )
    })
  }

    })

    output$textbox_ui <- renderUI({

      textboxes()


      })

    purchased_feeds <- reactive({

      req(input[[paste0("feed_", 1)]])

      purchased_feeds <- tibble::tibble(
        "feeds"  = unlist(purrr::map(1:counter$n, function(i) { input[[paste0("feed_", i)]] } )),
        "Qntdds" = unlist(purrr::map(1:counter$n, function(i) { input[[paste0("qnt_", i)]]  } ))
      ) %>%
        dplyr::left_join(carbon_footprint(), by = c("feeds" = "feeds"))

      purchased_feeds

    })

    CO2eq_calculations <- reactive({

      req(input[[paste0("feed_", 1)]])

      purchased_feeds() %>%
         dplyr::mutate(
           CO2eq = carbon_foot * Qntdds * 1000 * DM / 100
          ) %>%
         dplyr::summarise(total = sum(CO2eq)) %>%
         dplyr::pull(total)
    })

# -------------------------------------------------------------------------
# Nutrient import calculations from purchased feeds -----------------------
# -------------------------------------------------------------------------

    # Nitrogen

    n_from_purchased_feeds <- reactive({

      purchased_feeds() %>%
        dplyr::mutate(
          total_n_ton = (cp_content / 100 * Qntdds * DM / 100 / 6.25)
        ) %>%
        dplyr::summarise(total = sum(total_n_ton)) %>%
        dplyr::pull(total)

    })

    # Phosphorous

    p_from_purchased_feeds <- reactive({

      purchased_feeds() %>%
        dplyr::mutate(
          total_p_ton = (p_content / 100 * Qntdds * DM / 100)
        ) %>%
        dplyr::summarise(total = sum(total_p_ton)) %>%
        dplyr::pull(total)

    })

    # Potassium

    k_from_purchased_feeds <- reactive({

      purchased_feeds() %>%
        dplyr::mutate(
          total_k_ton = (k_content / 100 * Qntdds * DM / 100)
        ) %>%
        dplyr::summarise(total = sum(total_k_ton)) %>%
        dplyr::pull(total)

    })

# -------------------------------------------------------------------------

    output$teste <- renderTable({

      # print(paste(
      #   "Nitrogenio: " , n_from_purchased_feeds(),
      #   "Fosforo:" , p_from_purchased_feeds(),
      #   "Potassio:", k_from_purchased_feeds()
      # ))
      #
      #  #CO2eq_calculations()

    })

# -------------------------------------------------------------------------
# Outputs from this module to populate others -----------------------------
# -------------------------------------------------------------------------

    return(
      list(
        teste             = reactive( CO2eq_calculations() ),
        purchased_feeds   = reactive( purchased_feeds() ),
        n_purchased_feeds = reactive( n_from_purchased_feeds() ),
        k_purchased_feeds = reactive( k_from_purchased_feeds() ),
        p_purchased_feeds = reactive( p_from_purchased_feeds() )
      )
    )

  })

}
