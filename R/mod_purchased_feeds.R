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
               uiOutput(ns("textbox_ui")))#,
       #tableOutput(ns("teste"))
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
        ~"feeds",                   ~"carbon_foot",
        "Corn",                      0.37,
        "Corn Silage",               0.15,
        "Alfalfa hay",               0.18,
        "Soybean/canola meal",       0.50,
        "Corn gluten feed",          0.30,
        "Protein mix",               0.50,
        "General by product feed",   0.20,
        "Distillers's grain",        0.60,
        "Fat",                       1.52,
        "Mineral and Vitamins",      1.62,
        "Milk replacer",             12.10
      )
    })

    feed_list <- reactive({

      carbon_footprint()$feeds

    })

    # Track the number of input boxes to render
    counter <- reactiveValues(n = 2)

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

      #dois <- evens(vector_crops)

      if (n > 0) {
        isolate({
          list(
            fluidRow(
          #lapply(um, function(i) {
            #list(
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
                                    value   = 15))))


            #)

          #}
          )),

          #lapply(dois, function(i) {
            #list(
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
                                    value   = 15))))
              #)

         # }
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
           CO2eq = carbon_foot * Qntdds * 1000
          ) %>%
         dplyr::summarise(total = sum(CO2eq)) %>%
         dplyr::pull(total)


    })

    output$teste <- renderTable({

       CO2eq_calculations()

    })


# -------------------------------------------------------------------------
# Outputs from this module to populate others -----------------------------
# -------------------------------------------------------------------------




    return(
      list(
        teste           = reactive({ CO2eq_calculations() }),
        purchased_feeds = reactive({ purchased_feeds() })
      )
    )

  })

}

## To be copied in the UI
# mod_purchased_feeds_ui("purchased_feeds_1")

## To be copied in the server
# mod_purchased_feeds_server("purchased_feeds_1")
