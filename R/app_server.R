#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Your application server logic

  mod_animal_server("animal")

  mod_economics_server("economics")

  facilitie <- reactive(input$facilitie)

  county <- reactive(input$county)

  bedding <- reactive(input$bedding_type)

  biodigester <- reactive(input$biodigester)

  biod_ef <- reactive(input$biod_ef)

  solid_liquid_separation <- reactive(input$solid_liquid)

  enclosed_manure <- reactive(input$enclosed_manure)

  empty_time <- reactive(input$empty)

  type_manure <- reactive(input$type_manure)


  mod_barn_nh3_emissions_server("barn_nh3_emissions",
                                county = county,
                                facilitie = facilitie)

  mod_ch4_emissions_server("ch4_emissions",
                           county          = county,
                           facilitie       = facilitie,
                           bedding         = bedding,
                           biodigester     = biodigester,
                           biodigester_ef  = biod_ef,
                           type_manure     = type_manure,
                           solid_liquid    = solid_liquid_separation,
                           enclosed_manure = enclosed_manure,
                           empty_time      = empty_time)









  df <- reactive({

    df <- stats::rnorm(1000, 25, 4)

  })

  output$rmd_report <- downloadHandler(

      "EZMoneyToolReport.html",

      content =
        function(file) {

          rmarkdown::render(
            input       = "inst/rmd_report/report.Rmd",
            output_file = "built_report.html",

            params = list(
              dry_matter_intake = df()
            )
          )

          readBin(con  = "inst/rmd_report/built_report.html",
                  what = "raw",
                  n    = file.info("inst/rmd_report/built_report.html")[ , "size"]) %>%

            writeBin(con = file)
        }
    )

}
