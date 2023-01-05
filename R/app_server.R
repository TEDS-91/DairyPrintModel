#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Your application server logic

  # general prmts from UI

  facilitie <- reactive(input$facilitie)

  county <- reactive(input$county)

  bedding <- reactive(input$bedding_type)

  biodigester <- reactive(input$biodigester)

  biod_ef <- reactive(input$biod_ef)

  solid_liquid_separation <- reactive(input$solid_liquid)

  enclosed_manure <- reactive(input$enclosed_manure)

  empty_time <- reactive(input$empty)

  type_manure <- reactive(input$type_manure)

  crust <- reactive(input$crust)

  manure_storage_area <- reactive(input$storage_area)

  # calling modules

  animal_data <- mod_animal_server("animal")

  mod_economics_server("economics",
                       animal_data = animal_data)


  mod_nh3_emissions_server("nh3_emissions",
                           county              = county,
                           facilitie           = facilitie,
                           biodigester         = biodigester,
                           type_manure         = type_manure,
                           crust               = crust,
                           empty_time          = empty_time,
                           manure_storage_area = manure_storage_area)

  mod_ch4_emissions_server("ch4_emissions",
                           animal_data     = animal_data,
                           county          = county,
                           facilitie       = facilitie,
                           bedding         = bedding,
                           biodigester     = biodigester,
                           biodigester_ef  = biod_ef,
                           type_manure     = type_manure,
                           solid_liquid    = solid_liquid_separation,
                           enclosed_manure = enclosed_manure,
                           empty_time      = empty_time)

  mod_nitrous_oxide_emissions_server("nitrous_oxide",
                                     enclosed_manure = enclosed_manure,
                                     manure_storage_area = manure_storage_area)

  mod_crop_server("crop",
                  animal_data = animal_data)



  report <- reactive({

    animal_data()

  })

  output$rmd_report <- downloadHandler(

      "EZMoneyToolReport.html",

      content =
        function(file) {

          rmarkdown::render(
            input       = "inst/rmd_report/report.Rmd",
            output_file = "built_report.html",

            params = list(
              suma_table = report()
            )
          )

          readBin(con  = "inst/rmd_report/built_report.html",
                  what = "raw",
                  n    = file.info("inst/rmd_report/built_report.html")[ , "size"]) %>%

            writeBin(con = file)
        }
    )

}
