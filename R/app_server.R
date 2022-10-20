#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Your application server logic

  mod_animal_server("animal")


  df <- reactive({

    df <- stats::rnorm(1000, 25, 4)

  })

  output$rmd_report <- downloadHandler(

      "EZMoneyToolReport.html",

      content =
        function(file) {

          rmarkdown::render(
            input = "inst/rmd_report/report.Rmd",
            output_file = "built_report.html",

            params = list(
              dry_matter_intake = df()
            )
          )

          readBin(con = "inst/rmd_report/built_report.html",
                  what = "raw",
                  n = file.info("inst/rmd_report/built_report.html")[ , "size"]) %>%

            writeBin(con = file)
        }
    )

}
