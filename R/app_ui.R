#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      h1("EZMoneyTool", align = "center"),

      mod_animal_ui("animal"),

      fluidRow(
        column(3,
               selectInput("facilitie", label = "Select the facilitie", choices = c("freestall", "tie-stall"), selected = "tie-stall")),
        column(3,
               selectInput("county", label = "Select the county", choices = unique(wi_wheather$county), selected = "Adams")),
        column(3,
               selectInput("bedding_type", label = "Select the Bedding type", choices = c("Sand", "Sawdust", "Chopped straw"), selected = "Sawdust")),
        column(3,
               selectInput("biodigester", label = "Biodigester", choices = c("yes", "no"), selected = "no"))),
      fluidRow(
        column(3,
               numericInput("biod_ef", label = "Biodigester Efficiency:", value = 30))
      ),

      mod_barn_nh3_emissions_ui("barn_nh3_emissions"),

      mod_ch4_emissions_ui("ch4_emissions")



    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "EZMoneyTool"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
