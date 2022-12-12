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
               selectInput("facilitie",       label = "Facilitie",                choices = c("freestall", "tie-stall"), selected = "tie-stall")),
        column(3,
               selectInput("county",          label = "County",                   choices = unique(wi_weather$county), selected = "Adams")),
        column(3,
               selectInput("bedding_type",    label = "Bedding type",             choices = c("Sand", "Sawdust", "Chopped straw"), selected = "Sawdust")),
        column(3,
               selectInput("biodigester",     label = "Biodigester",              choices = c("yes", "no"), selected = "yes"))),

      fluidRow(
        column(3,
               numericInput("biod_ef",        label = "Biodigester efficiency:",  value = 30)),
        column(3,
               selectInput("solid_liquid",    label = "Solid-liquid separation:", choices = c("yes", "no"), selected = "yes")),
        column(3,
               selectInput("type_manure",     label = "Manure",                   choices = c("slurry", "solid"), selected = "solid")),
        column(3,
               selectInput("enclosed_manure", label = "Enclosed manure store:",   choices = c("yes", "no"), selected = "no")),
        column(3,
               selectInput("empty",           label = "Empty time:",              choices = c("Fall", "Spring", "Fall and Spring"), selected = "Fall"))
      ),

      mod_barn_nh3_emissions_ui("barn_nh3_emissions"),

      mod_ch4_emissions_ui("ch4_emissions"),

      mod_economics_ui("economics")



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
