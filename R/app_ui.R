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
        column(2,
               numericInput("biod_ef",        label = "Biodigester efficiency:",  value = 30)),
        column(2,
               selectInput("solid_liquid",    label = "Solid-liquid separation:", choices = c("yes", "no"), selected = "yes")),
        column(2,
               selectInput("type_manure",     label = "Manure",                   choices = c("slurry", "solid"), selected = "solid")),
        column(2,
               selectInput("enclosed_manure", label = "Enclosed manure store:",   choices = c("yes", "no"), selected = "no")),
        column(2,
               selectInput("empty",           label = "Empty time:",              choices = c("Fall", "Spring", "Fall and Spring"), selected = "Fall")),
        column(2,
               selectInput("crust",           label = "Crust formation:",         choices = c("yes", "no"), selected = "no")),
        column(2,
               numericInput("storage_area",   label = "Manure Storage Area (m2):", value = 200))
      ),

      mod_nh3_emissions_ui("nh3_emissions"),

      mod_ch4_emissions_ui("ch4_emissions"),

      mod_nitrous_oxide_emissions_ui("nitrous_oxide"),

      mod_economics_ui("economics"),

      mod_crop_ui("crop")



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
