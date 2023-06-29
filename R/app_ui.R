#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources

    #shiny.telemetry::use_telemetry(), # 2. Add necessary Javascript to Shiny

    golem_add_external_resources(),

    tags$style("
              body {
    -moz-transform: scale(0.95, 0.95); /* Moz-browsers */
    zoom: 0.95; /* Other non-webkit browsers */
    zoom: 95%; /* Webkit browsers */
}
              "),

    # Your application UI logic

    bs4Dash::bs4DashPage(

      fullscreen = TRUE,

      bs4Dash::bs4DashNavbar(

        ),
      bs4Dash::bs4DashSidebar(

        bs4Dash::sidebarUserPanel(
          image = "www/DIH_logo.png",
          name  = "DairyPrint Model"),

        bs4Dash::bs4SidebarMenu(

          bs4Dash::bs4SidebarMenuItem(
            "Herd Calibration",
            tabName = "herd_calibration",
            icon    = tags$i(fontawesome::fa("cow"))),

          bs4Dash::bs4SidebarMenuItem(
            "Manure Handling",
            tabName = "manure",
            icon    = tags$i(fontawesome::fa("poop"))),

          bs4Dash::bs4SidebarMenuItem(
            "Purc. Feeds and Crops",
            tabName = "crop",
            icon    = tags$i(fontawesome::fa("wheat-awn"))),

          bs4Dash::bs4SidebarMenuItem(
            "Miscellaneous",
            tabName = "miscellaneous",
            icon    = icon("usd", lib = "glyphicon")),

          bs4Dash::bs4SidebarMenuItem(
            "Dashboard",
            tabName = "dashboard",
            icon    = tags$i(fontawesome::fa("gauge"))),

          bs4Dash::bs4SidebarMenuItem(
            "About DairyPrint Model",
            tabName = "info",
            icon    = icon("info-sign", lib = "glyphicon")),

        fluidRow(
          column(offset = 1,
                 4,
                 HTML(paste("Authors:", "</br>",
                            "Prof. Victor E. Cabrera, PhD.", "</br>",
                            "Tadeu E. da Silva, PhD.")
              )
            )
          ),

        br(),

        fluidRow(
          column(offset = 1,
                 4,
                 h6("Project supported by:"))),
        fluidRow(
          column(offset = 1,
                 4,
                 img(src='www/DIH_logo.png', align = "left", width = 91, height = 75)
          )
        )
      )
    ),

      bs4Dash::bs4DashBody(
        bs4Dash::bs4TabItems(

          bs4Dash::bs4TabItem(
            tabName = "herd_calibration",
            mod_animal_ui("animal")

            # modalDialog(
            #   h5("To run the model, first you have to calibrate it according to your own farm inputs."),
            #
            #   h5("After filling up all inputs in the herd calibration page, you're ready to run the model:
            #
            #      just hit 'Run!' and have fun!"),
            #
            #   h5("The other tabs will respond imediately after input changes without the need to hit any button."),
            #   title = "Fisrt-steps with DairyPrint Model:",
            #   size = "m",
            #   easyClose = FALSE
            # )


            ),

          bs4Dash::bs4TabItem(
            tabName = "manure",
            #mod_nh3_emissions_ui("nh3_emissions"),
            mod_manure_ghg_emissions_ui("ch4_emissions")),

          bs4Dash::bs4TabItem(
            tabName = "crop",
            mod_purchased_feeds_ui("purchased_feeds_1"),
            mod_crop_ui("crop")),

          bs4Dash::bs4TabItem(
            tabName = "miscellaneous",
            mod_miscellaneous_ui("miscellaneous")),

          bs4Dash::bs4TabItem(
            tabName = "dashboard",
            mod_dashboard_ui("dashboard")),

          bs4Dash::bs4TabItem(
            tabName = "info",
            mod_info_ui("info_1")
          )
        )
      )
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
      app_title = "DairyPrintModel"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
