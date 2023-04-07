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

    bs4Dash::bs4DashPage(
      fullscreen = TRUE,
      bs4Dash::bs4DashNavbar(
        ),
      bs4Dash::bs4DashSidebar(

        bs4Dash::sidebarUserPanel(
          image = "www/DIH_logo.png",
          name = "DairyPrint"
        ),
        bs4Dash::bs4SidebarMenu(

          bs4Dash::bs4SidebarMenuItem(
            "Herd Calibration",
            tabName = "herd_calibration",
            icon = icon("wrench", lib = "glyphicon")
          ),
          bs4Dash::bs4SidebarMenuItem(
            "Manure Handling",
            tabName = "manure",
            icon = icon("option-horizontal", lib = "glyphicon")
          ),
          bs4Dash::bs4SidebarMenuItem(
            "Crops",
            tabName = "crop",
            icon = icon("leaf", lib = "glyphicon")
          ),
          bs4Dash::bs4SidebarMenuItem(
            "Economics",
            tabName = "economics",
            icon = icon("usd", lib = "glyphicon")
          ),
          bs4Dash::bs4SidebarMenuItem(
            "Dashboard",
            tabName = "dashboard",
            icon = icon("stats", lib = "glyphicon")
          ),
          bs4Dash::bs4SidebarMenuItem(
            "About DairyPrint Model",
            tabName = "info",
            icon = icon("info-sign", lib = "glyphicon")
          ),
          fluidRow(
            column(offset = 1,
                   4,
                   h6("Supported by:"))),
          fluidRow(
            column(offset = 1,
                   4,
                   img(src='www/DIH_logo.png', align = "left", width = 91, height = 75))
        )
        )
      ),
      bs4Dash::bs4DashBody(
        bs4Dash::bs4TabItems(
          bs4Dash::bs4TabItem(
            tabName = "herd_calibration",
            mod_animal_ui("animal")
          ),
          bs4Dash::bs4TabItem(
            tabName = "manure",
            #mod_nh3_emissions_ui("nh3_emissions"),
            mod_manure_ghg_emissions_ui("ch4_emissions")
          ),
          bs4Dash::bs4TabItem(
            tabName = "crop",
            mod_purchased_feeds_ui("purchased_feeds_1"),
            mod_crop_ui("crop")
          ),
          bs4Dash::bs4TabItem(
            tabName = "economics",
            mod_economics_ui("economics")
          ),
          bs4Dash::bs4TabItem(
            tabName = "dashboard",
            mod_dashboard_ui("dashboard")
          ),
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
