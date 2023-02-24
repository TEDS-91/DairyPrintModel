#' info UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_info_ui <- function(id){
  ns <- NS(id)
  tagList(

    bs4Dash::bs4Jumbotron(
      title = "DairyPrint Model: Helping Dairy Farmers Towards Higher Sustainability!",

      lead = "DairyPrint model is a high-level, simple, minimalistic, user-friendly,
              still powerful, and scientifically sound whole-farm decision support
              model to assess economic and environmental tradeoffs of dairy farming
              for strategic nutrient management decisions.",

      status = "info",
      btnName = "To get more information...",
      href = "https://gitlab.com/rpodcast/shinylego"
    ),
    fluidRow(
      column(6,
        bs4Dash::bs4UserCard(
          title = bs4Dash::bs4UserDescription(
            title = "Project Lead and Model Developer",
            subtitle = "Victor E. Cabrera",
            image = "www/victor.jpg",
            type = 1
          ),
          status = "teal",
          width = 12,

          bs4Dash::bs4ListGroup(
            width = 12,
            type = "action",
            bs4Dash::bs4ListGroupItem(
              "Department of Animal and Dairy Sciences - University of Wisconsin-Madison",
              href = "https://andysci.wisc.edu/directory/victor-cabrera/"
            ),
            bs4Dash::bs4ListGroupItem(
              "Dairy Management - https://DairyMGT.info",
              href = "https://dairymgt.info/about.php"
            ),
            bs4Dash::bs4ListGroupItem(
              "Linkedin: Victor Cabrera",
              href = "https://www.linkedin.com/in/victor-cabrera-9243a112/"
            )
          )
        )
      ),
      column(6,
             bs4Dash::bs4UserCard(
               title = bs4Dash::bs4UserDescription(
                 title = "Model Developer",
                 subtitle = "Tadeu E. da Silva",
                 image = "www/tadeu.jpeg",
                 type = 1
               ),
               status = "teal",
               width = 12,

               bs4Dash::bs4ListGroup(
                 width = 12,
                 type = "action",
                 bs4Dash::bs4ListGroupItem(
                   "Department of Animal and Dairy Sciences - University of Wisconsin-Madison",
                   href = "https://dairymgt.info/people.php"
                 ),
                 bs4Dash::bs4ListGroupItem(
                   "Github: TEDS-91",
                   href = "https://github.com/TEDS-91"
                 ),
                 bs4Dash::bs4ListGroupItem(
                   "Linkedin: Tadeu Eder da Silva",
                   href = "https://www.linkedin.com/in/tadeu-eder-da-silva-78b679181/"
                 )
               )
             )
           )
         )
       )
}

#' info Server Functions
#'
#' @noRd
mod_info_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_info_ui("info_1")

## To be copied in the server
# mod_info_server("info_1")
