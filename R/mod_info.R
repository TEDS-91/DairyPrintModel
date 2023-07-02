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
      title = "DairyPrint model: Paving pathways for dairy farmers towards higher sustainability.",

      lead = "Quantifying greenhouse gas (GHG) emissions (i.e., methane, nitrous oxide, and carbon dioxide) from all significant sources in dairy farms is difficult
      and prohibitively expensive. The same applies to nutrient balance and management. Therefore, farmers must rely on mathematical models to achieve this. Available
      models are cumbersome and overwhelming to use. Thus, our objective is to develop the DairyPrint Model: a simple, minimalistic, user-friendly, and scientifically
      sound whole-farm decision support model to assess environmental tradeoffs of dairy farming. The DairyPrint Model is composed of herd, manure, crop, and economic
      modules. In the herd module, based on inputs such as total number of cows, calving interval, and culling rate the model runs the simulations performing herd dynamics
      in monthly steps, outputting annual herd demographics. Moreover, milk yield curves and milk composition, body weight, dry matter intake, and feed efficiency are
      estimated. These variables, in turn, are used to estimate the total excreted manure, urine, feces, nitrogen, phosphorus, and potassium, in addition to enteric
      methane emissions. From the herd module, there is a distribution of the produced outputs into other modules, in addition to the specific user inputs for each
      module. The barn module receives all the manure produced and from it, along with information from weather data and type of facility (freestall or tie-stall),
      methane, ammonia, and nitrous oxide emissions are estimated. Once the manure in the barn is transferred to the manure module, the manure is handled and processed
      according to the practices adopted by the farm. After manure processing, the processed material is distributed to the crop fields in the crop module. In the crop
      module, all GHG emissions are accounted for due to the application of manure, fertilizers, and limestone. Additionally, nutrient balances are estimated. Therefore,
      the DairyPrint Model is capable of helping farmers move toward higher sustainability, providing a user-friendly and intuitive graphical user interface allowing the
      user to respond to 'what-if' questions.",

      status = "info",
      btnName = "To get more information check this out!",
      href = "https://www.youtube.com/watch?v=clRn6xX78H4&list=PLohs4ZJkGPyGA9sPGQ4TnJt1fV0bN8sHs&index=5&t=1245s"
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

