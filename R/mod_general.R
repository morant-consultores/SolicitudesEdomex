#' general UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_general_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' general Server Functions
#'
#' @noRd 
mod_general_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_general_ui("general_1")
    
## To be copied in the server
# mod_general_server("general_1")
