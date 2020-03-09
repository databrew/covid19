# Module UI
  
#' @title   mod_example_module_ui and mod_example_module_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_example_module
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_example_module_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_example_module
#' @export
#' @keywords internal
    
mod_example_module_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_example_module_ui("example_module_ui_1")
    
## To be copied in the server
# callModule(mod_example_module_server, "example_module_ui_1")
 
#' example_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_example_module_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' example_module Server Function
#'
#' @noRd 
mod_example_module_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_example_module_ui("example_module_ui_1")
    
## To be copied in the server
# callModule(mod_example_module_server, "example_module_ui_1")
 
