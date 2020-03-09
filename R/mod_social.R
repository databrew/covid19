# Mod social

#' @title mod_social_ui and mod_social_server
#' @description  Make the social section
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_social
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_social_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Social media share buttons
    column(12, align = 'center',
           shinydashboard::box(
             width = 12, 
             status = 'success',
             # title = "Social Buttons",
             shinydashboardPlus::socialButton(
               url = "https://twitter.com/intent/tweet?text=The%20Databrew%20COVID19%20App&url=https://bohemia.team/covid19/",
               type = "twitter"
             ),
             
             shinydashboardPlus::socialButton(
               url = "https://www.linkedin.com/shareArticle?mini=true&url=https%3A%2F%2Fwww.bohemia.team/covid19/&title=The%20Databrew%20COVID19%20App",
               type = "linkedin"
             ),
             
             a(actionButton(inputId = "email", label = "", 
                            icon = icon("envelope", lib = "font-awesome")),
               href="mailto:info@databrew.cc?subject=https%3A%2F%2Fwww.bohemia.team/covid19/&body=The%20Databrew%20COVID19%20App%20https%3A%2F%2Fwww.bohemia.team/covid19/"),
             
             shinydashboardPlus::socialButton(
               url = "http://www.facebook.com/sharer.php?u=https%3A%2F%2Fwww.bohemia.team/covid19/",
               type = "facebook"
             ),
             
             shinydashboardPlus::socialButton(
               url = "https://github.com/databrew/covid19",
               type = "github"
             )
           )
    )
  )
}

# Module Server
#' @title mod_social_server
#' @description Mod social server
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_social
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 

mod_social_server <- function(input, output, session){
  ns <- session$ns
  # Right now, no functionality to detect the hostname, etc.
  # And pass the parameters appropriately to the UI
  # (rather than hard-coding them into the UI, as is currently written)
  # This can be implemented later
}

## To be copied in the UI
# mod_social_ui("social_module_1")

## To be copied in the server
# callModule(mod_social_server, "social_module_1")
