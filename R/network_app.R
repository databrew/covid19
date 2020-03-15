
##################################################
# UI
##################################################
#' @import shiny
#' @import shinydashboard
#' @import leaflet
#' @import shiny
#' @import ggplot2
#' @import shinyMobile
#' @import nd3 
network_app_ui <- function(request) {
  #devtools::install_github('databrew/nd3')
  tagList(
    network_golem_add_external_resources(),
    
    f7Page(
      init = f7Init(
        skin = 'ios', #  c("ios", "md", "auto", "aurora"),
        theme = 'light', #c("dark", "light"),
        filled = TRUE
      ),
      title = "Contagion in a network",
      f7SingleLayout(
        navbar = f7Navbar(
          title = "Databrew's network contagion explorer",
          hairline = TRUE,
          shadow = TRUE
        ),
        toolbar = f7Toolbar(
          position = "bottom",
          f7Link(label = "Databrew", src = "https://databrew.cc", external = TRUE),
          f7Link(label = "Blog post on COVID-19 epidemic curves", src = "https://www.databrew.cc/posts/covid.html", external = TRUE)
        ),
        # main content
        f7Shadow(
          intensity = 10,
          hover = TRUE,
          f7Card(
            forceNetworkOutput('fn', height = '500px'),
            f7Button('regen', 'Re-generate'),
            height = 500,
          )
        ),
        f7Shadow(
          intensity = 10,
          hover = TRUE,
          f7Card(
            
            sliderInput('n', 'Size of community',
                        min = 10, max = 200, value = 100, step = 10),
            sliderInput('contacts', 'Average number of contacts',
                        min = 0, max = 3, value = 0.7, step = 0.1),
            sliderInput('sd', 'Standard deviation in number of contacts',
                        min = 0.5, max = 3, value = 0.5),

            sliderInput('sick', 'Percent infected',
                        min = 0, max = 100, value = 2, step = 1),
            sliderInput('font_size', 'Font size',
                        min = 6, max = 20, value = 14)
            
            
            
          
            
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
network_golem_add_external_resources <- function(){
  # addResourcePath(
  #   'www', system.file('app/www', package = 'covid19')
  # )
  
  share <- list(
    title = "Databrew's COVID-19 Data Explorer",
    url = "https://datacat.cc/covid19/",
    image = "http://www.databrew.cc/images/blog/covid2.png",
    description = "Comparing epidemic curves across countries",
    twitter_user = "data_brew"
  )
  
  tags$head(
    
    # Facebook OpenGraph tags
    tags$meta(property = "og:title", content = share$title),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(property = "og:url", content = share$url),
    tags$meta(property = "og:image", content = share$image),
    tags$meta(property = "og:description", content = share$description),
    
    # Twitter summary cards
    tags$meta(name = "twitter:card", content = "summary"),
    tags$meta(name = "twitter:site", content = paste0("@", share$twitter_user)),
    tags$meta(name = "twitter:creator", content = paste0("@", share$twitter_user)),
    tags$meta(name = "twitter:title", content = share$title),
    tags$meta(name = "twitter:description", content = share$description),
    tags$meta(name = "twitter:image", content = share$image),
    
    # golem::activate_js(),
    # golem::favicon(),
    # Add here all the external resources
    # Google analytics script
    includeHTML(system.file('app/www/google-analytics-mini.html', package = 'covid19')),
    includeScript(system.file('app/www/script.js', package = 'covid19')),
    includeScript(system.file('app/www/mobile.js', package = 'covid19')),
    # includeScript('inst/app/www/script.js'),
    
    # includeScript('www/google-analytics.js'),
    # If you have a custom.css in the inst/app/www
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
    # tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}

##################################################
# SERVER
##################################################
#' @import shiny
#' @import leaflet
network_app_server <- function(input, output, session) {
  
  output$fn <- renderForceNetwork({
    regen <- input$regen
    plot_network(n = input$n,
                 contacts = input$contacts,
                 # sd = 1,
                 sd = input$sd,
                 save = NULL,
                 sick = input$sick,
                 font_size = input$font_size)
  })
}

network_app <- function(){
  # Detect the system. If on AWS, don't launch browswer
  is_aws <- grepl('aws', tolower(Sys.info()['release']))
  shinyApp(ui = network_app_ui,
           server = network_app_server,
           options = list('launch.browswer' = !is_aws))
}