#' @import shiny
app_server <- function(input, output,session) {
  
  
  ## Social
  callModule(mod_social_server, "social_module_1")
  
  # # Capture the URL parameters
  # observe({
  #   query <- parseQueryString(session$clientData$url_search)
  #   if (!is.null(query[['text']])) {
  #     updateTextInput(session, "text", value = query[['text']])
  #   }
  #   # if (!is.null(query[['age']])) {
  #   #   updateNumericInput(session, "age", value = query[['age']])
  #   # }
  # })
  
  # List the first level callModules here
  callModule(mod_leaflet_server, 'leaf1')

  output$plot1 <- renderPlot({
    barplot(1:10, col = grey(seq(0, 1, length = 10)),
            main = 'This is a plot',
            sub = "You can't modify it from html/css, it's an image file")
  })
  
  output$region_ui <- renderUI({
    
    # Capture the country
    the_country <- input$country
    ok <- FALSE
    if(!is.null(the_country)){
      the_choices <- covid19::df %>% filter(country == the_country)
      the_choices <- sort(unique(the_choices$district))
      print(paste0('The choices are: '))
      print(the_choices)
      if(length(the_choices) > 0){
        ok <- TRUE
      }
    }
    if(!ok){
      the_choices <- '<Not available>'
    }
    
    
    selectInput('region', 'Sub-region (if available)',
                choices = the_choices)
  })
  
  library(leaflet)
  output$l1 <- leaflet::renderLeaflet({
    leaflet() %>%
      addProviderTiles('Esri.WorldImagery') %>%
      addMarkers(data = tibble(lng = rnorm(10, sd = 40),
                               lat = rnorm(10, sd = 40)))
  })
  
  observeEvent(input$action, {
    showModal(
      modalDialog(title = 'This is a modal',
                  size = 'l',
                  easyClose = TRUE,
                  fade = TRUE,
                  fluidPage(
                    fluidRow(
                      column(6, h3('Here is some text'),
                             p('And some sub-text')),
                      column(6, h3('Here is some more text'),
                             helpText('And some helper text'))
                    )
                  ))
    )
  })
  
}
