
##################################################
# UI
##################################################
#' @import shiny
#' @import shinydashboard
#' @import leaflet
#' @import shiny
#' @import ggplot2
#' @import shinyMobile
mobile_app_ui <- function(request) {
  
  tagList(
    mobile_golem_add_external_resources(),
    
    f7Page(
      init = f7Init(
        skin = 'ios', #  c("ios", "md", "auto", "aurora"),
        theme = 'light', #c("dark", "light"),
        filled = TRUE
      ),
      title = "Databrew's COVID-19 epidemic curve explorer",
      f7SingleLayout(
        navbar = f7Navbar(
          title = "Databrew's COVID-19 epidemic curve explorer",
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
            plotOutput('day0'),
            selectInput('country', 'Country/Countries',
                        multiple = TRUE,
                        choices = sort(unique(sort(unique(covid19::df_country$country)))),
                        selected = c('Italy', 'Spain', 'France', 'US')),
            # f7Stepper('day0', '"Critical mass": number of cases to be considered start of outbreak (day 0)', min = 1, max = 500, value = 150, step = 5),
            
            sliderInput('day0', '"Critical mass" adjustment: Number of cases to be considered "day 0"',
                        min = 1,
                        max = 500,
                        value = 150,
                        # scale = TRUE,
                        step = 1),
            f7Toggle('deaths', 'Deaths instead of cases?',
                     checked = FALSE),
            f7Toggle('pop', 'Adjust by population?',
                     checked = FALSE),
            height = 300,
          )
        ),
        f7Shadow(
          intensity = 10,
          hover = TRUE,
          f7Card(
            sliderInput('time_before', 'Number of days to show before "critical mass"',
                        min = -20,
                        max = 0,
                        value = 0,
                        # scale = TRUE,
                        step = 1),
            br(),
            f7Toggle('ylog', 'Logarithmic y-axis?',
                     checked = TRUE), 
            br(),
            f7Toggle('cumulative', 'Cumulative cases?',
                     checked = TRUE),
            # br(),
            # f7Toggle('add_markers', 'Add visual markers at "critical mass"?',
            #          checked = TRUE),
            br(),
            f7Stepper('line_size', 'Line thickness', min = 0.5, max = 4, value = 1, step = 0.5),
            br(),
            
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
mobile_golem_add_external_resources <- function(){
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
mobile_app_server <- function(input, output, session) {
  
  output$day0 <- renderPlot({
    plot_day_zero(countries = input$country,
                  ylog = input$ylog,
                  day0 = input$day0,
                  cumulative = input$cumulative,
                  time_before = input$time_before,
                  line_size = input$line_size,
                  add_markers = FALSE,
                  # add_markers = input$add_markers,
                  deaths = input$deaths,
                  pop = input$pop)
  })
}

mobile_app <- function(){
  # Detect the system. If on AWS, don't launch browswer
  is_aws <- grepl('aws', tolower(Sys.info()['release']))
  shinyApp(ui = mobile_app_ui,
           server = mobile_app_server,
           options = list('launch.browswer' = !is_aws))
}