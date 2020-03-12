
##################################################
# UI
##################################################
#' @import shiny
#' @import shinydashboard
#' @import leaflet
#' @import shiny
#' @import ggplot2
app_ui <- function(request) {
  options(scipen = '999')
  options(shiny.sanitize.errors = FALSE)

  
  #############################
  # UI COMBINATION
  #############################
  tagList(
    golem_add_external_resources(),
    # Force view of non-mobile

    
    # UI
    navbarPage(title = 'Databrew\'s COVID-19 data explorer',
               collapsible = TRUE,
               id = 'navs',
               footer = shiny::includeHTML(system.file('app/www/footer.html', package = 'covid19')),
               tabPanel('Epidemic curves',
                        fluidPage(
                          fluidRow(
                            shinydashboard::box(width = 12,
                                                title = '"Critical mass" comparison plots',
                                                column(6,
                                                       plotOutput('plot0')),
                                                column(6,
                                                       plotOutput('plot0_b'))),
                            shinydashboard::box(width = 12,
                                                title = 'Plot parameters',
                                                fluidRow(
                                                  column(4,
                                                         selectInput('country', 'Country/Countries',
                                                                     multiple = TRUE,
                                                                     choices = sort(unique(sort(unique(covid19::df_country$country)))),
                                                                     selected = c('Italy', 'Spain', 'France', 'US'))),
                                                  column(4,
                                                         sliderInput('day0', '"Critical mass" adjustment: Number of cases to be considered "day 0"',
                                                                     min = 1,
                                                                     width = '100%',
                                                                     max = 500,
                                                                     value = 150,
                                                                     step = 1),
                                                         helpText("The slider above allows for the comparison between countries' trajectories once 'x' number of people are already infected. For example, moving the value to 100, will show the curve of each country setting as 'day zero' the moment at which at least 100 people were infected.")),
                                                  column(4,
                                                         checkboxInput('ylog', 'Logarithmic y-axis?',
                                                                       value = TRUE),
                                                         checkboxInput('cumulative', 'Cumulative cases?',
                                                                       value = TRUE),
                                                         helpText('(Because the relationship between time and number of cases is exponential, using a logarithmic, rather than linear, scale is a better way to visualize comparisons countries over time.)'))
                                                ))
                          )
                        )
               ),
               tabPanel('World Map', 
                        fluidPage(
                          fluidRow(
                            shinydashboard::box(width = 6,
                                                title = 'World map',
                                                leafletOutput('leafy')),
                            shinydashboard::box(width = 6,
                                                title = 'Map parameters',
                                                selectInput('map_type',
                                                            'Map type',
                                                            choices = c('Polygons (choropleth)',
                                                                        # 'One point per person (jittering)',
                                                                        'Points (radius)')),
                                                selectInput('indicator',
                                                            'Indicator',
                                                            choices = c('Confirmed cases',
                                                                        'Recoveries',
                                                                        'Deaths'))
                            )),
                          fluidRow(
                            shinydashboard::box(width = 12,
                                                plotOutput('plot_overall')))
                        )),
               tabPanel('About',
                        fluidPage(
                          fluidRow(
                            div(img(src='www/logo.png', align = "center"), style="text-align: center;"),
                            h4('Built by ',
                               a(href = 'http://databrew.cc',
                                 target='_blank', 'Databrew'),
                               align = 'center'),
                            p('Empowering research and analysis through collaborative data science.', align = 'center'),
                            div(a(actionButton(inputId = "email", label = "info@databrew.cc", 
                                               icon = icon("envelope", lib = "font-awesome")),
                                  href="mailto:info@databrew.cc",
                                  align = 'center')), 
                            style = 'text-align:center;'
                          )
                        ))
               # tabPanel('Contagion Simulator'),
               # tabPanel('COVID-19 online'),
               # tabPanel('Economic impact'),
               # navbarMenu('Comparison to other diseases',
               #            tabPanel('Contagion'),
               #            tabPanel('Mortality'))
    )
    # dashboardPage(
    #   header = header,
    #   sidebar = sidebar,
    #   body = body)
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
golem_add_external_resources <- function(){
  addResourcePath(
    'www', system.file('app/www', package = 'covid19')
  )
  
  share <- list(
    title = "Databrew's COVID-19 Data Explorer",
    url = "https://datacat.cc/covid19/",
    image = "http://www.databrew.cc/images/blog/covid2.png",
    description = "Comparing epidemic curves across countries",
    twitter_user = "data_brew"
  )
  
  tags$head(
    
    # # Force to wide / non-mobile view to avoid cut-off
    # HTML('<meta name="viewport" content="width=1024">'),
    
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
    
    golem::activate_js(),
    golem::favicon(),
    # Add here all the external resources
    # Google analytics script
    includeHTML(system.file('app/www/google-analytics.html', package = 'covid19')),
    includeScript(system.file('app/www/script.js', package = 'covid19')),
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
app_server <- function(input, output, session) {
  
  ## Social
  callModule(mod_social_server, "social_module_1")
  
  # Overall chart
  output$plot_overall <- renderPlot({
    indicator <- input$indicator
    
    if(indicator == 'Confirmed cases'){
      right <- df_country %>%
        group_by(country) %>%
        filter(date == max(date)) %>%
        summarise(value = sum(confirmed_cases))
    }
    if(indicator == 'Recoveries'){
      right <- df_country %>%
        group_by(country) %>%
        filter(date == max(date)) %>%
        summarise(value = sum(recovered))
    }
    if(indicator == 'Deaths'){
      right <- df_country %>%
        group_by(country) %>%
        filter(date == max(date)) %>%
        summarise(value = sum(deaths))
    }
    
    right <- right %>% arrange(desc(value)) 
    # right <- right[1:10,]
    right$country <- factor(right$country, levels = right$country)
    ggplot(data = right,
           aes(x = country,
               y = value)) +
      geom_point() +
      geom_segment(aes(xend = country,
                       yend = 0)) +
      scale_y_log10() +
      theme_simple() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      labs(x = '',
           y = '',
           title = indicator,
           subtitle = paste0('Data as of ', as.character(max(df_country$date)))) 
  })
  
  # Day zero adjustment chart
  output$plot0 <- renderPlot({
    plot_day_zero(countries = input$country,
                  ylog = input$ylog,
                  day0 = input$day0,
                  cumulative = input$cumulative)
  },
  height = 400, width = 700)
  
  # Day zero adjustment chart
  output$plot0_b <- renderPlot({
    
    plot_day_zero(countries = input$country,
                  ylog = input$ylog,
                  day0 = input$day0,
                  cumulative = input$cumulative,
                  time_before = -10,
                  add_markers = TRUE)
  },
  height = 400, width = 700)
  
  # Main leaflet chart  
  output$leafy <- renderLeaflet({
    shp <- world
    
    leaflet(data = shp) %>% 
      addProviderTiles('Stamen.Toner') %>%
      setView( lat=10, lng=0 , zoom=2) 
  })
  
  observeEvent({
    input$indicator
    input$map_type
    input$navs
    1
  },{
    
    # Define the world map
    shp <- world
    
    # Capture the indicator input
    indicator <- input$indicator
    
    # Capture the map type input
    map_type <- input$map_type
    # 'Polygons (choropleth)',
    # 'Points (radius)',
    # 'One point per person (jittering)'
    
    # Get the values per country
    if(indicator == 'Confirmed cases'){
      right <- df_country %>%
        group_by(country) %>%
        filter(date == max(date)) %>%
        summarise(value = sum(confirmed_cases))
    }
    if(indicator == 'Recoveries'){
      right <- df_country %>%
        group_by(country) %>%
        filter(date == max(date)) %>%
        summarise(value = sum(recovered))
    }
    if(indicator == 'Deaths'){
      right <- df_country %>%
        group_by(country) %>%
        filter(date == max(date)) %>%
        summarise(value = sum(deaths))
    }
    shp@data <- left_join(shp@data,
                          right, by = 'country')
    
    # Make tooltip
    mytext <- paste(
      "Country: ", as.character(shp@data$country),"<br/>",
      "Value: ", round(shp@data$value, digits = 3), "<br/>",
      sep="") %>%
      lapply(htmltools::HTML)
    
    
    if(map_type == 'Points (radius)'){
      mypalette <- colorBin(RColorBrewer::brewer.pal(n= 8, name = 'Spectral'),
                            domain=log(shp@data$value),
                            na.color="transparent",
                            reverse = TRUE,
                            pretty = TRUE,
                            bins = round(exp(seq(0, max(log(shp@data$value), na.rm = T), length = 8))))
      
      leafletProxy('leafy', session) %>%
        clearShapes() %>%
        clearMarkers() %>%
        clearControls() %>%
        addPolygons(data = shp,
                    fillColor = NA,
                    color = 'black',
                    weight = 0.5,
                    fillOpacity = 0) %>%
        addCircleMarkers(data = shp@data,
                         lng = shp@data$LON,
                         lat = shp@data$LAT,
                         radius = log(shp@data$value) * 3,
                         fillOpacity = 0.8,
                         label = mytext,
                         color = ~mypalette(value),
                         labelOptions = labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px"),
                           textsize = "13px",
                           direction = "auto"
                         ))
      
    }
    
    # if(map_type == 'One point per person (jittering)'){
    #   pd <- df_country %>% 
    #     group_by(country) %>%
    #     filter(date == max(date)) %>%
    #     ungroup 
    #   out_list <- list()
    #   for(i in 1:nrow(pd)){
    #     this_row <- pd[i,]
    #   }
    #   
    # }
    
    ## CHOROPLETH
    if(map_type == 'Polygons (choropleth)'){
      # Make color palette
      mypalette <- colorBin(RColorBrewer::brewer.pal(n= 8, name = 'Spectral'),
                            domain=log(shp@data$value),
                            na.color="transparent",
                            reverse = TRUE,
                            pretty = TRUE,
                            bins = round(exp(seq(0, max(log(shp@data$value), na.rm = T), length = 8))))
      
      
      leafletProxy('leafy', session) %>%
        clearShapes() %>%
        clearMarkers() %>%
        clearControls() %>%
        addPolygons(
          data = shp,
          fillColor = ~mypalette(value),
          stroke=TRUE,
          fillOpacity = 0.8,
          color="black",
          weight=0.7,
          label = mytext,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "13px",
            direction = "auto"
          )
        ) %>%
        addLegend(data = shp, pal=mypalette, values=(shp@data$value), opacity=0.9, title = "", position = "bottomleft" )
    }
  })
  
  
  
  
  # output$region_ui <- renderUI({
  #   
  #   # Capture the country
  #   the_country <- input$country
  #   ok <- FALSE
  #   if(!is.null(the_country)){
  #     the_choices <- covid19::df %>% filter(country == the_country)
  #     the_choices <- sort(unique(the_choices$district))
  #     if(length(the_choices) > 0){
  #       ok <- TRUE
  #     }
  #   }
  #   if(!ok){
  #     the_choices <- '<Not available>'
  #   }
  #   
  #   
  #   selectInput('region', 'Sub-region (if available)',
  #               choices = the_choices)
  # })
  
  
  
}

app <- function(){
  # Detect the system. If on AWS, don't launch browswer
  is_aws <- grepl('aws', tolower(Sys.info()['release']))
  shinyApp(ui = app_ui,
           server = app_server,
           options = list('launch.browswer' = !is_aws))
}