
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
               id = 'navs',
               footer = shiny::includeHTML(system.file('app/www/footer.html', package = 'covid19')),
               tabPanel('Epidemic curves',
                        fluidPage(
                          fluidRow(
                            shinydashboard::box(width = 6,
                                                title = 'Day-zero comparison plot',
                                                plotOutput('plot_day_zero')),
                            shinydashboard::box(width = 6,
                                                title = 'Plot parameters',
                                                selectInput('country', 'Country',
                                                            multiple = TRUE,
                                                            choices = sort(unique(sort(unique(covid19::df$country)))),
                                                            selected = c('Italy', 'Spain', 'France', 'US')),
                                                checkboxInput('ylog', 'Logarithmic y-axis?',
                                                              value = TRUE),
                                                checkboxInput('cumulative', 'Cumulative cases?',
                                                              value = TRUE),
                                                helpText('Because the relationship between time and number of cases is exponential, using a logarithmic, rather than linear, scale is a better way to visualize comparisons countries over time.'),
                                                br(), br(),
                                                sliderInput('day0', 'DAY ZERO ADJUSTMENT: Minimum number of cases to be considered for "day 0"',
                                                            min = 1,
                                                            width = '100%',
                                                            max = 500,
                                                            value = 1,
                                                            step = 1),
                                                helpText("The slider above allows for the comparison between countries' trajectories once 'x' number of people are already infected. For example, moving the value to 100, will show the curve of each country beginning on the day at which at least 100 people were infected (either cumulatively or on the specific day, depending on your choice)."))
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
    includeScript(system.file('app/www/script.js', package = 'covid19'))#,
    # includeScript('inst/app/www/script.js'),
    
    # includeScript('www/google-analytics.js'),
    # If you have a custom.css in the inst/app/www
    # tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
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
      right <- df %>%
        group_by(country) %>%
        filter(date == max(date)) %>%
        summarise(value = sum(confirmed_cases))
    }
    if(indicator == 'Recoveries'){
      right <- df %>%
        group_by(country) %>%
        filter(date == max(date)) %>%
        summarise(value = sum(recovered))
    }
    if(indicator == 'Deaths'){
      right <- df %>%
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
           subtitle = paste0('Data as of ', as.character(max(df$date)))) 
  })
  
  # Day zero adjustment chart
  output$plot_day_zero <- renderPlot({
    
    # Get countries
    these_countries <- input$country
    if(is.null(these_countries)){
      these_countries <- c('France', 'Italy', 'Spain', 'South Korea', 'Japan')
    }
    
    # Get y scale
    ylog <- input$ylog
    if(is.null(ylog)){ylog <- TRUE}    
    
    # Get day zero definition
    day0 <- input$day0
    if(is.null(day0)){
      day0 <-0
    }
    
    # Get whether cumulative or not
    cumulative <- input$cumulative
    if(is.null(cumulative)){
      cumulative <- TRUE
    }
    
    pd <- df_country %>%
      # mutate(country = ifelse(country != 'Mainland China', 'Other', 'China')) %>%
      arrange(date, country) %>%
      filter(country %in% these_countries) %>%
      group_by(country, date) %>%
      summarise(confirmed_cases = sum(confirmed_cases),
                confirmed_cases_non_cum = sum(confirmed_cases_non_cum)) %>%
      ungroup %>%
      group_by(country) %>%
      mutate(first_case = min(date[confirmed_cases >= day0])) %>%
      ungroup %>%
      mutate(days_since_first_case = date - first_case) %>%
      filter(days_since_first_case >= 0)
    
    
    if(length(these_countries) == 0){
      return(NULL)
    }
    if(length(these_countries) == 1){
      cols <- 'black'
    }
    if(length(these_countries) == 2){
      cols <- c('black', 'red')
    }
    if(length(these_countries) > 2){
      cols <- colorRampPalette(RColorBrewer::brewer.pal(n = 8,
                                                        name = 'Set1'))(length(these_countries))
    }
    
    # Assign which to plot
    if(cumulative){
      pd$value <- pd$confirmed_cases
    } else {
      pd$value <- pd$confirmed_cases_non_cum
    }
    
    g <- ggplot(data = pd,
                aes(x = as.numeric(days_since_first_case),
                    y = value)) +
      geom_line(aes(color = country),  alpha = 1, size = 1) +
      geom_point(aes(color = country), size = 1, alpha = 0.6) +
      theme_bw() +
      scale_color_manual(name = '',
                         values = cols) +
      labs(x = paste0("Days since country's first day with ",
                      day0, " or more cases"),
           y = paste0(ifelse(cumulative, "Cumulative n", "N"), 'umber of confirmed cases',
                      ifelse(ylog, '\n(Logarithmic scale)', '')),
           title = paste0('COVID-19 cases since country\'s\nfirst day with ',
                          day0, " or more ", ifelse(cumulative, "cumulative", "daily"),  " cases"),
           subtitle = paste0('Data as of ', max(df$date))) +
      theme_simple()
    if(ylog){
      g <- g + scale_y_log10()
    }
    return(g)
  })
  
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
      right <- df %>%
        group_by(country) %>%
        filter(date == max(date)) %>%
        summarise(value = sum(confirmed_cases))
    }
    if(indicator == 'Recoveries'){
      right <- df %>%
        group_by(country) %>%
        filter(date == max(date)) %>%
        summarise(value = sum(recovered))
    }
    if(indicator == 'Deaths'){
      right <- df %>%
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