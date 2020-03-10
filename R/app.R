#' @import shiny
#' @import shinydashboard
#' @import leaflet
#' @import shiny
#' @import ggplot2

##################################################
# UI
##################################################
app_ui <- function() {
  options(scipen = '999')
  
  #############################
  # HEADER
  header <- dashboardHeader(title = tags$a(tags$img(src='www/logo.png', alt = 'Databrew')))
  #############################
  
  #############################
  # SIDEBAR
  #############################
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem(
        text="Main",
        tabName="main"
      ),
      menuItem(
        text = 'About',
        tabName = 'about'
      )
    )
  )
  
  #############################
  # BODY
  #############################
  body <- dashboardBody(
    golem_add_external_resources(),
    tabItems(
      tabItem(
        tabName="main",
        navbarPage(title = '',
                   id = 'navs',
                   footer = includeHTML("footer.html"),
                   tabPanel('World',
                            fluidPage(
                              fluidRow(
                                shinydashboard::box(width = 6,
                                                    title = 'World map',
                                                    leafletOutput('leafy'),
                                                    selectInput('map_type',
                                                                'Map type',
                                                                choices = c('Polygons (choropleth)',
                                                                            # 'One point per person (jittering)',
                                                                            'Points (radius)')),
                                                    selectInput('indicator',
                                                                'Indicator',
                                                                choices = c('Confirmed cases',
                                                                            'Recoveries',
                                                                            'Deaths'))),
                                shinydashboard::box(width = 6,
                                                    title = 'Day-zero comparison plot',
                                                    plotOutput('plot_day_zero'),
                                                    selectInput('country', 'Country',
                                                                multiple = TRUE,
                                                                choices = sort(unique(sort(unique(covid19::df$country)))),
                                                                selected = c('Italy', 'Spain', 'France', 'US')),
                                                    checkboxInput('ylog', 'Logarithmic y-axis?',
                                                                  value = TRUE),
                                                    sliderInput('day0', 'Minimum number of cases to be considered "day 0"',
                                                                min = 1,
                                                                max = 500,
                                                                value = 1,
                                                                step = 1))),
                              fluidRow(
                                shinydashboard::box(width = 12,
                                                    plotOutput('plot_overall')))
                            )
                   )#,
        # tabPanel('Contagion Simulator'),
        # tabPanel('COVID-19 online'),
        # tabPanel('Economic impact'),
        # navbarMenu('Comparison to other diseases',
        #            tabPanel('Contagion'),
        #            tabPanel('Mortality'))
        ),
      mod_social_ui("social_module_1")
    ),
    tabItem(
      tabName = 'about',
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
      )
    )
  )
  )

#############################
# UI COMBINATION
#############################
tagList(
  golem_add_external_resources(),
  # UI
  dashboardPage(
    header = header,
    sidebar = sidebar,
    body = body, 
    skin="blue", title = 'databrew')
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
    
    pd <- df %>%
      # mutate(country = ifelse(country != 'Mainland China', 'Other', 'China')) %>%
      arrange(date, country) %>%
      filter(country %in% these_countries) %>%
      group_by(country, date) %>%
      summarise(confirmed_cases = sum(confirmed_cases)) %>%
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
    
    g <- ggplot(data = pd,
                aes(x = as.numeric(days_since_first_case),
                    y = confirmed_cases)) +
      geom_line(aes(color = country),  alpha = 1, size = 1) +
      geom_point(aes(color = country), size = 3, alpha = 0.6) +
      theme_bw() +
      scale_color_manual(name = '',
                         values = cols) +
      labs(x = paste0("Days since country's first day with ",
                      day0, " or more cases"),
           y = paste0('Cumulative number of confirmed cases',
                      ifelse(ylog, '\n(Logarithmic scale)', '')),
           title = paste0('COVID-19 cases since country\'s\nfirst day with ',
                          day0, " or more cases"),
           subtitle = paste0('Data as of ', max(df$date)),
           caption = 'Raw data from Johns Hopkins University: https://github.com/CSSEGISandData/COVID-19\nData processing and visualization: Databrew LLC | www.databrew.cc') +
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


##################################################
# UTILITIES
##################################################
golem_add_external_resources <- function(){
  addResourcePath(
    'www', system.file('app/www', package = 'covid19')
  )
  
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
    # tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}


app <- function(){
  shinyApp(ui = app_ui,
           server = app_server)
}