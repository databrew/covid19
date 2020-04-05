#' Prepare day zero data region
#' 
#' Generate a plot with time adjusted for the day at which the outbreak is considered to have started by region
#' @param countries Character vector of country names
#' @param day0 An integer, the number of cumulative cases at which the outbreak is considered to have started
#' @param cumulative Whether to count cases cumulatively
#' @param time_before How many days before outbreak to show
#' @param max_date The maximum date
#' @param deaths Whether to use deaths instead of cases
#' @param pop Adjust by population
#' @param pop_adjustor Deaths per x (default million) when doing population adjustment
#' @param by_district Show by district
#' @param districts Show by districts, default FALSE
#' @param roll How many days to get rolling values for
#' @param roll_fun Mean or Sum
#' @import dplyr
#' @import zoo
#' @export
prepare_day_zero_data_region <-  function(countries = c('US', 'Canada'),
                                   day0 = 50,
                                   cumulative = TRUE,
                                   time_before = 0,
                                   max_date = Sys.Date(),
                                   deaths = FALSE,
                                   pop = FALSE,
                                   pop_adjustor = 1000000,
                                   by_district = TRUE,
                                   districts =  c('New York', 'Ontario'),
                                   roll = 0,
                                   roll_fun = 'mean'){
  
  # If rolling, cannot be cumulative
  if(roll > 0){
    cumulative <- FALSE
  }
  
  if(time_before > 0){
    stop('time_before must be less than or equal to 0')
  }
  
  # Get countries
  these_countries <- countries
  if(is.null(these_countries)){
    these_countries <- c('US', 'Canada')
  }
  
  # Get day zero definition
  if(is.null(day0)){
    day0 <-0
  }
  
  # Get whether cumulative or not
  if(is.null(cumulative)){
    cumulative <- TRUE
  }
  
  # Get values by district or country
  pd <- df_region %>%
    arrange(date, country) %>%
    filter(country %in% these_countries)
  if(by_district){
    if(!is.null(districts)){
      pd <- pd %>% filter(district %in% districts)
    } 
    pd <- pd %>%
      group_by(iso, 
               geo = ifelse(is.na(district), country, district), date) %>%
      summarise(cases = sum(cases),
                cases_non_cum = sum(cases_non_cum),
                deaths = sum(deaths),
                deaths_non_cum = sum(deaths_non_cum)) %>%
      ungroup
    
  } else {
    pd <- pd %>%
      group_by(iso, geo = country, date) %>%
      summarise(cases = sum(cases),
                cases_non_cum = sum(cases_non_cum),
                deaths = sum(deaths),
                deaths_non_cum = sum(deaths_non_cum)) %>%
      ungroup
  }
  
  # Assign which to plot
  if(deaths){
    if(cumulative){
      pd$value <- pd$deaths
    } else {
      pd$value <- pd$deaths_non_cum
    }
  } else {
    if(cumulative){
      pd$value <- pd$cases
    } else {
      pd$value <- pd$cases_non_cum
    }
  }
  
  # If roll, do
  if(roll > 0){
    if(roll_fun == 'mean'){
      # Rolling average
      pd <- pd %>%
        arrange(iso, geo, date) %>%
        group_by(iso, geo) %>%
        mutate(new_value = rollmean(x = value, roll, align = 'right', fill = NA))
    } else {
      # Rolling sum
      pd <- pd %>%
        arrange(iso, geo, date) %>%
        group_by(iso, geo) %>%
        mutate(new_value = rollsum(x = value, roll, align = 'right', fill = NA))
    }
    pd$value <- pd$new_value
  }
  
  # Adjust by population
  if(pop){
    if(by_district){
      # Use district level populations for those we have
      pd_region <- pd %>% filter(iso %in% regions_pop$iso)
      pd_other <- pd %>% filter(!iso %in% regions_pop$iso)
      
      pd_region <- pd_region %>% left_join(regions_pop %>% dplyr::select(ccaa, pop), by = c('geo'='ccaa'))
      pd_other <- pd_other %>% left_join(world_pop)
      pd <- bind_rows(pd_region,
                      pd_other) 
      
    } else {
      pd <- pd %>%
        left_join(world_pop)
    }
    pd <- pd %>%
      mutate(value = (value / pop) * pop_adjustor)
  }
  
  # Deal with day 0 adjustments
  pd <- pd %>%
    group_by(geo) %>%
    mutate(first_day = min(date[value >= day0], na.rm = TRUE)) %>%
    ungroup %>%
    mutate(days_since_first_day = date - first_day) %>%
    filter(days_since_first_day >= time_before)
  
  
  if(length(these_countries) == 0){
    return(NULL)
  } else {
    pd <- pd %>% dplyr::filter(date <= max_date)
  }
  
  # Narrow down
  pd <- pd %>%
    dplyr::select(iso, geo,
                  date, first_day,
                  days_since_first_day,
                  value)
  # Clean up type
  pd$days_since_first_day <- as.integer(pd$days_since_first_day)
  return(pd)
}


#' Plot day zero
#' 
#' Generate a plot with time adjusted for the day at which the outbreak is considered to have started by region
#' @param countries Character vector of country names
#' @param ylog Whether the y-axis should be on log scale
#' @param day0 An integer, the number of cumulative cases at which the outbreak is considered to have started
#' @param cumulative Whether to count cases cumulatively
#' @param time_before How many days before outbreak to show
#' @param add_markets Whether to show lines / circle at outbreak start
#' @param line_size Size of line
#' @param max_date The maximum date
#' @param calendar Whether to plot by calendar date
#' @param point_size Size of points
#' @param point_alpha Alpha of points
#' @param deaths Whether to show deaths instead of cases
#' @param pop Adjust by population
#' @param pop_adjustor Deaths per x (default million) when doing population adjustment
#' @param by_district Show by district
#' @param districts Show by districts, default FALSE
#' @param alpha alpha of lines
#' @param roll How many days to get rolling values for
#' @param roll_fun Mean or Sum
#' @import dplyr
#' @import ggplot2
#' @import RColorBrewer
#' @import scales
#' @export
plot_day_zero_region <- function(countries = c('US', 'Canada'),
                          ylog = TRUE,
                          day0 = 100,
                          cumulative = TRUE,
                          time_before = 0,
                          add_markers = FALSE,
                          line_size = 1.5,
                          point_size = 1.5,
                          point_alpha = 0.9,
                          max_date = Sys.Date(),
                          calendar = FALSE, 
                          deaths = FALSE,
                          pop = FALSE,
                          pop_adjustor = 1000000,
                          by_district = FALSE,
                          districts = c('New York', 'Ontario'),
                          alpha = 0.8,
                          roll = 0,
                          roll_fun = 'mean',
                          color_var = 'geo'){
  options(scipen = '999')
  
  pd <- prepare_day_zero_data(countries = countries,
                              day0 = day0,
                              cumulative = cumulative,
                              time_before = time_before,
                              max_date = max_date,
                              deaths = deaths,
                              pop = pop,
                              pop_adjustor = pop_adjustor,
                              by_district = by_district,
                              districts = districts,
                              roll = roll,
                              roll_fun = roll_fun)
  
  # If rolling, cannot be cumulative
  if(roll > 0){
    cumulative <- FALSE
  }
  
  # Fix geos for those without districts, etc.
  if(any(is.na(pd$geo))){
    pd <- left_join(pd,
                    world_pop %>%
                      dplyr::select(iso, country),
                    by = 'iso') %>%
      mutate(geo = ifelse(is.na(geo),
                          country,
                          geo)) %>%
      filter(!is.na(geo))
  }
  
  
  # Get y scale
  if(is.null(ylog)){ylog <- TRUE}    
  
  
  # Deal with colors
  pd$color_var <- as.character(unlist(pd[,color_var]))
  n_geo <- length(unique(pd$color_var))
  
  
  if(n_geo == 0){
    return(NULL)
  }
  if(n_geo == 1){
    cols <- 'black'
  }
  if(n_geo == 2){
    cols <- c('black', 'red')
  }
  # if(n_geo == 3){
  #   cols <- c('#008080','#b4c8a8','#ca562c')
  # }
  # if(n_geo == 4){
  #   cols <- c('#008080','#b4c8a8','#edbb8a','#de8a5a','#ca562c')
  # }
  # if(n_geo == 5){
  #   cols <- c('#008080','#70a494','#b4c8a8','#edbb8a','#de8a5a','#ca562c')
  # }
  
  if(n_geo > 2){
    # cols <- colorRampPalette(RColorBrewer::brewer.pal(n = 8,
    #                                                   name = 'Dark2'))(n_geo)
    c25 <- c(
      "dodgerblue2", "#E31A1C", # red
      "green4",
      "#6A3D9A", # purple
      "#FF7F00", # orange
      "black", "gold1",
      "skyblue2", "#FB9A99", # lt pink
      "palegreen2",
      "#CAB2D6", # lt purple
      "#FDBF6F", # lt orange
      "gray70", "khaki2",
      "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
      "darkturquoise", "green1", "yellow4", "yellow3",
      "darkorange4", "brown"
    )
    # pie(rep(1, 25), col = c25)
    cols <- colorRampPalette(c25)(n_geo)
  }
  
  selfy <- function(x){abs(x)}
  
  # Define pop_text
  pop_text <- ifelse(pop, paste0('\nper ',
                                 scales::comma(pop_adjustor),
                                 ' population '), '')
  
  # Define whether calendar or not
  if(calendar){
    pd$xvar <- pd$date
  } else {
    pd$xvar <- pd$days_since_first_day
  }
  
  
  
  g <- ggplot(data = pd,
              aes(x = xvar,
                  y = value)) +
    geom_line(aes(color = color_var,
                  group = geo),  alpha = alpha, size = line_size) +
    geom_point(aes(color = color_var,
                   group = geo),  alpha = point_alpha, size = point_size) +
    # geom_point(aes(color = country), size = line_size, alpha = 0.6) +
    theme_bw() +
    scale_color_manual(name = '',
                       values = cols) +
    labs(x = paste0("Days since place's first day with ",
                    day0, " or more ", ifelse(deaths, 'deaths ', 'cases '),
                    pop_text),
         y = paste0(#ifelse(cumulative, "Cumulative n", "N"), 'umber of ', ifelse(deaths, 'deaths', 'cases'),
           #pop_text,
           ifelse(deaths, 'Deaths', 'Cases'),
           ifelse(ylog, '\n(Logarithmic scale)', '')),
         title = paste0('COVID-19 ', ifelse(deaths, 'deaths', 'cases'), ' since place\'s first day with ',
                        day0, " or more ", ifelse(cumulative, "cumulative ", "daily "),  ifelse(deaths, 'deaths ', 'cases '),
                        pop_text, ifelse(roll > 0, 
                                         paste0(' (rolling ', 
                                                ifelse(roll_fun == 'mean', 'average', 'sum'),
                                                ' of ',
                                                roll, ' days)'), '')),
         subtitle = paste0('Data as of ', max(pd$date))) +
    theme_simple() +
    # scale_x_continuous(breaks = seq(-100, 100, 2)) +
    theme(plot.title = element_text(size = 14)) +
    theme(legend.position = 'right')
  if(ylog){
    g <- g + scale_y_log10()
  }
  if(time_before < 0){
    g <- g +
      scale_x_continuous(
        breaks = seq(-1000, 1000, 5),
        sec.axis = sec_axis(~ . + 0,
                            breaks = c(0.5 * time_before, 0.5 * max(as.numeric(pd$days_since_first_day))),
                            labels = c('Before\n"critical mass"',
                                       'After\n"critical mass"'))) 
  }
  if(add_markers & cumulative){
    g <- g + 
      geom_hline(yintercept = day0, lty = 2, alpha = 0.7) +
      geom_vline(xintercept = 0, lty = 2, alpha = 0.7) +
      geom_point(data = tibble(days_since_first_day = 0,
                               cases = day0),
                 aes(x = days_since_first_day,
                     y = cases),
                 color = 'red', 
                 pch = 1,
                 size = 20) 
  }
  return(g)
}
