#' Prepare day zero data
#' 
#' Generate a plot with time adjusted for the day at which the outbreak is considered to have started
#' @param countries Character vector of country names
#' @param day0 An integer, the number of cumulative cases at which the outbreak is considered to have started
#' @param cumulative Whether to count cases cumulatively
#' @param time_before How many days before outbreak to show
#' @param max_date The maximum date
#' @param deaths Whether to use deaths instead of cases
#' @param pop Adjust by population
#' @import dplyr
#' @export
prepare_day_zero_data <-  function(countries = c('Italy', 'Spain', 'France', 'US', 'Germany'),
                                   day0 = 150,
                                   cumulative = TRUE,
                                   time_before = 0,
                                   max_date = Sys.Date(),
                                   deaths = FALSE,
                                   pop = FALSE){
  
  
  
  if(time_before > 0){
    stop('time_before must be less than or equal to 0')
  }
  
  # Get countries
  these_countries <- countries
  if(is.null(these_countries)){
    these_countries <- c('France', 'Italy', 'Spain', 'South Korea', 'Japan')
  }

  # Get day zero definition
  if(is.null(day0)){
    day0 <-0
  }
  
  # Get whether cumulative or not
  if(is.null(cumulative)){
    cumulative <- TRUE
  }
  
  pd <- df %>%
    mutate(country) %>%
    arrange(date, country) %>%
    filter(country %in% these_countries) %>%
    group_by(country, date) %>%
    summarise(confirmed_cases = sum(confirmed_cases),
              confirmed_cases_non_cum = sum(confirmed_cases_non_cum),
              deaths = sum(deaths),
              deaths_non_cum = sum(deaths_non_cum)) %>%
    ungroup
  
  if(deaths){
    pd <- pd %>%
      group_by(country) %>%
      mutate(first_case = min(date[deaths >= day0])) %>%
      ungroup %>%
      mutate(days_since_first_case = date - first_case) %>%
      filter(days_since_first_case >= time_before)
    
  } else {
    pd <- pd %>%
      group_by(country) %>%
      mutate(first_case = min(date[confirmed_cases >= day0])) %>%
      ungroup %>%
      mutate(days_since_first_case = date - first_case) %>%
      filter(days_since_first_case >= time_before)
  }
  
  if(length(these_countries) == 0){
    return(NULL)
  } else {
    pd <- pd %>% dplyr::filter(date <= max_date)
    
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
      pd$value <- pd$confirmed_cases
    } else {
      pd$value <- pd$confirmed_cases_non_cum
    }
  }
  
  # Adjust by population
  if(pop){
   pd <- pd %>%
     left_join(world_pop) %>%
     mutate(value = (value / pop) * 100000)
  }
  return(pd)
}

#' Prepare day zero data for Spain
#' 
#' Generate a plot with time adjusted for the day at which the outbreak is considered to have started
#' @param ccaa Character vector of country names
#' @param deaths Whether to show deaths rather than cases
#' @param day0 An integer, the number of cumulative cases at which the outbreak is considered to have started
#' @param cumulative Whether to count cases cumulatively
#' @param time_before How many days before outbreak to show
#' @param max_date The maximum date
#' @param pop Adjust by population
#' @import dplyr
#' @export
prepare_day_zero_data_esp <-  function(ccaa = c('Cataluña', 'Madrid'),
                                       deaths = FALSE,
                                   day0 = 150,
                                   cumulative = TRUE,
                                   time_before = 0,
                                   max_date = Sys.Date(),
                                   pop = FALSE){
  
  
  if(time_before > 0){
    stop('time_before must be less than or equal to 0')
  }
  
  # Get countries
  these_countries <- ccaa
  if(is.null(these_countries)){
    these_countries <- c('Cataluña', 'Madrid')
  }
  
  # Get day zero definition
  if(is.null(day0)){
    day0 <-0
  }
  
  # Get whether cumulative or not
  if(is.null(cumulative)){
    cumulative <- TRUE
  }
  
  pd <- esp_df %>%
    mutate(country = ccaa) %>%
    arrange(date, country) %>%
    filter(country %in% these_countries) %>%
    group_by(country, date) %>%
    summarise(confirmed_cases = sum(confirmed_cases),
              confirmed_cases_non_cum = sum(confirmed_cases_non_cum),
              deaths = sum(deaths),
              deaths_non_cum = sum(deaths_non_cum)) %>%
    ungroup
  
  if(deaths){
    pd <- pd %>%
      group_by(country) %>%
      mutate(first_case = min(date[deaths >= day0])) %>%
      ungroup %>%
      mutate(days_since_first_case = date - first_case) %>%
      filter(days_since_first_case >= time_before)
      
  } else {
    pd <- pd %>%
      group_by(country) %>%
      mutate(first_case = min(date[confirmed_cases >= day0])) %>%
      ungroup %>%
      mutate(days_since_first_case = date - first_case) %>%
      filter(days_since_first_case >= time_before)
  }
    
  
  
  if(length(these_countries) == 0){
    return(NULL)
  } else {
    pd <- pd %>% dplyr::filter(date <= max_date)
    
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
      pd$value <- pd$confirmed_cases
    } else {
      pd$value <- pd$confirmed_cases_non_cum
    }
  }
  
  # Adjust by population
  if(pop){
    pd <- pd %>%
      left_join(esp_pop %>%
                  dplyr::rename(country = ccaa)) %>%
      mutate(value = (value / pop) * 100000)
  }
  
  return(pd)
}




#' Plot day zero
#' 
#' Generate a plot with time adjusted for the day at which the outbreak is considered to have started
#' @param countries Character vector of country names
#' @param ylog Whether the y-axis should be on log scale
#' @param day0 An integer, the number of cumulative cases at which the outbreak is considered to have started
#' @param cumulative Whether to count cases cumulatively
#' @param time_before How many days before outbreak to show
#' @param add_markets Whether to show lines / circle at outbreak start
#' @param line_size Size of line
#' @param max_date The maximum date
#' @param calendar Whether to plot by calendar date
#' @param deaths Whether to show deaths instead of cases
#' @param pop Adjust by population
#' @import dplyr
#' @import ggplot2
#' @import RColorBrewer
#' @export
plot_day_zero <- function(countries = c('Italy', 'Spain', 'France', 'US', 'Germany'),
                          ylog = TRUE,
                          day0 = 150,
                          cumulative = TRUE,
                          time_before = 0,
                          add_markers = FALSE,
                          line_size = 1.5,
                          max_date = Sys.Date(),
                          calendar = FALSE, deaths = FALSE,
                          pop = FALSE){
  
  pd <- prepare_day_zero_data(countries = countries,
                              day0 = day0,
                              cumulative = cumulative,
                              time_before = time_before,
                              max_date = max_date,
                              deaths = deaths,
                              pop = pop)
  these_countries <- countries
  
 
  # Get y scale
  if(is.null(ylog)){ylog <- TRUE}    
  
  
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
  
  selfy <- function(x){abs(x)}
  
  # Define pop_text
  pop_text <- ifelse(pop, ' per 100,000 population. ', '')
  
  g <- ggplot(data = pd,
              aes(x = as.numeric(days_since_first_case),
                  y = value)) +
    geom_line(aes(color = country),  alpha = 0.85, size = line_size) +
    # geom_point(aes(color = country), size = line_size, alpha = 0.6) +
    theme_bw() +
    scale_color_manual(name = '',
                       values = cols) +
    labs(x = paste0("Days since country's first day with ",
                    day0, " or more ", ifelse(deaths, 'deaths', 'cases')),
         y = paste0(ifelse(cumulative, "Cumulative n", "N"), 'umber of ', ifelse(deaths, 'deaths', 'cases'),
                    pop_text,
                    ifelse(ylog, '\n(Logarithmic scale)', '')),
         title = paste0('COVID-19 ', ifelse(deaths, 'deaths', 'cases'), ' since country\'s\nfirst day with ',
                        day0, " or more ", ifelse(cumulative, "cumulative ", "daily "),  ifelse(deaths, 'deaths', 'cases')),
         subtitle = paste0(pop_text, 'Data as of ', max(df_country$date))) +
    theme_simple() +
    scale_x_continuous(breaks = seq(-100, 100, 2)) +
    theme(plot.title = element_text(size = 14)) +
    theme(legend.position = 'top')
  if(ylog){
    g <- g + scale_y_log10()
  }
  if(time_before < 0){
    g <- g +
      scale_x_continuous(
        breaks = seq(-1000, 1000, 5),
        sec.axis = sec_axis(~ . + 0,
                                                      breaks = c(0.5 * time_before, 0.5 * max(as.numeric(pd$days_since_first_case))),
                                                      labels = c('Before\n"critical mass"',
                                                                 'After\n"critical mass"'))) 
  }
  if(add_markers & cumulative){
    g <- g + 
      geom_hline(yintercept = day0, lty = 2, alpha = 0.7) +
      geom_vline(xintercept = 0, lty = 2, alpha = 0.7) +
      geom_point(data = tibble(days_since_first_case = 0,
                               confirmed_cases = day0),
                 aes(x = days_since_first_case,
                     y = confirmed_cases),
                 color = 'red', 
                 pch = 1,
                 size = 20) 
  }
  return(g)
}



#' Plot day zero for Spain
#' 
#' Generate a plot with time adjusted for the day at which the outbreak is considered to have started
#' @param ccaa Character vector of ccaa names
#' @param deaths Whether to show deaths rather than cases
#' @param ylog Whether the y-axis should be on log scale
#' @param day0 An integer, the number of cumulative cases at which the outbreak is considered to have started
#' @param cumulative Whether to count cases cumulatively
#' @param time_before How many days before outbreak to show
#' @param add_markets Whether to show lines / circle at outbreak start
#' @param line_size Size of line
#' @param max_date The maximum date
#' @param pop Adjust for population
#' @import dplyr
#' @import ggplot2
#' @import RColorBrewer
#' @export
plot_day_zero_esp <- function(ccaa = c('Cataluña', 'Madrid'),
                              deaths = FALSE,
                          ylog = TRUE,
                          day0 = 150,
                          cumulative = TRUE,
                          time_before = 0,
                          add_markers = FALSE,
                          line_size = 1.5,
                          max_date = Sys.Date(),
                          pop = FALSE){
  
  pd <- prepare_day_zero_data_esp(ccaa = ccaa,
                                  deaths = deaths,
                              day0 = day0,
                              cumulative = cumulative,
                              time_before = time_before,
                              max_date = max_date,
                              pop = pop)
  these_countries <- sort(unique(pd$country))
  
  
  # Get y scale
  if(is.null(ylog)){ylog <- TRUE}    
  
  
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
  
  selfy <- function(x){abs(x)}
  
  
  # Define pop_text
  pop_text <- ifelse(pop, ' per 100,000 population. ', '')
  
  g <- ggplot(data = pd,
              aes(x = as.numeric(days_since_first_case),
                  y = value)) +
    geom_line(aes(color = country),  alpha = 0.85, size = line_size) +
    geom_point(aes(color = country), size = line_size, alpha = 0.6) +
    theme_bw() +
    scale_color_manual(name = '',
                       values = cols) +
    labs(x = paste0("Days since region's first day with ",
                    day0, " or more ", ifelse(deaths, 'deaths', 'cases')),
         y = paste0(ifelse(cumulative, "Cumulative n", "N"), 'umber of ', 
                    ifelse(deaths, 'deaths', 'cases'),
                    ' ', pop_text, 
                    ifelse(ylog, '\n(Logarithmic scale)', '')),
         title = paste0('COVID-19 cases since region\'s\nfirst day with ',
                        day0, " or more ", ifelse(cumulative, "cumulative ", "daily "),  ifelse(deaths,
                                                                                              ' deaths',
                                                                                              'cases')),
         subtitle = paste0(pop_text, 'Data as of ', max(esp_df$date))) +
    theme_simple() +
    scale_x_continuous(breaks = seq(-100, 100, 2)) +
    theme(plot.title = element_text(size = 14)) +
    theme(legend.position = 'top')
  if(ylog){
    g <- g + scale_y_log10()
  }
  if(time_before < 0){
    g <- g +
      scale_x_continuous(
        breaks = seq(-1000, 1000, 5),
        sec.axis = sec_axis(~ . + 0,
                            breaks = c(0.5 * time_before, 0.5 * max(as.numeric(pd$days_since_first_case))),
                            labels = c('Before\n"critical mass"',
                                       'After\n"critical mass"'))) 
  }
  if(add_markers & cumulative){
    g <- g + 
      geom_hline(yintercept = day0, lty = 2, alpha = 0.7) +
      geom_vline(xintercept = 0, lty = 2, alpha = 0.7) +
      geom_point(data = tibble(days_since_first_case = 0,
                               confirmed_cases = day0),
                 aes(x = days_since_first_case,
                     y = confirmed_cases),
                 color = 'red', 
                 pch = 1,
                 size = 20) 
  }
  return(g)
}
