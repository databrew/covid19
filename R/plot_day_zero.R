#' Prepare day zero data
#' 
#' Generate a plot with time adjusted for the day at which the outbreak is considered to have started
#' @param countries Character vector of country names
#' @param day0 An integer, the number of cumulative cases at which the outbreak is considered to have started
#' @param cumulative Whether to count cases cumulatively
#' @param time_before How many days before outbreak to show
#' @import dplyr
#' @export
prepare_day_zero_data <-  function(countries = c('Italy', 'Spain', 'France', 'US', 'Germany'),
                                   day0 = 150,
                                   cumulative = TRUE,
                                   time_before = 0){
  
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
  
  pd <- df_country %>%
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
    filter(days_since_first_case >= time_before)
  
  
  if(length(these_countries) == 0){
    return(NULL)
  }
 
  # Assign which to plot
  if(cumulative){
    pd$value <- pd$confirmed_cases
  } else {
    pd$value <- pd$confirmed_cases_non_cum
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
                          line_size = 1.5){
  
  pd <- prepare_day_zero_data(countries = countries,
                              day0 = day0,
                              cumulative = cumulative,
                              time_before = time_before)
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
  
  g <- ggplot(data = pd,
              aes(x = as.numeric(days_since_first_case),
                  y = value)) +
    geom_line(aes(color = country),  alpha = 0.85, size = line_size) +
    geom_point(aes(color = country), size = line_size, alpha = 0.6) +
    theme_bw() +
    scale_color_manual(name = '',
                       values = cols) +
    labs(x = paste0("Days since country's first day with ",
                    day0, " or more cases"),
         y = paste0(ifelse(cumulative, "Cumulative n", "N"), 'umber of confirmed cases',
                    ifelse(ylog, '\n(Logarithmic scale)', '')),
         title = paste0('COVID-19 cases since country\'s\nfirst day with ',
                        day0, " or more ", ifelse(cumulative, "cumulative", "daily"),  " cases"),
         subtitle = paste0('Data as of ', max(df_country$date))) +
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
