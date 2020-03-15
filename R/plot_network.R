#' Plot interactive network
#' 
#' Generate a plot of network connections
#' @param n Number of units (families, for example)
#' @param contacts Average number of contacts between families
#' @param sd Standard deviation in number of contacts
#' @param sick Percent sick
#' @import dplyr
#' @import nd3
#' @import dplyr
#' @import htmlwidgets
#' @export
plot_network <- function(n = 100,
                         contacts = 1,
                         sd = 1,
                         save = NULL,
                         sick = 1){
  
  # Define helper function for sampling with a 0 floor
  rtruncnorm <- function(N, mean = 0, sd = 1, a = -Inf, b = Inf) {
    if (a > b) stop('Error: Truncation range is empty');
    U <- runif(N, pnorm(a, mean, sd), pnorm(b, mean, sd));
    qnorm(U, mean, sd); }
  
  # Define vector of last names
  last_names <- sample(surnames, size = n,
                       replace = TRUE)
  
  # Vectors
  ids <- 1:n
  ids_done <- c()
  
  # Pre-assign number of contacts
  # Need to improve, loses accuracy on lower numbers
  if(contacts > 0){
    id_n_contacts <- round(sample(
      # rnorm(n = 10000, mean = contacts, sd = sd), size = n, replace = TRUE)
      rtruncnorm(N = 10000, 
                 mean = ifelse(contacts < 1.5 & contacts > 0.5,
                               contacts - 0.3,
                               contacts), 
                 sd = sd, a = 0, b = 1000), 
      size = n,
      replace = TRUE)
    )
  } else {
    id_n_contacts <- rep(0, length(n))
  }
  
  # Create dataset
  data_list <- list()
  counter <- 0
  data <- tibble(from = ids,
                 to = NA,
                 n = id_n_contacts)
  
  # Pre-assign to "done" all those ids with 0 contacts
  ids_done <- c(ids_done, data$from[data$n < 1])
  
  for(i in 1:nrow(data)){
    message(i)
    this_id <- this_from <-  data$from[i]
    n_contacts <- data$n[i]
    # only keep going if this contact has not already been used up
    # and has any contacts at all
    if((!this_id %in% ids_done) & 
       (n_contacts > 0)){
      # Mark this id as having been done
      ids_done <- c(ids_done, this_id)
      available_ids <- ids[!ids %in% ids_done] #these are already maxed out on their contacts
      if(length(available_ids) >= n_contacts){
        counter <- counter + 1
        this_to <- sample(available_ids, size = n_contacts, replace = FALSE)
        this_out <- tibble(from = this_from,
                           to = this_to,
                           n = n_contacts)
        data_list[[counter]] <- this_out
      }
    }
  }
  done <- bind_rows(data_list)
  # # Add in the non-connections to (nodes only)
  # done <- bind_rows(done, data)
  # done <- done %>% arrange(from)
  # Zero index
  ids <- ids -1
  done$from <- done$from -1
  done$to <- done$to -1
  
  # Define whether sick
  n_sick <- round((0.01 * sick) * length(ids))
  sick_ids <- sample(ids, size = n_sick, replace = FALSE)
  
  # Make a nodes dataframe
  nodes <-tibble(name =last_names,
                 group = ifelse(ids %in% sick_ids,
                                'Infected (primary)',
                                'Safe')) %>%
    mutate(size = 1)
  
  # Make a links dataframe
  if(nrow(done) > 0){
    links <- done %>% dplyr::rename(a = from, b = to, value = n) 
    
    # Define secondary sicks
    ss_ids <- sick_ids
    secondary_sicks <- done %>% filter(from %in% sick_ids | to %in% sick_ids) 
    secondary_sick_ids <- sort(unique(c(secondary_sicks$from, secondary_sicks$to))) 
    secondary_sick_ids <- sort(unique(secondary_sick_ids))
    add_these <- secondary_sick_ids[!secondary_sick_ids %in% ss_ids]
    ss <- length(add_these)
    ss_ids <- c(ss_ids, add_these)
    while(ss > 0){
      secondary_sicks <- done %>% filter(from %in% add_these | to %in% add_these) 
      secondary_sick_ids <- sort(unique(c(secondary_sicks$from, secondary_sicks$to))) 
      add_these <- secondary_sick_ids[!secondary_sick_ids %in% ss_ids]
      ss <- length(add_these)
      ss_ids <- c(ss_ids, add_these)
    }
    ss_ids <- sort(unique(ss_ids))
    ss_ids <- ss_ids[!ss_ids %in% sick_ids]  
    nodes$group <- ifelse(ids %in% ss_ids,
                          'At risk',
                          nodes$group)
     
  } else {
    links <- tibble(a = 0, b = 0, value = 0)
  }
  
  # arrange for coloring purposes
  nodes <- nodes %>% arrange(group)
  
  # Define color scale
  ColourScale <- 'd3.scaleOrdinal()
            .domain(["Infected (primary)", "At risk", "Safe"])
           .range(["#8B0000", "#FFA500", "#0000FF"]);'

  p <- forceNetwork(Links = links, 
               Nodes = nodes,
               Value = 'value',
               legend = TRUE,
               NodeID = "name", Group = "group",
               Nodesize="size",                                                    # column names that gives the size of nodes
               radiusCalculation = JS(" d.nodesize^2+5"),                         # How to use this column to calculate radius of nodes? (Java script expression)
               opacity = 1,                                                      # Opacity of nodes when you hover it
               opacityNoHover = 0.8,     
               colourScale = JS(ColourScale),
               # Opacity of nodes you do not hover
               # colourScale = JS("d3.scaleOrdinal(d3.schemeCategory10);"),          # Javascript expression, schemeCategory10 and schemeCategory20 work
               fontSize = 10,                                                      # Font size of labels
               # fontFamily = "serif",                                               # Font family for labels
               
               # custom edges
               # Value="my_width",
               arrows = FALSE,                                                     # Add arrows?
               # linkColour = c("grey","orange"),                                    # colour of edges
               linkWidth = 0.2, #"function(d) { return (d.value^5)*0.4}",
               
               # layout
               linkDistance = 150,                                                 # link size, if higher, more space between nodes
               charge = -5,                                                       # if highly negative, more space betqeen nodes
               
               # -- general parameters
               height = NULL,                                                      # height of frame area in pixels
               width = NULL,
               zoom = TRUE,                                                        # Can you zoom on the figure
               # legend = TRUE,                                                      # add a legend?
               bounded = TRUE#, 
               # clickAction = 'Shiny.onInputChange("id", d.name)'
               )
  
              # Can you zoom on the figure?)
  if(!is.null(save)){
    saveWidget(p, file=paste0(save))
  }
  p
}
