# Libraries
library(igraph)
library(nd3) # devtools::install_github('databrew/nd3')
library(dplyr)

# Define parameters
n <- 100
contacts <- 1
ids <- 1:n

# Create dataset
data_list <- list()
counter <- 0
for(i in ids){
  this_id <- ids[i]
  this_from <- this_id
  n_contacts <- round(sample(rnorm(n = 1000, mean = contacts, sd = 1), size = 1, replace = FALSE))
  if(n_contacts > 0){
    counter <- counter + 1
    other_ids <- ids[ids != this_id]
    this_to <- sample(other_ids, size = n_contacts, replace = FALSE)
    this_out <- tibble(from = this_from,
                       to = this_to)
    data_list[[counter]] <- this_out
  }
}
data <- bind_rows(data_list)

# Plot
p <- simpleNetwork(data, height="100px", width="100px",
                   Source = 1,                 # column number of source
                   Target = 2,                 # column number of target
                   linkDistance = 10,          # distance between node. Increase this value to have more space between nodes
                   charge = -10,                # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
                   fontSize = 12,               # size of the node names
                   fontFamily = "arial",       # font og node names
                   linkColour = "#666",        # colour of edges, MUST be a common colour for the whole graph
                   nodeColour = "#69b3a2",     # colour of nodes, MUST be a common colour for the whole graph
                   opacity = 0.9,              # opacity of nodes. 0=transparent. 1=no transparency
                   zoom = T  )                  # Can you zoom on the figure?)
p
# p

# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/networkInteractive1.html"))