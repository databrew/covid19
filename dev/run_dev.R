  # Set options here
  options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode
  
  # Detach all loaded packages and clean your environment
  golem::detach_all_attached()
  # rm(list=ls(all.names = TRUE))
  
  # Document and reload your package
  # remove.packages('covid19')
  # devtools::install() # if underlying changes to system files
  golem::document_and_reload()
  
  # Re-capture fresh data from JHU
  # (Set to true when you want to fetch fresh data, but generally keep to false in 
  # order to avoid over-pinging the server)
  refresh <- F
  if(refresh){
    source('data-raw/update_data.R', chdir = T)
  }
  
  # Run the application
  run_app()
  
