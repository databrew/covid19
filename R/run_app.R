#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(...) {
  golem::with_golem_options(
    app = app(),
    golem_opts = list(...)
  )
}


#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app_mini <- function(...) {
  golem::with_golem_options(
    app = mini_app(),
    golem_opts = list(...)
  )
}

#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app_mobile <- function(...) {
  golem::with_golem_options(
    app = mobile_app(),
    golem_opts = list(...)
  )
}

