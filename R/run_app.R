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
run_app_network <- function(...) {
  golem::with_golem_options(
    app = network_app(),
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

#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app_mobile_region <- function(...) {
  golem::with_golem_options(
    app = mobile_app_region(),
    golem_opts = list(...)
  )
}

#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app_esp <- function(...) {
  golem::with_golem_options(
    app = mobile_app_esp(),
    golem_opts = list(...)
  )
}