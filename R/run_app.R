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
