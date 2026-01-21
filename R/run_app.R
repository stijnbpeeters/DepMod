#' Run the Shiny app
#'
#' Launches the Shiny app included in this package.
#' @export
#' 
#' #' @examples
#' \dontrun{
#' run_app()
#' }
#' 
run_app <- function() {
  app_dir <- system.file("app", package = utils::packageName())
  if (!nzchar(app_dir)) {
    stop("App directory not found. Was the package installed correctly?",
         call. = FALSE)
  }
  shiny::shinyAppDir(appDir = app_dir)
}
