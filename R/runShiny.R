
#' Run in-built Shiny Apps in the package
#'
#' @param appName In-built Shiny App name.
#'
#' @return Opens up the in-built Shiny App.
#' @export
#'
#' @examples 
#' \dontrun{
#' runShiny("PrAnaViz")
#' }
#'
runShiny <- function(appName) {
  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("shiny-Apps", package = "PRANA"))

  validExamplesMsg <-
    paste0(
      "Valid in-built apps are: '",
      paste(validExamples, collapse = "', '"),
      "'")

  # if an invalid example is given, throw an error
  if (missing(appName) || !nzchar(appName) ||
      !appName %in% validExamples) {
    stop(
      'Please run `runShiny()` with a valid in-built app as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }

  # find and launch the app
  appDir <- system.file("shiny-Apps", appName, package = "PRANA")
  shiny::runApp(appDir, display.mode = "normal")
}
