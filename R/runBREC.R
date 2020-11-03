#' @export

runBREC <- function() {
    appDir <- system.file("shinyApp", "BREC_app", package = "Brec")
    if (appDir == "") {
        stop("Could not find the package directory. Try re-installing `Brec`.", call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal")
}
