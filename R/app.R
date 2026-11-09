#' Launch the Cogitem Shiny application
#'
#' Opens a browser application with four panels: *Generate* (choose shape,
#' rule, number of items, seed and option count; preview and download the
#' item set as `.rds`, a feature CSV, a screen CSV or a PDF booklet), *Browse* (inspect single items with the
#' answer key and design features), *Administer* (present the items one by
#' one, log the chosen option, response time and the hover trace of every
#' option, and download the results as CSV) and *About*.
#'
#' Requires the suggested packages **shiny** and **bslib**. With **callr**
#' installed, generation runs in a background R process: the sidebar shows
#' progress and a *Stop* button that keeps the items produced so far.
#'
#' @param items Optional `cog_item_set` to pre-load into the application.
#' @param ... Passed to [shiny::runApp()], for example `port` or
#'   `launch.browser`.
#' @return Called for its side effect.
#' @examples
#' if (interactive()) {
#'   run_app()
#'   run_app(generate_items("dice", "addition", n = 10, seed = 1))
#' }
#' @export
run_app <- function(items = NULL, ...) {
  for (pkg in c("shiny", "bslib")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Package '%s' is required for the app: install.packages('%s')", pkg, pkg),
           call. = FALSE)
    }
  }
  app_dir <- system.file("app", package = "cogitem")
  if (app_dir == "") stop("Could not find the app directory; reinstall cogitem.", call. = FALSE)
  if (!is.null(items)) {
    stopifnot(inherits(items, "cog_item_set"))
    old <- options(cogitem.preloaded = items)
    on.exit(options(old), add = TRUE)
  }
  shiny::runApp(app_dir, ...)
}
