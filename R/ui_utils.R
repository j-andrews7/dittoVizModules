#' Function to organize inputs into a grid layout
#' @param tag_list A tagList containing UI inputs or a named list 
#'   containing multiple tagLists containing UI inputs.
#' @param title An optional title for the grid, should be a UI element,
#'   e.g. h3("Title").
#' @param tack An optional UI input to tack onto the end of the grid.
#' @param columns Number of columns.
#' @param rows Number of rows.
#'
#' @return A Shiny tagList with inputs organized into a grid, optionally
#'   nested inside a tabsetPanel.
#'
#' @importFrom shiny fluidRow column tagList tabsetPanel tabPanel
#' @export
#' 
#' @author Jared Andrews
#' @examples
#' library(dittoVizModules)
#' # Example 1: Basic usage with a simple grid
#' ui_inputs <- tagList(
#'   textInput("name", "Name"),
#'   numericInput("age", "Age", value = 30),
#'   selectInput("gender", "Gender", choices = c("Male", "Female", "Other"))
#' )
#' organize_inputs(ui_inputs, columns = 2, rows = 2)
#'
#' # Example 2: Using a named list to create tabs
#' ui_inputs_tabs <- list(
#'   Personal = tagList(
#'     textInput("firstname", "First Name"),
#'     textInput("lastname", "Last Name")
#'   ),
#'   Settings = tagList(
#'     checkboxInput("newsletter", "Subscribe to newsletter", value = TRUE),
#'     sliderInput("volume", "Volume", min = 0, max = 100, value = 50)
#'   )
#' )
#' organize_inputs(ui_inputs_tabs)
#'
#' # Example 3: Adding an additional UI element with 'tack'
#' additional_ui <- actionButton("submit", "Submit")
#' organize_inputs(ui_inputs, tack = additional_ui, columns = 3)
#'
#' # Example 4: Handling a case with more inputs than grid cells
#' many_inputs <- tagList(replicate(10, textInput("input", "Input")))
#' organize_inputs(many_inputs, columns = 3) # Creates more than one row
#'
organize_inputs <- function(tag_list, title = NULL, tack = NULL, columns = NULL, rows = NULL) {
  # Check if tag_list is a list of named lists
  if (!is(tag_list, "shiny.tag.list")) {

    # Create a tabsetPanel with a tabPanel for each list element
    out <- do.call(tabsetPanel, c(
      lapply(names(tag_list), function(tab_name) {
        tabPanel(
          tab_name,
          do.call(tagList, organize_inputs(tag_list[[tab_name]], columns = columns, rows = rows))
        )
      }))
    )

  } else {
    n_tags <- length(tag_list)
    
    # Calculate missing dimension based on the provided one and total tags
    if (is.null(columns) & !is.null(rows)) {
      columns <- ceiling(n_tags / rows)
    } else if (is.null(rows) & !is.null(columns)) {
      rows <- ceiling(n_tags / columns)
    } else if (is.null(rows) & is.null(columns)) {
      stop("Either rows or columns must be provided.")
    }
    
    out <- lapply(seq_len(rows), function(r) {
      do.call(fluidRow, lapply(seq_len(columns), function(c) {
        idx <- (r - 1) * columns + c
        if (idx <= n_tags) {
          column(width = 12 / columns, tag_list[[idx]])
        }
      }))
    })
  }

  if (!is.null(tack)) {
    out <- tagList(out, tack)
  }

  if (!is.null(title)) {
    out <- tagList(title, out)
  }

  out
}