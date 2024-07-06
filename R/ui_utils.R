#' Organize arbitrary Shiny inputs into a grid layout
#' @param tag.list A tagList containing UI inputs or a named list
#'   containing multiple tagLists containing UI inputs.
#' @param title An optional title for the grid, should be a UI element,
#'   e.g. h3("Title").
#' @param tack An optional UI input to tack onto the end of the grid.
#' @param columns Number of columns.
#' @param rows Number of rows.
#' @param id An optional ID for the tabsetPanel if a named list is provided.
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
#' ui.inputs <- tagList(
#'     textInput("name", "Name"),
#'     numericInput("age", "Age", value = 30),
#'     selectInput("gender", "Gender", choices = c("Male", "Female", "Other"))
#' )
#' organize_inputs(ui.inputs, columns = 2, rows = 2)
#'
#' # Example 2: Using a named list to create tabs
#' ui.inputs.tabs <- list(
#'     Personal = tagList(
#'         textInput("firstname", "First Name"),
#'         textInput("lastname", "Last Name")
#'     ),
#'     Settings = tagList(
#'         checkboxInput("newsletter", "Subscribe to newsletter", value = TRUE),
#'         sliderInput("volume", "Volume", min = 0, max = 100, value = 50)
#'     )
#' )
#' organize_inputs(ui.inputs.tabs)
#'
#' # Example 3: Adding an additional UI element with 'tack'
#' additional.ui <- actionButton("submit", "Submit")
#' organize_inputs(ui.inputs, tack = additional.ui, columns = 3)
#'
#' # Example 4: Handling a case with more inputs than grid cells
#' many.inputs <- tagList(replicate(10, textInput("input", "Input")))
#' organize_inputs(many.inputs, columns = 3) # Creates more than one row
#'
organize_inputs <- function(
    tag.list,
    id = NULL,
    title = NULL,
    tack = NULL,
    columns = NULL,
    rows = NULL) {
    # Check if tag.list is a list of named lists
    if (!is(tag.list, "shiny.tag.list")) {
        # Create a tabsetPanel with a tabPanel for each list element
        tabs <- c(
            lapply(names(tag.list), function(tab.name) {
                tabPanel(
                    tab.name,
                    do.call(tagList, organize_inputs(tag.list[[tab.name]], columns = columns, rows = rows))
                )
            })
        )

        if (!is.null(id)) {
            tabs[["id"]] <- id
        }

        out <- do.call(tabsetPanel, tabs)

    } else {
        n.tags <- length(tag.list)

        # Calculate missing dimension based on the provided one and total tags
        if (is.null(columns) & !is.null(rows)) {
            columns <- ceiling(n.tags / rows)
        } else if (is.null(rows) & !is.null(columns)) {
            rows <- ceiling(n.tags / columns)
        } else if (is.null(rows) & is.null(columns)) {
            stop("Either rows or columns must be provided.")
        }

        out <- lapply(seq_len(rows), function(r) {
            do.call(fluidRow, lapply(seq_len(columns), function(c) {
                idx <- (r - 1) * columns + c
                if (idx <= n.tags) {
                    column(width = 12 / columns, tag.list[[idx]])
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

#' Color palette options for palettePicker
#' @importFrom scales hue_pal viridis_pal brewer_pal
#' @importFrom dittoViz dittoColors
#' @export
#' @author Jared Andrews, based largely on [esquisse::default_pals()]
default_palettes <- function() {
  pals <- list(
    choices = list(
      Defaults = list(
        "dittoColors" = dittoColors()[1:8],
        "ggplot2" = hue_pal()(9)
      ),
      Viridis = list(
        "viridis" = viridis_pal(option = "viridis")(10),
        "magma" = viridis_pal(option = "magma")(10),
        "inferno" = viridis_pal(option = "inferno")(10),
        "plasma" = viridis_pal(option = "plasma")(10),
        "cividis" = viridis_pal(option = "cividis")(10)
      ),
      Diverging = list(
        "BrBG" = brewer_pal(palette = "BrBG")(11), 
        "PiYG" = brewer_pal(palette = "PiYG")(11), 
        "PRGn" = brewer_pal(palette = "PRGn")(11), 
        "PuOr" = brewer_pal(palette = "PuOr")(11), 
        "RdBu" = brewer_pal(palette = "RdBu")(11), 
        "RdGy" = brewer_pal(palette = "RdGy")(11), 
        "RdYlBu" = brewer_pal(palette = "RdYlBu")(11), 
        "RdYlGn" = brewer_pal(palette = "RdYlGn")(11), 
        "Spectral" = brewer_pal(palette = "Spectral")(11)
      ), 
      Qualitative = list(
        "Accent" = brewer_pal(palette = "Accent")(8),
        "Dark2" = brewer_pal(palette = "Dark2")(8), 
        "Paired" = brewer_pal(palette = "Paired")(12), 
        "Pastel1" = brewer_pal(palette = "Pastel1")(9), 
        "Pastel2" = brewer_pal(palette = "Pastel2")(8), 
        "Set1" = brewer_pal(palette = "Set1")(8), 
        "Set2" = brewer_pal(palette = "Set2")(8), 
        "Set3" = brewer_pal(palette = "Set3")(12)
      ),
      Sequential = list(
        "Blues" = brewer_pal(palette = "Blues")(9),
        "BuGn" = brewer_pal(palette = "BuGn")(9),
        "BuPu" = brewer_pal(palette = "BuPu")(9), 
        "GnBu" = brewer_pal(palette = "GnBu")(9), 
        "Greens" = brewer_pal(palette = "Greens")(9), 
        "Greys" = brewer_pal(palette = "Greys")(9), 
        "Oranges" = brewer_pal(palette = "Oranges")(9), 
        "OrRd" = brewer_pal(palette = "OrRd")(9), 
        "PuBu" = brewer_pal(palette = "PuBu")(9), 
        "PuBuGn" = brewer_pal(palette = "PuBuGn")(9), 
        "PuRd" = brewer_pal(palette = "PuRd")(9), 
        "Purples" = brewer_pal(palette = "Purples")(9), 
        "RdPu" = brewer_pal(palette = "RdPu")(9), 
        "Reds" = brewer_pal(palette = "Reds")(9), 
        "YlGn" = brewer_pal(palette = "YlGn")(9), 
        "YlGnBu" = brewer_pal(palette = "YlGnBu")(9), 
        "YlOrBr" = brewer_pal(palette = "YlOrBr")(9), 
        "YlOrRd" = brewer_pal(palette = "YlOrRd")(9)
      )
    ), 
    textColor = c(
      rep(c("white", "black"), times = c(24, 18))
    )
  )

  pals
}