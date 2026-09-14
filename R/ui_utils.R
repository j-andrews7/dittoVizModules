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
#' @import shiny
#' @importFrom methods is
#' @export
#'
#' @author Jared Andrews
#' @examples
#' library(VizModules)
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
#' organize_inputs(ui.inputs.tabs, columns = 1)
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
  rows = NULL
) {
    # Check if tag.list is a list of named lists
    if (!is(tag.list, "shiny.tag.list")) {
        # Create a tabsetPanel with a tabPanel for each list element
        tabs <- c(
            lapply(names(tag.list), function(tab.name) {
                tabPanel(
                    tab.name,
                    organize_inputs(tag.list[[tab.name]], columns = columns, rows = rows)
                )
            })
        )

        if (!is.null(id)) {
            tabs[["id"]] <- id
        }

        # Marked so the stylesheet can let the tab strip wrap. Seven tabs do not
        # fit a narrow sidebar in one row, and Bootstrap 5 lays them out with
        # flex rather than floats, so they overflow rather than wrapping.
        out <- tagAppendAttributes(
            do.call(tabsetPanel, tabs),
            class = "vizmodules-input-tabs"
        )
    } else {
        # Flatten nested input groups (e.g. the tagLists returned by the
        # .uniform_*_inputs_ui() helpers) so each of their inputs occupies its
        # own grid cell instead of being stacked together in a single cell.
        # A tipify()/bsTooltip() wrapper is also a tagList, but its children are
        # individual tags rather than tagLists, so it is left intact as one cell.
        is_input_group <- function(el) {
            is(el, "shiny.tag.list") && length(el) > 0 &&
                all(vapply(el, function(child) is(child, "shiny.tag.list"), logical(1)))
        }
        tag.list <- do.call(tagList, unlist(
            lapply(tag.list, function(el) {
                if (is_input_group(el)) el else list(el)
            }),
            recursive = FALSE
        ))
        # Drop NULL entries (e.g. the placeholders returned by the uniform input
        # helpers for disabled optional controls, such as the rotate/flip
        # switches) so they do not create empty grid cells.
        tag.list <- Filter(Negate(is.null), tag.list)
        n.tags <- length(tag.list)

        # Determine the number of columns; derive it from `rows` if only `rows`
        # was provided.
        if (is.null(columns) && !is.null(rows)) {
            columns <- ceiling(n.tags / rows)
        } else if (is.null(columns) && is.null(rows)) {
            stop("Either rows or columns must be provided.")
        }
        columns <- as.integer(columns)

        # Lay the inputs out in a single wrapping flex container rather than a set
        # of fixed Bootstrap rows. Each input lives in its own `.vizmodules-input-cell`
        # so that hiding a cell (see `hide_input()`) lets the remaining inputs
        # reflow and pack together with no empty gaps.
        #
        # The layout itself lives in vizModules.css so a host app can override it
        # normally; only the column count comes through here, as a custom
        # property the cells inherit.
        out <- div(
            class = "vizmodules-input-grid",
            style = paste0("--viz-input-columns: ", columns, ";"),
            lapply(seq_len(n.tags), function(idx) {
                div(class = "vizmodules-input-cell", tag.list[[idx]])
            })
        )
    }

    if (!is.null(tack)) {
        out <- tagList(out, tack)
    }

    if (!is.null(title)) {
        out <- tagList(title, out)
    }

    attachDependencies(out, .viz_modules_dependency(), append = TRUE)
}


#' HTML dependency for the shared module layout styles
#'
#' Carries the rules for the control grid and tab strip that
#' [organize_inputs()] builds. Kept in a stylesheet rather than inline on every
#' element so a host app can override them with ordinary specificity instead of
#' `!important`.
#'
#' @return An `htmltools::htmlDependency` object.
#'
#' @importFrom htmltools htmlDependency
#'
#' @author Jared Andrews
#' @rdname INTERNAL_viz_modules_dependency
#' @keywords internal
.viz_modules_dependency <- function() {
    htmlDependency(
        name = "viz-modules",
        version = as.character(utils::packageVersion("VizModules")),
        src = "src",
        package = "VizModules",
        stylesheet = "vizModules.css"
    )
}

#' Hide or show the grid cell wrapping a module input
#'
#' Toggles the visibility of the `.vizmodules-input-cell` that wraps a given input
#' (as laid out by [organize_inputs()]). Hiding the cell rather than just the input
#' itself lets the surrounding inputs reflow so the panel stays compact instead of
#' leaving an empty gap, as a plain `shinyjs::hide()` on the input would.
#'
#' @param session The module `session` object (provides `session$ns`).
#' @param ids Character vector of un-namespaced input IDs to toggle.
#' @param show Logical; `TRUE` to show the cell, `FALSE` to hide it.
#'
#' @return Invisibly `NULL`, called for the side effect of running client-side JS.
#'
#' @importFrom shinyjs runjs
#' @export
#' @author Jared Andrews
#' @seealso [hide_input()], [show_input()], [organize_inputs()]
#' @examples
#' \dontrun{
#' # Call inside a module server, e.g. from an observeEvent():
#' toggle_input_cell(session, "size", show = input$show.size)
#' }
toggle_input_cell <- function(session, ids, show) {
    if (length(ids) == 0) {
        return(invisible(NULL))
    }
    display <- if (isTRUE(show)) "" else "none"
    for (id in ids) {
        dom.id <- session$ns(id)
        runjs(sprintf(
            paste0(
                "(function(){var el=document.getElementById('%s');",
                "if(el){var cell=el.closest('.vizmodules-input-cell');",
                "if(cell){cell.style.display='%s';}}})();"
            ),
            dom.id, display
        ))
    }
    invisible(NULL)
}

#' Hide the grid cells wrapping module inputs
#'
#' @inheritParams toggle_input_cell
#' @inherit toggle_input_cell description return
#' @export
#' @author Jared Andrews
#' @seealso [show_input()], [toggle_input_cell()], [organize_inputs()]
#' @examples
#' \dontrun{
#' # Call inside a module server:
#' hide_input(session, c("size", "opacity"))
#' }
hide_input <- function(session, ids) {
    toggle_input_cell(session, ids, show = FALSE)
}

#' Show the grid cells wrapping module inputs
#'
#' @inheritParams toggle_input_cell
#' @inherit toggle_input_cell description return
#' @export
#' @author Jared Andrews
#' @seealso [hide_input()], [toggle_input_cell()], [organize_inputs()]
#' @examples
#' \dontrun{
#' # Call inside a module server:
#' show_input(session, c("size", "opacity"))
#' }
show_input <- function(session, ids) {
    toggle_input_cell(session, ids, show = TRUE)
}


#' Color palette options for palettePicker
#'
#' Returns a list of predefined color palettes grouped by category (Defaults,
#' Viridis, Diverging, Qualitative, Sequential) for use with color picker UI
#' components.
#'
#' @return A named list with two elements: `choices` (a nested list of palette
#'   name to color vector mappings, grouped by category) and `textColor` (a
#'   character vector of text colors for each palette).
#'
#' @importFrom scales hue_pal viridis_pal brewer_pal
#' @importFrom dittoViz dittoColors
#' @export
#' @author Jared Andrews
#'
#' @examples
#' pals <- default_palettes()
#' names(pals$choices)
default_palettes <- function() {
    pals <- list(
        choices = list(
            Defaults = list(
                "dittoColors" = dittoColors()[1:16],
                "dittoColors_full" = dittoColors()[1:32],
                "ggplot2" = hue_pal()(16)
            ),
            Viridis = list(
                "viridis" = viridis_pal(option = "viridis")(12),
                "magma" = viridis_pal(option = "magma")(12),
                "inferno" = viridis_pal(option = "inferno")(12),
                "plasma" = viridis_pal(option = "plasma")(12),
                "cividis" = viridis_pal(option = "cividis")(12)
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

#' Create standard tack UI for module inputs
#'
#' Generates a consistent set of control buttons for VizModules that includes
#' Auto Update toggle, Update and Reset buttons, and a full source download
#' button (self-contained HTML of the plot, SVG and PNG images of it, and the
#' source data and statistics as CSVs).
#'
#' The download button carries the markup the image capture needs: the class the
#' script binds to, and the module's namespace prefix, which is how the script
#' works out which plot on the page belongs to this button. See
#' [create_source_download_handler()].
#'
#' @param ns Namespace function from the module (e.g., `ns <- NS(id)`).
#' @param defaults Optional named list of default values. Reserved for future use.
#'
#' @return A Shiny tagList containing the standard control buttons and inputs.
#'
#' @import shiny
#' @importFrom htmltools attachDependencies
#' @importFrom shinyWidgets materialSwitch
#'
#' @export
#' @author Jared Andrews
#' @examples
#' library(VizModules)
#' library(shiny)
#' ns <- NS("myModule")
#' module_tack_ui(ns)
module_tack_ui <- function(ns, defaults = NULL) {
    # Auto-update toggle on its own line, then a wrapping button row. Buttons
    # size to content (no fixed-width columns) so nothing truncates in a narrow
    # sidebar; Update/Reset share a row and the wider Source Download wraps to
    # its own full-width line.
    ui <- tagList(
        div(
            class = "module-tack",
            style = "margin-top: 12px;",
            div(
                class = "module-tack-switch",
                style = "margin-bottom: 4px;",
                materialSwitch(
                    ns("auto.update"),
                    "Auto Update",
                    value = TRUE,
                    status = "success"
                )
            ),
            div(
                class = "module-tack-buttons",
                style = "display: flex; flex-wrap: wrap; gap: 8px;",
                actionButton(ns("update"), "Update",
                    class = "btn-primary", style = "flex: 1 1 45%;"),
                actionButton(ns("reset"), "Reset",
                    class = "btn-secondary", style = "flex: 1 1 45%;"),
                tipify(
                    downloadButton(
                        ns("download.source"),
                        "Source Download",
                        class = "btn-secondary viz-source-download",
                        icon = icon("file-code"),
                        style = "flex: 1 1 100%;",
                        # How sourceExport.js finds this module's plot: every
                        # output of this module, and only this module, is named
                        # with this prefix.
                        `data-viz-source-ns` = ns("")
                    ),
                    title = paste(
                        "Download the plot as a self-contained HTML file and as",
                        "SVG and PNG images of it as it looks now, along with",
                        "the plot source data and statistics (if applicable) as",
                        "CSV files."
                    ),
                    placement = "top",
                    options = list(container = "body")
                )
            )
        )
    )

    # Carried on the tack rather than the module's UI as a whole, so a module
    # inserted into the Figure Builder at runtime brings the script with it.
    attachDependencies(ui, .source_export_dependency(), append = TRUE)
}
