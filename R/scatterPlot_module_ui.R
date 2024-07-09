#' Input UI components for the scatterPlot module
#' 
#' This should be placed in the UI where the inputs should be shown, with an `id` 
#' that matches the `id` used in the `scatterPlotServer()` and `scatterPlotOutputUI()` functions.
#' 
#' @details The user inputs for this module are separated from the outputs to allow for 
#' more flexible UI design. 
#' 
#' The inputs will automatically be organized into a grid layout via the `organize_inputs()` function,
#' with `columns` controlling the number of columns in the grid. 
#' 
#' Defaults can be set for each input by providing a named list of values to the `defaults` argument.
#' Nearly all parameters for [dittoViz::scatterPlot()] can be set via these inputs, so see the help
#' for that function for an exhaustive list. 
#' 
#' Note that some of the parameters may have input types that differ from the actual function, e.g. `shape.panel`
#' is a text input for comma-separated integers, while the function expects a vector of integers.
#' The module will parse such inputs into the appropriate format for [dittoViz::scatterPlot()] automatically. 
#' 
#' There are also a handful that are specific to the Shiny module that additionally modify the plotly output:
#' 
#' - `id`: The ID for the Shiny module.
#' 
#' @param id The ID for the Shiny module.
#' @param data The data frame used for plot generation.
#' @param defaults A named list of default values for the inputs.
#' @param title An optional title for the UI grid.
#' @param columns Number of columns for the UI grid.
#' @return A Shiny tagList containing the UI elements
#'
#' @importFrom shiny tagList NS selectInput numericInput sliderInput
#'   checkboxInput textInput actionButton br selectizeInput
#' @importFrom colourpicker colourInput
#' @importFrom esquisse palettePicker
#'
#' @export
#' @author Jared Andrews
#' @seealso [dittoViz::scatterPlot()], [dittoVizModules::organize_inputs()], 
#' [dittoVizModules::scatterPlotOutputUI()], [dittoVizModules::scatterPlotServer()], [dittoVizModules::createScatterPlotApp()]
#' @examples
#' library(dittoVizModules)
#' data(mtcars)
#' scatterPlotInputsUI("scatterPlot", mtcars)
scatterPlotInputsUI <- function(id, data, defaults = NULL, title = NULL, columns = 2) {
    ns <- NS(id)

    # Get variables of data.
    choices <- c("", names(data))

    # Get numeric variables of data.
    num.choices <- c("", names(data)[unlist(lapply(data, is.numeric),
        use.names = FALSE
    )])

    # Get categorical variables of data.
    cat.choices <- c("", names(data)[unlist(lapply(data,
        FUN = function(x) !is.numeric(x)
    ), use.names = FALSE)])

    # Various other choice vectors
    adj.choices <- c("", "z-score", "relative.to.max")
    adj.fxn.choices <- c("", "log2", "log", "log10", "log1p", "as.factor", "abs", "sqrt")

    # Create list of Shiny inputs for most scatterPlot parameters
    # Broken up by sensible categories (e.g. "Data", "Point Styling")
    inputs <- list(
        "Data" = tagList(
            selectInput(ns("x.by"), "X-axis variable",
                choices = choices,
                selected = ifelse("x.by" %in% names(defaults),
                    ifelse(defaults[["x.by"]] %in% choices, defaults[["x.by"]], choices[2]),
                    choices[2]
                )
            ),
            selectInput(ns("y.by"), "Y-axis variable",
                choices = choices,
                selected = ifelse("y.by" %in% names(defaults),
                    ifelse(defaults[["y.by"]] %in% choices, defaults[["y.by"]], choices[3]),
                    choices[3]
                )
            ),
            selectInput(ns("color.by"), "Color by",
                choices = choices,
                selected = ifelse("color.by" %in% names(defaults),
                    ifelse(defaults[["color.by"]] %in% choices, defaults[["color.by"]], ""),
                    ""
                )
            ),
            selectInput(ns("shape.by"), "Shape by",
                choices = cat.choices,
                selected = ifelse("shape.by" %in% names(defaults),
                    ifelse(defaults[["shape.by"]] %in% cat.choices, defaults[["shape.by"]], ""),
                    ""
                )
            ),
            selectizeInput(ns("split.by"), "Split by",
                choices = cat.choices,
                selected = ifelse("split.by" %in% names(defaults),
                    ifelse(all(defaults[["split.by"]] %in% cat.choices), defaults[["split.by"]], ""),
                    ""
                ),
                multiple = TRUE,
                options = list(maxItems = 2)
            ),
            textInput(ns("rows.use"), "Rows to plot",
                placeholder = "Filter expression, e.g. Sepal.Length > 5",
                value = ifelse("rows.use" %in% names(defaults), defaults[["rows.use"]], "")
            )
        ),
        "Adjustments" = tagList(
            selectInput(ns("x.adjustment"), "X-axis adjustment",
                choices = adj.choices,
                selected = ifelse("x.adjustment" %in% names(defaults),
                    ifelse(defaults[["x.adjustment"]] %in% adj.choices, defaults[["x.adjustment"]], ""),
                    ""
                )
            ),
            selectInput(ns("y.adjustment"), "Y-axis adjustment",
                choices = adj.choices,
                selected = ifelse("y.adjustment" %in% names(defaults),
                    ifelse(defaults[["y.adjustment"]] %in% adj.choices, defaults[["y.adjustment"]], ""),
                    ""
                )
            ),
            selectInput(ns("color.adjustment"), "Color adjustment",
                choices = adj.choices,
                selected = ifelse("color.adjustment" %in% names(defaults),
                    ifelse(defaults[["color.adjustment"]] %in% adj.choices, defaults[["color.adjustment"]], ""),
                    ""
                )
            ),
            selectInput(ns("x.adj.fxn"), "X-axis adjustment function",
                choices = adj.fxn.choices,
                selected = ifelse("x.adj.fxn" %in% names(defaults),
                    ifelse(defaults[["x.adj.fxn"]] %in% adj.fxn.choices, defaults[["x.adj.fxn"]], ""),
                    ""
                )
            ),
            selectInput(ns("y.adj.fxn"), "Y-axis adjustment function",
                choices = adj.fxn.choices,
                selected = ifelse("y.adj.fxn" %in% names(defaults),
                    ifelse(defaults[["y.adj.fxn"]] %in% adj.fxn.choices, defaults[["y.adj.fxn"]], ""),
                    ""
                )
            ),
            selectInput(ns("color.adj.fxn"), "Color adjustment function",
                choices = adj.fxn.choices,
                selected = ifelse("color.adj.fxn" %in% names(defaults),
                    ifelse(defaults[["color.adj.fxn"]] %in% adj.fxn.choices, defaults[["color.adj.fxn"]], ""),
                    ""
                )
            )
        ),
        "Points" = tagList(
            numericInput(ns("size"), "Point size",
                value = ifelse("size" %in% names(defaults),
                    ifelse(is.numeric(defaults[["size"]]), defaults[["size"]], 1),
                    1
                ),
                min = 0.1
            ),
            numericInput(ns("opacity"), "Point opacity",
                value = ifelse("opacity" %in% names(defaults),
                    ifelse(is.numeric(defaults[["opacity"]]), defaults[["opacity"]], 1),
                    1
                ),
                max = 1,
                min = 0,
                step = 0.05
            ),
            checkboxInput(ns("show.others"), "Show others",
                value = ifelse("show.others" %in% names(defaults),
                    ifelse(is.logical(defaults[["show.others"]]), defaults[["show.others"]], TRUE),
                    TRUE
                )
            ),
            checkboxInput(ns("split.show.all.others"),
                "Show split others",
                value = ifelse("split.show.all.others" %in% names(defaults),
                    ifelse(is.logical(defaults[["split.show.all.others"]]), defaults[["split.show.all.others"]], TRUE),
                    TRUE
                )
            ),
            selectInput(ns("plot.order"), "Plot order",
                choices = c("unordered", "increasing", "decreasing", "randomize"),
                selected = ifelse("plot.order" %in% names(defaults),
                    ifelse(defaults[["plot.order"]] %in% c(
                        "unordered", "increasing", "decreasing", "randomize"
                    ), defaults[["plot.order"]], "unordered"),
                    "unordered"
                )
            ),
            textInput(ns("shape.panel"), "Shape panel",
                value = ifelse("shape.panel" %in% names(defaults),
                    defaults[["shape.panel"]], "16, 15, 17, 23, 25, 8"
                )
            )
        ),
        "Colors" = tagList(
            colourInput(ns("min.color"), "Min color",
                value = ifelse("min.color" %in% names(defaults),
                    defaults[["min.color"]], "#F0E442"
                )
            ),
            colourInput(ns("max.color"), "Max color",
                value = ifelse("max.color" %in% names(defaults),
                    defaults[["max.color"]], "#0072B2"
                )
            ),
            colourInput(ns("contour.color"), "Contour color",
                value = ifelse("contour.color" %in% names(defaults),
                    defaults[["contour.color"]], "black"
                )
            ),
            selectInput(ns("contour.linetype"), "Contour linetype",
                choices = c(
                    "solid", "dashed", "dotted", "dotdash",
                    "longdash", "twodash"
                ),
                selected = ifelse("contour.linetype" %in% names(defaults),
                    ifelse(defaults[["contour.linetype"]] %in% c(
                        "solid", "dashed", "dotted", "dotdash",
                        "longdash", "twodash"
                    ), defaults[["contour.linetype"]], "solid"),
                    "solid"
                )
            ),
            palettePicker(ns("color.panel"), "Color panel",
                choices = default_palettes()[["choices"]],
                textColor = default_palettes()[["textColor"]],
                selected = ifelse("color.panel" %in% names(defaults),
                    defaults[["color.panel"]], "dittoColors"
                )
            )
        ),
        "Facets" = tagList(
            numericInput(ns("split.nrow"), "Split nrow",
                step = 1, min = 0,
                value = ifelse("split.nrow" %in% names(defaults) & is.numeric(defaults[["split.nrow"]]),
                    ifelse(is.numeric(defaults[["split.nrow"]]), defaults[["split.nrow"]], NA),
                    NA
                )
            ),
            numericInput(ns("split.ncol"), "Split ncol",
                step = 1, min = 0,
                value = ifelse("split.ncol" %in% names(defaults),
                    ifelse(is.numeric(defaults[["split.ncol"]]), defaults[["split.ncol"]], NA),
                    NA
                )
            ),
            selectInput(ns("multivar.split.dir"), "Multivar split dir",
                choices = c("col", "row"),
                selected = ifelse("multivar.split.dir" %in% names(defaults),
                    ifelse(defaults[["multivar.split.dir"]] %in% c("col", "row"),
                        defaults[["multivar.split.dir"]], "col"
                    ),
                    "col"
                )
            )
        ),
        "Annotations" = tagList(
            selectInput(ns("annotate.by"), "Annotate by",
                choices = choices,
                selected = ifelse("annotate.by" %in% names(defaults),
                    ifelse(defaults[["annotate.by"]] %in% choices, defaults[["annotate.by"]], ""),
                    ""
                )
            ),
            colourInput(ns("annotation.color"), "Annotation color",
                value = ifelse("annotation.color" %in% names(defaults),
                    defaults[["annotation.color"]], "black"
                )
            ),
            numericInput(ns("annotation.ax"), "Annotation x-axis offset",
                step = 1,
                value = ifelse("annotation.ax" %in% names(defaults),
                    ifelse(is.numeric(defaults[["annotation.ax"]]), defaults[["annotation.ax"]], 20),
                    20
                )
            ),
            numericInput(ns("annotation.ay"), "Annotation y-axis offset",
                step = 1,
                value = ifelse("annotation.ay" %in% names(defaults),
                    ifelse(is.numeric(defaults[["annotation.ay"]]), defaults[["annotation.ay"]], -20),
                    -20
                )
            ),
            numericInput(ns("annotation.size"), "Annotation size",
                min = 1, step = 0.5,
                value = ifelse("annotation.size" %in% names(defaults),
                    ifelse(is.numeric(defaults[["annotation.size"]]), defaults[["annotation.size"]], 10),
                    10
                )
            ),
            checkboxInput(ns("annotation.showarrow"), "Show arrow",
                value = ifelse("annotation.showarrow" %in% names(defaults),
                    ifelse(is.logical(defaults[["annotation.showarrow"]]), defaults[["annotation.showarrow"]], TRUE),
                    TRUE
                )
            ),
            colourInput(ns("annotation.arrowcolor"), "Arrow color",
                value = ifelse("annotation.arrowcolor" %in% names(defaults),
                    defaults[["annotation.arrowcolor"]], "black"
                )
            ),
            numericInput(ns("annotation.arrowhead"), "Arrowhead style",
                min = 0, step = 1, max = 7,
                value = ifelse("annotation.arrowhead" %in% names(defaults),
                    ifelse(is.numeric(defaults[["annotation.arrowhead"]]), defaults[["annotation.arrowhead"]], 2),
                    2
                )
            ),
            numericInput(ns("annotation.arrowwidth"), "Arrow linewidth",
                min = 0.1, step = 0.25,
                value = ifelse("annotation.arrowwidth" %in% names(defaults),
                    ifelse(is.numeric(defaults[["annotation.arrowwidth"]]), defaults[["annotation.arrowwidth"]], 1.5),
                    1.5
                )
            ),
            actionButton(ns("annotation.clear"), "Clear annotations")
        ),
        "Legend/Scale" = tagList(
            checkboxInput(ns("legend.show"), "Enable legend",
                value = ifelse("legend.show" %in% names(defaults),
                    ifelse(is.logical(defaults[["legend.show"]]), defaults[["legend.show"]], TRUE),
                    TRUE
                )
            ),
            textInput(ns("legend.color.title"), "Legend title",
                value = ifelse("legend.color.title" %in% names(defaults),
                    defaults[["legend.color.title"]], "make"
                )
            ),
            numericInput(ns("legend.color.size"), "Legend color size",
                min = 1,
                value = ifelse("legend.color.size" %in% names(defaults),
                    ifelse(is.numeric(defaults[["legend.color.size"]]), defaults[["legend.color.size"]], 5),
                    5
                )
            ),
            numericInput(ns("legend.shape.size"), "Legend shape size",
                min = 1,
                value = ifelse("legend.shape.size" %in% names(defaults),
                    ifelse(is.numeric(defaults[["legend.shape.size"]]), defaults[["legend.shape.size"]], 5),
                    5
                )
            ),
            textInput(ns("legend.color.breaks"), "Legend tick breaks",
                placeholder = "e.g. -3, 0, 3",
                value = ifelse("legend.color.breaks" %in% names(defaults),
                    ifelse(is.character(defaults[["legend.color.breaks"]]), defaults[["legend.color.breaks"]], ""),
                    ""
                )
            ),
            numericInput(ns("min.value"), "Min value",
                value = ifelse("min.value" %in% names(defaults),
                    ifelse(is.numeric(defaults[["min.value"]]), defaults[["min.value"]], NA),
                    NA
                )
            ),
            numericInput(ns("max.value"), "Max value",
                value = ifelse("max.value" %in% names(defaults),
                    ifelse(is.numeric(defaults[["max.value"]]), defaults[["max.value"]], NA),
                    NA
                )
            )
        ),
        "Trajectory" = tagList(
            selectInput(ns("trajectory.group.by"), "Trajectory group by",
                choices = cat.choices,
                selected = ifelse("trajectory.group.by" %in% names(defaults),
                    ifelse(defaults[["trajectory.group.by"]] %in% cat.choices, defaults[["trajectory.group.by"]], ""),
                    ""
                )
            ),
            textInput(ns("add.trajectory.by.groups"), "Add trajectory by groups",
                placeholder = "e.g. [A,B],[C,D,E]",
                value = ifelse("add.trajectory.by.groups" %in% names(defaults),
                    defaults[["add.trajectory.by.groups"]], ""
                )
            ),
            numericInput(ns("trajectory.arrow.size"), "Trajectory arrow size",
                value = ifelse("trajectory.arrow.size" %in% names(defaults),
                    ifelse(is.numeric(defaults[["trajectory.arrow.size"]]), defaults[["trajectory.arrow.size"]], 0.15),
                    0.15
                ),
                min = 0,
                step = 0.05
            )
        ),
        "Plotly" = tagList(
            checkboxInput(ns("webgl"), "Plot with webGL",
                value = ifelse("webgl" %in% names(defaults),
                    ifelse(is.logical(defaults[["webgl"]]), defaults[["webgl"]], TRUE),
                    TRUE
                )
            ),
            selectInput(ns("download.format"), "Download format",
                choices = c("svg", "png"),
                selected = ifelse("download.format" %in% names(defaults),
                    ifelse(defaults[["download.format"]] %in% c("svg", "png"), defaults[["download.format"]], "svg"),
                    "svg"
                )
            ),
            colourInput(ns("shape.fill"), "Shape fill",
                allowTransparent = TRUE,
                value = ifelse("shape.fill" %in% names(defaults),
                    defaults[["shape.fill"]], "rgba(0, 0, 0, 0)"
                )
            ),
            colourInput(ns("shape.line.color"), "Shape line color",
                allowTransparent = TRUE,
                value = ifelse("shape.line.color" %in% names(defaults),
                    defaults[["shape.line.color"]], "black"
                )
            ),
            numericInput(ns("shape.line.width"), "Shape line width",
                value = ifelse("shape.line.width" %in% names(defaults),
                    ifelse(is.numeric(defaults[["shape.line.width"]]), defaults[["shape.line.width"]], 4),
                    4
                ),
                min = 0,
                step = 0.25
            ),
            selectInput(ns("shape.linetype"), "Shape linetype",
                choices = c(
                    "solid", "dot", "dash", "longdash",
                    "dashdot", "longdashdot"
                ),
                selected = ifelse("shape.linetype" %in% names(defaults),
                    ifelse(defaults[["shape.linetype"]] %in% c(
                        "solid", "dot", "dash", "longdash",
                        "dashdot", "longdashdot"
                    ), defaults[["shape.linetype"]], "solid"),
                    "solid"
                )
            ),
            numericInput(ns("shape.opacity"), "Shape opacity",
                value = ifelse("shape.opacity" %in% names(defaults),
                    ifelse(is.numeric(defaults[["shape.opacity"]]), defaults[["shape.opacity"]], 1),
                    1
                ),
                min = 0,
                max = 1,
                step = 0.01
            )
        ),
        "Extras" = tagList(
            checkboxInput(ns("do.ellipse"), "Enable ellipses",
                value = ifelse("do.ellipse" %in% names(defaults),
                    ifelse(is.logical(defaults[["do.ellipse"]]), defaults[["do.ellipse"]], FALSE),
                    FALSE
                )
            ),
            checkboxInput(ns("do.contour"), "Enable contour",
                value = ifelse("do.contour" %in% names(defaults),
                    ifelse(is.logical(defaults[["do.contour"]]), defaults[["do.contour"]], FALSE),
                    FALSE
                )
            ),
            checkboxInput(ns("show.grid.lines"), "Show gridlines",
                value = ifelse("show.grid.lines" %in% names(defaults),
                    ifelse(is.logical(defaults[["show.grid.lines"]]), defaults[["show.grid.lines"]], TRUE),
                    TRUE
                )
            ),
            selectizeInput(ns("hover.data"), "Hover data",
                choices = choices,
                multiple = TRUE,
                selected = ifelse("hover.data" %in% names(defaults),
                    ifelse(all(defaults[["hover.data"]] %in% choices), defaults[["hover.data"]], ""),
                    ""
                )
            ),
            numericInput(ns("hover.round.digits"), "Hover round digits",
                value = ifelse("hover.round.digits" %in% names(defaults),
                    ifelse(is.numeric(defaults[["hover.round.digits"]]), defaults[["hover.round.digits"]], 5),
                    5
                ),
                step = 1,
                min = 1
            ),
            textInput(ns("add.xline"), "Add xlines",
                placeholder = "e.g. 2, -2",
                value = ifelse("add.xline" %in% names(defaults),
                    ifelse(is.character(defaults[["add.xline"]]), defaults[["add.xline"]], ""),
                    ""
                )
            ),
            textInput(ns("add.yline"), "Add ylines",
                placeholder = "e.g. 2, -2",
                value = ifelse("add.yline" %in% names(defaults),
                    ifelse(is.character(defaults[["add.yline"]]), defaults[["add.yline"]], ""),
                    ""
                )
            ),
            selectInput(ns("xline.linetype"), "xline linetype",
                choices = c(
                    "solid", "dashed", "dotted", "dotdash",
                    "longdash", "twodash"
                ),
                selected = ifelse("xline.linetype" %in% names(defaults),
                    ifelse(defaults[["xline.linetype"]] %in% c(
                        "solid", "dashed", "dotted", "dotdash",
                        "longdash", "twodash"
                    ), defaults[["xline.linetype"]], "solid"),
                    "solid"
                )
            ),
            selectInput(ns("yline.linetype"), "yline linetype",
                choices = c(
                    "solid", "dashed", "dotted", "dotdash",
                    "longdash", "twodash"
                ),
                selected = ifelse("yline.linetype" %in% names(defaults),
                    ifelse(defaults[["yline.linetype"]] %in% c(
                        "solid", "dashed", "dotted", "dotdash",
                        "longdash", "twodash"
                    ), defaults[["yline.linetype"]], "solid"),
                    "solid"
                )
            ),
            colourInput(ns("xline.color"), "xline color",
                value = ifelse("xline.color" %in% names(defaults),
                    defaults[["xline.color"]], "black"
                )
            ),
            colourInput(ns("yline.color"), "yline color",
                value = ifelse("yline.color" %in% names(defaults),
                    defaults[["yline.color"]], "black"
                )
            )
        )
    )

    organize_inputs(
        inputs,
        id = ns("scatterPlotTabsetPanel"),
        title = title,
        tack = tagList(actionButton(ns("update"), "Update Plot"), br()),
        columns = columns
    )
}


#' Output UI components for the scatterPlot module
#' 
#' This should be placed in the UI where the plot should be shown.
#' 
#' @param id The ID for the Shiny module.
#'
#' @return A Shiny plotlyOutput for the scatterplot
#'
#' @importFrom shiny NS
#' @importFrom plotly plotlyOutput
#'
#' @export
#' @author Jared Andrews
scatterPlotOutputUI <- function(id) {
    ns <- NS(id)
    plotlyOutput(ns("scatterPlot"))
}