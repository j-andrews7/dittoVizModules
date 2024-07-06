###### Module UI ######

#' Input UI components for the scatterPlot module
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
                min = 0
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
        "Labels" = tagList(
            checkboxInput(ns("do.label"), "Enable labels",
                value = ifelse("do.label" %in% names(defaults),
                    ifelse(is.logical(defaults[["do.label"]]), defaults[["do.label"]], FALSE),
                    FALSE
                )
            ),
            checkboxInput(ns("labels.highlight"), "Highlight labels",
                value = ifelse("labels.highlight" %in% names(defaults),
                    ifelse(is.logical(defaults[["labels.highlight"]]), defaults[["labels.highlight"]], TRUE),
                    TRUE
                )
            ),
            checkboxInput(ns("labels.repel"), "Repel labels",
                value = ifelse("labels.repel" %in% names(defaults),
                    ifelse(is.logical(defaults[["labels.repel"]]), defaults[["labels.repel"]], TRUE),
                    TRUE
                )
            ),
            numericInput(ns("labels.size"), "Labels size",
                min = 1,
                value = ifelse("labels.size" %in% names(defaults),
                    ifelse(is.numeric(defaults[["labels.size"]]), defaults[["labels.size"]], 5),
                    5
                )
            )
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
            selectInput(ns("hover.data"), "Hover data",
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
#' @param id The ID for the Shiny module
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


###### Module Server ######

#' Server logic for scatterplot module
#' @param id The ID for the Shiny module
#' @param data A `reactive` containing the data frame to plot
#' @param hide.inputs A character vector of input IDs to hide.
#'   These will still be initialized and their values passed to the plot function,
#'   but the user will not be able to see/adjust them in the UI.
#' @param hide.tabs A character vector of tab names to hide.
#'   Inputs in these tabs will still be initialized and their values passed to the plot function,
#'   but the user will not be able to see/adjust them in the UI.
#'
#' @importFrom shiny moduleServer isolate hideTab reactive
#' @importFrom dittoViz scatterPlot dittoColors
#' @importFrom ggplot2 theme_bw waiver
#' @importFrom plotly renderPlotly %>% config layout toWebGL
#' @importFrom shinyjs hide
#'
#' @export
#' @author Jared Andrews
scatterPlotServer <- function(id, data, hide.inputs = NULL, hide.tabs = NULL) {
    stopifnot(is.reactive(data))

    moduleServer(id, function(input, output, session) {
        # Hide individual inputs if specified
        if (!is.null(hide.inputs)) {
            lapply(hide.inputs, function(input.name) {
                hide(input.name)
            })
        }

        # Hide tabs if specified
        if (!is.null(hide.tabs)) {
            lapply(hide.tabs, function(tab.name) {
                hideTab(inputId = "scatterPlotTabsetPanel", target = tab.name)
            })
        }

        # Get color panel
        color.panel <- reactive({
            if (is.null(input$color.panel) || input$color.panel == "dittoColors") {
                palette <- dittoColors()
            } else if (!is.null(input$color.by)) {
                if (input$color.panel %in% c("viridis", "magma", "inferno", "plasma", "cividis")) {
                    palette <- viridis_pal(option = input$color.panel)(length(colLevels(input$color.by, data())))
                } else if (input$color.panel == "ggplot2") {
                    palette <- hue_pal()(length(colLevels(input$color.by, data())))
                } else {
                    palette <- brewer_pal(option = input$color.panel)(length(colLevels(input$color.by, data())))
                }
            }

            palette
        })

        output$scatterPlot <- renderPlotly({
            input$update

            # Change textInputs and selectInputs to NULL if empty
            null.na.inputs <- list(
                "trajectory.group.by" = isolate(input$trajectory.group.by),
                "add.trajectory.by.groups" = isolate(input$add.trajectory.by.groups),
                "add.xline" = isolate(input$add.xline),
                "add.yline" = isolate(input$add.yline),
                "color.by" = isolate(input$color.by),
                "shape.by" = isolate(input$shape.by),
                "split.by" = isolate(input$split.by),
                "x.adjustment" = isolate(input$x.adjustment),
                "y.adjustment" = isolate(input$y.adjustment),
                "color.adjustment" = isolate(input$color.adjustment),
                "x.adj.fxn" = isolate(input$x.adj.fxn),
                "y.adj.fxn" = isolate(input$y.adj.fxn),
                "color.adj.fxn" = isolate(input$color.adj.fxn),
                "split.nrow" = isolate(input$split.nrow),
                "split.ncol" = isolate(input$split.ncol)
            )

            for (input.name in names(null.na.inputs)) {
                if (!is.null(null.na.inputs[[input.name]])) {
                    if (is.na(null.na.inputs[[input.name]])) {
                        null.na.inputs[[input.name]] <- NULL
                    } else if (null.na.inputs[[input.name]] == "") {
                        null.na.inputs[[input.name]] <- NULL
                    }
                }
            }

            # Waiver inputs
            waiver.inputs <- list(
                "legend.color.breaks" = isolate(input$legend.color.breaks)
            )

            # If input is empty, set to waiver()
            for (input.name in names(waiver.inputs)) {
                if (waiver.inputs[[input.name]] == "") {
                    waiver.inputs[[input.name]] <- waiver()
                } else {
                    waiver.inputs[[input.name]] <- as.numeric(.string_to_vector(waiver.inputs[[input.name]]))
                }
            }

            fig <- scatterPlot(
                data(),
                x.by = isolate(input$x.by),
                y.by = isolate(input$y.by),
                color.by = null.na.inputs$color.by,
                shape.by = null.na.inputs$shape.by,
                split.by = null.na.inputs$split.by,
                size = isolate(input$size),
                rows.use = with(data(), eval(str2expression(isolate(input$rows.use)))),
                show.others = isolate(input$show.others),
                x.adjustment = null.na.inputs$x.adjustment,
                y.adjustment = null.na.inputs$y.adjustment,
                color.adjustment = null.na.inputs$color.adjustment,
                x.adj.fxn = eval(str2expression(isolate(input$x.adj.fxn))),
                y.adj.fxn = eval(str2expression(isolate(input$y.adj.fxn))),
                color.adj.fxn = eval(str2expression(isolate(input$color.adj.fxn))),
                split.show.all.others = isolate(input$split.show.all.others),
                opacity = isolate(input$opacity),
                color.panel = isolate(color.panel()),
                colors = seq_along(isolate(color.panel())),
                split.nrow = null.na.inputs$split.nrow,
                split.ncol = null.na.inputs$split.ncol,
                split.adjust = list(),
                multivar.split.dir = isolate(input$multivar.split.dir),
                shape.panel = as.numeric(.string_to_vector(isolate(input$shape.panel))),
                rename.color.groups = NULL,
                rename.shape.groups = NULL,
                min.color = isolate(input$min.color),
                max.color = isolate(input$max.color),
                min.value = isolate(input$min.value),
                max.value = isolate(input$max.value),
                plot.order = isolate(input$plot.order),
                theme = theme_bw(),
                do.hover = TRUE,
                hover.data = unique(c(
                    null.na.inputs$color.by,
                    paste0(null.na.inputs$color.by, ".color.adj"),
                    "color.multi", "color.which",
                    isolate(input$x.by),
                    paste0(isolate(input$x.by), ".x.adj"),
                    isolate(input$y.by),
                    paste0(isolate(input$y.by), ".y.adj"),
                    null.na.inputs$shape.by,
                    null.na.inputs$split.by
                )),
                hover.round.digits = isolate(input$hover.round.digits),
                do.contour = isolate(input$do.contour),
                contour.color = isolate(input$contour.color),
                contour.linetype = isolate(input$contour.linetype),
                add.trajectory.by.groups = .string_to_list_of_vectors(null.na.inputs$add.trajectory.by.groups),
                trajectory.group.by = null.na.inputs$trajectory.group.by,
                trajectory.arrow.size = isolate(input$trajectory.arrow.size),
                add.xline = as.numeric(.string_to_vector(null.na.inputs$add.xline)),
                xline.linetype = isolate(input$xline.linetype),
                xline.color = isolate(input$xline.color),
                add.yline = as.numeric(.string_to_vector(null.na.inputs$add.yline)),
                yline.linetype = isolate(input$yline.linetype),
                yline.color = isolate(input$yline.color),
                do.ellipse = isolate(input$do.ellipse),
                legend.show = isolate(input$legend.show),
                legend.color.title = isolate(input$legend.color.title),
                legend.color.size = isolate(input$legend.color.size),
                legend.color.breaks = waiver.inputs$legend.color.breaks,
                legend.color.breaks.labels = waiver(),
                legend.shape.title = null.na.inputs$shape.by,
                legend.shape.size = isolate(input$legend.shape.size),
                show.grid.lines = isolate(input$show.grid.lines)
            )

            fig <- fig %>% config(
                edits = list(
                    axisTitleText = TRUE,
                    titleText = TRUE,
                    legendText = TRUE,
                    legendPosition = TRUE,
                    colorbarPosition = TRUE,
                    colorbarTitleText = TRUE,
                    shapePosition = TRUE,
                    annotationTail = TRUE
                ),
                toImageButtonOptions = list(
                    format = isolate(input$download.format)
                ),
                modeBarButtonsToAdd = list(
                    "drawline",
                    "drawopenpath",
                    "drawclosedpath",
                    "drawcircle",
                    "drawrect",
                    "eraseshape"
                ),
                displaylogo = FALSE
            )

            fig <- fig %>% layout(
                newshape = list(
                    fillcolor = isolate(input$shape.fill),
                    line = list(
                        color = isolate(input$shape.line.color),
                        width = isolate(input$shape.line.width),
                        dash = isolate(input$shape.linetype)
                    ),
                    opacity = isolate(input$shape.opacity)
                )
            )

            if (isolate(input$webgl)) {
                fig <- fig %>% toWebGL()
            }

            fig
        })
    })
}

###### Example App ######

#' Create a Modular scatterPlot Shiny Application
#'
#' This function generates a Shiny application with modular [dittoViz::scatterPlot()] components.
#' A module is created for each data frame provided in the named list of data frames.
#'
#' @param data_list A named list of data frames for which scatterPlot modules will be created.
#'
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel mainPanel shinyApp h3 reactive
#' @importFrom shinyjs useShinyjs
#' @export
#'
#' @examples
#' data_list <- list("mtcars" = mtcars, "iris" = iris)
#' app <- createScatterPlotApp(data_list)
#' runApp(app)
createScatterPlotApp <- function(data_list) {
    # Validate input
    stopifnot(is.list(data_list))
    lapply(data_list, function(data) {
        stopifnot(is.data.frame(data))
    })

    # UI definition
    ui <- fluidPage(
        useShinyjs(),
        titlePanel("Modular scatterPlots"),
        sidebarLayout(
            sidebarPanel(
                scatterPlotInputsUI("iris", data_list[["iris"]],
                    title = h3("iris Settings"),
                    defaults = list("x.by" = "Petal.Width")
                ),
                scatterPlotInputsUI("mtcars", data_list[["mtcars"]],
                    title = h3("mtcars Settings"),
                    defaults = list("y.by" = "vs")
                )
            ),
            mainPanel(
                tagList(scatterPlotOutputUI("iris"), br(), scatterPlotOutputUI("mtcars"))
            )
        )
    )

    # Server function
    server <- function(input, output, session) {
        lapply(names(data_list), function(name) {
            scatterPlotServer(name, data = reactive(data_list[[name]]))
        })
    }

    # Return the Shiny app
    shinyApp(ui, server)
}
