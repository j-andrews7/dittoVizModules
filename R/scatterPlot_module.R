###### Module UI ######

#' Input UI components for the scatterPlot module
#' @param id The ID for the Shiny module.
#' @param data The data frame used for plot generation.
#' @param title An optional title for the UI grid.
#' @param columns Number of columns for the UI grid.
#' @return A Shiny tagList containing the UI elements
#'
#' @importFrom shiny tagList NS selectInput numericInput sliderInput
#'   checkboxInput textInput actionButton br
#' @importFrom colourpicker colourInput
#' @export
#' @author Jared Andrews
#' @examples
#' library(dittoVizModules)
#' data(mtcars)
#' scatterPlotInputsUI("scatterPlot", mtcars)
scatterPlotInputsUI <- function(id, data, title = NULL, columns = 2) {
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

    # Create list of Shiny inputs for most scatterPlot parameters
    # Broken up by sensible categories (e.g. "Data", "Point Styling")
    inputs <- list(
        "Data" = tagList(
            selectInput(ns("x.by"), "X-axis variable",
                choices = choices,
                selected = choices[2]
            ),
            selectInput(ns("y.by"), "Y-axis variable",
                choices = choices,
                selected = choices[3]
            ),
            selectInput(ns("color.by"), "Color by", choices = choices),
            selectInput(ns("shape.by"), "Shape by", choices = choices),
            selectInput(ns("split.by"), "Split by", choices = choices),
            textInput(ns("rows.use"), "Rows to plot",
                placeholder = "Filter expression, e.g. Sepal.Length > 5"
            )
        ),
        "Adjustments" = tagList(
            selectInput(ns("x.adjustment"), "X-axis adjustment",
                choices = c("", "z-score", "relative.to.max")
            ),
            selectInput(ns("y.adjustment"), "Y-axis adjustment",
                choices = c("", "z-score", "relative.to.max")
            ),
            selectInput(ns("color.adjustment"), "Color adjustment",
                choices = c("", "z-score", "relative.to.max")
            ),
            selectInput(ns("x.adj.fxn"), "X-axis adjustment function",
                choices = c("", "log2", "log", "log10", "log1p", "as.factor")
            ),
            selectInput(ns("y.adj.fxn"), "Y-axis adjustment function",
                choices = c("", "log2", "log", "log10", "log1p", "as.factor")
            ),
            selectInput(ns("color.adj.fxn"), "Color adjustment function",
                choices = c("", "log2", "log", "log10", "log1p", "as.factor")
            )
        ),
        "Points" = tagList(
            numericInput(ns("size"), "Point size", value = 1, min = 0.1),
            numericInput(ns("opacity"), "Point opacity", value = 1, max = 1, min = 0),
            checkboxInput(ns("show.others"), "Show others", value = TRUE),
            checkboxInput(ns("split.show.all.others"),
                "Show split others",
                value = TRUE
            ),
            selectInput(ns("plot.order"), "Plot order",
                choices = c("unordered", "increasing", "decreasing", "randomize")
            ),
            textInput(ns("shape.panel"), "Shape panel",
                value = "16, 15, 17, 23, 25, 8"
            ),
            selectInput(ns("rename.color.groups"), "Rename color groups",
                choices = cat.choices
            ),
            selectInput(ns("rename.shape.groups"), "Rename shape groups",
                choices = cat.choices
            )
        ),
        "Colors" = tagList(
            colourInput(ns("min.color"), "Min color", value = "#F0E442"),
            colourInput(ns("max.color"), "Max color", value = "#0072B2"),
            colourInput(ns("contour.color"), "Contour color", value = "black"),
            selectInput(ns("contour.linetype"), "Contour linetype",
                choices = c(
                    "solid", "dashed", "dotted", "dotdash",
                    "longdash", "twodash"
                )
            )
        ),
        "Facets" = tagList(
            numericInput(ns("split.nrow"), "Split nrow", step = 1, value = NA),
            numericInput(ns("split.ncol"), "Split ncol", step = 1, value = NA),
            selectInput(ns("multivar.split.dir"), "Multivar split dir", choices = c("col", "row")),
            checkboxInput(ns("split.show.all.others"), "Split show all others", value = TRUE)
        ),
        "Labels" = tagList(
            checkboxInput(ns("do.label"), "Enable labels", value = FALSE),
            checkboxInput(ns("labels.highlight"), "Highlight labels", value = TRUE),
            checkboxInput(ns("labels.repel"), "Repel labels", value = TRUE),
            numericInput(ns("labels.size"), "Labels size", min = 1, value = 5)
        ),
        "Legend/Scale" = tagList(
            checkboxInput(ns("legend.show"), "Enable legend", value = TRUE),
            textInput(ns("legend.color.title"), "Legend title", value = "make"),
            numericInput(ns("legend.color.size"), "Legend color size", min = 1, value = 5),
            numericInput(ns("legend.shape.size"), "Legend shape size", min = 1, value = 5),
            textInput(ns("legend.color.breaks"), "Legend tick breaks", placeholder = "e.g. -3, 0, 3"),
            numericInput(ns("min.value"), "Min value", value = NA),
            numericInput(ns("max.value"), "Max value", value = NA)
        ),
        "Trajectory" = tagList(
            selectInput(ns("trajectory.group.by"), "Trajectory group by", choices = cat.choices),
            textInput(ns("add.trajectory.by.groups"), "Add trajectory by groups",
                placeholder = "e.g. ['A', 'B']['C', 'D']"
            ),
            numericInput(ns("trajectory.arrow.size"), "Trajectory arrow size", value = 0.15),
        ),
        "Plotly" = tagList(
            checkboxInput(ns("webgl"), "Plot with webGL", value = TRUE),
            selectInput(ns("download.format"), "Download format", choices = c("svg", "png")),
            colourInput(ns("shape.fill"), "Shape fill",
                allowTransparent = TRUE, value = "rgba(0, 0, 0, 0)"
            ),
            colourInput(ns("shape.line.color"), "Shape line color",
                allowTransparent = TRUE, value = "black"
            ),
            numericInput(ns("shape.line.width"), "Shape line width", value = 4, min = 0, step = 1),
            selectInput(ns("shape.linetype"), "Shape linetype",
                choices = c(
                    "solid", "dot", "dash", "longdash",
                    "dashdot", "longdashdot"
                )
            ),
            numericInput(ns("shape.opacity"), "Shape opacity", value = 1, min = 0, max = 1, step = 0.01)
        ),
        "Extras" = tagList(
            checkboxInput(ns("do.ellipse"), "Enable ellipses", value = FALSE),
            checkboxInput(ns("do.contour"), "Enable contour", value = FALSE),
            checkboxInput(ns("show.grid.lines"), "Show gridlines", value = TRUE),
            selectInput(ns("hover.data"), "Hover data",
                choices = choices,
                multiple = TRUE
            ),
            numericInput(ns("hover.round.digits"), "Hover round digits",
                value = 5, step = 1
            ),
            textInput(ns("add.xline"), "Add xlines", placeholder = "e.g. 2, -2"),
            textInput(ns("add.yline"), "Add ylines", placeholder = "e.g. 2, -2"),
            selectInput(ns("xline.linetype"), "xline linetype",
                choices = c(
                    "solid", "dashed", "dotted", "dotdash",
                    "longdash", "twodash"
                )
            ),
            selectInput(ns("yline.linetype"), "yline linetype",
                choices = c(
                    "solid", "dashed", "dotted", "dotdash",
                    "longdash", "twodash"
                )
            ),
            colourInput(ns("xline.color"), "xline color", value = "black"),
            colourInput(ns("yline.color"), "yline color", value = "black")
        )
    )

    organize_inputs(
        inputs,
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
#' @param data The data frame to plot
#'
#' @importFrom shiny moduleServer isolate
#' @importFrom dittoViz scatterPlot dittoColors
#' @importFrom ggplot2 theme_bw waiver
#' @importFrom plotly renderPlotly %>% config layout toWebGL
#'
#' @export
#' @author Jared Andrews
scatterPlotServer <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        output$scatterPlot <- renderPlotly({
            input$update

            # Change textInputs and selectInputs to NULL if empty
            null_inputs <- list(
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
                "color.adj.fxn" = isolate(input$color.adj.fxn)
            )

            for (input_name in names(null_inputs)) {
                if (null_inputs[[input_name]] == "") {
                    null_inputs[[input_name]] <- NULL
                }
            }

            fig <- scatterPlot(
                data,
                x.by = isolate(input$x.by),
                y.by = isolate(input$y.by),
                color.by = null_inputs$color.by,
                shape.by = null_inputs$shape.by,
                split.by = null_inputs$split.by,
                size = isolate(input$size),
                rows.use = with(data, eval(str2expression(isolate(input$rows.use)))),
                show.others = isolate(input$show.others),
                x.adjustment = null_inputs$x.adjustment,
                y.adjustment = null_inputs$y.adjustment,
                color.adjustment = null_inputs$color.adjustment,
                x.adj.fxn = null_inputs$x.adj.fxn,
                y.adj.fxn = null_inputs$y.adj.fxn,
                color.adj.fxn = null_inputs$color.adj.fxn,
                split.show.all.others = isolate(input$split.show.all.others),
                opacity = isolate(input$opacity),
                color.panel = dittoColors(),
                colors = seq_along(dittoColors()),
                split.nrow = NULL,
                split.ncol = NULL,
                split.adjust = list(),
                multivar.split.dir = c("col", "row"),
                shape.panel = c(16, 15, 17, 23, 25, 8),
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
                    null_inputs$color.by,
                    paste0(null_inputs$color.by, ".color.adj"),
                    "color.multi", "color.which",
                    isolate(input$x.by),
                    paste0(isolate(input$x.by), ".x.adj"),
                    isolate(input$y.by),
                    paste0(isolate(input$y.by), ".y.adj"),
                    null_inputs$shape.by,
                    null_inputs$split.by
                )),
                hover.round.digits = isolate(input$hover.round.digits),
                do.contour = isolate(input$do.contour),
                contour.color = isolate(input$contour.color),
                contour.linetype = isolate(input$contour.linetype),
                add.trajectory.by.groups = null_inputs$add.trajectory.by.groups,
                trajectory.group.by = null_inputs$trajectory.group.by,
                trajectory.arrow.size = isolate(input$trajectory.arrow.size),
                add.xline = NULL,
                xline.linetype = isolate(input$xline.linetype),
                xline.color = isolate(input$xline.color),
                add.yline = NULL,
                yline.linetype = isolate(input$yline.linetype),
                yline.color = isolate(input$yline.color),
                do.ellipse = isolate(input$do.ellipse),
                legend.show = isolate(input$legend.show),
                legend.color.title = "make",
                legend.color.size = isolate(input$legend.color.size),
                legend.color.breaks = waiver(),
                legend.color.breaks.labels = waiver(),
                legend.shape.title = null_inputs$shape.by,
                legend.shape.size = isolate(input$legend.shape.size),
                show.grid.lines = isolate(input$show.grid.lines)
            )

            fig <- fig %>% config(
                edits = list(
                    axisTitleText = TRUE,
                    titleText = TRUE,
                    legendText = TRUE
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
#' Each module is created for each data frame provided in the named list of data frames.
#'
#' @param data_list A named list of data frames for which scatterPlot modules will be created.
#'
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel mainPanel shinyApp h3
#' @export
#'
#' @examples
#' data_list <- list(mtcars = mtcars, iris = iris)
#' app <- createScatterPlotApp(data_list)
#' runApp(app)
createScatterPlotApp <- function(data_list) {
    # Validate input
    stopifnot(is.list(data_list), all(sapply(data_list, is.data.frame)))

    # UI definition
    ui <- fluidPage(
        titlePanel("Modular scatterPlots"),
        sidebarLayout(
            sidebarPanel(
                lapply(names(data_list), function(name) {
                    scatterPlotInputsUI(name, data_list[[name]], title = h3(paste0(name, " Settings")))
                })
            ),
            mainPanel(
                lapply(names(data_list), function(name) {
                    tagList(scatterPlotOutputUI(name), br())
                })
            )
        )
    )

    # Server function
    server <- function(input, output, session) {
        lapply(names(data_list), function(name) {
            scatterPlotServer(name, data = data_list[[name]])
        })
    }

    # Return the Shiny app
    shinyApp(ui, server)
}
