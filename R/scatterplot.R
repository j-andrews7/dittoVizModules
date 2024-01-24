###### Plot Generation ######

#' Generate a single scatterplot using Plotly
#'
#' @inheritParams make_scatterplot
#'
#' @return A Plotly scatterplot object
#'
#' @importFrom plotly plot_ly add_trace %>% layout add_text
#' @author Jared Andrews
#' @examples
#' data <- data.frame(x = rnorm(100), y = rnorm(100), color = rnorm(100), shape = factor(rep(1:2, 50)))
#' .scatterplot(data, "x", "y", "color", "shape")
.scatterplot <- function(data,
                         x.var,
                         y.var,
                         color.by = NULL,
                         shape.by = NULL,
                         gridlines = TRUE,
                         point.size = 4,
                         size.by = NULL,
                         hlines = NULL,
                         vlines = NULL,
                         line.width = 1,
                         line.color = "black",
                         xlim = NULL,
                         ylim = NULL,
                         opacity = 1,
                         label.by = NULL) {
    if (!is.null(xlim) && xlim != "") {
        xlim <- as.numeric(unlist(strsplit(xlim, ",")))
    } else {
        xlim <- NULL
    }

    if (!is.null(ylim) && ylim != "") {
        ylim <- as.numeric(unlist(strsplit(ylim, ",")))
    } else {
        ylim <- NULL
    }

    # Create a list to hold marker properties
    marker.list <- list()

    # Add optional styling
    if (!is.null(color.by) && color.by != "") {
        marker.list$color <- ~ .data[[color.by]]
    }

    if (!is.null(shape.by) && shape.by != "") {
        marker.list$symbol <- ~ .data[[shape.by]]
    }

    if (!is.null(size.by) && size.by != "") {
        marker.list$size <- ~ .data[[size.by]]
    } else {
        marker.list$size <- point.size
    }

    marker.list$opacity <- opacity

    p <- plot_ly(data, x = ~ .data[[x.var]], y = ~ .data[[y.var]], type = "scatter", mode = "markers", marker = marker.list)

    lines <- NULL

    if (!is.null(vlines) && vlines != "") {
        vlines <- as.numeric(unlist(strsplit(vlines, ",")))
        vlines <- lapply(vlines, function(v) {
            list(
                type = "line", x0 = v, x1 = v, y0 = 0, y1 = 1, xref = "x", yref = "paper",
                line = list(color = line.color, width = line.width)
            )
        })

        lines <- c(lines, vlines)
    }

    if (!is.null(hlines) && hlines != "") {
        hlines <- as.numeric(unlist(strsplit(hlines, ",")))
        hlines <- lapply(hlines, function(h) {
            list(
                type = "line", x0 = 0, x1 = 1, y0 = h, y1 = h, xref = "paper", yref = "y",
                line = list(color = line.color, width = line.width)
            )
        })

        lines <- c(lines, hlines)
    }

    p <- p %>% layout(
        xaxis = list(range = xlim, showgrid = gridlines),
        yaxis = list(range = ylim, showgrid = gridlines),
        shapes = lines
    )

    if (!is.null(label.by) && label.by != "") {
        p <- p %>% add_text(text = ~ .data[[label.by]])
    }

    p
}


#' Generate scatterplot(s) via Plotly
#'
#' Determines if subplots are needed based on the `split.by` parameter.
#'
#' @param data Data frame containing the data to plot
#' @param x.var Name of the variable for the x-axis
#' @param y.var Name of the variable for the y-axis
#' @param color.by Name of the variable for point color
#' @param shape.by Name of the variable for point shape
#' @param split.by Name of the variable to facet the plot
#' @param gridlines Whether or not to show gridlines.
#' @param nrows Number of rows for the subplot grid.
#' @param point.size Size of the points in the plot (default is 6)
#' @param hlines Horizontal lines to add to the plot.
#' @param vlines Vertical lines to add to the plot.
#' @param line.width Width of the lines to add to the plot.
#' @param line.color Color of the lines to add to the plot.
#' @param xlim Limits for the x-axis.
#' @param ylim Limits for the y-axis.
#' @param opacity Opacity of the points in the plot.
#' @param label.by Name of the variable to use for point labels.
#'
#' @return A Plotly plot object (either a single plot or a subplot)
#'
#' @importFrom plotly subplot
#'
#' @export
#' @author Jared Andrews
#' @examples
#' data <- data.frame(x = rnorm(100), y = rnorm(100), color = rnorm(100), shape = factor(rep(1:2, 50)), split = factor(rep(1:2, each = 50)))
#' make_scatterplot(data, x.var = "x", y.var = "y", color.by = "color", shape.by = "shape", split.by = "split")
make_scatterplot <- function(data,
                             x.var,
                             y.var,
                             color.by = NULL,
                             shape.by = NULL,
                             split.by = NULL,
                             gridlines = TRUE,
                             nrows = NULL,
                             point.size = 6,
                             size.by = NULL,
                             hlines = NULL,
                             vlines = NULL,
                             line.width = 1,
                             line.color = "black",
                             xlim = NULL,
                             ylim = NULL,
                             opacity = 1,
                             label.by = NULL) {
    if (is.null(split.by) || split.by == "") {
        p <- .scatterplot(
            data = data,
            x.var = x.var,
            y.var = y.var,
            color.by = color.by,
            shape.by = shape.by,
            gridlines = gridlines,
            point.size = point.size,
            size.by = size.by,
            hlines = hlines,
            vlines = vlines,
            line.width = line.width,
            line.color = line.color,
            xlim = xlim,
            ylim = ylim,
            opacity = opacity,
            label.by = label.by
        )
    } else {
        if (is.null(nrows) || nrows == 0) {
            nrows <- 1
        }

        plots_list <- lapply(unique(data[[split.by]]), function(val) {
            sub_data <- data[data[[split.by]] == val, ]
            .scatterplot(
                data = sub_data,
                x.var = x.var,
                y.var = y.var,
                color.by = color.by,
                shape.by = shape.by,
                gridlines = gridlines,
                point.size = point.size,
                size.by = size.by,
                hlines = hlines,
                vlines = vlines,
                line.width = line.width,
                line.color = line.color,
                xlim = xlim,
                ylim = ylim,
                opacity = opacity,
                label.by = label.by
            )
        })

        p <- subplot(plots_list, nrows = nrows, shareX = TRUE, shareY = TRUE, titleX = FALSE, titleY = FALSE)
    }

    p
}

###### Module UI ######

#' Input UI components for the scatterplot module
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
scatterplotInputsUI <- function(id, data, title = NULL, columns = 2) {
    ns <- NS(id)
    choices <- c("", names(data))
    inputs <- list(
        "Point Styling" = tagList(
            selectInput(ns("x.var"), "X Variable:", choices, selected = names(data)[1]),
            selectInput(ns("y.var"), "Y Variable:", choices, selected = names(data)[2]),
            selectInput(ns("color.by"), "Color By:", choices, selected = NULL),
            selectInput(ns("shape.by"), "Shape By:", choices, selected = NULL),
            selectInput(ns("size.by"), "Size By:", choices, selected = NULL),
            numericInput(ns("point.size"), "Point Size:", 6),
            sliderInput(ns("opacity"), "Point Opacity:", min = 0, max = 1, value = 1, step = 0.1),
            selectInput(ns("label.by"), "Label By:", choices, selected = NULL)
        ),
        "Axis & Lines" = tagList(
            textInput(ns("hlines"), "Horizontal Lines (comma-separated):", ""),
            textInput(ns("vlines"), "Vertical Lines (comma-separated):", ""),
            numericInput(ns("line.width"), "Line Width:", 1),
            colourInput(ns("line.color"), "Line Color:", "black"),
            textInput(ns("xlim"), "X-axis Limits (comma-separated):", ""),
            textInput(ns("ylim"), "Y-axis Limits (comma-separated):", ""),
            checkboxInput(ns("gridlines"), "Show Gridlines", TRUE)
        ),
        "Subplots" = tagList(
            selectInput(ns("split.by"), "Split By:", choices, selected = NULL),
            numericInput(ns("nrows"), "Subplot Rows:", 0),
        )
    )

    organize_inputs(inputs, title = title, tack = tagList(actionButton(ns("update"), "Update Plot"), br()), columns = columns)
}


#' Output UI components for the scatterplot module
#' @param id The ID for the Shiny module
#'
#' @return A Shiny plotlyOutput for the scatterplot
#'
#' @importFrom shiny NS
#' @importFrom plotly plotlyOutput
#'
#' @export
#' @author Jared Andrews
scatterplotOutputUI <- function(id) {
    ns <- NS(id)
    plotlyOutput(ns("scatterplot"))
}


###### Module Server ######

#' Server logic for scatterplot module
#' @param id The ID for the Shiny module
#' @param data The data frame to plot
#'
#' @importFrom shiny moduleServer isolate
#' @importFrom plotly renderPlotly
#'
#' @export
#' @author Jared Andrews
scatterplotServer <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        output$scatterplot <- renderPlotly({
            input$update

            make_scatterplot(
                data = data,
                x.var = isolate(input$x.var),
                y.var = isolate(input$y.var),
                color.by = isolate(input$color.by),
                shape.by = isolate(input$shape.by),
                split.by = isolate(input$split.by),
                gridlines = isolate(input$gridlines),
                nrows = isolate(input$nrows),
                point.size = isolate(input$point.size),
                size.by = isolate(input$size.by),
                hlines = isolate(input$hlines),
                vlines = isolate(input$vlines),
                line.width = isolate(input$line.width),
                line.color = isolate(input$line.color),
                xlim = isolate(input$xlim),
                ylim = isolate(input$ylim),
                opacity = isolate(input$opacity),
                label.by = isolate(input$label.by)
            )
        })
    })
}

###### Example App ######

#' Create a Modular Scatterplot Shiny Application
#'
#' This function generates a Shiny application with modular scatterplot components.
#' Each module is created for each data frame provided in the named list of data frames.
#'
#' @param data_list A named list of data frames for which scatterplot modules will be created.
#'
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel mainPanel shinyApp h3
#' @export
#'
#' @examples
#' data_list <- list(mtcars = mtcars, iris = iris)
#' app <- createScatterplotApp(data_list)
#' runApp(app)
createScatterplotApp <- function(data_list) {
    # Validate input
    stopifnot(is.list(data_list), all(sapply(data_list, is.data.frame)))

    # UI definition
    ui <- fluidPage(
        titlePanel("Modular Scatterplots"),
        sidebarLayout(
            sidebarPanel(
                lapply(names(data_list), function(name) {
                    scatterplotInputsUI(name, data_list[[name]], title = h3(paste0(name, " Settings")))
                })
            ),
            mainPanel(
                lapply(names(data_list), function(name) {
                    scatterplotOutputUI(name)
                })
            )
        )
    )

    # Server function
    server <- function(input, output, session) {
        lapply(names(data_list), function(name) {
            scatterplotServer(name, data = data_list[[name]])
        })
    }

    # Return the Shiny app
    shinyApp(ui, server)
}
