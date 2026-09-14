#' Input UI components for the yPlot module
#'
#' This should be placed in the UI where the inputs should be shown, with an `id`
#' that matches the `id` used in the `dittoViz_yPlotServer()` and `dittoViz_yPlotOutputUI()` functions.
#'
#' @details The user inputs for this module are separated from the outputs to allow for
#' more flexible UI design.
#'
#' The inputs will automatically be organized into a grid layout via the `organize_inputs()` function,
#' with `columns` controlling the number of columns in the grid.
#'
#' Defaults can be set for each input by providing a named list of values to the `defaults` argument.
#' Nearly all parameters for [dittoViz::yPlot()] can be set via these inputs, so see the help
#' for that function for an exhaustive list.
#'
#' The "Y Data" input accepts several columns at once. "Multivar Aesthetic" (on the
#' Facet tab) then decides how they are shown: `"split"` gives each variable its own
#' facet (alongside any `split.by` facets), while `"group"` and `"color"` put the
#' variables on the x-axis or the fill legend respectively. With several variables the
#' Stats tab is only available for the `"split"` aesthetic with no `split.by` set, and
#' comparisons are then run separately within each variable's facet. The other layouts
#' either replace the x-axis groups being compared or facet on two dimensions at once,
#' neither of which the significance brackets can be placed against.
#'
#' @section Plot parameters not implemented or with altered functionality:
#' The following [dittoViz::yPlot()] parameters are not available via UI inputs:
#'
#' - `xlab` - X-axis label (plotly allows interactive editing)
#' - `ylab` - Y-axis label (auto-generated to reflect any applied Y adjustment,
#'   e.g. `"log2(z-score(units))"`; plotly allows interactive editing). With several
#'   Y variables only the adjustment is shown, as the variables are named by the
#'   facet strips or the legend instead
#' - `main` - Plot title (plotly allows interactive editing)
#' - `sub` - Plot subtitle (not supported in plotly)
#' - `theme` - ggplot2 theme (not applicable to plotly)
#' - `legend.title` - Legend title (managed by plotly interactively)
#' - `add.line` - Use `hline.intercepts` instead for horizontal lines with full styling options
#' - `line.linetype` - Use `hline.linetypes` instead
#' - `line.color` - Use `hline.colors` instead
#' - `line.linewidth` - Use `hline.widths` instead
#' - `line.opacity` - Use `hline.opacities` instead
#' - `rows.use` - Row subset to plot (not implemented)
#' - `colors` - Integer index/order into `color.panel` (managed via the palette UI)
#' - `shape.panel` - Shapes used with `shape.by` (not implemented)
#' - `y.breaks` - Custom continuous-axis breaks (not implemented)
#' - `x.labels` - Override group labels (not implemented)
#' - `x.labels.rotate` - Rotate group labels (handled by the Axes tab tick-angle controls)
#' - `x.reorder` - Reorder x-axis groups (not implemented)
#' - `boxplot.width` - Boxplot width (controlled via `boxgap` and `boxgroupgap`)
#' - `boxplot.outlier.size` - Outlier point size (not implemented)
#' - `boxplot.position.dodge` - Boxplot dodge (controlled via `boxgap`)
#' - `vlnplot.quantiles` - Violin quantiles (doesn't translate to plotly)
#'
#' @section Plot parameters and defaults:
#' The following [dittoViz::yPlot()] parameters can be accessed via UI inputs and/or the `defaults` argument:
#'
#' - `var` - Y-axis variable(s) (UI: "Y Data", default: 2nd numeric variable). Several
#'   may be selected; `multivar.aes` then controls how they are displayed
#' - `multivar.aes` - Aesthetic used for multiple `var` columns (UI: "Multivar Aesthetic",
#'   default: "split")
#' - `multivar.split.dir` - Facet direction for multiple `var` columns (UI:
#'   "Multivar Split Dir", default: "col")
#' - `group.by` - Grouping variable for x-axis (UI: "Group by", default: 2nd categorical variable)
#' - `color.by` - Coloring variable (UI: "Color by", default: "")
#' - `shape.by` - Shape variable (UI: "Shape by", default: "")
#' - `split.by` - Faceting variable (UI: "Split by (facet)", default: "")
#' - `plots` - Plot types to show (UI: "Plots to show", default: c("boxplot", "jitter"))
#' - `color.panel` - Custom color values (UI: palette picker, derived from palette)
#' - `min` - Y-axis minimum (UI: "Y Axis Min", auto-calculated)
#' - `max` - Y-axis maximum (UI: "Y Axis Max", auto-calculated)
#' - `var.adjustment` - Y-axis data adjustment (UI: "Y Adjustment", default: "")
#' - `var.adj.fxn` - Y-axis adjustment function (UI: "Y Adjustment Function", default: "")
#' - `split.nrow` - Number of facet rows (UI: "Rows", default: 4)
#' - `split.ncol` - Number of facet columns (UI: "Columns", default: 4)
#' - `split.adjust` - Facet scale behavior (UI: "Facet Scaling", default: "fixed")
#' - `do.raster` - Rasterize jitter points (UI: "Rasterize Jitter", default: FALSE)
#' - `raster.dpi` - DPI for rasterization (UI: "Raster DPI", default: 600)
#' - `jitter.size` - Jitter point size (UI: "Jitter Point Size", default: 1)
#' - `jitter.width` - Jitter width (UI: "Jitter Width", default: 0.2)
#' - `jitter.color` - Jitter border color (UI: "Jitter Border Color", default: "#000000")
#' - `jitter.shape.legend.size` - Shape legend size (UI: "Shape Legend Size", default: 5)
#' - `jitter.shape.legend.show` - Show shape legend (UI: "Show Shape Legend", default: TRUE)
#' - `jitter.position.dodge` - Jitter position dodge (calculated from boxgap)
#' - `boxplot.show.outliers` - Show boxplot outliers
#' - `boxplot.color` - Boxplot outline color (UI: "Boxplot Color", default: "#000000")
#' - `boxplot.fill` - Fill boxplot (UI: "Fill Boxplot", default: TRUE)
#' - `boxplot.lineweight` - Boxplot line weight (UI: "Boxplot Line Weight", default: 0.5)
#' - `vlnplot.lineweight` - Violin line weight (UI: "Violin Line Weight", default: 0.5)
#' - `vlnplot.scaling` - Violin scaling method (UI: "Violin Scaling", default: "area")
#' - `vlnplot.width` - Violin width (derived from `boxgap`; not directly settable)
#' - `ridgeplot.lineweight` - Ridge line weight (UI: "Ridge Line Weight", default: 0.5)
#' - `ridgeplot.scale` - Ridge overlap scale (UI: "Ridge Scale (overlap)", default: 1.25)
#' - `ridgeplot.ymax.expansion` - Ridge Y-max expansion (UI: "Ridge Y-max Expansion", default: NA)
#' - `ridgeplot.shape` - Ridge shape (UI: "Ridge Shape", default: "smooth")
#' - `ridgeplot.bins` - Ridge bins (UI: "Ridge Bins", default: 30)
#' - `ridgeplot.binwidth` - Ridge binwidth (UI: "Ridge Binwidth", default: NULL)
#' - `hover.data` - Columns shown on hover (UI: "Hover Data", default: "";
#'   empty uses a sensible default set of columns)
#' - `hover.round.digits` - Hover value rounding (UI: "Hover Round Digits", default: 5)
#' - `legend.show` - Show legend (always `TRUE`; not directly settable)
#'
#' @section Parameters controlling additional functionality:
#' The following parameters implementing new functionality or controlling plotly-specific features are also available:
#'
#' - `boxmode` - Always "overlay": the boxes carry explicit x positions matching
#'   ggplot's dodge, so plotly.js is not asked to dodge them
#' - `boxgap` - Boxplot position dodge (UI: "Boxplot Position Dodge", default: 0.3)
#' - `boxgroupgap` - Boxplot group dodge (UI: "Boxplot Group Dodge", default: 0.2)
#' - `title.font.size` - Plot title font size (UI: "Title Size", default: 26)
#' - `title.font.family` - Font family for title text (UI: "Title Font", default: "Arial")
#' - `title.font.color` - Color for plot title (UI: "Title Color", default: "#000000")
#' - `axis.title.font.size` - Axis title font size (UI: "Axis Title Size", default: 18)
#' - `axis.title.font.color` - Axis title font color (UI: "Axis Title Color", default: "#000000")
#' - `axis.title.font.family` - Axis title font family (UI: "Axis Title Font", default: "Arial")
#' - `axis.showline` - Show axis border lines (UI: "Show Axis Lines", default: TRUE)
#' - `axis.mirror` - Mirror axis lines on opposite side (UI: "Mirror Axis Lines", default: TRUE)
#' - `show.grid.x` - Show X-axis major gridlines (UI: "Show X Major Gridlines", default: TRUE)
#' - `show.grid.y` - Show Y-axis major gridlines (UI: "Show Y Major Gridlines", default: TRUE)
#' - `axis.linecolor` - Color of axis lines (UI: "Axis Line Color", default: "black")
#' - `axis.linewidth` - Width of axis lines (UI: "Axis Line Width", default: 0.5)
#' - `axis.tickfont.size` - Size of tick labels (UI: "Tick Label Size", default: 12)
#' - `axis.tickfont.color` - Color of tick labels (UI: "Tick Label Color", default: "black")
#' - `axis.tickfont.family` - Font family for tick labels (UI: "Tick Label Font", default: "Arial")
#' - `axis.tickangle.x` - Rotation angle for X-axis tick labels (UI: "X-axis Tick Label Angle", default: 0)
#' - `axis.tickangle.y` - Rotation angle for Y-axis tick labels (UI: "Y-axis Tick Label Angle", default: 0)
#' - `axis.ticks` - Position of tick marks (UI: "Tick Position", default: "outside")
#' - `axis.tickcolor` - Color of tick marks (UI: "Tick Mark Color", default: "black")
#' - `axis.ticklen` - Length of tick marks (UI: "Tick Mark Length", default: 5)
#' - `axis.tickwidth` - Width of tick marks (UI: "Tick Mark Width", default: 1)
#' - `hline.intercepts` - Y-coordinates for horizontal reference lines (UI: "Y-intercepts", default: "")
#' - `hline.colors` - Colors for horizontal lines (UI: "Colors", default: "#000000")
#' - `hline.widths` - Widths for horizontal lines (UI: "Widths", default: "1")
#' - `hline.linetypes` - Line types for horizontal lines (UI: "Line Types", default: "dashed")
#' - `hline.opacities` - Opacities for horizontal lines (UI: "Opacities (0-1)", default: "1")
#' - `vline.intercepts` - X-coordinates for vertical reference lines (UI: "X-intercepts", default: "")
#' - `vline.colors` - Colors for vertical lines (UI: "Colors", default: "#000000")
#' - `vline.widths` - Widths for vertical lines (UI: "Widths", default: "1")
#' - `vline.linetypes` - Line types for vertical lines (UI: "Line Types", default: "dashed")
#' - `vline.opacities` - Opacities for vertical lines (UI: "Opacities (0-1)", default: "1")
#' - `abline.slopes` - Slopes for diagonal reference lines (UI: "Slopes", default: "")
#' - `abline.intercepts` - Y-intercepts for diagonal lines (UI: "Y-intercepts", default: "")
#' - `abline.colors` - Colors for diagonal lines (UI: "Colors", default: "#000000")
#' - `abline.widths` - Widths for diagonal lines (UI: "Widths", default: "1")
#' - `abline.linetypes` - Line types for diagonal lines (UI: "Line Types", default: "dashed")
#' - `abline.opacities` - Opacities for diagonal lines (UI: "Opacities (0-1)", default: "1")
#' - `palette.colours` - Named character vector mapping group levels to colors, e.g.
#'   `c(A = "#FF0000", B = "blue")` (UI: "Plot colors"). Seeds the picker; unnamed groups fall
#'   back to the default palette and user edits take precedence.
#' - `annotate.by` - Column whose values identify and label jitter points (UI: "Annotate By", default: "")
#' - `highlight.points` - Values from the `annotate.by` column to highlight (UI: "Points to Highlight", default: "")
#' - `highlight.color` - Fill color for highlighted points (UI: "Highlight Fill", default: "#00FFF7")
#' - `highlight.size` - Size of highlighted points (UI: "Highlight Size", default: 7)
#' - `highlight.border.color` - Border color for highlighted points (UI: "Highlight Border Color", default: "#000000")
#' - `highlight.border.width` - Border width for highlighted points (UI: "Highlight Border Width", default: 1)
#' - `highlight.auto.annotate` - Label highlighted points automatically (UI: "Auto-annotate Highlights", default: TRUE)
#' - `annotation.color` - Annotation text color (UI: "Annotation Color", default: "black")
#' - `annotation.ax` - Horizontal label offset in pixels (UI: "Annotation X Offset", default: 20)
#' - `annotation.ay` - Vertical label offset in pixels (UI: "Annotation Y Offset", default: -20)
#' - `annotation.size` - Annotation font size (UI: "Annotation Size", default: 10)
#' - `annotation.showarrow` - Draw an arrow from label to point (UI: "Show Arrow", default: TRUE)
#' - `annotation.arrowcolor` - Annotation arrow color (UI: "Arrow Color", default: "black")
#' - `annotation.arrowhead` - Annotation arrowhead style (UI: "Arrowhead Style", default: 2)
#' - `annotation.arrowwidth` - Annotation arrow line width (UI: "Arrow Linewidth", default: 1.5)
#'
#' @param id The ID for the Shiny module.
#' @param data The data frame used for plot generation.
#' @param defaults A named list of default values for the inputs. An entry may also be a
#'   [shiny::reactive()] or [shiny::reactiveVal()]; it is resolved with [shiny::isolate()] to
#'   seed the control, and the module then keeps it live (see [setup_reactive_defaults()]).
#' @param title An optional title for the UI grid.
#' @param columns Number of columns for the UI grid.
#' @return A Shiny tagList containing the UI elements
#'
#' @import shiny
#' @importFrom colourpicker colourInput
#' @importFrom shinyWidgets materialSwitch
#' @importFrom shinyBS tipify
#'
#' @export
#' @author Jared Andrews, Jacob Martin
#' @seealso [dittoViz::yPlot()], [VizModules::organize_inputs()],
#' [VizModules::dittoViz_yPlotOutputUI()], [VizModules::dittoViz_yPlotServer()],
#' [VizModules::dittoViz_yPlotApp()]
#' @examples
#' library(VizModules)
#' data(mtcars)
#' dittoViz_yPlotInputsUI("yPlot", mtcars)
dittoViz_yPlotInputsUI <- function(id, data, defaults = NULL, title = NULL, columns = 2) {
    ns <- NS(id)

    # Get variables of data.
    choices <- c("", names(data))

    # Get numeric variables of data.
    num.choices <- c("", names(data)[vapply(data, is.numeric, logical(1))])
    cat.choices <- c("", names(data)[vapply(data, function(x) !is.numeric(x), logical(1))])

    # Recognized data adjustments for the (numeric) continuous variable.
    adj.choices <- c("", "z-score", "relative.to.max")
    adj.fxn.choices <- c("", "log2", "log", "log10", "neg_log10", "log1p", "as.factor", "abs", "sqrt")

    # `var` may hold several columns, in which case the limits span all of them.
    default.var <- get_default(defaults, "var", num.choices[2], function(x) all(x %in% num.choices))
    y.range <- .calculate_range(
        df = data, data_col_y = default.var,
        axis_scale_factor = .y_axis_scale_factor, grouping = FALSE
    )
    max.y <- if (!is.null(y.range)) y.range$max else 1
    min.y <- if (!is.null(y.range)) y.range$min else 0

    selected <- list(
        "var", "group.by", "color.by", "shape.by",
        "plots", c("min", "max"), "var.adjustment", "var.adj.fxn",
        "split.by", c("split.nrow", "split.ncol"),
        "multivar.aes", "multivar.split.dir",
        "split.adjust", "do.raster", "raster.dpi",
        "jitter.size", "jitter.width", "jitter.color",
        "jitter.shape.legend.size", "jitter.shape.legend.show",
        "boxplot.show.outliers", "boxplot.color", "boxplot.fill",
        "boxplot.lineweight",
        "vlnplot.lineweight", "vlnplot.scaling",
        "ridgeplot.lineweight", "ridgeplot.scale",
        "ridgeplot.ymax.expansion", "ridgeplot.shape",
        "ridgeplot.bins", "ridgeplot.binwidth",
        "hover.data", "hover.round.digits"
    )
    documentParameters <- get_documentation(
        package_name = "dittoViz::yPlot", type = "param",
        selected = selected, cap = TRUE
    )

    inputs <- list(
        "Data" = tagList(
            tipify(
                viz_select_input(ns("var"), "Y Data",
                    choices = num.choices[nzchar(num.choices)],
                    selected = default.var,
                    multiple = TRUE
                ),
                documentParameters$var,
                placement = "top", options = list(container = "body")
            ),
            tipify(
                viz_select_input(ns("group.by"), "Group By",
                    choices = cat.choices,
                    selected = get_default(
                        defaults, "group.by", cat.choices[2],
                        function(x) x %in% cat.choices
                    )
                ),
                documentParameters$group.by,
                placement = "top", options = list(container = "body")
            ),
            tipify(
                viz_select_input(ns("color.by"), "Color By",
                    choices = cat.choices,
                    selected = get_default(
                        defaults, "color.by", "",
                        function(x) x %in% cat.choices
                    )
                ),
                documentParameters$color.by,
                placement = "top", options = list(container = "body")
            ),
            tipify(
                viz_select_input(ns("shape.by"), "Shape By",
                    choices = cat.choices,
                    selected = get_default(
                        defaults, "shape.by", "",
                        function(x) x %in% cat.choices
                    )
                ),
                documentParameters$shape.by,
                placement = "top", options = list(container = "body")
            ),
            tipify(viz_select_input(
                ns("plots"),
                "Plots",
                choices = c("Violin" = "vlnplot", "Box" = "boxplot", "Jitter" = "jitter", "Ridge" = "ridgeplot"),
                selected = get_default(
                    defaults, "plots", c("boxplot", "jitter"),
                    function(x) all(x %in% c("vlnplot", "boxplot", "jitter", "ridgeplot"))
                ),
                multiple = TRUE
            ), documentParameters$plots, placement = "top", options = list(container = "body")),
            helpText("Order not currently respected"),
            uiOutput(ns("palette.selection"))
        ),
        "Adjustments" = tagList(
            tipify(
                viz_select_input(ns("var.adjustment"), "Y Adjustment",
                    choices = adj.choices,
                    selected = get_default(
                        defaults, "var.adjustment", "",
                        function(x) x %in% adj.choices
                    )
                ),
                documentParameters$var.adjustment,
                placement = "top", options = list(container = "body")
            ),
            tipify(
                viz_select_input(ns("var.adj.fxn"), "Y Adjustment Function",
                    choices = adj.fxn.choices,
                    selected = get_default(
                        defaults, "var.adj.fxn", "",
                        function(x) x %in% adj.fxn.choices
                    )
                ),
                documentParameters$var.adj.fxn,
                placement = "top", options = list(container = "body")
            ),
            tipify(
                numericInput(ns("y.max"), "Y Axis Max",
                    value = get_default(defaults, "max", max.y, is.numeric),
                    min = -1000, max = 1000
                ),
                documentParameters$max,
                placement = "top", options = list(container = "body")
            ),
            tipify(
                numericInput(ns("y.min"), "Y Axis Min",
                    value = get_default(defaults, "min", min.y, is.numeric),
                    min = -1000, max = 1000
                ),
                documentParameters$min,
                placement = "top", options = list(container = "body")
            )
        ),
        "Jitter" = tagList(
            tipify(
                numericInput(ns("jitter.size"), "Jitter Point Size",
                    max = 10, min = 0.1,
                    value = get_default(defaults, "jitter.size", 1, is.numeric)
                ),
                documentParameters$jitter.size,
                placement = "top", options = list(container = "body")
            ),
            tipify(
                numericInput(ns("jitter.width"), "Jitter Width",
                    min = 0, max = 1, step = 0.05,
                    value = get_default(defaults, "jitter.width", 0.2, is.numeric)
                ),
                documentParameters$jitter.width,
                placement = "top", options = list(container = "body")
            ),
            tipify(
                colourInput(ns("jitter.color"), "Jitter Border Color",
                    value = get_default(defaults, "jitter.color", "#000000")
                ),
                documentParameters$jitter.color,
                placement = "top", options = list(container = "body")
            ),
            tipify(viz_select_input(ns("hover.data"), "Hover Data",
                choices = choices,
                multiple = TRUE,
                selected = get_default(
                    defaults, "hover.data", "",
                    function(x) all(x %in% choices)
                )
            ), documentParameters$hover.data, placement = "top", options = list(container = "body")),
            tipify(
                numericInput(ns("hover.round.digits"), "Hover Round Digits",
                    value = get_default(defaults, "hover.round.digits", 5, is.numeric),
                    step = 1,
                    min = 1
                ), documentParameters$hover.round.digits,
                placement = "top", options = list(container = "body")
            ),
            tipify(
                numericInput(ns("jitter.shape.legend.size"), "Shape Legend Size",
                    value = get_default(defaults, "jitter.shape.legend.size", 5, is.numeric),
                    min = 0, max = 20
                ),
                documentParameters$jitter.shape.legend.size,
                placement = "top", options = list(container = "body")
            ),
            tipify(
                materialSwitch(ns("jitter.shape.legend.show"), "Show Shape Legend",
                    value = get_default(defaults, "jitter.shape.legend.show", TRUE, is.logical),
                    status = "success"
                ),
                documentParameters$jitter.shape.legend.show,
                placement = "top", options = list(container = "body")
            ),
            tipify(
                materialSwitch(ns("do.raster"), "Rasterize Jitter",
                    value = get_default(defaults, "do.raster", FALSE, is.logical),
                    status = "success"
                ),
                documentParameters$do.raster,
                placement = "top", options = list(container = "body")
            ),
            tipify(
                numericInput(ns("raster.dpi"), "Raster DPI",
                    value = get_default(defaults, "raster.dpi", 600, is.numeric),
                    min = 100, max = 1200
                ),
                documentParameters$raster.dpi,
                placement = "top", options = list(container = "body")
            )
        ),
        "Box" = tagList(
            tipify(
                materialSwitch(ns("boxplot.show.outliers"), "Show Outliers",
                    value = get_default(defaults, "boxplot.show.outliers", FALSE, is.logical),
                    status = "success"
                ),
                documentParameters$boxplot.show.outliers,
                placement = "top", options = list(container = "body")
            ),
            tipify(
                colourInput(ns("boxplot.color"), "Boxplot Color",
                    value = get_default(defaults, "boxplot.color", "#000000")
                ),
                documentParameters$boxplot.color,
                placement = "top", options = list(container = "body")
            ),
            tipify(
                materialSwitch(ns("boxplot.fill"), "Fill Boxplot",
                    value = get_default(defaults, "boxplot.fill", TRUE, is.logical),
                    status = "success"
                ),
                documentParameters$boxplot.fill,
                placement = "top", options = list(container = "body")
            ),
            tipify(
                numericInput(ns("boxplot.lineweight"), "Boxplot Line Weight",
                    value = get_default(defaults, "boxplot.lineweight", 0.5, is.numeric),
                    min = 0, max = 5, step = 0.1
                ),
                documentParameters$boxplot.lineweight,
                placement = "top", options = list(container = "body")
            ),
            tipify(
                numericInput(ns("boxgap"), "Boxplot Position Dodge",
                    value = get_default(defaults, "boxgap", 0.3, is.numeric),
                    min = 0, max = 1, step = 0.05
                ),
                "Set the gap between boxplots within the same group, controlling how closely boxes are spaced",
                placement = "top", options = list(container = "body")
            ),
            tipify(
                numericInput(ns("boxgroupgap"), "Boxplot Group Dodge",
                    value = get_default(defaults, "boxgroupgap", 0.2, is.numeric),
                    min = 0, max = 1, step = 0.05
                ),
                "Set the gap between groups of boxplots when a color.by variable is used",
                placement = "top", options = list(container = "body")
            )
        ),
        "Violin" = tagList(
            tipify(
                numericInput(ns("vlnplot.lineweight"), "Violin Line Weight",
                    value = get_default(defaults, "vlnplot.lineweight", 0.5, is.numeric),
                    min = 0, max = 5, step = 0.1
                ),
                documentParameters$vlnplot.lineweight,
                placement = "top", options = list(container = "body")
            ),
            tipify(
                viz_select_input(ns("vlnplot.scaling"), "Violin Scaling",
                    selected = get_default(
                        defaults, "vlnplot.scaling", "area",
                        function(x) x %in% c("area", "count", "width")
                    ),
                    choices = c("area", "count", "width")
                ),
                documentParameters$vlnplot.scaling,
                placement = "top", options = list(container = "body")
            )
        ),
        "Ridge" = tagList(
            tipify(
                numericInput(ns("ridgeplot.lineweight"), "Ridge Line Weight",
                    value = get_default(defaults, "ridgeplot.lineweight", 0.5, is.numeric),
                    min = 0, max = 5, step = 0.1
                ),
                documentParameters$ridgeplot.lineweight,
                placement = "top", options = list(container = "body")
            ),
            tipify(
                numericInput(ns("ridgeplot.scale"), "Ridge Scale (overlap)",
                    value = get_default(defaults, "ridgeplot.scale", 1.25, is.numeric),
                    min = 0.5, max = 3
                ),
                documentParameters$ridgeplot.scale,
                placement = "top", options = list(container = "body")
            ),
            tipify(
                numericInput(ns("ridgeplot.ymax.expansion"), "Ridge Y-max Expansion",
                    value = get_default(
                        defaults, "ridgeplot.ymax.expansion", NA,
                        function(x) is.numeric(x) || is.na(x)
                    ),
                    min = 0, max = 1
                ),
                documentParameters$ridgeplot.ymax.expansion,
                placement = "top", options = list(container = "body")
            ),
            tipify(
                viz_select_input(ns("ridgeplot.shape"), "Ridge Shape",
                    selected = get_default(
                        defaults, "ridgeplot.shape", "smooth",
                        function(x) x %in% c("smooth", "hist")
                    ),
                    choices = c("smooth", "hist")
                ),
                documentParameters$ridgeplot.shape,
                placement = "top", options = list(container = "body")
            ),
            tipify(
                numericInput(ns("ridgeplot.bins"), "Ridge Bins",
                    value = get_default(defaults, "ridgeplot.bins", 30, is.numeric),
                    min = 5, max = 100
                ),
                documentParameters$ridgeplot.bins,
                placement = "top", options = list(container = "body")
            ),
            tipify(
                numericInput(ns("ridgeplot.binwidth"), "Ridge Binwidth",
                    value = get_default(
                        defaults, "ridgeplot.binwidth", NULL,
                        function(x) is.numeric(x) || is.null(x)
                    ),
                    min = 0
                ),
                documentParameters$ridgeplot.binwidth,
                placement = "top", options = list(container = "body")
            )
        ),
        "Stats" = .uniform_stats_inputs_ui(ns, defaults),
        "Facet" = tagList(
            tipify(
                viz_select_input(ns("split.by"), "Split by (facet)",
                    choices = c("", .facet_check(data)),
                    selected = get_default(
                        defaults, "split.by", "",
                        function(x) x %in% cat.choices
                    )
                ),
                documentParameters$split.by,
                placement = "top", options = list(container = "body")
            ),
            tipify(
                viz_select_input(ns("split.adjust"), "Facet Scaling",
                    selected = get_default(
                        defaults, "split.adjust", "fixed",
                        function(x) x %in% c("fixed", "free", "free_y", "free_x")
                    ),
                    choices = c("fixed", "free", "free_y", "free_x")
                ),
                documentParameters$split.adjust,
                placement = "top", options = list(container = "body")
            ),
            tipify(numericInput(ns("split.ncol"), "Columns",
                step = 1, min = 0,
                value = get_default(defaults, "split.ncol", NA, is.numeric)
            ), documentParameters$split.ncol, placement = "top", options = list(container = "body")),
            tipify(numericInput(ns("split.nrow"), "Rows",
                step = 1, min = 0,
                value = get_default(defaults, "split.nrow", NA, is.numeric)
            ), documentParameters$split.nrow, placement = "top", options = list(container = "body")),
            tipify(
                viz_select_input(ns("multivar.aes"), "Multivar Aesthetic",
                    choices = c("split", "group", "color"),
                    selected = get_default(
                        defaults, "multivar.aes", "split",
                        function(x) x %in% c("split", "group", "color")
                    )
                ),
                documentParameters$multivar.aes,
                placement = "top", options = list(container = "body")
            ),
            tipify(
                viz_select_input(ns("multivar.split.dir"), "Multivar Split Dir",
                    choices = c("col", "row"),
                    selected = get_default(
                        defaults, "multivar.split.dir", "col",
                        function(x) x %in% c("col", "row")
                    )
                ),
                documentParameters$multivar.split.dir,
                placement = "top", options = list(container = "body")
            ),
            .uniform_subplot_spacing_inputs_ui(ns, defaults)
        ),
        "Annotations" = uniform_annotation_inputs_ui(ns, defaults, choices,
            annotate.note = paste(
                "Highlighting and labelling apply to jitter points, so include 'jitter'",
                "in the Plot Types and leave 'Rasterize Jitter' off"
            )
        ),
        "Legend" = uniform_legend_inputs_ui(ns, defaults),
        "Plotly" = uniform_plotly_inputs_ui(ns, defaults),
        "Axes" = uniform_axes_inputs_ui(ns, defaults),
        "Lines" = uniform_lines_inputs_ui(ns, defaults)
    )

    organize_inputs(
        inputs,
        id = ns("yPlotTabsetPanel"),
        title = title,
        tack = module_tack_ui(ns, defaults = defaults),
        columns = columns
    )
}


#' Output UI components for the yPlot module
#'
#' This should be placed in the UI where the plot should be shown.
#'
#' @param id The ID for the Shiny module.
#' @param resizable Logical; when `TRUE` (the default) the plot output
#'   is wrapped in [shinyjqui::jqui_resizable()] so it can be resized
#'   by dragging. Set to `FALSE` when embedding the output in a container
#'   that already provides resizing.
#'
#' @return A Shiny plotlyOutput for the yPlot
#'
#' @import shiny
#' @import plotly
#' @importFrom shinyjqui jqui_resizable
#'
#' @export
#' @author Jared Andrews
dittoViz_yPlotOutputUI <- function(id, resizable = TRUE) {
    ns <- NS(id)
    plot_output <- plotlyOutput(ns("yPlot"))
    if (isTRUE(resizable)) {
        plot_output <- jqui_resizable(plot_output)
    }
    plot_output
}
