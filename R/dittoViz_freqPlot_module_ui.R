#' Does every value of one column map to exactly one value of another?
#'
#' [dittoViz::freqPlot()] `stop()`s when `group.by` (or `color.by`) does not
#' resolve to a single value per sample, so the module filters the offered
#' choices rather than letting the plot call error out.
#'
#' @param keys Vector of key values (e.g. the sample column).
#' @param values Vector of values checked for 1-per-key mapping.
#'
#' @return `TRUE` when each distinct key carries exactly one distinct value.
#'
#' @author Jared Andrews
#' @rdname INTERNAL_freq_maps_one_per
#' @keywords internal
.freq_maps_one_per <- function(keys, values) {
    if (length(keys) == 0 || length(keys) != length(values)) {
        return(FALSE)
    }
    length(unique(keys)) == length(unique(paste(keys, values)))
}

#' Columns usable as `sample.by` for a given grouping
#'
#' A sample column must be categorical, have more than one level, must not be one
#' of the grouping columns, and every one of its levels must sit inside a single
#' level of each supplied grouping column.
#'
#' Unlike the facet and grouping selectors this deliberately does not go through
#' [.facet_check()]: samples are the unit of observation rather than a facet, so a
#' study with more than fifty of them is perfectly reasonable. Columns with a level
#' for (nearly) every row are excluded instead, since a row identifier would give
#' one observation per "sample" and so a frequency of 0 or 1 everywhere.
#'
#' @param data The data frame.
#' @param group.cols Character vector of grouping columns the samples must nest
#'   inside (typically `group.by` and, when set, `color.by`).
#'
#' @return A character vector of column names, possibly empty.
#'
#' @author Jared Andrews
#' @rdname INTERNAL_freq_sample_choices
#' @keywords internal
.freq_sample_choices <- function(data, group.cols = character(0)) {
    if (is.null(data) || ncol(data) == 0 || nrow(data) == 0) {
        return(character(0))
    }
    group.cols <- group.cols[!is.na(group.cols) & nzchar(group.cols) & group.cols %in% names(data)]
    candidates <- setdiff(names(data), group.cols)

    keep <- vapply(candidates, function(nm) {
        col <- data[[nm]]
        if (!is.character(col) && !is.factor(col)) {
            return(FALSE)
        }
        n.levels <- length(unique(col[!is.na(col)]))
        if (n.levels < 2 || n.levels > nrow(data) / 2) {
            return(FALSE)
        }
        all(vapply(group.cols, function(g) .freq_maps_one_per(col, data[[g]]), logical(1)))
    }, logical(1))

    candidates[keep]
}

#' Normalize the multi-select `vars.use` input
#'
#' [.blank_to_null()] returns `NULL` for anything that is not length one, so a
#' multi-value selection would read as "no selection" and silently draw every
#' facet. Empty entries are dropped and an empty result becomes `NULL`, which is
#' [dittoViz::freqPlot()]'s "use them all".
#'
#' @param x The raw input value.
#'
#' @return A character vector of selected levels, or `NULL`.
#'
#' @author Jared Andrews
#' @rdname INTERNAL_freq_selected_vars
#' @keywords internal
.freq_selected_vars <- function(x) {
    if (is.null(x)) {
        return(NULL)
    }
    x <- x[!is.na(x) & nzchar(x)]
    if (length(x) == 0) NULL else x
}

#' The nested grouping column statistics should use, if any
#'
#' The x-axis of the summarised frame is always its `grouping` column. A separate
#' `color.by` nests boxes inside each group and so is a second grouping factor for
#' the tests; a `color.by` that is unset or the same column as `group.by` is not.
#'
#' @param group.by The `group.by` input value.
#' @param color.by The `color.by` input value.
#'
#' @return The color column name, or `NULL`.
#'
#' @author Jared Andrews
#' @rdname INTERNAL_freq_stats_group_col
#' @keywords internal
.freq_stats_group_col <- function(group.by, color.by) {
    color.by <- .blank_to_null(color.by)
    group.by <- .blank_to_null(group.by)
    if (is.null(color.by) || identical(color.by, group.by)) NULL else color.by
}

#' Name of the summary column [dittoViz::freqPlot()] plots on the y-axis
#'
#' @param scale Either `"percent"` or `"count"`.
#' @param max.normalize Logical; whether `max.normalize` is enabled, which makes
#'   `freqPlot()` plot the `.norm` variant instead.
#'
#' @return A single column name.
#'
#' @author Jared Andrews
#' @rdname INTERNAL_freq_y_col
#' @keywords internal
.freq_y_col <- function(scale = "percent", max.normalize = FALSE) {
    scale <- if (identical(scale, "count")) "count" else "percent"
    if (isTRUE(max.normalize)) paste0(scale, ".norm") else scale
}

#' The frequency table [dittoViz::freqPlot()] actually plots
#'
#' `freqPlot()` does not plot columns of the incoming data; it tabulates the
#' frequency of `var` within each sample and plots that summary. Axis limits,
#' statistics and point annotations must therefore all be computed against this
#' frame rather than against the input.
#'
#' `freqPlot(data.only = TRUE)` returns *before* applying its own `vars.use`
#' subsetting, so the summary it hands back disagrees with the plot whenever
#' `vars.use` is set. The subsetting is reapplied here.
#'
#' @param data The input data frame.
#' @param var Column whose per-sample frequency is tabulated.
#' @param sample.by Sample column, or `NULL`.
#' @param group.by Grouping column forming the x-axis.
#' @param color.by Coloring column, or `NULL` to follow `group.by`.
#' @param scale Either `"percent"` or `"count"`.
#' @param max.normalize Logical; normalize each label to its maximum.
#' @param vars.use Character vector of `var` levels to keep, or `NULL` for all.
#'
#' @return The summary data frame, or `NULL` when it cannot be computed.
#'
#' @importFrom dittoViz freqPlot
#'
#' @author Jared Andrews
#' @rdname INTERNAL_freq_summary
#' @keywords internal
.freq_summary <- function(data, var, sample.by = NULL, group.by, color.by = NULL,
                          scale = "percent", max.normalize = FALSE, vars.use = NULL) {
    if (is.null(data) || is.null(var) || !nzchar(var) || !var %in% names(data)) {
        return(NULL)
    }
    if (is.null(group.by) || !nzchar(group.by) || !group.by %in% names(data)) {
        return(NULL)
    }

    summary_df <- tryCatch(
        freqPlot(
            data,
            var = var,
            sample.by = sample.by,
            group.by = group.by,
            color.by = if (is.null(color.by)) group.by else color.by,
            scale = scale,
            max.normalize = max.normalize,
            data.only = TRUE
        ),
        error = function(e) NULL
    )

    if (is.null(summary_df) || !"label" %in% names(summary_df)) {
        return(NULL)
    }

    # Reapply the subsetting freqPlot() skips on the data.only path, so this
    # frame matches what is drawn.
    if (!is.null(vars.use) && length(vars.use) > 0) {
        summary_df <- summary_df[summary_df$label %in% vars.use, , drop = FALSE]
    }

    summary_df
}


#' Input UI components for the freqPlot module
#'
#' This should be placed in the UI where the inputs should be shown, with an `id`
#' that matches the `id` used in the `dittoViz_freqPlotServer()` and
#' `dittoViz_freqPlotOutputUI()` functions.
#'
#' @details The user inputs for this module are separated from the outputs to allow for
#' more flexible UI design.
#'
#' The inputs will automatically be organized into a grid layout via the `organize_inputs()`
#' function, with `columns` controlling the number of columns in the grid.
#'
#' Defaults can be set for each input by providing a named list of values to the `defaults`
#' argument. Nearly all parameters for [dittoViz::freqPlot()] can be set via these inputs, so see
#' the help for that function for an exhaustive list.
#'
#' Unlike most modules here, this one does not plot columns of the incoming data. It tabulates
#' how often each level of "Frequency Of" occurs within each sample and plots *those* per-sample
#' frequencies, one facet per level. Axis limits, statistics, the point annotations and the
#' source download therefore all describe the summarised frequency table, not the input rows.
#'
#' Because the frequencies are per-sample, "Sample" must nest inside "Group By": each sample has
#' to carry exactly one group value (and one color value, when "Color By" is set), or
#' [dittoViz::freqPlot()] errors. "Group By" and "Color By" are therefore chosen freely and
#' "Sample" is narrowed to the columns that nest inside them, which makes an erroring combination
#' unselectable. With no sample column each group collapses to a single point per facet, which is
#' rarely what is wanted.
#'
#' @section Plot parameters not implemented or with altered functionality:
#' The following [dittoViz::freqPlot()] parameters are not available via UI inputs:
#'
#' - `xlab` - X-axis label (plotly allows interactive editing)
#' - `ylab` - Y-axis label (plotly allows interactive editing)
#' - `main` - Plot title (plotly allows interactive editing)
#' - `sub` - Plot subtitle (not supported in plotly)
#' - `theme` - ggplot2 theme (not applicable to plotly)
#' - `legend.title` - Legend title (managed by plotly interactively)
#' - `legend.show` - Show legend (always `TRUE`; not directly settable)
#' - `split.by` - Not a parameter of `freqPlot()`; it always facets on the frequency
#'   variable's levels. Use `vars.use` ("Levels To Show") to choose which of those facets
#'   are drawn
#' - `rows.use` - Row subset to plot (use the app's data table filter instead)
#' - `data.out`, `data.only` - Return the summary rather than the plot (the module
#'   summarises internally; use the Source Download button for the table)
#' - `do.hover` - Always `TRUE`, so the frequency, count and sample are readable on hover
#' - `colors` - Integer index/order into `color.panel` (managed via the palette UI)
#' - `color.panel` - Managed internally via the palette selection UI
#' - `y.breaks` - Custom continuous-axis breaks (not implemented)
#' - `x.labels` - Override group labels (not implemented)
#' - `x.reorder` - Reorder x-axis groups (not implemented)
#' - `x.labels.rotate` - Rotate group labels (handled by the Axes tab tick-angle controls)
#' - `var.labels.rename` - Rename facet labels (not implemented)
#' - `var.labels.reorder` - Reorder facet labels (not implemented)
#' - `add.line` - Use `hline.intercepts` instead for horizontal lines with full styling options
#' - `line.linetype` - Use `hline.linetypes` instead
#' - `line.color` - Use `hline.colors` instead
#' - `line.linewidth` - Use `hline.widths` instead
#' - `line.opacity` - Use `hline.opacities` instead
#' - `boxplot.width` - Boxplot width (controlled via `boxgap` and `boxgroupgap`)
#' - `boxplot.outlier.size` - Outlier point size (not implemented)
#' - `boxplot.position.dodge` - Boxplot dodge (controlled via `boxgap`)
#' - `vlnplot.quantiles` - Violin quantiles (doesn't translate to plotly)
#' - `jitter.position.dodge` - Jitter position dodge (calculated from `boxgap`)
#'
#' @section Plot parameters and defaults:
#' The following [dittoViz::freqPlot()] parameters can be accessed via UI inputs and/or the
#' `defaults` argument:
#'
#' - `var` - Variable whose per-sample frequency is tabulated, one facet per level
#'   (UI: "Frequency Of", default: 1st low-cardinality categorical variable)
#' - `sample.by` - Sample identifier the frequencies are computed within
#'   (UI: "Sample", default: "", i.e. one point per group)
#' - `group.by` - Grouping variable forming the x-axis (UI: "Group By", default: 1st
#'   categorical variable that is constant within each sample)
#' - `color.by` - Coloring variable (UI: "Color By", default: "", which follows `group.by`)
#' - `vars.use` - Which levels of `var` get a facet (UI: "Levels To Show", default: "", i.e. all)
#' - `scale` - Plot frequencies as proportions or raw counts (UI: "Scale", default: "percent")
#' - `max.normalize` - Scale each facet to its own maximum (UI: "Max Normalize", default: FALSE)
#' - `plots` - Plot types to show (UI: "Plots", default: `c("boxplot", "jitter")`)
#' - `min` - Y-axis minimum (UI: "Y Axis Min", auto-calculated from the frequency table)
#' - `max` - Y-axis maximum (UI: "Y Axis Max", auto-calculated from the frequency table)
#' - `split.nrow` - Number of facet rows (UI: "Rows", default: NA)
#' - `split.ncol` - Number of facet columns (UI: "Columns", default: NA)
#' - `split.adjust` - Facet scale behavior (UI: "Facet Scaling", default: "fixed")
#' - `do.raster` - Rasterize jitter points (UI: "Rasterize Jitter", default: FALSE)
#' - `raster.dpi` - DPI for rasterization (UI: "Raster DPI", default: 600)
#' - `hover.round.digits` - Hover value rounding (UI: "Hover Round Digits", default: 5)
#' - `jitter.size` - Jitter point size (UI: "Jitter Point Size", default: 1)
#' - `jitter.width` - Jitter width (UI: "Jitter Width", default: 0.2)
#' - `jitter.color` - Jitter border color (UI: "Jitter Border Color", default: "#000000")
#' - `boxplot.show.outliers` - Show boxplot outliers (UI: "Show Outliers", default: FALSE)
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
#' - `ridgeplot.binwidth` - Ridge binwidth (UI: "Ridge Binwidth", default: NA)
#'
#' @section Parameters controlling additional functionality:
#' The following parameters implementing new functionality or controlling plotly-specific
#' features are also available:
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
#' - `annotate.by` - Column whose values identify and label jitter points (UI: "Annotate By",
#'   default: ""). Restricted to the columns carried in the plot's hover text, which for this
#'   plot are the sample and color columns
#' - `highlight.points` - Values from the `annotate.by` column to highlight (UI: "Points to
#'   Highlight", default: "")
#' - `highlight.color` - Fill color for highlighted points (UI: "Highlight Fill", default: "#00FFF7")
#' - `highlight.size` - Size of highlighted points (UI: "Highlight Size", default: 7)
#' - `highlight.border.color` - Border color for highlighted points (UI: "Highlight Border Color",
#'   default: "#000000")
#' - `highlight.border.width` - Border width for highlighted points (UI: "Highlight Border Width",
#'   default: 1)
#' - `highlight.auto.annotate` - Label highlighted points automatically (UI: "Auto-annotate
#'   Highlights", default: TRUE)
#' - `annotation.color` - Annotation text color (UI: "Annotation Color", default: "black")
#' - `annotation.ax` - Horizontal label offset in pixels (UI: "Annotation X Offset", default: 20)
#' - `annotation.ay` - Vertical label offset in pixels (UI: "Annotation Y Offset", default: -20)
#' - `annotation.size` - Annotation font size (UI: "Annotation Size", default: 10)
#' - `annotation.showarrow` - Draw an arrow from label to point (UI: "Show Arrow", default: TRUE)
#' - `annotation.arrowcolor` - Annotation arrow color (UI: "Arrow Color", default: "black")
#' - `annotation.arrowhead` - Annotation arrowhead style (UI: "Arrowhead Style", default: 2)
#' - `annotation.arrowwidth` - Annotation arrow line width (UI: "Arrow Linewidth", default: 1.5)
#' - `stats.enabled` and the other `stat.*` parameters - Pairwise testing of the per-sample
#'   frequencies between x-axis groups. Tests always run within each facet, since frequencies
#'   of different levels are not comparable quantities; the "Per Facet Panel" control is
#'   therefore hidden and forced on
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
#' @author Jared Andrews
#' @seealso [dittoViz::freqPlot()], [VizModules::organize_inputs()],
#' [VizModules::dittoViz_freqPlotOutputUI()], [VizModules::dittoViz_freqPlotServer()],
#' [VizModules::dittoViz_freqPlotApp()]
#' @examples
#' library(VizModules)
#' dittoViz_freqPlotInputsUI("freqPlot", example_composition)
dittoViz_freqPlotInputsUI <- function(id, data, defaults = NULL, title = NULL, columns = 2) {
    ns <- NS(id)

    choices <- c("", names(data))

    # Frequencies are tabulated over levels, so only low-cardinality categorical
    # columns are meaningful for any of the structural selectors.
    cat.choices <- .facet_check(data)

    var.default <- get_default(
        defaults, "var",
        if (length(cat.choices)) cat.choices[1] else "",
        function(x) x %in% cat.choices
    )

    # The grouping columns are chosen freely and the sample column is narrowed to
    # those nesting inside them. Constraining in this one direction is what keeps
    # the two selectors from chasing each other in a loop; an incompatible pair is
    # impossible rather than an error from freqPlot().
    group.choices <- setdiff(cat.choices, var.default)
    group.default <- get_default(
        defaults, "group.by",
        if (length(group.choices)) group.choices[1] else "",
        function(x) x %in% group.choices
    )
    color.default <- get_default(
        defaults, "color.by", "",
        function(x) x == "" || x %in% group.choices
    )
    sample.choices <- .freq_sample_choices(data, c(group.default, color.default))
    sample.default <- get_default(
        defaults, "sample.by", "",
        function(x) x == "" || x %in% sample.choices
    )

    vars.use.choices <- if (nzchar(var.default) && var.default %in% names(data)) {
        levels(as.factor(data[[var.default]]))
    } else {
        character(0)
    }

    selected <- list(
        "var", "sample.by", "group.by", "color.by", "vars.use",
        "scale", "max.normalize", "plots", c("min", "max"),
        c("split.nrow", "split.ncol"), "split.adjust",
        "do.raster", "raster.dpi", "hover.round.digits",
        "jitter.size", "jitter.width", "jitter.color",
        "boxplot.show.outliers", "boxplot.color", "boxplot.fill", "boxplot.lineweight",
        "vlnplot.lineweight", "vlnplot.scaling",
        "ridgeplot.lineweight", "ridgeplot.scale",
        "ridgeplot.ymax.expansion", "ridgeplot.shape",
        "ridgeplot.bins", "ridgeplot.binwidth"
    )
    documentParameters <- get_documentation(
        package_name = "dittoViz::freqPlot", type = "param",
        selected = selected, cap = TRUE
    )

    inputs <- list(
        "Data" = tagList(
            tipify(
                viz_select_input(ns("var"), "Frequency Of",
                    choices = cat.choices,
                    selected = var.default
                ),
                documentParameters$var,
                placement = "top", options = list(container = "body")
            ),
            tipify(
                viz_select_input(ns("sample.by"), "Sample",
                    choices = c("", sample.choices),
                    selected = sample.default
                ),
                paste(
                    "Frequencies are computed within each sample, giving one point per sample.",
                    "Only columns nesting inside the grouping are offered.",
                    "With no sample column each group collapses to a single point."
                ),
                placement = "top", options = list(container = "body")
            ),
            tipify(
                viz_select_input(ns("group.by"), "Group By",
                    choices = group.choices,
                    selected = group.default
                ),
                documentParameters$group.by,
                placement = "top", options = list(container = "body")
            ),
            tipify(
                viz_select_input(ns("color.by"), "Color By",
                    choices = c("", group.choices),
                    selected = color.default
                ),
                paste(
                    "Column used for fill color. Leave empty to color by the grouping.",
                    "Must also be constant within each sample."
                ),
                placement = "top", options = list(container = "body")
            ),
            tipify(
                viz_select_input(ns("vars.use"), "Levels To Show",
                    choices = vars.use.choices,
                    selected = get_default(
                        defaults, "vars.use", "",
                        function(x) all(x == "") || all(x %in% vars.use.choices)
                    ),
                    multiple = TRUE
                ),
                "Which levels of the frequency variable get a facet. Empty shows all of them.",
                placement = "top", options = list(container = "body")
            ),
            tipify(
                viz_select_input(ns("plots"), "Plots",
                    choices = c("Violin" = "vlnplot", "Box" = "boxplot",
                                "Jitter" = "jitter", "Ridge" = "ridgeplot"),
                    selected = get_default(
                        defaults, "plots", c("boxplot", "jitter"),
                        function(x) all(x %in% c("vlnplot", "boxplot", "jitter", "ridgeplot"))
                    ),
                    multiple = TRUE
                ),
                documentParameters$plots,
                placement = "top", options = list(container = "body")
            ),
            helpText("Order not currently respected"),
            uiOutput(ns("palette.selection"))
        ),
        "Scale" = tagList(
            tipify(
                viz_select_input(ns("scale"), "Scale",
                    choices = c("percent", "count"),
                    selected = get_default(
                        defaults, "scale", "percent",
                        function(x) x %in% c("percent", "count")
                    )
                ),
                documentParameters$scale,
                placement = "top", options = list(container = "body")
            ),
            tipify(
                materialSwitch(ns("max.normalize"), "Max Normalize",
                    value = get_default(defaults, "max.normalize", FALSE, is.logical),
                    status = "success"
                ),
                documentParameters$max.normalize,
                placement = "top", options = list(container = "body")
            ),
            tipify(
                numericInput(ns("y.max"), "Y Axis Max",
                    value = get_default(defaults, "y.max", NA, is.numeric)
                ),
                "Upper limit of the frequency axis. Recomputed from the frequency table when the summary changes",
                placement = "top", options = list(container = "body")
            ),
            tipify(
                numericInput(ns("y.min"), "Y Axis Min",
                    value = get_default(defaults, "y.min", 0, is.numeric)
                ),
                "Lower limit of the frequency axis. Recomputed from the frequency table when the summary changes",
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
            tipify(
                numericInput(ns("hover.round.digits"), "Hover Round Digits",
                    value = get_default(defaults, "hover.round.digits", 5, is.numeric),
                    step = 1, min = 1
                ),
                documentParameters$hover.round.digits,
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
                        defaults, "ridgeplot.binwidth", NA,
                        function(x) is.numeric(x) || is.na(x)
                    ),
                    min = 0
                ),
                documentParameters$ridgeplot.binwidth,
                placement = "top", options = list(container = "body")
            )
        ),
        "Stats" = .uniform_stats_inputs_ui(ns, defaults),
        "Facet" = tagList(
            helpText("Faceted on the frequency variable; use \"Levels To Show\" to pick facets."),
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
            .uniform_subplot_spacing_inputs_ui(ns, defaults)
        ),
        "Annotations" = uniform_annotation_inputs_ui(ns, defaults, choices,
            annotate.note = paste(
                "Points are samples. Only the sample and color columns are carried in the",
                "hover text, so only those can be annotated; include 'jitter' in Plots and",
                "leave 'Rasterize Jitter' off"
            )
        ),
        "Legend" = uniform_legend_inputs_ui(ns, defaults),
        "Plotly" = uniform_plotly_inputs_ui(ns, defaults),
        "Axes" = uniform_axes_inputs_ui(ns, defaults),
        "Lines" = uniform_lines_inputs_ui(ns, defaults)
    )

    organize_inputs(
        inputs,
        id = ns("freqPlotTabsetPanel"),
        title = title,
        tack = module_tack_ui(ns, defaults = defaults),
        columns = columns
    )
}


#' Output UI components for the freqPlot module
#'
#' This should be placed in the UI where the plot should be shown.
#'
#' @param id The ID for the Shiny module.
#' @param resizable Logical; when `TRUE` (the default) the plot output
#'   is wrapped in [shinyjqui::jqui_resizable()] so it can be resized
#'   by dragging. Set to `FALSE` when embedding the output in a container
#'   that already provides resizing.
#'
#' @return A Shiny plotlyOutput for the freqPlot
#'
#' @import shiny
#' @import plotly
#' @importFrom shinyjqui jqui_resizable
#'
#' @export
#' @author Jared Andrews
#' @seealso [dittoViz::freqPlot()], [VizModules::dittoViz_freqPlotInputsUI()],
#' [VizModules::dittoViz_freqPlotServer()], [VizModules::dittoViz_freqPlotApp()]
dittoViz_freqPlotOutputUI <- function(id, resizable = TRUE) {
    ns <- NS(id)
    plot_output <- plotlyOutput(ns("freqPlot"))
    if (isTRUE(resizable)) {
        plot_output <- jqui_resizable(plot_output)
    }
    plot_output
}
