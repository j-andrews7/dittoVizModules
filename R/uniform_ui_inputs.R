#' Resolve a default value from a named list
#'
#' Looks up `key` in `defaults`. If present and passes `validator` (when
#' supplied), returns the stored value; otherwise returns `fallback`.
#' Uses standard `if`/`else` instead of vectorized `ifelse()` to avoid
#' silent truncation of multi-valued defaults.
#'
#' Entries that are a [shiny::reactive()] or [shiny::reactiveVal()] are resolved
#' with [shiny::isolate()] before validation, so this returns the reactive's
#' *current* value. See [setup_reactive_defaults()] for how modules keep such
#' entries live at render time.
#'
#' @param defaults A named list of default values, or NULL. Individual entries
#'   may be a `reactive()`/`reactiveVal`.
#' @param key Character string — the name to look up.
#' @param fallback The value to return when `key` is absent or fails validation.
#' @param validator An optional single-argument predicate function (e.g.,
#'   `is.numeric`, `is.logical`). When supplied, the stored value is returned
#'   only if `validator(value)` is `TRUE`. Reactive entries are validated on
#'   their resolved value.
#'
#' @return The resolved default value or `fallback`.
#'
#' @importFrom shiny is.reactive isolate
#'
#' @author Jared Andrews
#' @export
#' @examples
#' get_default(list(color = "red"), "color", "black")
#' get_default(list(), "missing", 10)
#' get_default(list(n = "x"), "n", 5, is.numeric)
#' get_default(list(color = shiny::reactiveVal("blue")), "color", "black")
get_default <- function(defaults, key, fallback, validator = NULL) {
    if (!is.null(defaults) && key %in% names(defaults)) {
        value <- defaults[[key]]
        if (is.reactive(value)) {
            value <- isolate(value())
        }
        if (is.null(validator) || isTRUE(validator(value))) {
            return(value)
        }
    }
    fallback
}


#' Generate uniform Lines input UI
#'
#' Creates a standardized tagList of line-related inputs (horizontal, vertical,
#' and diagonal lines) for use across plot modules.
#'
#' @param ns A namespace function, typically created by `NS(id)`.
#' @param defaults A named list of default values for the inputs.
#' @param include.fit.lines Logical; whether to include "line of best fit" and
#'   "linear model line" inputs. Only applicable for scatter plots. Default is FALSE.
#'
#' @return A `tagList` containing the line input UI elements.
#'
#' @importFrom shiny textInput br tagList numericInput
#' @importFrom shinyWidgets materialSwitch
#' @importFrom colourpicker colourInput
#' @importFrom shinyBS tipify
#'
#' @author Jared Andrews
#' @export
#' @examples
#' ns <- shiny::NS("plot")
#' uniform_lines_inputs_ui(ns)
#' uniform_lines_inputs_ui(ns, include.fit.lines = TRUE)
uniform_lines_inputs_ui <- function(ns, defaults = NULL, include.fit.lines = FALSE) {
    tip_opts <- list(container = "body")
    intercept_tip <- paste(
        "For categorical or factor axes, enter the index (position) of the",
        "category rather than its name. For example, if the axis categories",
        "are 'Audi', 'Mercedes', 'Bugatti', enter 2 to place a line at 'Mercedes'."
    )

    inputs <- list(
        tipify(
            textInput(ns("hline.intercepts"), "Y-intercepts",
                placeholder = "e.g. 2, -2",
                value = get_default(defaults, "hline.intercepts", "")
            ),
            intercept_tip,
            placement = "top", options = tip_opts
        ),
        tipify(
            textInput(ns("hline.colors"), "Y Colors",
                value = get_default(defaults, "hline.colors", "#000000")
            ),
            "Color(s) for horizontal reference lines, as comma-separated hex codes or color names",
            placement = "top", options = tip_opts
        ),
        tipify(
            textInput(ns("hline.widths"), "Y Widths",
                value = get_default(defaults, "hline.widths", "1")
            ),
            "Width(s) for horizontal reference lines in pixels, as comma-separated values",
            placement = "top", options = tip_opts
        ),
        tipify(
            textInput(ns("hline.linetypes"), "Y Line Types",
                placeholder = "solid, dashed, dotted, ...",
                value = get_default(defaults, "hline.linetypes", "dashed")
            ),
            "Line style(s) for horizontal reference lines (solid, dashed, dotted, longdash, dashdot)",
            placement = "top", options = tip_opts
        ),
        tipify(
            textInput(ns("hline.opacities"), "Y Opacities (0-1)",
                value = get_default(defaults, "hline.opacities", "1")
            ),
            "Opacity of horizontal reference lines between 0 (transparent) and 1 (opaque), as comma-separated values",
            placement = "top", options = tip_opts
        ),
        tipify(
            textInput(ns("vline.intercepts"), "X-intercepts",
                placeholder = "e.g. 2, -2",
                value = get_default(defaults, "vline.intercepts", "")
            ),
            intercept_tip,
            placement = "top", options = tip_opts
        ),
        tipify(
            textInput(ns("vline.colors"), "X Colors",
                value = get_default(defaults, "vline.colors", "#000000")
            ),
            "Color(s) for vertical reference lines, as comma-separated hex codes or color names",
            placement = "top", options = tip_opts
        ),
        tipify(
            textInput(ns("vline.widths"), "X Widths",
                value = get_default(defaults, "vline.widths", "1")
            ),
            "Width(s) for vertical reference lines in pixels, as comma-separated values",
            placement = "top", options = tip_opts
        ),
        tipify(
            textInput(ns("vline.linetypes"), "X Line Types",
                placeholder = "solid, dashed, dotted, ...",
                value = get_default(defaults, "vline.linetypes", "dashed")
            ),
            "Line style(s) for vertical reference lines (solid, dashed, dotted, longdash, dashdot)",
            placement = "top", options = tip_opts
        ),
        tipify(
            textInput(ns("vline.opacities"), "X Opacities (0-1)",
                value = get_default(defaults, "vline.opacities", "1")
            ),
            "Opacity of vertical reference lines between 0 (transparent) and 1 (opaque), as comma-separated values",
            placement = "top", options = tip_opts
        ),
        tipify(
            textInput(ns("abline.slopes"), "Ab Slopes",
                value = get_default(defaults, "abline.slopes", "")
            ),
            "Slope(s) of diagonal reference lines (rise/run), as comma-separated values",
            placement = "top", options = tip_opts
        ),
        tipify(
            textInput(ns("abline.intercepts"), "Ab Y-intercepts",
                value = get_default(defaults, "abline.intercepts", "")
            ),
            intercept_tip,
            placement = "top", options = tip_opts
        ),
        tipify(
            textInput(ns("abline.colors"), "Ab Colors",
                value = get_default(defaults, "abline.colors", "#000000")
            ),
            "Color(s) for diagonal reference lines, as comma-separated hex codes or color names",
            placement = "top", options = tip_opts
        ),
        tipify(
            textInput(ns("abline.widths"), "Ab Widths",
                value = get_default(defaults, "abline.widths", "1")
            ),
            "Width(s) for diagonal reference lines in pixels, as comma-separated values",
            placement = "top", options = tip_opts
        ),
        tipify(
            textInput(ns("abline.linetypes"), "Ab Line Types",
                placeholder = "solid, dashed, dotted, ...",
                value = get_default(defaults, "abline.linetypes", "dashed")
            ),
            "Line style(s) for diagonal reference lines (solid, dashed, dotted, longdash, dashdot)",
            placement = "top", options = tip_opts
        ),
        tipify(
            textInput(ns("abline.opacities"), "Ab Opacities (0-1)",
                value = get_default(defaults, "abline.opacities", "1")
            ),
            "Opacity of diagonal reference lines between 0 (transparent) and 1 (opaque), as comma-separated values",
            placement = "top", options = tip_opts
        )
    )

    if (include.fit.lines) {
        inputs <- c(inputs, list(
            tipify(
                materialSwitch(ns("best.fit"), "Plot Best Fit Line",
                    value = get_default(defaults, "best.fit", FALSE, is.logical),
                    status = "success"
                ),
                "Add a LOESS smoothed curve of best fit to the scatter plot",
                placement = "top", options = tip_opts
            ),
            tipify(
                numericInput(ns("line.best.smoothness"), "Best Fit Line Smoothness",
                    value = get_default(defaults, "line.best.smoothness", 1, is.numeric),
                    min = 0,
                    max = 10000
                ),
                "Smoothing span for the LOESS curve; higher values produce a smoother, less wiggly fit",
                placement = "top", options = tip_opts
            ),
            tipify(
                colourInput(ns("line.best.colour"), "Best Fit Line Color",
                    value = get_default(defaults, "line.best.colour", "#000000")
                ),
                "Color for the LOESS best fit line",
                placement = "top", options = tip_opts
            ),
            tipify(
                materialSwitch(ns("linear.model"), "Linear Model Line",
                    value = get_default(defaults, "linear.model", FALSE, is.logical),
                    status = "success"
                ),
                "Add a linear regression line to the scatter plot",
                placement = "top", options = tip_opts
            ),
            tipify(
                materialSwitch(ns("custom.model.enable"), "Custom Model Lines",
                    value = get_default(defaults, "custom.model.enable", FALSE, is.logical),
                    status = "success"
                ),
                paste(
                    "Fit one or more custom models from formulas you define below",
                    "and overlay them as lines. Only data columns and basic",
                    "math/transform terms are allowed."
                ),
                placement = "top", options = tip_opts
            ),
            multiDynamicInput(
                ns("custom.models"),
                label = "Models",
                row_spec = build_model_row_spec(),
                max_per_row = 4,
                panel = FALSE,
                elements = get_default(defaults, "custom.models", NULL)
            )
        ))
    }

    do.call(tagList, inputs)
}


#' Generate uniform Axes input UI
#'
#' Creates a standardized tagList of axis-related inputs for use across plot modules.
#'
#' @param ns A namespace function, typically created by `NS(id)`.
#' @param defaults A named list of default values for the inputs.
#' @param include.rotate Logical; whether to include the "Rotate" input for swapping
#'   x and y axes (e.g., horizontal bar plots). Default is FALSE.
#' @param include.flip Logical; whether to include the "Flip" input for flipping
#'   the axis. Default is FALSE.
#'
#' @return A `tagList` containing the axis input UI elements.
#'
#' @importFrom shiny numericInput checkboxInput tagList
#' @importFrom colourpicker colourInput
#'
#' @importFrom shinyWidgets materialSwitch
#'
#' @author Jared Andrews Jacob Martin
#' @export
#' @examples
#' ns <- shiny::NS("plot")
#' uniform_axes_inputs_ui(ns)
#' uniform_axes_inputs_ui(ns, include.rotate = TRUE, include.flip = TRUE)
uniform_axes_inputs_ui <- function(ns, defaults = NULL, include.rotate = FALSE, include.flip = FALSE) {
    font_choices <- c(
        "Arial", "Balto", "Courier New", "Droid Sans", "Droid Serif",
        "Droid Sans Mono", "Gravitas One", "Old Standard TT", "Open Sans",
        "Overpass", "PT Sans Narrow", "Raleway", "Times New Roman",
        "Verdana", "sans-serif", "serif", "monospace"
    )

    rotate_input <- if (include.rotate) {
        materialSwitch(ns("rotate"), "Rotate (swap X/Y)",
            value = get_default(defaults, "rotate", FALSE, is.logical),
            status = "success"
        )
    } else {
        NULL
    }

    if (include.flip) {
        flip_x <- materialSwitch(ns("flip.x"), "Flip X Axis",
            value = get_default(defaults, "flip.x", FALSE, is.logical),
            status = "success"
        )
        flip_y <- materialSwitch(ns("flip.y"), "Flip Y Axis",
            value = get_default(defaults, "flip.y", FALSE, is.logical),
            status = "success"
        )
    } else {
        flip_x <- NULL
        flip_y <- NULL
    }

    tagList(
        rotate_input,
        flip_x,
        flip_y,
        viz_select_input(ns("title.font.family"), "Title Font",
            choices = font_choices,
            selected = get_default(
                defaults, "title.font.family", "Arial",
                function(x) x %in% font_choices
            )
        ),
        colourInput(ns("title.font.color"), "Title Color",
            value = get_default(defaults, "title.font.color", "#000000")
        ),
        numericInput(ns("title.font.size"), "Title Size",
            value = get_default(defaults, "title.font.size", 26, is.numeric),
            min = 1,
            step = 1
        ),
        numericInput(ns("axis.title.horizontal.position"), "Title position",
            value = get_default(defaults, "axis.title.horizontal.position", 0.5, is.numeric),
            max = 1, min = 0, step = 0.1
        ),
        numericInput(ns("axis.title.font.size"), "Axis Title Size",
            value = get_default(defaults, "axis.title.font.size", 18, is.numeric),
            min = 1,
            step = 1
        ),
        colourInput(ns("axis.title.font.color"), "Axis Title Color",
            value = get_default(defaults, "axis.title.font.color", "#000000")
        ),
        viz_select_input(ns("axis.title.font.family"), "Axis Title Font",
            choices = font_choices,
            selected = get_default(
                defaults, "axis.title.font.family", "Arial",
                function(x) x %in% font_choices
            )
        ),
        checkboxInput(ns("axis.showline"), "Show Axis Borders",
            value = get_default(defaults, "axis.showline", TRUE, is.logical)
        ),
        checkboxInput(ns("axis.mirror"), "Mirror Axis Borders",
            value = get_default(defaults, "axis.mirror", TRUE, is.logical)
        ),
        checkboxInput(ns("show.grid.x"), "Show X Gridlines",
            value = get_default(defaults, "show.grid.x", TRUE, is.logical)
        ),
        checkboxInput(ns("show.grid.y"), "Show Y Gridlines",
            value = get_default(defaults, "show.grid.y", TRUE, is.logical)
        ),
        colourInput(ns("grid.color"), "Gridline Color",
            value = get_default(defaults, "grid.color", "#CCCCCC")
        ),
        colourInput(ns("axis.linecolor"), "Axis Line Color",
            value = get_default(defaults, "axis.linecolor", "black")
        ),
        numericInput(ns("axis.linewidth"), "Axis Line Width",
            value = get_default(defaults, "axis.linewidth", 0.5, is.numeric),
            min = 0,
            step = 0.1
        ),
        numericInput(ns("axis.tickfont.size"), "Tick Label Size",
            value = get_default(defaults, "axis.tickfont.size", 12, is.numeric),
            min = 1,
            step = 1
        ),
        colourInput(ns("axis.tickfont.color"), "Tick Label Color",
            value = get_default(defaults, "axis.tickfont.color", "black")
        ),
        viz_select_input(ns("axis.tickfont.family"), "Tick Label Font",
            choices = font_choices,
            selected = get_default(
                defaults, "axis.tickfont.family", "Arial",
                function(x) x %in% font_choices
            )
        ),
        numericInput(ns("axis.tickangle.x"), "X Tick Label Angle",
            value = get_default(defaults, "axis.tickangle.x", 0, is.numeric),
            min = -180,
            max = 180,
            step = 15
        ),
        numericInput(ns("axis.tickangle.y"), "Y Tick Label Angle",
            value = get_default(defaults, "axis.tickangle.y", 0, is.numeric),
            min = -180,
            max = 180,
            step = 15
        ),
        viz_select_input(ns("axis.ticks"), "Tick Position",
            choices = c("Outside" = "outside", "Inside" = "inside", "None" = ""),
            selected = get_default(
                defaults, "axis.ticks", "outside",
                function(x) x %in% c("outside", "inside", "")
            )
        ),
        colourInput(ns("axis.tickcolor"), "Tick Mark Color",
            value = get_default(defaults, "axis.tickcolor", "black")
        ),
        numericInput(ns("axis.ticklen"), "Tick Mark Length",
            value = get_default(defaults, "axis.ticklen", 5, is.numeric),
            min = 0,
            step = 1
        ),
        numericInput(ns("axis.tickwidth"), "Tick Mark Width",
            value = get_default(defaults, "axis.tickwidth", 1, is.numeric),
            min = 0,
            step = 0.1
        ),
        numericInput(ns("facet.title.font.size"), "Facet Subplot Title Size",
            value = get_default(defaults, "facet.title.font.size", 18, is.numeric),
            min = 1,
            step = 1
        ),
        colourInput(ns("facet.title.font.color"), "Facet Title Color",
            value = get_default(defaults, "facet.title.font.color", "#000000")
        ),
        viz_select_input(ns("facet.title.font.family"), "Facet Title Font",
            choices = font_choices,
            selected = get_default(
                defaults, "facet.title.font.family", "Arial",
                function(x) x %in% font_choices
            )
        )
    )
}


#' Generate uniform Stats input UI
#'
#' Creates a standardized tagList of statistical testing inputs for use across
#' plot modules that support pairwise comparisons (BoxPlot, yPlot, freqPlot).
#'
#' @param ns A namespace function, typically created by `NS(id)`.
#' @param defaults A named list of default values for the inputs.
#'
#' @return A `tagList` containing the stats input UI elements.
#'
#' @importFrom shiny numericInput tagList
#' @importFrom shinyWidgets materialSwitch
#' @importFrom colourpicker colourInput
#' @importFrom shinyBS tipify
#'
#' @author Jared Andrews
#' @rdname INTERNAL_uniform_stats_inputs_ui
#' @keywords internal
.uniform_stats_inputs_ui <- function(ns, defaults = NULL) {
    tip_opts <- list(container = "body")
    tagList(
        tipify(
            materialSwitch(ns("stats.enabled"), "Enable Stats",
                value = get_default(defaults, "stats.enabled", FALSE, is.logical),
                status = "success"
            ),
            "Toggle pairwise statistical testing with bracket annotations on the plot",
            placement = "top", options = tip_opts
        ),
        tipify(
            viz_select_input(ns("stat.test"), "Test",
                choices = c(
                    "Wilcoxon" = "wilcox.test",
                    "t-test" = "t.test",
                    "Kruskal-Wallis" = "kruskal.test",
                    "ANOVA" = "anova"
                ),
                selected = get_default(defaults, "stat.test", "wilcox.test")
            ),
            paste(
                "Statistical test for comparisons.",
                "Wilcoxon and t-test perform pairwise comparisons.",
                "Kruskal-Wallis and ANOVA perform omnibus tests."
            ),
            placement = "top", options = tip_opts
        ),
        tipify(
            viz_select_input(ns("stat.p.adjust"), "P-value Adjustment",
                choices = c(
                    "holm", "hochberg", "hommel", "bonferroni",
                    "BH", "BY", "fdr", "none"
                ),
                selected = get_default(defaults, "stat.p.adjust", "holm")
            ),
            "Method for multiple testing correction applied to all p-values",
            placement = "top", options = tip_opts
        ),
        tipify(
            viz_select_input(ns("stat.display"), "Display",
                choices = c(
                    "Adjusted P-value" = "p.adj",
                    "P-value" = "p.value",
                    "Symbols" = "symbol"
                ),
                selected = get_default(defaults, "stat.display", "p.adj")
            ),
            "What to display on brackets: adjusted p-values, raw p-values, or significance symbols (*, **, ***, ****)",
            placement = "top", options = tip_opts
        ),
        tipify(
            numericInput(ns("stat.sig.threshold"), "Significance Threshold",
                value = get_default(defaults, "stat.sig.threshold", 0.05, is.numeric),
                min = 0, max = 1, step = 0.01
            ),
            "(Adjusted, if applied) P-values above this threshold are labeled 'ns'. Also used as the boundary for the '*' significance symbol.",
            placement = "top", options = tip_opts
        ),
        tipify(
            materialSwitch(ns("stat.hide.ns"), "Hide Non-Significant",
                value = get_default(defaults, "stat.hide.ns", TRUE, is.logical),
                status = "success"
            ),
            "Hide comparison brackets where the adjusted p-value exceeds the significance threshold",
            placement = "top", options = tip_opts
        ),
        tipify(
            materialSwitch(ns("stat.paired"), "Paired Test",
                value = get_default(defaults, "stat.paired", FALSE, is.logical),
                status = "success"
            ),
            paste(
                "Perform paired tests (Wilcoxon signed-rank or paired t-test).",
                "Each group must have the same number of observations in corresponding order.",
                "Data should be sorted so that paired samples align row-by-row within each group."
            ),
            placement = "top", options = tip_opts
        ),
        tipify(
            viz_select_input(ns("stat.pairs"), "Comparisons",
                choices = c(), multiple = TRUE
            ),
            "Select specific pairwise comparisons to display. If empty, all possible pairs are tested.",
            placement = "top", options = tip_opts
        ),
        tipify(
            colourpicker::colourInput(ns("stat.line.color"), "Line Color",
                value = get_default(defaults, "stat.line.color", "#000000")
            ),
            "Color for bracket lines and annotation text",
            placement = "top", options = tip_opts
        ),
        tipify(
            numericInput(ns("stat.line.width"), "Line Width",
                value = get_default(defaults, "stat.line.width", 1, is.numeric),
                min = 1, max = 50, step = 1
            ),
            "Width of bracket lines in pixels",
            placement = "top", options = tip_opts
        ),
        tipify(
            viz_select_input(ns("stat.bracket.style"), "Bracket Style",
                choices = c("Capped" = "capped", "Flat" = "flat"),
                selected = get_default(defaults, "stat.bracket.style", "capped")
            ),
            "Capped brackets have vertical ticks at each end; flat brackets are a single horizontal line",
            placement = "top", options = tip_opts
        ),
        tipify(
            numericInput(ns("stat.step.increase"), "Bracket Spacing",
                value = get_default(defaults, "stat.step.increase", 0.06, is.numeric),
                min = 0.01, max = 0.3, step = 0.01
            ),
            "Fraction of the y-axis range used as vertical spacing between successive bracket levels",
            placement = "top", options = tip_opts
        ),
        tipify(
            numericInput(ns("stat.text.bump"), "Text Offset",
                value = get_default(defaults, "stat.text.bump", 0.04, is.numeric),
                min = 0.005, max = 0.2, step = 0.005
            ),
            "Fraction of the y-axis range for vertical distance between the bracket line and annotation text",
            placement = "top", options = tip_opts
        ),
        tipify(
            numericInput(ns("stat.bracket.inset"), "Bracket Inset",
                value = get_default(defaults, "stat.bracket.inset", 0.025, is.numeric),
                min = 0, max = 0.2, step = 0.005
            ),
            "Fixed amount to inset bracket endpoints from group centers, preventing overlap of adjacent brackets",
            placement = "top", options = tip_opts
        ),
        tipify(
            materialSwitch(ns("stat.per.facet"), "Per Facet Panel",
                value = get_default(defaults, "stat.per.facet", TRUE, is.logical),
                status = "success"
            ),
            "When enabled, statistical tests run independently within each facet panel. When disabled, tests run on the full dataset.",
            placement = "top", options = tip_opts
        )
    )
}


#' Generate uniform Plotly input UI
#'
#' Creates a standardized tagList of Plotly-specific inputs used across all plot
#' modules. Includes interactive download controls, plot margin adjustments,
#' and user-drawn shape styling for Plotly's drawing tools. (Subplot spacing
#' controls live in each module's "Facet" tab via
#' `.uniform_subplot_spacing_inputs_ui()`.)
#'
#' @param ns A namespace function, typically created by `NS(id)`.
#' @param defaults A named list of default values for the inputs.
#'
#' @return A `tagList` containing the Plotly input UI elements.
#'
#' @importFrom shiny downloadButton numericInput tagList br icon
#' @importFrom colourpicker colourInput
#' @importFrom shinyBS tipify
#'
#' @author Jared Andrews
#' @export
#' @examples
#' ns <- shiny::NS("plot")
#' uniform_plotly_inputs_ui(ns)
uniform_plotly_inputs_ui <- function(ns, defaults = NULL) {
    tip_opts <- list(container = "body")
    tagList(
        viz_select_input(
            ns("download.format"),
            "Download Format",
            selected = get_default(
                defaults, "download.format", "svg",
                function(x) x %in% c("svg", "png", "jpeg", "webp")
            ),
            choices = c("svg", "png", "jpeg", "webp"),
            width = "100%"
        ),
        tipify(
            numericInput(ns("margin.t"), "Margin Top",
                value = get_default(defaults, "margin.t", 70, is.numeric),
                min = 0, step = 5
            ),
            "Top margin of the plot in pixels",
            placement = "top", options = tip_opts
        ),
        tipify(
            numericInput(ns("margin.b"), "Margin Bottom",
                value = get_default(defaults, "margin.b", 70, is.numeric),
                min = 0, step = 5
            ),
            "Bottom margin of the plot in pixels",
            placement = "top", options = tip_opts
        ),
        tipify(
            numericInput(ns("margin.l"), "Margin Left",
                value = get_default(defaults, "margin.l", 70, is.numeric),
                min = 0, step = 5
            ),
            "Left margin of the plot in pixels",
            placement = "top", options = tip_opts
        ),
        tipify(
            numericInput(ns("margin.r"), "Margin Right",
                value = get_default(defaults, "margin.r", 90, is.numeric),
                min = 0, step = 5
            ),
            "Right margin of the plot in pixels",
            placement = "top", options = tip_opts
        ),
        tipify(
            colourInput(ns("shape.fill"), "Shape Fill",
                allowTransparent = TRUE,
                value = get_default(defaults, "shape.fill", "rgba(0, 0, 0, 0)")
            ),
            "Interior fill color for shapes drawn on the plot using Plotly's drawing tools",
            placement = "top", options = tip_opts
        ),
        tipify(
            colourInput(ns("shape.line.color"), "Shape Line Color",
                allowTransparent = TRUE,
                value = get_default(defaults, "shape.line.color", "black")
            ),
            "Outline color for shapes drawn on the plot using Plotly's drawing tools",
            placement = "top", options = tip_opts
        ),
        tipify(
            numericInput(ns("shape.line.width"), "Shape Line Width",
                value = get_default(defaults, "shape.line.width", 4, is.numeric),
                min = 0, step = 0.25
            ),
            "Outline width for shapes drawn on the plot using Plotly's drawing tools",
            placement = "top", options = tip_opts
        ),
        tipify(
            viz_select_input(ns("shape.linetype"), "Shape Linetype",
                choices = c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"),
                selected = get_default(
                    defaults, "shape.linetype", "solid",
                    function(x) x %in% c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot")
                )
            ),
            "Line dash style for shapes drawn on the plot using Plotly's drawing tools",
            placement = "top", options = tip_opts
        ),
        tipify(
            numericInput(ns("shape.opacity"), "Shape Opacity",
                value = get_default(defaults, "shape.opacity", 1, is.numeric),
                min = 0, max = 1, step = 0.01
            ),
            "Opacity of shapes drawn on the plot, where 0 is fully transparent and 1 is fully opaque",
            placement = "top", options = tip_opts
        )
    )
}


#' Generate uniform subplot spacing input UI
#'
#' Creates a standardized tagList of the horizontal/vertical subplot spacing
#' inputs. These controls only have an effect when faceting is active, so they
#' are rendered within each module's "Facet" tab.
#'
#' @param ns A namespace function, typically created by `NS(id)`.
#' @param defaults A named list of default values for the inputs.
#'
#' @return A `tagList` containing the subplot spacing input UI elements.
#'
#' @importFrom shiny numericInput tagList
#' @importFrom shinyBS tipify
#'
#' @author Jared Andrews
#' @rdname INTERNAL_uniform_subplot_spacing_inputs_ui
#' @keywords internal
.uniform_subplot_spacing_inputs_ui <- function(ns, defaults = NULL) {
    tip_opts <- list(container = "body")

    tagList(
        tipify(
            numericInput(ns("subplot.margin.x"), "Subplot Spacing (Horizontal)",
                value = get_default(defaults, "subplot.margin.x", 0.03, is.numeric),
                min = 0, max = 1, step = 0.01
            ),
            paste(
                "Horizontal spacing between facet panel columns as a fraction of the plot area",
                "(e.g. 0.03). Only applies when faceting is active."
            ),
            placement = "top", options = tip_opts
        ),
        tipify(
            numericInput(ns("subplot.margin.y"), "Subplot Spacing (Vertical)",
                value = get_default(defaults, "subplot.margin.y", 0.1, is.numeric),
                min = 0, max = 1, step = 0.01
            ),
            paste(
                "Vertical spacing between facet panel rows as a fraction of the plot area",
                "(e.g. 0.1). Only applies when faceting is active."
            ),
            placement = "top", options = tip_opts
        )
    )
}


#' Generate uniform Legend input UI
#'
#' Creates a standardized tagList of legend styling inputs used across plot
#' modules. Currently exposes the legend title and entry label font sizes so
#' they can be adjusted consistently regardless of plot type.
#'
#' @param ns A namespace function, typically created by `NS(id)`.
#' @param defaults A named list of default values for the inputs.
#'
#' @return A `tagList` containing the legend input UI elements.
#'
#' @importFrom shiny numericInput tagList
#' @importFrom shinyBS tipify
#'
#' @author Jared Andrews
#' @export
#' @examples
#' ns <- shiny::NS("plot1")
#' uniform_legend_inputs_ui(ns)
#' uniform_legend_inputs_ui(ns, defaults = list(legend.title.size = 16, legend.text.size = 12))
uniform_legend_inputs_ui <- function(ns, defaults = NULL) {
    tip_opts <- list(container = "body")
    tagList(
        tipify(
            numericInput(ns("legend.title.size"), "Legend Title Size",
                value = get_default(defaults, "legend.title.size", 14, is.numeric),
                min = 0, step = 1
            ),
            "Font size of the legend title.",
            placement = "top", options = tip_opts
        ),
        tipify(
            numericInput(ns("legend.text.size"), "Legend Text Size",
                value = get_default(defaults, "legend.text.size", 12, is.numeric),
                min = 0, step = 1
            ),
            "Font size of the legend entry labels.",
            placement = "top", options = tip_opts
        )
    )
}


#' Generate uniform Annotation input UI
#'
#' Creates a standardized tagList of the point highlighting and annotation
#' inputs shared by modules that draw individual data points. Points are
#' identified by the values of the column chosen in "Annotate By", which are
#' also used as the label text.
#'
#' @param ns A namespace function, typically created by `NS(id)`.
#' @param defaults A named list of default values for the inputs.
#' @param choices Character vector of column names offered by "Annotate By".
#' @param annotate.note Character, or `NULL`. Extra sentence appended to the
#'   "Annotate By" tooltip, for module-specific caveats.
#'
#' @return A `tagList` containing the annotation input UI elements.
#'
#' @importFrom shiny numericInput checkboxInput textAreaInput actionButton tagList
#' @importFrom colourpicker colourInput
#' @importFrom shinyBS tipify
#'
#' @author Jared Andrews
#' @export
#' @examples
#' ns <- shiny::NS("plot1")
#' uniform_annotation_inputs_ui(ns, choices = c("", "Species", "Sepal.Length"))
uniform_annotation_inputs_ui <- function(ns, defaults = NULL, choices = "", annotate.note = NULL) {
    tip_opts <- list(container = "body")
    annotate_tip <- paste(c(
        "Select a column whose values will be used to identify points for highlighting and annotation",
        annotate.note
    ), collapse = ". ")

    tagList(
        tipify(
            viz_select_input(ns("annotate.by"), "Annotate By",
                choices = choices,
                selected = get_default(
                    defaults, "annotate.by", "",
                    function(x) x %in% choices
                )
            ), annotate_tip,
            placement = "top", options = tip_opts
        ),
        tipify(
            textAreaInput(ns("highlight.points"), "Points to Highlight",
                placeholder = "Values from 'Annotate by' column\n(comma, space, or newline delimited)",
                value = get_default(defaults, "highlight.points", ""),
                rows = 3
            ), "Enter specific values from the 'Annotate By' column to highlight those points on the plot",
            placement = "top", options = tip_opts
        ),
        tipify(
            colourInput(ns("highlight.color"), "Highlight Fill",
                value = get_default(defaults, "highlight.color", "#00FFF7"),
                allowTransparent = TRUE
            ), "Choose the fill color for highlighted points",
            placement = "top", options = tip_opts
        ),
        tipify(
            numericInput(ns("highlight.size"), "Highlight Size",
                min = 0.1, step = 0.5,
                value = get_default(defaults, "highlight.size", 7, is.numeric)
            ), "Set the size of highlighted points on the plot",
            placement = "top", options = tip_opts
        ),
        tipify(
            colourInput(ns("highlight.border.color"), "Highlight Border Color",
                value = get_default(defaults, "highlight.border.color", "#000000")
            ), "Choose the border color for highlighted points",
            placement = "top", options = tip_opts
        ),
        tipify(
            numericInput(ns("highlight.border.width"), "Highlight Border Width",
                min = 0, step = 0.25,
                value = get_default(defaults, "highlight.border.width", 1, is.numeric)
            ), "Set the width of the border around highlighted points",
            placement = "top", options = tip_opts
        ),
        tipify(
            checkboxInput(ns("highlight.auto.annotate"), "Auto-annotate Highlights",
                value = get_default(defaults, "highlight.auto.annotate", TRUE, is.logical)
            ), "When enabled, automatically adds text labels to highlighted points using their 'Annotate By' values",
            placement = "top", options = tip_opts
        ),
        tipify(
            colourInput(ns("annotation.color"), "Annotation Color",
                value = get_default(defaults, "annotation.color", "black")
            ), "Set the text color for annotation labels",
            placement = "top", options = tip_opts
        ),
        tipify(
            numericInput(ns("annotation.ax"), "Annotation X Offset",
                step = 1,
                value = get_default(defaults, "annotation.ax", 20, is.numeric)
            ), "Horizontal pixel offset of annotation labels from their target points",
            placement = "top", options = tip_opts
        ),
        tipify(
            numericInput(ns("annotation.ay"), "Annotation Y Offset",
                step = 1,
                value = get_default(defaults, "annotation.ay", -20, is.numeric)
            ), "Vertical pixel offset of annotation labels from their target points (negative values move up)",
            placement = "top", options = tip_opts
        ),
        tipify(
            numericInput(ns("annotation.size"), "Annotation Size",
                min = 1, step = 0.5,
                value = get_default(defaults, "annotation.size", 10, is.numeric)
            ), "Set the font size of annotation text labels in points",
            placement = "top", options = tip_opts
        ),
        tipify(
            checkboxInput(ns("annotation.showarrow"), "Show Arrow",
                value = get_default(defaults, "annotation.showarrow", TRUE, is.logical)
            ), "Toggle whether an arrow is drawn from the annotation label to the target point",
            placement = "top", options = tip_opts
        ),
        tipify(
            colourInput(ns("annotation.arrowcolor"), "Arrow Color",
                value = get_default(defaults, "annotation.arrowcolor", "black")
            ), "Set the color of the annotation arrow connecting the label to the point",
            placement = "top", options = tip_opts
        ),
        tipify(
            numericInput(ns("annotation.arrowhead"), "Arrowhead Style",
                min = 0, step = 1, max = 7,
                value = get_default(defaults, "annotation.arrowhead", 2, is.numeric)
            ), "Choose the arrowhead style (0-7) for annotation arrows, where 0 is no arrowhead",
            placement = "top", options = tip_opts
        ),
        tipify(
            numericInput(ns("annotation.arrowwidth"), "Arrow Linewidth",
                min = 0.1, step = 0.25,
                value = get_default(defaults, "annotation.arrowwidth", 1.5, is.numeric)
            ), "Set the line width of the annotation arrow",
            placement = "top", options = tip_opts
        ),
        tipify(actionButton(ns("annotation.clear"), "Clear Annotations"),
            "Remove all annotation labels and arrows from the current plot",
            placement = "top", options = tip_opts
        )
    )
}
