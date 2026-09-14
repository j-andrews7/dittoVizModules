#' Server logic for yPlot module
#'
#' @param id The ID for the Shiny module.
#' @param data A `reactive` containing the data frame to plot. Values that are not
#'   data frames are coerced with [as.data.frame()]; a `NULL` value is treated as
#'   "not ready yet" and the module waits for data.
#' @param hide.inputs A character vector of input IDs to hide.
#'   These will still be initialized and their values passed to the plot function,
#'   but the user will not be able to see/adjust them in the UI.
#' @param hide.tabs A character vector of tab names to hide.
#'   Inputs in these tabs will still be initialized and their values passed to the plot function,
#'   but the user will not be able to see/adjust them in the UI.
#' @param defaults A named list of default values for the inputs. When the reset button is
#'   clicked, inputs are reset to these values rather than hardcoded fallbacks. Typically
#'   the same list passed to the corresponding UI function. An entry may also be a
#'   [shiny::reactive()] or [shiny::reactiveVal()], in which case the input tracks it as the
#'   parent app's state changes; see [setup_reactive_defaults()].
#' @return The `moduleServer` function for the yPlot module.
#'
#' @import shiny
#' @import plotly
#' @importFrom ggplot2 theme_bw theme unit element_blank
#' @importFrom stats na.omit
#' @importFrom dittoViz yPlot
#' @importFrom shinyjs hide show delay
#' @importFrom shinyWidgets updateMaterialSwitch
#' @importFrom colourpicker updateColourInput
#'
#' @seealso [dittoViz::yPlot()], [VizModules::dittoViz_yPlotInputsUI()],
#' [VizModules::dittoViz_yPlotOutputUI()], [VizModules::dittoViz_yPlotApp()]
#'
#' @export
#' @author Jared Andrews, Jacob Martin
dittoViz_yPlotServer <- function(id, data, hide.inputs = NULL, hide.tabs = NULL, defaults = NULL) {
    stopifnot(is.reactive(data))
    data <- .require_data_frame(data)

    moduleServer(id, function(input, output, session) {
        params <- setup_reactive_defaults(defaults, input, session)

        # Hide individual inputs/tabs if specified. The inputs UI is injected by the
        # parent app via renderUI (and re-injected when the dataset changes), so the
        # hiding must be (re)applied after the controls exist in the DOM rather than
        # once at module initialization.
        if (!is.null(hide.inputs) || !is.null(hide.tabs)) {
            observeEvent(data(), {
                delay(100, {
                    hide_input(session, hide.inputs)
                    for (tab.name in hide.tabs) hideTab(inputId = "yPlotTabsetPanel", target = tab.name)
                })
            })
        }

        # Keep the Y-data selector in step with the data. Only refresh when the set
        # of numeric columns actually changes (not on every row-filter), to
        # preserve the user's current selection.
        var_choice_cache <- reactiveVal(NULL)
        observeEvent(data(), {
            df <- data()
            req(df)
            num.cols <- names(df)[vapply(df, is.numeric, logical(1))]
            if (identical(num.cols, var_choice_cache())) {
                return()
            }
            var_choice_cache(num.cols)
            current <- isolate(input$var)
            default.var <- get_default(
                defaults, "var",
                if (length(num.cols)) num.cols[1] else "",
                function(x) all(x %in% num.cols)
            )
            # `var` is a multi-select, so every currently selected column must still
            # be numeric for the selection to be worth preserving.
            selected.var <- if (length(current) > 0 && all(nzchar(current)) && all(current %in% num.cols)) {
                current
            } else {
                default.var
            }
            update_viz_select(session, "var", choices = num.cols, selected = selected.var)
        }, ignoreNULL = TRUE)

        # Conditionally show/hide Stats tab based on plot type and Y data selection
        observeEvent(c(input$plots, input$var, input$multivar.aes, input$split.by), {
            ridge.only <- length(input$plots) == 1 && input$plots == "ridgeplot"
            # With several Y variables the comparisons offered here only describe what
            # is drawn when the variables are the sole faceting dimension: the "group"
            # and "color" aesthetics replace the x-axis groups with the variable names,
            # and an additional split.by facets on two dimensions at once, which the
            # significance brackets cannot be placed against.
            multivar.reshaped <- length(input$var) > 1 &&
                (!identical(input$multivar.aes, "split") ||
                    (!is.null(input$split.by) && any(nzchar(input$split.by))))
            if (ridge.only || multivar.reshaped) {
                hideTab(inputId = "yPlotTabsetPanel", target = "Stats")
            } else {
                showTab(inputId = "yPlotTabsetPanel", target = "Stats")
            }
        })

        # Update stat comparison pairs when group.by or color.by changes
        observeEvent(c(input$group.by, input$color.by), {
            req(input$group.by)
            color_by <- if (!is.null(input$color.by) && nzchar(input$color.by)) input$color.by else NULL
            pair_strings <- generate_pair_strings(data(), input$group.by, color_by)
            # Pause readers until the client echoes the cleared selection, otherwise
            # the plot renders once now and again when that echo lands.
            freezeReactiveValue(input, "stat.pairs")
            update_viz_select(session, "stat.pairs", choices = c("", pair_strings), selected = "")
        })

        ns <- session$ns

        # Persist manual legend/annotation/colorbar repositioning across rebuilds.
        plot_source <- session$ns("yplot")
        edit_store <- setup_manual_edits(input, session, plot_source)

        # Axis side(s) whose title is regenerated (not persisted) because a data
        # adjustment is active; set inside generate_yPlot() and read at render.
        regen_keys_rv <- reactiveVal(character(0))

        # Axis-defining variables at the last build; when they change we drop any
        # persisted manual axis-title text so the title regenerates for the new
        # variable (its dragged position still persists).
        last_axis_vars <- reactiveVal(NULL)

        # Store last computed stats table for download
        last_stats_df <- reactiveVal(NULL)

        # Points gathered from box/lasso selections, accumulated across selections
        selected.data <- reactiveVal()

        observeEvent(
            event_data("plotly_selected", source = plot_source),
            {
                selected <- event_data("plotly_selected", source = plot_source)
                selected.full <- rbind(selected.data(), selected)
                keep <- selected.full[!duplicated(selected.full), ]

                if (nrow(keep) == 0) {
                    selected.data(NULL)
                } else {
                    selected.data(keep)
                }
            }
        )

        observeEvent(input$annotation.clear, {
            selected.data(NULL)
            edit_store$annotations <- list()
        })

        # Selections are held as trace/point indices, which only describe the layout
        # they were made on, so they are dropped when that layout changes.
        observeEvent(
            c(
                input$var, input$group.by, input$color.by, input$shape.by,
                input$split.by, input$plots
            ),
            {
                selected.data(NULL)
            },
            ignoreInit = TRUE
        )

        default_palette_name <- "dittoColors"
        palette_lookup <- .flatten_palette_options(default_palettes()[["choices"]])
        default_palette_values <- palette_lookup[[default_palette_name]]
        if (is.null(default_palette_values) || length(default_palette_values) == 0) {
            default_palette_values <- if (length(palette_lookup) > 0) palette_lookup[[1]] else character(0)
        }

        palette_groups <- reactive({
            df <- data()
            if (is.null(df)) {
                return(character(0))
            }

            # With several Y variables on the "color" aesthetic, dittoViz fills by its
            # internal "var.which" column, so the palette is keyed by variable name
            # rather than by any data column. Names that do not match the fill values
            # leave every box grey.
            if (length(input$var) > 1 && identical(input$multivar.aes, "color")) {
                return(levels(as.factor(unique(input$var))))
            }

            # Determine which column to use for palette groups
            color_col <- input$color.by
            group_col <- input$group.by

            # Use color.by if specified, otherwise fall back to group.by
            col_to_use <- if (!is.null(color_col) && nzchar(color_col) && color_col %in% names(df)) {
                color_col
            } else if (!is.null(group_col) && nzchar(group_col) && group_col %in% names(df)) {
                group_col
            } else {
                NULL
            }

            if (!is.null(col_to_use)) {
                col_data <- na.omit(df[[col_to_use]])
                # Use factor level order to match ggplot2/dittoViz color assignment.
                # For factors, use the defined levels (preserves order);
                # for character/other, convert to factor (alphabetical order).
                if (is.factor(col_data)) {
                    levels(col_data)
                } else {
                    levels(as.factor(col_data))
                }
            } else {
                character(0)
            }
        })

        # What the plot actually colours by. The picker is rebuilt whenever the group
        # set changes and is re-seeded from this same resolution, so the value it
        # then reports resolves to the palette already in use - notably when the
        # rebuild is deferred to whenever the user next opens the Data tab, where it
        # would otherwise re-render the plot for a tab click. A reactiveVal only
        # invalidates on a real change, so that costs nothing, while a colour the
        # user actually picks comes straight through.
        palette_store <- setup_group_colors(
            input, "palette.colours", palette_groups,
            default_palette_values, defaults, params
        )

        output$palette.selection <- renderUI({
            groups <- palette_groups()
            if (length(groups) == 0) {
                return(NULL)
            }

            initial_colors <- isolate(resolve_palette(
                groups, input$palette.colours, default_palette_values,
                .default_group_colors(defaults, "palette.colours")
            ))

            # The picker is seeded with this, so it is also what the plot should be
            # drawing with from now until the user changes something. Setting it here
            # rather than waiting for the client to report back keeps the first draw
            # on the right palette.
            palette_store(initial_colors)

            multiColorPicker(
                ns("palette.colours"),
                label = "Plot colors",
                groups = groups,
                palette_options = default_palettes()[["choices"]],
                selected_palette = default_palette_name,
                colors = initial_colors,
                compact = TRUE
            )
        })

        # Reset functionality
        observeEvent(input$reset, {
            char.choices <- c("", names(data())[vapply(data(), function(x) !is.numeric(x), logical(1))])
            num.choices <- c("", names(data())[vapply(data(), is.numeric, logical(1))])
            choices <- c("", names(data()))

            # `var` may hold several columns; the reset limits must span all of them.
            default.var <- get_default(defaults, "var", num.choices[2], function(x) all(x %in% num.choices))

            # Calculate y.max and y.min from the default selections
            y.range <- .calculate_range(
                df = data(), data_col_y = default.var,
                axis_scale_factor = .y_axis_scale_factor, grouping = FALSE
            )
            max.y <- if (!is.null(y.range)) y.range$max else 1
            min.y <- if (!is.null(y.range)) y.range$min else 0

            # Data
            update_viz_select(session, "var",
                choices = num.choices[nzchar(num.choices)],
                selected = default.var)
            update_viz_select(session, "group.by",
                selected = get_default(defaults, "group.by", char.choices[2], function(x) x %in% char.choices))
            update_viz_select(session, "color.by",
                selected = get_default(defaults, "color.by", "", function(x) x == "" || x %in% char.choices))
            update_viz_select(session, "shape.by",
                selected = get_default(defaults, "shape.by", "", function(x) x == "" || x %in% char.choices))


            # Plot Type
            update_viz_select(session, "plots",
                selected = get_default(defaults, "plots", c("boxplot", "jitter")))

            # Adjustments
            update_viz_select(session, "var.adjustment",
                selected = get_default(defaults, "var.adjustment", ""))
            update_viz_select(session, "var.adj.fxn",
                selected = get_default(defaults, "var.adj.fxn", ""))
            reset.y.min <- get_default(defaults, "y.min", min.y, is.numeric)
            reset.y.max <- get_default(defaults, "y.max", max.y, is.numeric)
            y_range_store(list(min = reset.y.min, max = reset.y.max))
            updateNumericInput(session, "y.min", value = reset.y.min)
            updateNumericInput(session, "y.max", value = reset.y.max)
            updateMaterialSwitch(session, "do.raster", value = get_default(defaults, "do.raster", FALSE, is.logical))
            updateNumericInput(session, "raster.dpi", value = get_default(defaults, "raster.dpi", 600, is.numeric))

            # Jitter
            updateNumericInput(session, "jitter.size", value = get_default(defaults, "jitter.size", 1, is.numeric))
            updateNumericInput(session, "jitter.width", value = get_default(defaults, "jitter.width", 0.2, is.numeric))
            updateColourInput(session, "jitter.color",
                value = get_default(defaults, "jitter.color", "#000000"))
            updateNumericInput(session, "jitter.shape.legend.size",
                value = get_default(defaults, "jitter.shape.legend.size", 5, is.numeric))
            updateMaterialSwitch(session, "jitter.shape.legend.show",
                value = get_default(defaults, "jitter.shape.legend.show", TRUE, is.logical))

            # Box
            updateMaterialSwitch(session, "boxplot.show.outliers",
                value = get_default(defaults, "boxplot.show.outliers", FALSE, is.logical))
            updateColourInput(session, "boxplot.color",
                value = get_default(defaults, "boxplot.color", "#000000"))
            updateMaterialSwitch(session, "boxplot.fill",
                value = get_default(defaults, "boxplot.fill", TRUE, is.logical))
            updateNumericInput(session, "boxplot.lineweight",
                value = get_default(defaults, "boxplot.lineweight", 0.5, is.numeric))
            updateNumericInput(session, "boxgap", value = get_default(defaults, "boxgap", 0.3, is.numeric))
            updateNumericInput(session, "boxgroupgap", value = get_default(defaults, "boxgroupgap", 0.2, is.numeric))

            # Violin
            updateNumericInput(session, "vlnplot.lineweight",
                value = get_default(defaults, "vlnplot.lineweight", 0.5, is.numeric))
            update_viz_select(session, "vlnplot.scaling",
                selected = get_default(defaults, "vlnplot.scaling", "area"))

            # Ridge
            updateNumericInput(session, "ridgeplot.lineweight",
                value = get_default(defaults, "ridgeplot.lineweight", 0.5, is.numeric))
            updateNumericInput(session, "ridgeplot.scale",
                value = get_default(defaults, "ridgeplot.scale", 1.25, is.numeric))
            updateNumericInput(session, "ridgeplot.ymax.expansion",
                value = get_default(defaults, "ridgeplot.ymax.expansion", NA, is.numeric))
            update_viz_select(session, "ridgeplot.shape",
                selected = get_default(defaults, "ridgeplot.shape", "smooth"))
            updateNumericInput(session, "ridgeplot.bins",
                value = get_default(defaults, "ridgeplot.bins", 30, is.numeric))
            updateNumericInput(session, "ridgeplot.binwidth",
                value = get_default(defaults, "ridgeplot.binwidth", NA, is.numeric))

            # Facet
            update_viz_select(session, "split.by",
                selected = get_default(defaults, "split.by", "", function(x) x == "" || x %in% char.choices))
            update_viz_select(session, "split.adjust", selected = get_default(defaults, "split.adjust", "fixed"))
            updateNumericInput(session, "split.ncol", value = get_default(defaults, "split.ncol", NA, is.numeric))
            updateNumericInput(session, "split.nrow", value = get_default(defaults, "split.nrow", NA, is.numeric))
            update_viz_select(session, "multivar.aes",
                selected = get_default(defaults, "multivar.aes", "split",
                    function(x) x %in% c("split", "group", "color")))
            update_viz_select(session, "multivar.split.dir",
                selected = get_default(defaults, "multivar.split.dir", "col",
                    function(x) x %in% c("col", "row")))

            # Axes
            reset_axes_inputs(session, defaults)

            # Plotly
            # Group colors
            .reset_group_colors(session, "palette.colours", defaults, palette_groups(), default_palette_values)

            reset_plotly_inputs(session, defaults)
            reset_legend_inputs(session, defaults)

            # Hover
            update_viz_select(session, "hover.data",
                selected = get_default(defaults, "hover.data", "", function(x) all(x == "") || all(x %in% choices)))
            updateNumericInput(session, "hover.round.digits",
                value = get_default(defaults, "hover.round.digits", 5, is.numeric))

            # Annotations
            reset_annotation_inputs(session, defaults, choices)
            selected.data(NULL)

            # Lines
            reset_lines_inputs(session, defaults = defaults)

            # Stats
            .reset_stats_inputs(session, defaults)
        })

        # How high the significance brackets will reach, so the y-axis can reserve
        # room for them rather than have the plot drawn with them clipped. Mirrors the
        # render's stats context: group.by is the x-axis, color.by the nested grouping,
        # and several Y variables are tested against dittoViz's reshaped frame.
        stat_headroom <- function() {
            if (!isTRUE(input$stats.enabled)) {
                return(NULL)
            }

            y.vars <- input$var
            if (is.null(y.vars) || length(y.vars) == 0) {
                return(NULL)
            }
            xvar <- .blank_to_null(input$group.by)
            if (is.null(xvar)) {
                return(NULL)
            }

            color.by <- .blank_to_null(input$color.by)
            grp_var <- if (!is.null(color.by) && !identical(color.by, xvar)) color.by else NULL

            multivar <- length(y.vars) > 1
            if (multivar) {
                stats.data <- .multivar_long_df(data(), y.vars)
                yvar <- "var.multi"
                facet.var <- "var.which"
                per.facet <- TRUE
            } else {
                stats.data <- data()
                yvar <- y.vars
                facet.var <- .blank_to_null(input$split.by)
                per.facet <- isTRUE(input$stat.per.facet)
            }

            .stat_bracket_headroom(
                df = stats.data, x = xvar, y = yvar,
                group.by = grp_var, facet.by = facet.var,
                per.facet = per.facet, input = input,
                dodge.width = 1 - .box_num(input$boxgap, 0.3)
            )
        }

        # What the plot actually draws with. The limits are pushed into the y.min and
        # y.max controls below, which is a client round-trip; reading the store rather
        # than the raw inputs means the echo of a limit just set costs no rebuild.
        y_range_store <- setup_axis_range(
            input, session,
            headroom = stat_headroom, params = params
        )

        # Update y-axis range when var (y data) column is changed
        observeEvent(input$var, {
            y_range <- .calculate_range(df = data(), data_col_y = input$var, axis_scale_factor = .y_axis_scale_factor, grouping = FALSE)
            if (!is.null(y_range)) {
                y_range_store(list(min = y_range$min, max = y_range$max))
                updateNumericInput(session, "y.max", value = y_range$max)
                updateNumericInput(session, "y.min", value = y_range$min)
            }
        })

        observeEvent(input$split.by, {
            if (!is.null(input$split.by) && nzchar(input$split.by)) {
                show_input(session, c("facet.title.font.size", "facet.title.font.color", "facet.title.font.family"))
            } else {
                hide_input(session, c("facet.title.font.size", "facet.title.font.color", "facet.title.font.family"))
            }
        })

        # Generate yPlot reactive
        generate_yPlot <- reactive({
            req(input$var)
            isolate_fn <- setup_auto_update_logic(input, params)

            # Resolved server-side, so they already clear any significance brackets.
            y.limits <- isolate_fn(y_range_store())

            # Parse inputs that might need conversion
            split.by <- .na_to_null(isolate_fn(input$split.by))
            color.by <- .na_to_null(isolate_fn(input$color.by))
            shape.by <- .na_to_null(isolate_fn(input$shape.by))

            # Several Y variables can be plotted at once; dittoViz then reshapes the
            # data internally (into "var.multi"/"var.which") and maps the variables
            # onto the aesthetic named by multivar.aes.
            y.vars <- isolate_fn(input$var)
            multivar <- length(y.vars) > 1
            multivar.aes <- isolate_fn(input$multivar.aes)
            if (is.null(multivar.aes) || !nzchar(multivar.aes)) {
                multivar.aes <- "split"
            }

            # Parse split dimensions
            split.ncol <- .na_to_null(isolate_fn(input$split.ncol))
            split.nrow <- .na_to_null(isolate_fn(input$split.nrow))

            # Handle ridgeplot.ymax.expansion
            ridgeplot.ymax.expansion <- isolate_fn(input$ridgeplot.ymax.expansion)
            if (is.na(ridgeplot.ymax.expansion)) {
                ridgeplot.ymax.expansion <- NA
            }

            # Handle ridgeplot.binwidth
            ridgeplot.binwidth <- .na_to_null(isolate_fn(input$ridgeplot.binwidth))

            # Resolve color palette
            palette_values <- isolate_fn(palette_store())

            # Keep names so scale_fill_manual matches colors to groups by name,
            # making the mapping independent of positional order.
            color.panel.arg <- NULL
            if (!is.null(palette_values) && length(palette_values) > 0) {
                color.panel.arg <- palette_values
            }

            # Set default color.by to group.by if not specified
            if (is.null(color.by) || color.by == "") {
                color.by <- isolate_fn(input$group.by)
            }

            # Formatting split adjustment into correct structure for dittoViz parameter input
            split.adjust <- list(scales = "free")
            if (isolate_fn(input$split.adjust) != "free") {
                split.adjust$scales <- isolate_fn(input$split.adjust)
            }

            # Columns the rendered plot is actually faceted by. dittoViz facets on its
            # internal "var.which" column when several Y variables are split, so the
            # plot can be faceted even with no split.by set.
            facet.cols <- c(
                if (!is.null(split.by) && any(nzchar(split.by))) split.by,
                if (multivar && multivar.aes == "split") "var.which"
            )
            faceted <- length(facet.cols) > 0

            # Reflect any applied Y-axis data adjustment in the continuous-axis title so it
            # accurately describes the values displayed (e.g. "log2(z-score(units))").
            var.adjustment <- .na_to_null(isolate_fn(input$var.adjustment))
            var.adj.fxn.name <- isolate_fn(input$var.adj.fxn)

            # The Y Axis Min/Max inputs are derived from the raw data range, so let the
            # continuous axis auto-scale whenever an adjustment rescales the values.
            adjustment.active <- !is.null(var.adjustment) ||
                (!is.null(var.adj.fxn.name) && nzchar(var.adj.fxn.name))

            y_axis_label <- if (multivar) {
                # No single column name describes an axis shared by several variables
                # (they are named by the facet strips or the legend instead), so keep
                # only the adjustment description, if any.
                if (!adjustment.active) {
                    NULL
                } else if (!is.null(var.adjustment)) {
                    adjusted_axis_label(var.adjustment, adj.fxn = var.adj.fxn.name)
                } else {
                    var.adj.fxn.name
                }
            } else {
                adjusted_axis_label(y.vars, var.adjustment, var.adj.fxn.name)
            }

            # When an adjustment is active the axis title is regenerated to reflect it, so
            # flag its side for finalize_manual_edits() to skip persisting the text. The
            # continuous var lands on the x-axis for ridgeplots, otherwise the y-axis.
            regen_keys_rv(if (adjustment.active) {
                if ("ridgeplot" %in% isolate_fn(input$plots)) "axis:x" else "axis:y"
            } else {
                character(0)
            })

            # If a variable feeding an axis title (or the ridge orientation that swaps
            # which axis it lands on) changed, drop any persisted manual title text so the
            # title regenerates for the new variable. Runs before finalize_manual_edits()
            # in the same render pass, so the cleared store is what gets re-applied.
            axis_vars <- list(
                var = y.vars,
                group.by = isolate_fn(input$group.by),
                multivar.aes = if (multivar) multivar.aes else NULL,
                ridge = "ridgeplot" %in% isolate_fn(input$plots)
            )
            if (!identical(axis_vars, last_axis_vars())) {
                reset_axis_title_text(edit_store)
                last_axis_vars(axis_vars)
            }

            # Draw axis borders at the ggplot level
            additional_theme <- create_ggplot_axis_style(input, isolate_fn = isolate_fn)
            theme_style <- theme_bw() + theme(
                panel.border = additional_theme$panel.border,
                axis.line = additional_theme$axis.line,
                axis.ticks = additional_theme$axis.ticks,
                strip.background = element_blank()
            )

            # Collect hover data. When the user makes no explicit selection,
            # reconstruct dittoViz::yPlot()'s internal default set so hover
            # content is unchanged from the package default. Columns that do not
            # exist in the plotted data are ignored downstream by dittoViz.
            annotate.by <- .na_to_null(isolate_fn(input$annotate.by))
            hover.data <- .na_to_null(isolate_fn(input$hover.data))
            if (is.null(hover.data)) {
                var.name <- y.vars
                hover.data <- unique(c(
                    var.name,
                    paste0(var.name, ".adj"),
                    "var.multi", "var.which",
                    isolate_fn(input$group.by),
                    color.by,
                    shape.by,
                    split.by
                ))
            }
            # Point annotations are parsed back out of the hover text, so the
            # annotation column has to be carried in it.
            hover.data <- unique(c(hover.data, annotate.by))

            # One dodge width for the whole figure: the ggplot layers below, the box
            # traces afterwards, and the stat brackets all have to agree on it.
            boxgap <- .box_num(isolate_fn(input$boxgap), 0.3)
            boxgroupgap <- .box_num(isolate_fn(input$boxgroupgap), 0.2)

            p <- .with_stable_seed(yPlot(
                data_frame = data(),
                var = y.vars,
                multivar.aes = multivar.aes,
                multivar.split.dir = isolate_fn(input$multivar.split.dir),
                var.adjustment = var.adjustment,
                var.adj.fxn = safe_resolve_adj_fxn(var.adj.fxn.name),
                # Blank main title by default; dittoViz's "make" would otherwise
                # auto-generate one (the var name) and re-render it every rebuild.
                main = NULL,
                ylab = y_axis_label,
                group.by = isolate_fn(input$group.by),
                color.by = color.by,
                shape.by = shape.by,
                split.by = split.by,
                plots = isolate_fn(input$plots),
                do.hover = TRUE,
                hover.data = hover.data,
                hover.round.digits = isolate_fn(input$hover.round.digits),
                color.panel = if (!is.null(color.panel.arg)) color.panel.arg else dittoViz::dittoColors(),
                # A blank numeric input reports NULL (not NA) to Shiny, but
                # dittoViz::yPlot()'s internal is.na(min)/is.na(max) checks
                # require a scalar NA -- NULL crashes them ("missing value
                # where TRUE/FALSE needed" from `is.na(NULL) || is.na(NULL)`).
                min = if (adjustment.active) NA else y.limits$min %__% NA,
                max = if (adjustment.active) NA else y.limits$max %__% NA,
                split.nrow = split.nrow,
                split.ncol = split.ncol,
                split.adjust = split.adjust,
                do.raster = isolate_fn(input$do.raster),
                raster.dpi = isolate_fn(input$raster.dpi),
                jitter.size = isolate_fn(input$jitter.size),
                jitter.width = isolate_fn(input$jitter.width),
                jitter.color = isolate_fn(input$jitter.color),
                jitter.shape.legend.size = isolate_fn(input$jitter.shape.legend.size),
                jitter.shape.legend.show = isolate_fn(input$jitter.shape.legend.show),
                jitter.position.dodge = 1 - boxgap,
                boxplot.color = isolate_fn(input$boxplot.color),
                # Hide outliers when jitter points are shown (to avoid
                # double-plotting) or when the user disables them. dittoViz::yPlot
                # sets boxpoints = FALSE natively, so no post-hoc removal needed.
                boxplot.show.outliers = isolate_fn(input$boxplot.show.outliers) &&
                    !("jitter" %in% isolate_fn(input$plots)),
                boxplot.fill = isolate_fn(input$boxplot.fill),
                boxplot.lineweight = isolate_fn(input$boxplot.lineweight),
                vlnplot.lineweight = isolate_fn(input$vlnplot.lineweight),
                vlnplot.scaling = isolate_fn(input$vlnplot.scaling),
                vlnplot.width = 1 - boxgap,
                ridgeplot.lineweight = isolate_fn(input$ridgeplot.lineweight),
                ridgeplot.scale = isolate_fn(input$ridgeplot.scale),
                ridgeplot.ymax.expansion = ridgeplot.ymax.expansion,
                ridgeplot.shape = isolate_fn(input$ridgeplot.shape),
                ridgeplot.bins = isolate_fn(input$ridgeplot.bins),
                ridgeplot.binwidth = ridgeplot.binwidth,
                legend.show = TRUE,
                theme = theme_style
            ))

            fig <- p
            if (faceted) {
                fig <- apply_facet_subplot_spacing(
                    fig,
                    spacing = c(isolate_fn(input$subplot.margin.x), isolate_fn(input$subplot.margin.y)),
                    ncol = split.ncol,
                    nrow = split.nrow
                )

            }
            fig <- apply_title_layout(fig, input, isolate_fn, title_y = 0.98, title_x = isolate_fn(input$axis.title.horizontal.position))
            
            


            # Put the boxes back on the coordinates ggplot dodged them to, which is
            # where the jitter and violin traces already are. The dodge width has to
            # be the one yPlot() built the ggplot with: boxplot.position.dodge and
            # jitter.position.dodge both default to vlnplot.width, which is
            # passed as 1 - boxgap above.
            fig <- .align_box_positions(
                fig,
                dodge.width = 1 - boxgap,
                box.width = 1 - boxgroupgap
            )

            # Apply axis styling (borders handled at the ggplot level via theme_style above)
            xaxis_style <- create_axis_styles(input, axis_side = "x", isolate_fn = isolate_fn)
            yaxis_style <- create_axis_styles(input, axis_side = "y", isolate_fn = isolate_fn)

            fig <- apply_subplot_axis_styling(fig, xaxis_style, yaxis_style)


            # Apply axis title font to shared facet annotation titles
            if (faceted) {
                fig <- apply_axis_title_to_annotations(fig, input, isolate_fn)
            }

            # Add reference lines
            fig <- add_reference_lines(fig,
                hline.intercepts = isolate_fn(input$hline.intercepts),
                hline.colors = isolate_fn(input$hline.colors),
                hline.widths = isolate_fn(input$hline.widths),
                hline.linetypes = isolate_fn(input$hline.linetypes),
                hline.opacities = isolate_fn(input$hline.opacities),
                vline.intercepts = isolate_fn(input$vline.intercepts),
                vline.colors = isolate_fn(input$vline.colors),
                vline.widths = isolate_fn(input$vline.widths),
                vline.linetypes = isolate_fn(input$vline.linetypes),
                vline.opacities = isolate_fn(input$vline.opacities),
                abline.slopes = isolate_fn(input$abline.slopes),
                abline.intercepts = isolate_fn(input$abline.intercepts),
                abline.colors = isolate_fn(input$abline.colors),
                abline.widths = isolate_fn(input$abline.widths),
                abline.linetypes = isolate_fn(input$abline.linetypes),
                abline.opacities = isolate_fn(input$abline.opacities)
            )

            # Statistical annotations. With several Y variables the tests are run per
            # variable against dittoViz's reshaped data, which only lines up with what
            # is drawn while the variables are the sole faceting dimension (the "split"
            # aesthetic with no split.by) and the x-axis groups are left intact. The
            # Stats tab is hidden for the other multi-variable layouts.
            stats.supported <- !multivar || identical(facet.cols, "var.which")

            if (isolate_fn(input$stats.enabled) && stats.supported) {
                # yPlot uses group.by as the x-axis, color.by for nested grouping
                xvar <- isolate_fn(input$group.by)
                grp_var <- if (!is.null(color.by) && color.by != xvar) color.by else NULL
                stat_pairs <- parse_pair_strings(isolate_fn(input$stat.pairs))

                # Mirror dittoViz's multi-variable reshape so each variable's facet is
                # tested on its own values; a single variable keeps the raw data.
                stats.data <- if (multivar) .multivar_long_df(data(), y.vars) else data()
                yvar <- if (multivar) "var.multi" else y.vars
                facet.var <- if (multivar) "var.which" else split.by
                # Variables in separate facets are never pooled, whatever the Stats tab
                # asks for, since their values are not comparable.
                per.facet <- if (multivar) TRUE else isolate_fn(input$stat.per.facet)

                stats_df <- compute_pairwise_stats(
                    df = stats.data, x = xvar,
                    y = yvar, pairs = stat_pairs,
                    test = isolate_fn(input$stat.test),
                    p.adjust.method = isolate_fn(input$stat.p.adjust),
                    paired = isolate_fn(input$stat.paired),
                    group.by = grp_var, facet.by = facet.var,
                    per.facet = per.facet,
                    sig.threshold = isolate_fn(input$stat.sig.threshold)
                )

                last_stats_df(stats_df)

                stat_result <- create_stat_annotations(
                    stats_df = stats_df, fig = fig, df = stats.data,
                    x = xvar, y = yvar,
                    display = isolate_fn(input$stat.display),
                    hide.ns = isolate_fn(input$stat.hide.ns),
                    sig.threshold = isolate_fn(input$stat.sig.threshold),
                    line.color = isolate_fn(input$stat.line.color),
                    line.width = isolate_fn(input$stat.line.width),
                    bracket.style = isolate_fn(input$stat.bracket.style),
                    group.by = grp_var, facet.by = facet.var,
                    step.increase = isolate_fn(input$stat.step.increase),
                    text.bump = isolate_fn(input$stat.text.bump),
                    bracket.inset = isolate_fn(input$stat.bracket.inset),
                    # Brackets between two groups sit on the same slot centres
                    # .align_box_positions() puts the boxes on.
                    dodge.width = 1 - boxgap
                )

                # An active adjustment lets the axis auto-scale, so there are no
                # limits of ours for the brackets to be measured against.
                fig <- apply_stat_annotations(fig, stat_result,
                    y.min = if (adjustment.active) NULL else y.limits$min,
                    y.max = if (adjustment.active) NULL else y.limits$max
                )
            }

            # Highlight and label individual jitter points. Rasterized jitter is drawn
            # as a single image, so there are no points left to match against.
            jitter.drawn <- "jitter" %in% isolate_fn(input$plots) &&
                !isTRUE(isolate_fn(input$do.raster))
            annos <- NULL

            if (jitter.drawn && !is.null(annotate.by)) {
                highlight_points_raw <- isolate_fn(input$highlight.points)
                highlight_vals <- character(0)
                if (!is.null(highlight_points_raw) && highlight_points_raw != "") {
                    highlight_vals <- .string_to_vector(highlight_points_raw)
                    highlight_vals <- highlight_vals[highlight_vals != ""]
                }

                if (length(highlight_vals) > 0) {
                    fig <- .apply_highlight_styling(
                        fig,
                        annotate.by = annotate.by,
                        highlight_vals = highlight_vals,
                        style = list(
                            color = isolate_fn(input$highlight.color),
                            size = isolate_fn(input$highlight.size),
                            border.color = isolate_fn(input$highlight.border.color),
                            border.width = isolate_fn(input$highlight.border.width)
                        ),
                        default.size = isolate_fn(input$jitter.size),
                        require.markers = TRUE
                    )
                }

                annotation_params <- list(
                    ax = isolate_fn(input$annotation.ax),
                    ay = isolate_fn(input$annotation.ay),
                    showarrow = isolate_fn(input$annotation.showarrow),
                    arrowcolor = isolate_fn(input$annotation.arrowcolor),
                    arrowhead = isolate_fn(input$annotation.arrowhead),
                    arrowwidth = isolate_fn(input$annotation.arrowwidth),
                    size = isolate_fn(input$annotation.size),
                    color = isolate_fn(input$annotation.color)
                )

                if (!is.null(selected.data())) {
                    annos <- .create_selected_annotations(
                        selected_data = selected.data(),
                        fig = fig,
                        annotate.by = annotate.by,
                        annotation_params = annotation_params,
                        require.markers = TRUE
                    )
                }

                if (isTRUE(isolate_fn(input$highlight.auto.annotate)) && length(highlight_vals) > 0) {
                    highlight_annos <- .create_highlight_annotations(
                        plot_data = data(),
                        fig = fig,
                        annotate.by = annotate.by,
                        highlight_vals = highlight_vals,
                        x_col = isolate_fn(input$group.by),
                        y_col = y.vars[1],
                        annotation_params = annotation_params,
                        require.markers = TRUE
                    )
                    annos <- .merge_annotation_sets(annos, highlight_annos)
                }
            }

            # Appended rather than set, so facet strip labels and stat brackets survive
            if (!is.null(annos) && length(annos) > 0) {
                fig$x$layout$annotations <- c(fig$x$layout$annotations, annos)
            }

            config_list <- add_plot_config(download.format = isolate_fn(input$download.format), include.modebar.buttons = TRUE, facet.by = facet.cols)
            fig <- do.call(config, c(list(p = fig), config_list))
            fig <- apply_plotly_newshape(fig, input, isolate_fn)

            # Apply uniform legend title/label font sizes
            fig <- apply_legend_styling(
                fig,
                title.size = isolate_fn(input$legend.title.size),
                text.size = isolate_fn(input$legend.text.size)
            )

            # Make single-panel x/y axis titles draggable (matches faceted behaviour)
            fig <- axis_titles_as_annotations(fig)

            return(fig)
        })

        # Render the plot output
        output$yPlot <- renderPlotly({
            req(input$var)

            fig <- apply_render_margins(generate_yPlot(), input)
            fig <- finalize_manual_edits(
                fig, plot_source, edit_store, session,
                regen_keys = isolate(regen_keys_rv())
            )

            return(fig)
        })

        # Download handler for source (plot + data + stats)
        # Capture all UI inputs for the source download
        AllInputs <- reactive({
            x <- reactiveValuesToList(input)
            return(x)
        })

        plot_source_reactive <- reactive({
            collect_source_data(
                plot_reactive = generate_yPlot,
                stats_reactive = last_stats_df,
                inputs_reactive = AllInputs()
            )
        })

        output$download.source <- create_source_download_handler(
            data_list = plot_source_reactive,
            filename_base = "yPlot_source"
        )

        return(plot_source_reactive)
    })
}
