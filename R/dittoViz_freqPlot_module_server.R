#' Server logic for freqPlot module
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
#' @return The `moduleServer` function for the freqPlot module.
#'
#' @import shiny
#' @import plotly
#' @importFrom ggplot2 theme_bw theme element_blank
#' @importFrom stats na.omit
#' @importFrom dittoViz freqPlot dittoColors
#' @importFrom shinyjs delay
#' @importFrom shinyWidgets updateMaterialSwitch
#' @importFrom colourpicker updateColourInput
#'
#' @seealso [dittoViz::freqPlot()], [VizModules::dittoViz_freqPlotInputsUI()],
#' [VizModules::dittoViz_freqPlotOutputUI()], [VizModules::dittoViz_freqPlotApp()]
#'
#' @export
#' @author Jared Andrews
dittoViz_freqPlotServer <- function(id, data, hide.inputs = NULL, hide.tabs = NULL, defaults = NULL) {
    stopifnot(is.reactive(data))
    data <- .require_data_frame(data)

    moduleServer(id, function(input, output, session) {
        params <- setup_reactive_defaults(defaults, input, session)

        # Hide individual inputs/tabs if specified. The inputs UI is injected by the
        # parent app via renderUI (and re-injected when the dataset changes), so the
        # hiding must be (re)applied after the controls exist in the DOM rather than
        # once at module initialization.
        #
        # "Per Facet Panel" is always hidden: this plot is always faceted on the
        # frequency variable, and the frequencies of two different levels are not
        # comparable quantities, so pooling across facets would compare nonsense.
        # The tests are forced per-facet below.
        observeEvent(data(), {
            delay(100, {
                hide_input(session, c(hide.inputs, "stat.per.facet"))
                for (tab.name in hide.tabs) hideTab(inputId = "freqPlotTabsetPanel", target = tab.name)
            })
        })

        ns <- session$ns

        # Persist manual legend/annotation/axis-title repositioning across rebuilds.
        plot_source <- session$ns("freq")
        edit_store <- setup_manual_edits(input, session, plot_source)

        # Store last computed stats table for download
        last_stats_df <- reactiveVal(NULL)

        # Points gathered from box/lasso selections, accumulated across selections
        selected.data <- reactiveVal()

        # Axis-defining variables at the last build; when they change we drop any
        # persisted manual axis-title text so the title regenerates.
        last_axis_vars <- reactiveVal(NULL)

        # ---- The frequency table the plot is actually drawn from -----------------
        # freqPlot() summarises its input rather than plotting columns of it, so the
        # axis limits, the statistics and the point annotations all have to be
        # computed against this frame. Reads inputs live; the render path recomputes
        # it from isolated values so that "Auto Update" off still holds the plot.
        summary_df <- reactive({
            df <- data()
            req(df)
            .freq_summary(
                df,
                var = input$var %__% "",
                sample.by = .blank_to_null(input$sample.by),
                group.by = input$group.by %__% "",
                color.by = .blank_to_null(input$color.by),
                scale = input$scale %__% "percent",
                max.normalize = isTRUE(input$max.normalize),
                vars.use = .freq_selected_vars(input$vars.use)
            )
        })

        # ---- Keep dependent choices in sync -------------------------------------
        # The grouping selectors are the anchors and the sample selector is narrowed
        # to what nests inside them, so the two never chase each other in a loop.
        #
        # Every one of these observers is strictly corrective: it repoints a control
        # whose current selection has stopped being valid. None of them run before
        # that control has reported a value, because update_viz_select() falls back
        # to the first choice when the widget has nothing to keep - which at startup
        # would overwrite the value the inputs UI just seeded from `defaults` (the
        # UI and the server are given `defaults` separately, and an app may pass it
        # to only one of them).

        # Only the set of usable columns matters here, not a row filter that leaves
        # them alone, so a filtered dataset costs no rebuild.
        cat_choice_cache <- reactiveVal(NULL)
        observeEvent(data(), {
            df <- data()
            req(df)
            cat.choices <- .facet_check(df)
            if (identical(cat.choices, cat_choice_cache())) {
                return()
            }
            cat_choice_cache(cat.choices)

            var.current <- isolate(input$var)
            if (is.null(var.current)) {
                return()
            }

            var.sel <- if (var.current %in% cat.choices) {
                var.current
            } else {
                get_default(defaults, "var", if (length(cat.choices)) cat.choices[1] else "",
                    function(x) x %in% cat.choices)
            }
            if (!identical(var.sel, var.current)) {
                freezeReactiveValue(input, "var")
            }
            update_viz_select(session, "var", choices = cat.choices, selected = var.sel)

            group.choices <- setdiff(cat.choices, var.sel)
            group.current <- isolate(input$group.by)
            if (!is.null(group.current) && !group.current %in% group.choices) {
                freezeReactiveValue(input, "group.by")
                update_viz_select(session, "group.by", choices = group.choices,
                    selected = if (length(group.choices)) group.choices[1] else "")
            } else {
                update_viz_select(session, "group.by", choices = group.choices, selected = group.current)
            }

            color.current <- isolate(input$color.by)
            if (!is.null(color.current) && nzchar(color.current) && !color.current %in% group.choices) {
                freezeReactiveValue(input, "color.by")
                update_viz_select(session, "color.by", choices = c("", group.choices), selected = "")
            } else {
                update_viz_select(session, "color.by", choices = c("", group.choices), selected = color.current)
            }
        }, ignoreNULL = TRUE)

        # The facets are the levels of `var`, so its levels are what can be chosen.
        observeEvent(input$var, ignoreInit = TRUE, {
            df <- data()
            req(df, input$var, input$var %in% names(df))
            current <- isolate(input$vars.use)
            if (is.null(current)) {
                return()
            }
            lvls <- levels(as.factor(df[[input$var]]))
            keep <- intersect(current, lvls)
            # Pause readers until the client echoes the new selection, otherwise the
            # plot renders once now and again when that echo lands.
            freezeReactiveValue(input, "vars.use")
            update_viz_select(session, "vars.use", choices = lvls, selected = keep)
        })

        observeEvent(c(input$group.by, input$color.by, input$var), ignoreInit = TRUE, {
            df <- data()
            req(df)
            current <- isolate(input$sample.by)
            if (is.null(current)) {
                return()
            }
            group.cols <- c(input$group.by, input$color.by)
            group.cols <- group.cols[!is.na(group.cols) & nzchar(group.cols)]
            sample.choices <- setdiff(.freq_sample_choices(df, group.cols), input$var %__% "")
            keep <- if (current %in% sample.choices) current else ""
            if (!identical(keep, current)) {
                freezeReactiveValue(input, "sample.by")
            }
            update_viz_select(session, "sample.by", choices = c("", sample.choices), selected = keep)
        })

        # Only the sample and color columns are carried in freqPlot()'s hover text,
        # and the annotations are parsed back out of it, so only those can be labelled.
        observeEvent(c(input$sample.by, input$color.by, input$group.by), ignoreInit = TRUE, {
            current <- isolate(input$annotate.by)
            if (is.null(current)) {
                return()
            }
            annotate.choices <- unique(c(input$sample.by, input$color.by))
            annotate.choices <- annotate.choices[!is.na(annotate.choices) & nzchar(annotate.choices)]
            keep <- if (current %in% annotate.choices) current else ""
            if (!identical(keep, current)) {
                freezeReactiveValue(input, "annotate.by")
            }
            update_viz_select(session, "annotate.by", choices = c("", annotate.choices), selected = keep)
        })

        # Comparisons are between the x-axis groups. Those are the summary's
        # `grouping` column, but its levels are exactly the levels of `group.by` in
        # the input, so reading them from there keeps this off the summary and the
        # user's chosen comparisons survive a change of scale or of visible facets.
        observeEvent(c(input$group.by, input$color.by), {
            df <- data()
            req(df, input$group.by, input$group.by %in% names(df))
            color.col <- .freq_stats_group_col(input$group.by, input$color.by)
            pair_strings <- generate_pair_strings(df, input$group.by, color.col)
            # Pause readers until the client echoes the cleared selection, otherwise
            # the plot renders once now and again when that echo lands.
            freezeReactiveValue(input, "stat.pairs")
            update_viz_select(session, "stat.pairs", choices = c("", pair_strings), selected = "")
        })

        # Selections are held as trace/point indices, which only describe the layout
        # they were made on, so they are dropped when that layout changes.
        observeEvent(
            c(
                input$var, input$sample.by, input$group.by, input$color.by,
                input$vars.use, input$plots, input$scale, input$max.normalize
            ),
            {
                selected.data(NULL)
            },
            ignoreInit = TRUE
        )

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

        # ---- Colour picker -------------------------------------------------------
        default_palette_name <- "dittoColors"
        palette_lookup <- .flatten_palette_options(default_palettes()[["choices"]])
        default_palette_values <- palette_lookup[[default_palette_name]]
        if (is.null(default_palette_values) || length(default_palette_values) == 0) {
            default_palette_values <- if (length(palette_lookup) > 0) palette_lookup[[1]] else character(0)
        }

        # The summary carries the color column through under its original name, with
        # the same levels as the input, so the picker can be built from the input
        # without waiting on the summarisation.
        palette_groups <- reactive({
            df <- data()
            if (is.null(df)) {
                return(character(0))
            }
            col_to_use <- .blank_to_null(input$color.by) %__% .blank_to_null(input$group.by)
            if (is.null(col_to_use) || !col_to_use %in% names(df)) {
                return(character(0))
            }
            col_data <- na.omit(df[[col_to_use]])
            if (is.factor(col_data)) levels(droplevels(col_data)) else levels(as.factor(col_data))
        })

        # The picker is rebuilt by renderUI, so freezeReactiveValue() cannot cover it.
        # This server-side store is what the plot reads instead.
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

            # Seed the store with exactly what the picker is built from, so the first
            # draw uses the right palette rather than waiting for the client to report.
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

        # ---- Y-axis limits, computed from the frequency table --------------------
        # The plotted values are frequencies the module computes, so the limits have
        # to come from the summary; the raw data has no column holding them.
        stat_headroom <- function() {
            if (!isTRUE(input$stats.enabled)) {
                return(NULL)
            }
            summ <- summary_df()
            if (is.null(summ) || nrow(summ) == 0) {
                return(NULL)
            }
            y.col <- .freq_y_col(input$scale, input$max.normalize)
            if (!y.col %in% names(summ)) {
                return(NULL)
            }
            .stat_bracket_headroom(
                df = summ, x = "grouping", y = y.col,
                group.by = .freq_stats_group_col(input$group.by, input$color.by),
                facet.by = "label", per.facet = TRUE, input = input,
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

        observeEvent(
            c(input$var, input$sample.by, input$group.by, input$color.by,
              input$vars.use, input$scale, input$max.normalize),
            {
                summ <- summary_df()
                req(summ)
                y.col <- .freq_y_col(input$scale, input$max.normalize)
                y_range <- .calculate_range(
                    df = summ, data_col_y = y.col,
                    axis_scale_factor = .y_axis_scale_factor, grouping = FALSE
                )
                if (!is.null(y_range)) {
                    y_range_store(list(min = y_range$min, max = y_range$max))
                    updateNumericInput(session, "y.max", value = y_range$max)
                    updateNumericInput(session, "y.min", value = y_range$min)
                }
            }
        )

        # ---- Reset ---------------------------------------------------------------
        observeEvent(input$reset, {
            df <- data()
            choices <- c("", names(df))
            cat.choices <- .facet_check(df)

            var.default <- get_default(
                defaults, "var", if (length(cat.choices)) cat.choices[1] else "",
                function(x) x %in% cat.choices
            )
            group.choices <- setdiff(cat.choices, var.default)
            group.default <- get_default(
                defaults, "group.by", if (length(group.choices)) group.choices[1] else "",
                function(x) x %in% group.choices
            )
            color.default <- get_default(
                defaults, "color.by", "",
                function(x) x == "" || x %in% group.choices
            )
            sample.choices <- .freq_sample_choices(df, c(group.default, color.default))
            sample.default <- get_default(
                defaults, "sample.by", "",
                function(x) x == "" || x %in% sample.choices
            )
            vars.use.choices <- if (nzchar(var.default) && var.default %in% names(df)) {
                levels(as.factor(df[[var.default]]))
            } else {
                character(0)
            }

            # Data
            update_viz_select(session, "var", choices = cat.choices, selected = var.default)
            update_viz_select(session, "group.by", choices = group.choices, selected = group.default)
            update_viz_select(session, "color.by", choices = c("", group.choices), selected = color.default)
            update_viz_select(session, "sample.by", choices = c("", sample.choices), selected = sample.default)
            update_viz_select(session, "vars.use",
                choices = vars.use.choices,
                selected = get_default(defaults, "vars.use", "",
                    function(x) all(x == "") || all(x %in% vars.use.choices))
            )
            update_viz_select(session, "plots",
                selected = get_default(defaults, "plots", c("boxplot", "jitter")))

            # Scale
            update_viz_select(session, "scale", selected = get_default(defaults, "scale", "percent"))
            updateMaterialSwitch(session, "max.normalize",
                value = get_default(defaults, "max.normalize", FALSE, is.logical))

            reset.summary <- .freq_summary(
                df, var = var.default, sample.by = .blank_to_null(sample.default),
                group.by = group.default, color.by = .blank_to_null(color.default),
                scale = get_default(defaults, "scale", "percent"),
                max.normalize = get_default(defaults, "max.normalize", FALSE, is.logical)
            )
            reset.range <- if (!is.null(reset.summary)) {
                .calculate_range(
                    df = reset.summary,
                    data_col_y = .freq_y_col(
                        get_default(defaults, "scale", "percent"),
                        get_default(defaults, "max.normalize", FALSE, is.logical)
                    ),
                    axis_scale_factor = .y_axis_scale_factor, grouping = FALSE
                )
            } else {
                NULL
            }
            reset.y.min <- get_default(defaults, "y.min", if (!is.null(reset.range)) reset.range$min else 0)
            reset.y.max <- get_default(defaults, "y.max", if (!is.null(reset.range)) reset.range$max else NA)
            y_range_store(list(min = reset.y.min, max = reset.y.max))
            updateNumericInput(session, "y.min", value = reset.y.min)
            updateNumericInput(session, "y.max", value = reset.y.max)

            # Jitter
            updateNumericInput(session, "jitter.size", value = get_default(defaults, "jitter.size", 1, is.numeric))
            updateNumericInput(session, "jitter.width", value = get_default(defaults, "jitter.width", 0.2, is.numeric))
            updateColourInput(session, "jitter.color", value = get_default(defaults, "jitter.color", "#000000"))
            updateNumericInput(session, "hover.round.digits",
                value = get_default(defaults, "hover.round.digits", 5, is.numeric))
            updateMaterialSwitch(session, "do.raster", value = get_default(defaults, "do.raster", FALSE, is.logical))
            updateNumericInput(session, "raster.dpi", value = get_default(defaults, "raster.dpi", 600, is.numeric))

            # Box
            updateMaterialSwitch(session, "boxplot.show.outliers",
                value = get_default(defaults, "boxplot.show.outliers", FALSE, is.logical))
            updateColourInput(session, "boxplot.color", value = get_default(defaults, "boxplot.color", "#000000"))
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
            update_viz_select(session, "split.adjust", selected = get_default(defaults, "split.adjust", "fixed"))
            updateNumericInput(session, "split.ncol", value = get_default(defaults, "split.ncol", NA, is.numeric))
            updateNumericInput(session, "split.nrow", value = get_default(defaults, "split.nrow", NA, is.numeric))

            # Group colors
            .reset_group_colors(session, "palette.colours", defaults, palette_groups(), default_palette_values)

            # Shared tabs
            reset_axes_inputs(session, defaults)
            reset_plotly_inputs(session, defaults)
            reset_legend_inputs(session, defaults)
            reset_lines_inputs(session, defaults = defaults)
            reset_annotation_inputs(session, defaults, choices)
            selected.data(NULL)
            .reset_stats_inputs(session, defaults)
        })

        # ---- Build the figure ----------------------------------------------------
        generate_freqPlot <- reactive({
            req(input$var, input$group.by)
            isolate_fn <- setup_auto_update_logic(input, params)

            var.col <- isolate_fn(input$var)
            group.col <- isolate_fn(input$group.by)
            sample.col <- .blank_to_null(isolate_fn(input$sample.by))
            color.col <- .blank_to_null(isolate_fn(input$color.by))

            # `vars.use` is a multi-select, so .blank_to_null() would read any
            # multi-value selection as "no selection" and silently draw every facet.
            vars.use <- .freq_selected_vars(isolate_fn(input$vars.use))

            scale.arg <- isolate_fn(input$scale)
            max.normalize <- isolate_fn(input$max.normalize)
            y.col <- .freq_y_col(scale.arg, max.normalize)

            df <- data()
            req(df, var.col %in% names(df), group.col %in% names(df))

            # The frame the plot is drawn from, recomputed here from isolated values
            # so an "Auto Update" pause holds this build too.
            summ <- .freq_summary(
                df, var = var.col, sample.by = sample.col, group.by = group.col,
                color.by = color.col, scale = scale.arg, max.normalize = max.normalize,
                vars.use = vars.use
            )
            req(summ, nrow(summ) > 0)

            # Resolved server-side, so they already clear any significance brackets.
            y.limits <- isolate_fn(y_range_store())

            split.ncol <- .na_to_null(isolate_fn(input$split.ncol))
            split.nrow <- .na_to_null(isolate_fn(input$split.nrow))
            ridgeplot.binwidth <- .na_to_null(isolate_fn(input$ridgeplot.binwidth))

            # Keep names so scale_fill_manual matches colors to groups by name,
            # making the mapping independent of positional order.
            palette_values <- isolate_fn(palette_store())
            color.panel.arg <- if (!is.null(palette_values) && length(palette_values) > 0) {
                palette_values
            } else {
                dittoColors()
            }

            split.adjust <- list(scales = "free")
            if (isolate_fn(input$split.adjust) != "free") {
                split.adjust$scales <- isolate_fn(input$split.adjust)
            }

            # Drop any persisted manual axis-title text when a variable feeding a
            # title changes, so the title regenerates for the new variable (its
            # dragged position still persists). Runs before finalize_manual_edits()
            # in the same render pass, so the cleared store is what gets re-applied.
            axis_vars <- list(
                var = var.col, group.by = group.col, scale = scale.arg,
                max.normalize = max.normalize,
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

            # One dodge width for the whole figure: the ggplot layers below, the box
            # traces afterwards, and the stat brackets all have to agree on it.
            boxgap <- .box_num(isolate_fn(input$boxgap), 0.3)
            boxgroupgap <- .box_num(isolate_fn(input$boxgroupgap), 0.2)

            p <- .with_stable_seed(freqPlot(
                data_frame = df,
                var = var.col,
                sample.by = sample.col,
                group.by = group.col,
                color.by = if (is.null(color.col)) group.col else color.col,
                vars.use = vars.use,
                scale = scale.arg,
                max.normalize = max.normalize,
                plots = isolate_fn(input$plots),
                # Blank main title by default; freqPlot's "make" would otherwise
                # auto-generate one (the var name) and re-render it every rebuild.
                main = NULL,
                ylab = NULL,
                do.hover = TRUE,
                hover.round.digits = isolate_fn(input$hover.round.digits),
                color.panel = color.panel.arg,
                # A blank numeric input reports NULL (not NA) to Shiny, but
                # freqPlot()'s internal is.na(min)/is.na(max) checks require a
                # scalar NA -- NULL crashes them.
                min = y.limits$min %__% NA,
                max = y.limits$max %__% NA,
                split.nrow = split.nrow,
                split.ncol = split.ncol,
                split.adjust = split.adjust,
                do.raster = isolate_fn(input$do.raster),
                raster.dpi = isolate_fn(input$raster.dpi),
                jitter.size = isolate_fn(input$jitter.size),
                jitter.width = isolate_fn(input$jitter.width),
                jitter.color = isolate_fn(input$jitter.color),
                jitter.position.dodge = 1 - boxgap,
                boxplot.color = isolate_fn(input$boxplot.color),
                # Hide outliers when jitter points are shown (to avoid
                # double-plotting) or when the user disables them.
                boxplot.show.outliers = isolate_fn(input$boxplot.show.outliers) &&
                    !("jitter" %in% isolate_fn(input$plots)),
                boxplot.fill = isolate_fn(input$boxplot.fill),
                boxplot.lineweight = isolate_fn(input$boxplot.lineweight),
                vlnplot.lineweight = isolate_fn(input$vlnplot.lineweight),
                vlnplot.scaling = isolate_fn(input$vlnplot.scaling),
                vlnplot.width = 1 - boxgap,
                ridgeplot.lineweight = isolate_fn(input$ridgeplot.lineweight),
                ridgeplot.scale = isolate_fn(input$ridgeplot.scale),
                # Blanking the field reports NULL, which would collapse the
                # expansion vector; freqPlot()'s own default is a scalar NA.
                ridgeplot.ymax.expansion = isolate_fn(input$ridgeplot.ymax.expansion) %__% NA,
                ridgeplot.shape = isolate_fn(input$ridgeplot.shape),
                ridgeplot.bins = isolate_fn(input$ridgeplot.bins),
                ridgeplot.binwidth = ridgeplot.binwidth,
                legend.show = TRUE,
                theme = theme_style
            ))

            fig <- p

            # Always faceted, one panel per level of the frequency variable.
            fig <- apply_facet_subplot_spacing(
                fig,
                spacing = c(isolate_fn(input$subplot.margin.x), isolate_fn(input$subplot.margin.y)),
                ncol = split.ncol, nrow = split.nrow
            )
            fig <- apply_title_layout(fig, input, isolate_fn,
                title_y = 0.98, title_x = isolate_fn(input$axis.title.horizontal.position))
            # Put the boxes back on the coordinates ggplot dodged them to, which is
            # where the jitter and violin traces already are. The dodge width has to
            # be the one freqPlot() built the ggplot with: boxplot.position.dodge and
            # jitter.position.dodge both default to vlnplot.width, passed as
            # 1 - boxgap above.
            fig <- .align_box_positions(
                fig,
                dodge.width = 1 - boxgap,
                box.width = 1 - boxgroupgap
            )

            xaxis_style <- create_axis_styles(input, axis_side = "x", isolate_fn = isolate_fn)
            yaxis_style <- create_axis_styles(input, axis_side = "y", isolate_fn = isolate_fn)
            fig <- apply_subplot_axis_styling(fig, xaxis_style, yaxis_style)
            fig <- apply_axis_title_to_annotations(fig, input, isolate_fn)

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

            # Statistical annotations. Comparisons are between the x-axis groups of
            # the summarised frame, and always within a facet: the facets hold
            # frequencies of different levels, which are not comparable quantities.
            if (isolate_fn(input$stats.enabled)) {
                stats.group <- .freq_stats_group_col(group.col, color.col)
                stat_pairs <- parse_pair_strings(isolate_fn(input$stat.pairs))

                stats_df <- compute_pairwise_stats(
                    df = summ, x = "grouping", y = y.col,
                    pairs = stat_pairs,
                    test = isolate_fn(input$stat.test),
                    p.adjust.method = isolate_fn(input$stat.p.adjust),
                    paired = isolate_fn(input$stat.paired),
                    group.by = stats.group, facet.by = "label",
                    per.facet = TRUE,
                    sig.threshold = isolate_fn(input$stat.sig.threshold)
                )

                last_stats_df(stats_df)

                stat_result <- create_stat_annotations(
                    stats_df = stats_df, fig = fig, df = summ,
                    x = "grouping", y = y.col,
                    display = isolate_fn(input$stat.display),
                    hide.ns = isolate_fn(input$stat.hide.ns),
                    sig.threshold = isolate_fn(input$stat.sig.threshold),
                    line.color = isolate_fn(input$stat.line.color),
                    line.width = isolate_fn(input$stat.line.width),
                    bracket.style = isolate_fn(input$stat.bracket.style),
                    group.by = stats.group, facet.by = "label",
                    step.increase = isolate_fn(input$stat.step.increase),
                    text.bump = isolate_fn(input$stat.text.bump),
                    bracket.inset = isolate_fn(input$stat.bracket.inset),
                    # Brackets between two groups sit on the same slot centres
                    # .align_box_positions() puts the boxes on.
                    dodge.width = 1 - boxgap
                )

                fig <- apply_stat_annotations(fig, stat_result,
                    y.min = y.limits$min, y.max = y.limits$max
                )
            }

            # Highlight and label individual jitter points, which here are samples.
            # Rasterized jitter is drawn as a single image, so there are no points
            # left to match against.
            annotate.by <- .na_to_null(isolate_fn(input$annotate.by))
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
                    # Read from the summary: the highlighted points are rows of the
                    # frequency table, not of the input data.
                    highlight_annos <- .create_highlight_annotations(
                        plot_data = summ,
                        fig = fig,
                        annotate.by = annotate.by,
                        highlight_vals = highlight_vals,
                        x_col = "grouping",
                        y_col = y.col,
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

            config_list <- add_plot_config(
                download.format = isolate_fn(input$download.format),
                include.modebar.buttons = TRUE, facet.by = "label"
            )
            fig <- do.call(config, c(list(p = fig), config_list))
            fig <- apply_plotly_newshape(fig, input, isolate_fn)
            fig <- apply_legend_styling(fig,
                title.size = isolate_fn(input$legend.title.size),
                text.size = isolate_fn(input$legend.text.size)
            )
            # Make single-panel x/y axis titles draggable, matching faceted behaviour.
            fig <- axis_titles_as_annotations(fig)

            return(fig)
        })

        # ---- Render --------------------------------------------------------------
        output$freqPlot <- renderPlotly({
            req(input$var, input$group.by)

            fig <- apply_render_margins(generate_freqPlot(), input)
            fig <- finalize_manual_edits(fig, plot_source, edit_store, session)

            return(fig)
        })

        # ---- Source data download -------------------------------------------------
        AllInputs <- reactive({
            reactiveValuesToList(input)
        })

        # plotly_data() on the built figure is the summarised frequency table, so the
        # download is the data actually plotted rather than the input rows.
        plot_source_reactive <- reactive({
            collect_source_data(
                plot_reactive = generate_freqPlot,
                stats_reactive = last_stats_df,
                inputs_reactive = AllInputs()
            )
        })

        output$download.source <- create_source_download_handler(
            data_list = plot_source_reactive,
            filename_base = "freqPlot_source"
        )

        return(plot_source_reactive)
    })
}
