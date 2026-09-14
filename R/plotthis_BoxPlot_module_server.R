#' Server logic for BoxPlot module
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
#' @return The `moduleServer` function for the BoxPlot module.
#'
#' @import shiny
#' @import plotly
#' @importFrom stats na.omit
#' @importFrom colourpicker updateColourInput
#' @importFrom plotthis BoxPlot
#' @importFrom shinyjs hide show delay
#' @importFrom shinyWidgets updateMaterialSwitch
#' @importFrom ggplot2 geom_boxplot
#'
#' @export
#' @author Jacob Martin, Jared Andrews
plotthis_BoxPlotServer <- function(id, data, hide.inputs = NULL, hide.tabs = NULL, defaults = NULL) {
    stopifnot(is.reactive(data))
    data <- .require_data_frame(data)

    moduleServer(id, function(input, output, session) {
        params <- setup_reactive_defaults(defaults, input, session)

        # Constant for y-axis scaling to ensure highest box reaches ~90% of chart height


        # Hide individual inputs/tabs if specified. The inputs UI is injected by the
        # parent app via renderUI (and re-injected when the dataset changes), so the
        # hiding must be (re)applied after the controls exist in the DOM rather than
        # once at module initialization.
        if (!is.null(hide.inputs) || !is.null(hide.tabs)) {
            observeEvent(data(), {
                delay(100, {
                    hide_input(session, hide.inputs)
                    for (tab.name in hide.tabs) hideTab(inputId = "BoxPlotTabsetPanel", target = tab.name)
                })
            })
        }

        ns <- session$ns

        # Persist manual legend/annotation/colorbar repositioning across rebuilds.
        plot_source <- session$ns("box")
        edit_store <- setup_manual_edits(input, session, plot_source)

        default_palette_name <- "dittoColors"

        # Store last computed stats table for download
        last_stats_df <- reactiveVal(NULL)
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

            group_col <- input$group.by
            x_col <- input$x.data

            if (!is.null(group_col) && nzchar(group_col) && group_col %in% names(df)) {
                unique(na.omit(as.character(df[[group_col]])))
            } else if (!is.null(x_col) && nzchar(x_col) && x_col %in% names(df)) {
                unique(na.omit(as.character(df[[x_col]])))
            } else {
                character(0)
            }
        })

        # What the plot actually colours by. The picker is rebuilt whenever the group
        # set changes and is re-seeded from this same resolution, so the value it
        # then reports resolves to the palette already in use. A reactiveVal only
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

        # Update stat comparison pairs when x or group.by changes
        observeEvent(c(input$x.data, input$group.by), {
            req(input$x.data)
            pair_strings <- generate_pair_strings(data(), input$x.data, input$group.by)
            # Pause readers until the client echoes the cleared selection, otherwise
            # the plot renders once now and again when that echo lands.
            freezeReactiveValue(input, "stat.pairs")
            update_viz_select(session, "stat.pairs", choices = c("", pair_strings), selected = "")
        })

        # Reset functionality
        observeEvent(input$reset, {
            numeric.data <- data()[, vapply(data(), is.numeric, logical(1)), drop = FALSE]
            char.choices <- c("", names(data())[vapply(data(), function(x) !is.numeric(x), logical(1))])
            num.choices <- c("", names(data())[vapply(data(), is.numeric, logical(1))])

            # Calculate y.max and y.min from the default selections
            if (length(num.choices) >= 2) {
                max.y <- max(numeric.data[[num.choices[2]]], na.rm = TRUE) * .y_axis_scale_factor
                min.y <- min(numeric.data[[num.choices[2]]], na.rm = TRUE)
            } else {
                max.y <- 1
                min.y <- 0
            }
            # Reset numeric inputs to defaults derived from data

            # Data
            update_viz_select(session, "group.by",
                selected = get_default(defaults, "group.by", "", function(x) x == "" || x %in% char.choices)
            )
            update_viz_select(session, "x.data",
                selected = get_default(defaults, "x.data", char.choices[2], function(x) x %in% char.choices)
            )
            update_viz_select(session, "y.data",
                selected = get_default(defaults, "y.data", num.choices[2], function(x) x %in% num.choices)
            )
            updateMaterialSwitch(session, "show.outliers",
                value = get_default(defaults, "show.outliers", TRUE, is.logical)
            )

            # Adjustments
            update_viz_select(session, "sort_x", selected = get_default(defaults, "sort_x", ""))
            updateMaterialSwitch(session, "rotate", value = get_default(defaults, "rotate", FALSE, is.logical))
            reset.y.min <- get_default(defaults, "y.min", min.y, is.numeric)
            reset.y.max <- get_default(defaults, "y.max", max.y, is.numeric)
            y_range_store(list(min = reset.y.min, max = reset.y.max))
            updateNumericInput(session, "y.min", value = reset.y.min)
            updateNumericInput(session, "y.max", value = reset.y.max)

            # Points
            updateMaterialSwitch(session, "add.points", value = get_default(defaults, "add.points", FALSE, is.logical))
            updateNumericInput(session, "pt.size", value = get_default(defaults, "pt.size", 1, is.numeric))
            updateNumericInput(session, "pt.alpha", value = get_default(defaults, "pt.alpha", 1, is.numeric))
            updateNumericInput(session, "jitter.width", value = get_default(defaults, "jitter.width", 0.3, is.numeric))
            updateNumericInput(session, "boxplot.width", value = get_default(defaults, "boxplot.width", 0.8, is.numeric))

            # Colors
            updateColourInput(session, "pt.color",
                value = get_default(defaults, "pt.color", "#000000")
            )
            updateNumericInput(session, "alpha", value = get_default(defaults, "alpha", 1, is.numeric))

            # Annotations
            updateTextInput(session, "highlight", value = get_default(defaults, "highlight", ""))
            updateColourInput(session, "highlight.colour",
                value = get_default(defaults, "highlight.colour", "#000000")
            )
            updateNumericInput(session, "highlight.size",
                value = get_default(defaults, "highlight.size", 1, is.numeric)
            )
            updateNumericInput(session, "highlight.alpha",
                value = get_default(defaults, "highlight.alpha", 1, is.numeric)
            )

            # Facet
            update_viz_select(session, "facet.by",
                selected = get_default(defaults, "facet.by", "", function(x) x == "" || x %in% char.choices)
            )
            update_viz_select(session, "facet.scale",
                selected = get_default(defaults, "facet.scale", "fixed")
            )
            updateNumericInput(session, "facet.ncol", value = get_default(defaults, "facet.ncol", NA, is.numeric))
            updateNumericInput(session, "facet.nrow", value = get_default(defaults, "facet.nrow", NA, is.numeric))
            updateMaterialSwitch(session, "facet.by.row",
                value = get_default(defaults, "facet.by.row", TRUE, is.logical)
            )

            # Action Button
            # Group colors
            .reset_group_colors(session, "palette.colours", defaults, palette_groups(), default_palette_values)

            reset_plotly_inputs(session, defaults)
            reset_legend_inputs(session, defaults)

            # Lines
            reset_lines_inputs(session, defaults = defaults)

            # Axes
            reset_axes_inputs(session, defaults)

            # Stats
            .reset_stats_inputs(session, defaults)
        })

        # How high the significance brackets will reach, so the y-axis can reserve
        # room for them rather than have the plot drawn with them clipped.
        stat_headroom <- function() {
            if (!isTRUE(input$stats.enabled)) {
                return(NULL)
            }

            .stat_bracket_headroom(
                df = data(), x = input$x.data, y = input$y.data,
                # A numeric group.by is a fill gradient, not a nested grouping,
                # exactly as the render treats it.
                group.by = .blank_to_null(input$group.by, data(), numeric_is_null = TRUE),
                facet.by = .blank_to_null(input$facet.by),
                per.facet = isTRUE(input$stat.per.facet),
                input = input
            )
        }

        # What the plot actually draws with. The limits are pushed into the y.min and
        # y.max controls below, which is a client round-trip; reading the store rather
        # than the raw inputs means the echo of a limit just set costs no rebuild.
        y_range_store <- setup_axis_range(
            input, session,
            headroom = stat_headroom, params = params
        )

        # Update y-axis range when y data column is changed
        observeEvent(input$y.data, {
            y_range <- .calculate_range(df = data(), data_col_y = input$y.data, axis_scale_factor = .y_axis_scale_factor, grouping = FALSE)
            if (!is.null(y_range)) {
                y_range_store(list(min = y_range$min, max = y_range$max))
                updateNumericInput(session, "y.max", value = y_range$max)
                updateNumericInput(session, "y.min", value = y_range$min)
            }
        })

        observeEvent(c(input$facet.by, input$x.data),
            {
                req(!is.null(input$facet.by), !is.null(input$x.data))
                if (input$facet.by == input$x.data) {
                    # Pause readers until the client echoes the new scale, else the
                    # plot renders once now and again when that echo lands.
                    freezeReactiveValue(input, "facet.scale")
                    update_viz_select(session, "facet.scale", selected = "free_x")
                }
            },
            ignoreInit = FALSE
        )

        observeEvent(input$facet.by, {
            if (.nz_value(input$facet.by)) {
                show_input(session, c("facet.title.font.size", "facet.title.font.color", "facet.title.font.family"))
            } else {
                hide_input(session, c("facet.title.font.size", "facet.title.font.color", "facet.title.font.family"))
            }
        })

        generate_BoxPlot <- reactive({
            isolate_fn <- setup_auto_update_logic(input, params)

            # Resolved server-side, so they already clear any significance brackets.
            y.limits <- isolate_fn(y_range_store())

            # Facet By Null option Upstream:
            facet.by <- NULL
            if (.nz_value(isolate_fn(input$facet.by))) {
                update_viz_select(session, "sort_x", selected = "") # Makes sure order x is not active when facet by is active
                facet.by <- isolate_fn(input$facet.by)
            }

            group.by <- NULL
            if (.nz_value(isolate_fn(input$group.by))) {
                group.by <- isolate_fn(input$group.by)
            }
            sort.x <- NULL
            if (.nz_value(isolate_fn(input$sort_x))) {
                sort.x <- isolate_fn(input$sort_x)
            }
            highlight <- validate_expression(isolate_fn(input$highlight), names(data()))

            # Convert NA to NULL for facet.ncol and facet.nrow
            facet.ncol <- .na_to_null(isolate_fn(input$facet.ncol))
            facet.nrow <- .na_to_null(isolate_fn(input$facet.nrow))

            palette_values <- resolve_palette(
                isolate_fn(palette_groups()),
                isolate_fn(palette_store()),
                default_palette_values,
                .default_group_colors(defaults, "palette.colours")
            )
            palcolor_arg <- NULL
            if (!is.null(palette_values) && length(palette_values) > 0) {
                # BoxPlot expects a named list for palcolor when manually setting colors
                palcolor_arg <- as.list(palette_values)
            }

            theme_args <- create_ggplot_axis_style(input, isolate_fn = isolate_fn)

            # Fill By colour grading
            char.choices <- c("", names(data())[vapply(data(), function(x) !is.numeric(x), logical(1))])
            num.choices <- c("", names(data())[vapply(data(), is.numeric, logical(1))])
            fill.by <- NULL
            if (!is.null(group.by) && group.by %in% num.choices) {
                fill.by <- group.by
                group.by <- NULL
            }
            p <- BoxPlot(
                data = data(),
                x = isolate_fn(input$x.data),
                y = isolate_fn(input$y.data),
                flip = isolate_fn(input$rotate),
                sort_x = sort.x,
                theme = "theme_this",
                theme_args = theme_args,
                y_max = y.limits$max,
                y_min = y.limits$min,
                add_point = isolate_fn(input$add.points),
                pt_size = isolate_fn(input$pt.size),
                pt_alpha = isolate_fn(input$pt.alpha),
                jitter_width = isolate_fn(input$jitter.width),
                pt_color = isolate_fn(input$pt.color),
                alpha = isolate_fn(input$alpha),
                palcolor = palcolor_arg,
                facet_by = facet.by,
                facet_scales = isolate_fn(input$facet.scale),
                facet_ncol = facet.ncol,
                facet_nrow = facet.nrow,
                facet_byrow = isolate_fn(input$facet.by.row),
                group_by = group.by,
                highlight = highlight,
                highlight_color = isolate_fn(input$highlight.colour),
                highlight_size = isolate_fn(input$highlight.size),
                highlight_alpha = isolate_fn(input$highlight.alpha)
            )

            # Remove outliers if jitter points are shown or if user explicitly disabled outliers
            if (isolate_fn(input$add.points) || !isolate_fn(input$show.outliers)) {
                p <- p + geom_boxplot(outlier.shape = NA)
            }
            fig <- ggplotly(p) |>
                layout(
                    boxmode = ifelse(!is.null(group.by), "group", "overlay"),
                    boxgap = 0.1,
                    boxgroupgap = 1 - isolate_fn(input$boxplot.width)
                )
            # Fix boxplot positioning across faceted subplots
            if (!is.null(facet.by) && nzchar(facet.by)) {
                fig <- .fix_boxplot_facet_positions(fig)
                fig <- apply_facet_subplot_spacing(
                    fig,
                    spacing = c(isolate_fn(input$subplot.margin.x), isolate_fn(input$subplot.margin.y)),
                    ncol = facet.ncol,
                    nrow = facet.nrow
                )
            }

            fig <- apply_title_layout(
                fig,
                input,
                isolate_fn,
                title_y = 0.95,
                title_x = isolate_fn(input$axis.title.horizontal.position)
            )

            # Statistical annotations
            if (isolate_fn(input$stats.enabled)) {
                stat_pairs <- parse_pair_strings(isolate_fn(input$stat.pairs))
                stats_df <- compute_pairwise_stats(
                    df = data(), x = isolate_fn(input$x.data),
                    y = isolate_fn(input$y.data), pairs = stat_pairs,
                    test = isolate_fn(input$stat.test),
                    p.adjust.method = isolate_fn(input$stat.p.adjust),
                    paired = isolate_fn(input$stat.paired),
                    group.by = group.by, facet.by = facet.by,
                    per.facet = isolate_fn(input$stat.per.facet),
                    sig.threshold = isolate_fn(input$stat.sig.threshold)
                )

                last_stats_df(stats_df)

                stat_result <- create_stat_annotations(
                    stats_df = stats_df, fig = fig, df = data(),
                    x = isolate_fn(input$x.data), y = isolate_fn(input$y.data),
                    display = isolate_fn(input$stat.display),
                    hide.ns = isolate_fn(input$stat.hide.ns),
                    sig.threshold = isolate_fn(input$stat.sig.threshold),
                    line.color = isolate_fn(input$stat.line.color),
                    line.width = isolate_fn(input$stat.line.width),
                    bracket.style = isolate_fn(input$stat.bracket.style),
                    group.by = group.by, facet.by = facet.by,
                    step.increase = isolate_fn(input$stat.step.increase),
                    text.bump = isolate_fn(input$stat.text.bump),
                    bracket.inset = isolate_fn(input$stat.bracket.inset)
                )

                fig <- apply_stat_annotations(
                    fig,
                    stat_result,
                    y.min = y.limits$min,
                    y.max = y.limits$max
                )
            }


            # Apply axis styling to all subplot axes (handles faceting/split_by)
            xaxis_style <- create_axis_styles(input, axis_side = "x", isolate_fn = isolate_fn)
            yaxis_style <- create_axis_styles(input, axis_side = "y", isolate_fn = isolate_fn)

            fig <- apply_subplot_axis_styling(fig, xaxis_style, yaxis_style)

            # Apply axis title font to shared facet annotation titles
            if (!is.null(facet.by) && nzchar(facet.by)) {
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

            # Hide jitter points from legend if they are shown
            if (isolate_fn(input$add.points)) {
                fig <- .hide_jitter_from_legend(fig)
            }

            config_list <- add_plot_config(
                download.format = isolate_fn(input$download.format),
                include.modebar.buttons = TRUE, facet.by = facet.by
            )

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

            fig
        })

        # Returning all UI inputs:
        AllInputs <- reactive({
            x <- reactiveValuesToList(input)
            x
        })
    
        # Render the plot output
        output$BoxPlot <- renderPlotly({
            req(input$x.data, input$y.data)
            fig <- apply_render_margins(generate_BoxPlot(), input)

            fig <- finalize_manual_edits(fig, plot_source, edit_store, session)

            fig
        })

        # Download handler for source (plot + data + stats)
        plot_source_reactive <- reactive({
            collect_source_data(
                plot_reactive = generate_BoxPlot,
                stats_reactive = last_stats_df,
                inputs_reactive = AllInputs()
            )
        })

        output$download.source <- create_source_download_handler(
            data_list = plot_source_reactive,
            filename_base = "BoxPlot_source"
        )

        return(plot_source_reactive)
    })
}
