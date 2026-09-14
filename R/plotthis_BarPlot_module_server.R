#' Server logic for BarPlot module
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
#' @return The `moduleServer` function for the BarPlot module.
#'
#' @import shiny
#' @import plotly
#' @importFrom colourpicker updateColourInput
#' @importFrom plotthis BarPlot
#' @importFrom stats na.omit
#' @importFrom ggplot2 unit
#' @importFrom shinyjs hide delay
#' @importFrom shinyWidgets updateMaterialSwitch
#'
#' @export
#' @author Jacob Martin, Jared Andrews
plotthis_BarPlotServer <- function(id, data, hide.inputs = NULL, hide.tabs = NULL, defaults = NULL) {
    stopifnot(is.reactive(data))
    data <- .require_data_frame(data)


    moduleServer(id, function(input, output, session) {
        params <- setup_reactive_defaults(defaults, input, session)

        # Constant for y-axis scaling to ensure highest bar reaches ~85% of chart height
        y_axis_scale_factor <- 1.18


        # Hide individual inputs/tabs if specified. The inputs UI is injected by the
        # parent app via renderUI (and re-injected when the dataset changes), so the
        # hiding must be (re)applied after the controls exist in the DOM rather than
        # once at module initialization.
        if (!is.null(hide.inputs) || !is.null(hide.tabs)) {
            observeEvent(data(), {
                delay(100, {
                    hide_input(session, hide.inputs)
                    for (tab.name in hide.tabs) hideTab(inputId = "BarPlotTabsetPanel", target = tab.name)
                })
            })
        }

        ns <- session$ns

        # Persist manual legend/annotation/colorbar repositioning across rebuilds.
        plot_source <- session$ns("bar")
        edit_store <- setup_manual_edits(input, session, plot_source)

        default_palette_name <- "dittoColors"
        palette_lookup <- .flatten_palette_options(default_palettes()[["choices"]])
        default_palette_values <- palette_lookup[[default_palette_name]]
        if (is.null(default_palette_values) || length(default_palette_values) == 0) {
            default_palette_values <- if (length(palette_lookup) > 0) palette_lookup[[1]] else character(0)
        }


        fill_numeric <- reactive({
            df <- data()
            fill_col <- input$fill.by
            !is.null(fill_col) && nzchar(fill_col) &&
                fill_col %in% names(df) && is.numeric(df[[fill_col]])
        })

        palette_groups <- reactive({
            df <- data()
            if (is.null(df)) {
                return(character(0))
            }
            fill_col <- input$fill.by

            group_col <- input$group.by
            x_col <- input$x.data

            col_to_use <- if (!fill_numeric() && !is.null(fill_col) && nzchar(fill_col) && fill_col %in% names(df)) {
                fill_col
            } else if (!is.null(group_col) && nzchar(group_col) && group_col %in% names(df)) {
                group_col
            } else if (!is.null(x_col) && nzchar(x_col) && x_col %in% names(df)) {
                x_col
            } else {
                NULL
            }

            if (!is.null(col_to_use)) {
                col_data <- na.omit(df[[col_to_use]])
                # Use factor level order to match ggplot2/plotthis color assignment.
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
        # then reports resolves to the palette already in use. A reactiveVal only
        # invalidates on a real change, so that costs nothing, while a colour the
        # user actually picks comes straight through.
        palette_store <- setup_group_colors(
            input, "palette.colours", palette_groups,
            default_palette_values, defaults, params
        )

        output$palette.selection <- renderUI({
            if (fill_numeric()) {
                palette_names <- names(.flatten_palette_options(default_palettes()[["choices"]]))
                viz_select_input(
                    ns("palette.name"),
                    "Color Palette",
                    choices = palette_names,
                    selected = get_default(defaults, "palette.name", "viridis", function(x) x %in% palette_names)
                )
            } else {
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
            }
        })


        # Reset functionality
        observeEvent(input$reset, {
            numeric.data <- data()[, vapply(data(), is.numeric, logical(1)), drop = FALSE]
            char.choices <- c("", names(data())[vapply(data(), function(x) !is.numeric(x), logical(1))])
            num.choices <- c("", names(data())[vapply(data(), is.numeric, logical(1))])

            # Calculate y.max and y.min from the default selections
            if (length(num.choices) >= 2) {
                max.y <- max(numeric.data[[num.choices[2]]], na.rm = TRUE) * y_axis_scale_factor
            } else {
                max.y <- 1
            }
            min.y <- 0
            # Reset numeric inputs to defaults derived from data

            # Data
            update_viz_select(session, "x.data",
                selected = get_default(defaults, "x.data", char.choices[2], function(x) x %in% char.choices)
            )
            update_viz_select(session, "y.data",
                selected = get_default(defaults, "y.data", num.choices[2], function(x) x %in% num.choices)
            )
            update_viz_select(session, "group.by",
                selected = get_default(defaults, "group.by", char.choices[2], function(x) x %in% char.choices)
            )
            update_viz_select(session, "fill.by",
                selected = get_default(defaults, "fill.by", "", function(x) x == "" || x %in% char.choices)
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
            update_viz_select(session, "split.by",
                selected = get_default(defaults, "split.by", "", function(x) x == "" || x %in% char.choices)
            )

            # Aesthetics
            updateNumericInput(session, "alpha", value = get_default(defaults, "alpha", 1, is.numeric))
            updateNumericInput(session, "width", value = get_default(defaults, "width", NA, is.numeric))
            updateTextInput(session, "expand", value = get_default(defaults, "expand", ""))
            updateMaterialSwitch(session, "palreverse", value = get_default(defaults, "palreverse", FALSE, is.logical))
            updateNumericInput(session, "lower.quantile",
                value = get_default(defaults, "lower.quantile", 0, is.numeric)
            )
            updateNumericInput(session, "upper.quantile",
                value = get_default(defaults, "upper.quantile", 1, is.numeric)
            )
            updateNumericInput(session, "lower.cutoff", value = get_default(defaults, "lower.cutoff", NA, is.numeric))
            updateNumericInput(session, "upper.cutoff", value = get_default(defaults, "upper.cutoff", NA, is.numeric))

            # Axes
            updateMaterialSwitch(session, "rotate", value = get_default(defaults, "rotate", FALSE, is.logical))
            reset.y.max <- get_default(defaults, "y.max", max.y, is.numeric)
            reset.y.min <- get_default(defaults, "y.min", min.y, is.numeric)
            y_range_store(list(min = reset.y.min, max = reset.y.max))
            updateNumericInput(session, "y.max", value = reset.y.max)
            updateNumericInput(session, "y.min", value = reset.y.min)
            updateNumericInput(session, "axis.title.font.size",
                value = get_default(defaults, "axis.title.font.size", 18, is.numeric)
            )
            updateNumericInput(session, "title.font.size",
                value = get_default(defaults, "title.font.size", 26, is.numeric)
            )
            reset_axes_inputs(session, defaults)

            # Plotly
            # Colors
            update_viz_select(session, "palette.name",
                selected = get_default(defaults, "palette.name", "viridis", is.character)
            )
            .reset_group_colors(session, "palette.colours", defaults, palette_groups(), default_palette_values)

            reset_plotly_inputs(session, defaults)
            reset_legend_inputs(session, defaults)

            # Lines
            reset_lines_inputs(session, defaults = defaults)
        })

        # What the plot actually draws with. The limits are pushed into the y.min and
        # y.max controls below, which is a client round-trip; reading the store rather
        # than the raw inputs means the echo of a limit just set costs no rebuild.
        y_range_store <- setup_axis_range(input, session, params = params)

        # Update y-axis range when y data column is changed (when auto-update is off) df, y_data_col, y_axis_scale_factor
        observeEvent(list(input$y.data, input$group.by, input$fill.by), {
            req(input$y.data, input$x.data)
            req(input$y.data %in% names(data()))
            req(input$x.data %in% names(data()))

            group_by_val <- if (.nz_value(input$group.by)) input$group.by else NULL
            fill_by_val <- if (.nz_value(input$fill.by)) input$fill.by else NULL

            # Determine if stacking is happening:
            # Stacked when group.by is numeric OR fill.by is numeric
            group_is_numeric <- !is.null(group_by_val) && group_by_val %in% names(data()) && is.numeric(data()[[group_by_val]])
            fill_is_numeric <- !is.null(fill_by_val) && fill_by_val %in% names(data()) && is.numeric(data()[[fill_by_val]])

            y_range <- .calculate_range(
                df                = data(),
                data_col_x        = input$x.data,
                data_col_y        = input$y.data,
                axis_scale_factor = 1.18,
                grouping          = TRUE
            )

            if (!is.null(y_range)) {
                y_range_store(list(min = y_range$min, max = y_range$max))
                updateNumericInput(session, "y.max", value = y_range$max)
                updateNumericInput(session, "y.min", value = y_range$min)
            }
        })


        observeEvent(input$facet.by, {
            if (.nz_value(input$facet.by)) {
                show_input(session, c("facet.title.font.size", "facet.title.font.color", "facet.title.font.family"))
            } else {
                hide_input(session, c("facet.title.font.size", "facet.title.font.color", "facet.title.font.family"))
            }
        })

        # The color-scale trimming controls only affect a continuous fill gradient,
        # so only expose them when the selected fill column is numeric.
        observeEvent(list(input$fill.by, data()), {
            fill.scale.inputs <- c("lower.quantile", "upper.quantile", "lower.cutoff", "upper.cutoff")
            if (fill_numeric()) {
                show_input(session, fill.scale.inputs)
            } else {
                hide_input(session, fill.scale.inputs)
            }
        }, ignoreInit = FALSE)

        observeEvent(c(input$facet.by, input$x.data), {
            req(!is.null(input$facet.by), !is.null(input$x.data))
            if (input$facet.by == input$x.data) {
                update_viz_select(session, "facet.scale", selected = "free_x")
            }
        }, ignoreInit = FALSE)

        generate_BarPlot <- reactive({
            isolate_fn <- setup_auto_update_logic(input, params)

            # Resolved server-side, so the echo of a limit just set costs no rebuild.
            y.limits <- isolate_fn(y_range_store())

            # Null Values:
            facet.by <- NULL
            if (.nz_value(isolate_fn(input$facet.by))) {
                facet.by <- isolate_fn(input$facet.by)
            }
            expand <- waiver()
            expand.input <- .na_to_null(isolate_fn(input$expand))
            if (!is.null(expand.input)) {
                expand <- as.numeric(strsplit(expand.input, ",\\s*")[[1]])
            }
            if (.has_value(isolate_fn(input$width))) {
                width <- isolate_fn(input$width)
            } else {
                width <- waiver()
            }
            split.by <- NULL
            if (.nz_value(isolate_fn(input$split.by))) {
                split.by <- isolate_fn(input$split.by)
            }
            group.by <- NULL
            if (.nz_value(isolate_fn(input$group.by))) {
                group.by <- isolate_fn(input$group.by)
            }


            fill_by_input <- isolate_fn(input$fill.by)
            if (.nz_value(fill_by_input)) {
                fill.by <- fill_by_input
                group.by <- NULL
            } else {
                fill.by <- FALSE
            }

            if (isolate_fn(fill_numeric())) {
                palette_arg <- isolate_fn(input$palette.name)
                if (is.null(palette_arg) || !nzchar(palette_arg)) {
                    palette_arg <- get_default(defaults, "palette.name", "viridis", is.character)
                }
                palcolor_arg <- NULL
            } else {
                palette_arg <- NULL
                palette_values <- resolve_palette(
                    isolate_fn(palette_groups()),
                    isolate_fn(palette_store()),
                    default_palette_values,
                    .default_group_colors(defaults, "palette.colours")
                )
                palcolor_arg <- as.list(palette_values)
            }

            # Convert NA to NULL for facet.ncol and facet.nrow
            facet.ncol <- .na_to_null(isolate_fn(input$facet.ncol))
            facet.nrow <- .na_to_null(isolate_fn(input$facet.nrow))

            theme_args <- create_ggplot_axis_style(input, isolate_fn = isolate_fn)

            p <- BarPlot(
                data(),
                x = isolate_fn(input$x.data),
                y = isolate_fn(input$y.data),
                flip = isolate_fn(input$rotate),
                group_by = group.by,
                facet_by = facet.by,
                facet_scales = isolate_fn(input$facet.scale),
                facet_ncol = facet.ncol,
                facet_nrow = facet.nrow,
                facet_byrow = isolate_fn(input$facet.by.row),
                palette = palette_arg,
                palcolor = palcolor_arg,
                palreverse = isolate_fn(input$palreverse),
                y_min = y.limits$min,
                y_max = y.limits$max,
                theme = "theme_this",
                theme_args = theme_args,
                alpha = isolate_fn(input$alpha),
                expand = expand,
                width = width,
                split_by = split.by,
                fill_by = fill.by,
                lower_quantile = isolate_fn(input$lower.quantile),
                upper_quantile = isolate_fn(input$upper.quantile),
                lower_cutoff = .na_to_null(isolate_fn(input$lower.cutoff)),
                upper_cutoff = .na_to_null(isolate_fn(input$upper.cutoff))
            )

            fig <- ggplotly(p)
            if (!is.null(facet.by) && nzchar(facet.by)) {
                fig <- apply_facet_subplot_spacing(
                    fig,
                    spacing = c(isolate_fn(input$subplot.margin.x), isolate_fn(input$subplot.margin.y)),
                    ncol = facet.ncol,
                    nrow = facet.nrow
                )
            }

            fig <- apply_title_layout(fig, input, isolate_fn, title_y = 0.95, title_x = isolate_fn(input$axis.title.horizontal.position))

            # Apply axis styling to all subplot axes (handles faceting/split_by)
            # Disable plotly borders since we're handling them through ggplot theme_args
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

            config_list <- add_plot_config(download.format = isolate_fn(input$download.format), include.modebar.buttons = TRUE, facet.by = facet.by)
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
        output$BarPlot <- renderPlotly({
            req(input$x.data, input$y.data)

            return_empty <- FALSE
            txt <- c()

            # group.by is not req()'d above, so it can still be NULL here; a
            # bare == against it yields logical(0) and errors the render.
            if (.nz_value(input$group.by) && input$y.data == input$group.by) {
                return_empty <- TRUE
                txt <- c(txt, "Cannot have the y input and group.by be equal. Please change either inputs.")
            }

            if (return_empty) {
                fig <- empty_plot(text = txt, plotly = TRUE)
            } else {
                fig <- apply_render_margins(generate_BarPlot(), input)
            }

            fig <- finalize_manual_edits(fig, plot_source, edit_store, session)

            return(fig)
        })

        # Download handler for source (plot + data)
        # Capture all UI inputs for the source download
        AllInputs <- reactive({
            x <- reactiveValuesToList(input)
            return(x)
        })

        plot_source_reactive <- reactive({
            collect_source_data(
                plot_reactive = generate_BarPlot,
                inputs_reactive = AllInputs()
            )
        })

        output$download.source <- create_source_download_handler(
            data_list = plot_source_reactive,
            filename_base = "BarPlot_source"
        )

        return(plot_source_reactive)
    })
}
