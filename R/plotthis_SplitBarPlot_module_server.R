#' Server logic for SplitBarPlot module
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
#'
#' @return The `moduleServer` function for the SplitBarPlot module.
#'
#' @import shiny
#' @import plotly
#' @importFrom shinyjs hide show delay
#' @importFrom stats na.omit setNames
#' @importFrom ggplot2 sym .data element_text element_line theme unit
#' @importFrom plotthis SplitBarPlot
#'
#' @export
#'
#' @seealso [plotthis::SplitBarPlot()], [VizModules::organize_inputs()],
#' [VizModules::plotthis_SplitBarPlotInputsUI()], [VizModules::plotthis_SplitBarPlotOutputUI()],
#' [VizModules::plotthis_SplitBarPlotApp()]
#'
#' @author Jacob Martin, Jared Andrews
plotthis_SplitBarPlotServer <- function(id, data, hide.inputs = NULL, hide.tabs = NULL, defaults = NULL) {
    stopifnot(is.reactive(data))
    data <- .require_data_frame(data)

    moduleServer(id, function(input, output, session) {
        params <- setup_reactive_defaults(defaults, input, session)

        # Constant for y-axis scaling to ensure highest bar reaches ~85% of chart height

        axis_scale <- reactive({
            axis_scale_factor <- input$axis.scale.factor
        })
        # Initial call of .calculate_range() made into a reactive to be used later on in server
        axis_range <- reactive({
            return(.calculate_range(
                df                = data(),
                data_col_x        = input$y.data,
                data_col_y        = input$x.data,
                axis_scale_factor = axis_scale(),
                grouping          = TRUE
            ))
        })


        # Hide individual inputs/tabs if specified. The inputs UI is injected by the
        # parent app via renderUI (and re-injected when the dataset changes), so the
        # hiding must be (re)applied after the controls exist in the DOM rather than
        # once at module initialization.
        if (!is.null(hide.inputs) || !is.null(hide.tabs)) {
            observeEvent(data(), {
                delay(100, {
                    hide_input(session, hide.inputs)
                    for (tab.name in hide.tabs) hideTab(inputId = "SplitBarPlotTabsetPanel", target = tab.name)
                })
            })
        }

        # Toggle text.position slider visibility based on label.on.y.axis switch
        observeEvent(input$label.on.y.axis, {
            if (isTRUE(input$label.on.y.axis)) {
                hide_input(session, "text.position")
            } else {
                show_input(session, "text.position")
            }
        })

        ns <- session$ns

        # Persist manual legend/annotation/colorbar repositioning across rebuilds.
        plot_source <- session$ns("splitbar")
        edit_store <- setup_manual_edits(input, session, plot_source)

        default_palette_name <- "dittoColors"
        default_gradient_palette <- "Spectral"
        palette_lookup <- .flatten_palette_options(default_palettes()[["choices"]])
        default_palette_values <- palette_lookup[[default_palette_name]]
        if (is.null(default_palette_values) || length(default_palette_values) == 0) {
            default_palette_values <- if (length(palette_lookup) > 0) palette_lookup[[1]] else character(0)
        }

        fill_by_is_numeric <- reactive({
            df <- data()
            fill_col <- input$fill.by
            if (!is.null(df) && !is.null(fill_col) && nzchar(fill_col) && fill_col %in% names(df)) {
                is.numeric(df[[fill_col]])
            } else {
                FALSE
            }
        })

        palette_groups <- reactive({
            df <- data()
            if (is.null(df)) {
                return(character(0))
            }

            fill_col <- input$fill.by
            y_col <- input$y.data

            if (!is.null(fill_col) && nzchar(fill_col) && fill_col %in% names(df)) {
                unique(na.omit(as.character(df[[fill_col]])))
            } else if (!is.null(y_col) && nzchar(y_col) && y_col %in% names(df)) {
                unique(na.omit(as.character(df[[y_col]])))
            } else {
                character(0)
            }
        })

        # Track initialization
        initialized <- reactiveVal(FALSE)

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
            if (fill_by_is_numeric()) {
                # Numeric fill_by: show palette selector for gradient
                # Build choices with palette names as values (selectInput needs atomic values)
                raw_choices <- default_palettes()[["choices"]]
                palette_choices <- lapply(raw_choices, function(group) {
                    setNames(names(group), names(group))
                })
                viz_select_input(
                    ns("gradient.palette"),
                    "Color palette",
                    choices = palette_choices,
                    selected = get_default(defaults, "gradient.palette", default_gradient_palette, is.character)
                )
            } else {
                # Categorical fill_by: show multi-color picker
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

        # Initialize x-axis range on startup
        observe({
            # Only run once when inputs are first available
            if (!initialized()) {
                # Only require y.data, other inputs can be empty
                req(input$y.data)

                # Wait a moment for other inputs to be available
                if (!is.null(input$x.data) && input$x.data != "") {
                    x_range <- axis_range()
                    if (!is.null(x_range)) {
                        updateNumericInput(session, "x.max", value = x_range$max)
                        updateNumericInput(session, "x.min", value = -x_range$max)
                        initialized(TRUE)
                    }
                }
            }
        })

        # Auto-update x-axis range when relevant inputs change
        observe({
            # Trigger on changes to y.data, x.data, or fill.by
            y_col <- input$y.data
            x_col <- input$x.data
            fill_col <- input$fill.by

            # Skip if we haven't initialized yet or y.data is not set
            if (!initialized() || is.null(y_col) || y_col == "") {
                return()
            }
            x_range <- axis_range()
            # Only auto-update if auto.update is enabled
            if (!is.null(input$auto.update) && input$auto.update) {
                if (!is.null(x_range)) {
                    updateNumericInput(session, "x.max", value = x_range$max)
                    updateNumericInput(session, "x.min", value = -x_range$max)
                }
            }
            if (!is.null(x_range)) {
                updateSliderInput(session, "text.position", min = -x_range$max, max = x_range$max)
            }
        })

        # Reset functionality
        observeEvent(input$reset, {
            numeric.data <- data()[, vapply(data(), is.numeric, logical(1)), drop = FALSE]
            char.choices <- c("", names(data())[vapply(data(), function(x) !is.numeric(x), logical(1))])
            num.choices <- c("", names(data())[vapply(data(), is.numeric, logical(1))])

            # Calculate x.max and x.min from the default selections
            default_y_col <- if (length(num.choices) >= 2) num.choices[2] else NULL
            default_x_col <- if (length(char.choices) >= 2) char.choices[2] else NULL
            default_group_col <- if (length(char.choices) >= 2) char.choices[2] else NULL

            x_range <- axis_range()
            if (!is.null(x_range)) {
                min.x <- -x_range$max
                max.x <- x_range$max
            } else {
                # Fallback to all numeric data if no default column
                max.x <- max(numeric.data, na.rm = TRUE) * axis_scale()
                min.x <- -max.x
            }
            # Reset numeric inputs to defaults derived from data

            # Data
            # Data Section
            update_viz_select(session, "x.data",
                selected = get_default(defaults, "x.data", num.choices[2], function(x) x %in% num.choices)
            )
            update_viz_select(session, "y.data",
                selected = get_default(defaults, "y.data", char.choices[2], function(x) x %in% char.choices)
            )
            update_viz_select(session, "fill.by",
                selected = get_default(defaults, "fill.by", char.choices[2], function(x) x %in% char.choices)
            )

            # Facet Section
            update_viz_select(session, "facet.by",
                selected = get_default(defaults, "facet.by", "", function(x) x == "" || x %in% char.choices)
            )
            update_viz_select(session, "facet.scale",
                selected = get_default(defaults, "facet.scale", "free_y")
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
            update_viz_select(session, "theme", selected = get_default(defaults, "theme", "theme_this"))
            update_viz_select(session, "alpha.by",
                selected = get_default(defaults, "alpha.by", "", function(x) x == "" || x %in% char.choices)
            )
            updateMaterialSwitch(session, "alpha.reverse",
                value = get_default(defaults, "alpha.reverse", FALSE, is.logical)
            )
            updateTextInput(session, "alpha.name", value = get_default(defaults, "alpha.name", ""))
            updateMaterialSwitch(session, "palreverse", value = get_default(defaults, "palreverse", FALSE, is.logical))
            updateNumericInput(session, "bar.height", value = get_default(defaults, "bar.height", 0.9, is.numeric))
            updateNumericInput(session, "line.height", value = get_default(defaults, "line.height", 0.5, is.numeric))
            updateMaterialSwitch(session, "label.on.y.axis",
                value = get_default(defaults, "label.on.y.axis", FALSE, is.logical)
            )
            updateSliderInput(session, "axis.scale.factor",
                value = get_default(defaults, "axis.scale.factor", 1.2, is.numeric)
            )
            updateSliderInput(session, "text.position",
                value = get_default(defaults, "text.position", 0, is.numeric)
            )
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
            updateNumericInput(session, "x.max", value = get_default(defaults, "x.max", max.x, is.numeric))
            updateNumericInput(session, "x.min", value = get_default(defaults, "x.min", min.x, is.numeric))
            updateNumericInput(session, "axis.title.font.size",
                value = get_default(defaults, "axis.title.font.size", 18, is.numeric)
            )
            updateNumericInput(session, "title.font.size",
                value = get_default(defaults, "title.font.size", 26, is.numeric)
            )

            reset_axes_inputs(session, defaults)
            # Colors
            update_viz_select(session, "gradient.palette",
                selected = get_default(defaults, "gradient.palette", default_gradient_palette, is.character)
            )
            .reset_group_colors(session, "palette.colours", defaults, palette_groups(), default_palette_values)

            reset_plotly_inputs(session, defaults)
            reset_legend_inputs(session, defaults)
            reset_lines_inputs(session, defaults = defaults)
        })

        # Update x-axis range when data columns or fill.by change (when auto-update is off)
        observeEvent(list(input$x.data, input$y.data, input$fill.by), {
            req(input$x.data, input$y.data)
            req(input$x.data %in% names(data()))
            req(input$y.data %in% names(data()))

            x_range <- axis_range()
            if (!is.null(x_range)) {
                updateNumericInput(session, "x.max", value = x_range$max)
                updateNumericInput(session, "x.min", value = -x_range$max)
                updateSliderInput(session, "text.position", min = 0, max = x_range$max)
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
            if (fill_by_is_numeric()) {
                show_input(session, fill.scale.inputs)
            } else {
                hide_input(session, fill.scale.inputs)
            }
        }, ignoreInit = FALSE)

        generate_SplitBarPlot <- reactive({
            isolate_fn <- setup_auto_update_logic(input, params)

            # Null Values:
            facet.by <- NULL
            if (.nz_value(isolate_fn(input$facet.by))) {
                facet.by <- isolate_fn(input$facet.by)
            }

            split.by <- NULL
            if (.nz_value(isolate_fn(input$split.by))) {
                split.by <- isolate_fn(input$split.by)
            }
            fill.by <- NULL
            if (.nz_value(isolate_fn(input$fill.by))) {
                fill.by <- isolate_fn(input$fill.by)
            }

            # Convert NA to NULL for facet.ncol and facet.nrow
            facet.ncol <- .na_to_null(isolate_fn(input$facet.ncol))
            facet.nrow <- .na_to_null(isolate_fn(input$facet.nrow))

            # Determine palette/palcolor based on fill_by type
            palcolor_arg <- NULL
            palette_arg <- get_default(defaults, "gradient.palette", default_gradient_palette, is.character)
            if (isolate_fn(fill_by_is_numeric())) {
                # Numeric fill_by: look up hex colors and pass via palcolor
                sel_palette <- isolate_fn(input$gradient.palette)
                if (!is.null(sel_palette) && nzchar(sel_palette)) {
                    pal_colors <- palette_lookup[[sel_palette]]
                    if (!is.null(pal_colors) && length(pal_colors) > 0) {
                        palcolor_arg <- pal_colors
                    }
                }
            } else {
                # Categorical fill_by: use individual color pickers
                palette_values <- resolve_palette(
                    isolate_fn(palette_groups()),
                    isolate_fn(palette_store()),
                    default_palette_values,
                    .default_group_colors(defaults, "palette.colours")
                )
                if (!is.null(palette_values) && length(palette_values) > 0) {
                    palcolor_arg <- as.list(palette_values)
                }
            }

            alpha.by <- NULL
            if (.nz_value(isolate_fn(input$alpha.by))) {
                alpha.by <- isolate_fn(input$alpha.by)
            }


            theme_args <- create_ggplot_axis_style(input, isolate_fn = isolate_fn)
            theme_args$panel.spacing.x <- unit(isolate_fn(input$subplot.margin.x), "pt")
            theme_args$panel.spacing.y <- unit(isolate_fn(input$subplot.margin.y), "pt")

            # bar Plot
            p <- SplitBarPlot(
                data(),
                x = isolate_fn(input$x.data),
                y = isolate_fn(input$y.data),
                flip = isolate_fn(input$rotate),
                fill_by = fill.by,
                facet_by = facet.by,
                facet_scales = isolate_fn(input$facet.scale),
                facet_ncol = facet.ncol,
                facet_nrow = facet.nrow,
                facet_byrow = isolate_fn(input$facet.by.row),
                palcolor = palcolor_arg,
                palette = palette_arg,
                palreverse = isolate_fn(input$palreverse),
                x_min = isolate_fn(input$x.min),
                x_max = isolate_fn(input$x.max),
                theme = "theme_this",
                theme_args = theme_args,
                alpha_by = alpha.by,
                alpha_reverse = isolate_fn(input$alpha.reverse),
                alpha_name = isolate_fn(input$alpha.name),
                split_by = split.by,
                bar_height = isolate_fn(input$bar.height),
                lower_quantile = isolate_fn(input$lower.quantile),
                upper_quantile = isolate_fn(input$upper.quantile),
                lower_cutoff = .na_to_null(isolate_fn(input$lower.cutoff)),
                upper_cutoff = .na_to_null(isolate_fn(input$upper.cutoff))
            )

            y <- isolate_fn(input$y.data)
            x <- isolate_fn(input$x.data)

            # Remove the original geom_text layer added by plotthis::SplitBarPlot
            # to replace it with user-controlled positioning. This is necessary because
            # plotthis::SplitBarPlot() adds a non-customizable geom_text layer for
            # category labels at x=0 that cannot be controlled through its parameters.
            #
            # plotthis keeps the same aesthetics for both orientations (x = value,
            # y = category) and only swaps the visual axes via coord_flip() when the
            # plot is rotated. Under coord_flip() the category axis becomes the X axis
            # and the manual labels are rotated 90 degrees, so the custom positioning
            # must run for both orientations or the slider has no effect when flipped.
            rotated <- isTRUE(isolate_fn(input$rotate))

            p$layers <- p$layers[!vapply(p$layers, function(l) inherits(l$geom, "GeomText"), logical(1))]

            if (isTRUE(isolate_fn(input$label.on.y.axis))) {
                # Show category labels on the category axis by re-enabling the axis text
                # that plotthis::SplitBarPlot() hides internally. The category axis is
                # the X axis when flipped (coord_flip) and the Y axis otherwise.
                if (rotated) {
                    p <- p + theme(
                        axis.text.x = element_text(),
                        axis.ticks.x = element_line()
                    )
                } else {
                    p <- p + theme(
                        axis.text.y = element_text(),
                        axis.ticks.y = element_line()
                    )
                }
            } else {
                # Show category labels at the slider-controlled position along the
                # value axis. plotthis rotates these labels 90 degrees when flipped.
                position <- isolate_fn(input$text.position)
                lineheight <- 0.5
                label_angle <- if (rotated) 90 else 0

                p <- p + geom_text(
                    data = ~ dplyr::filter(.x, .data[[x]] >= 0), # Adding labels for categories with only positive x axis numbers
                    aes(
                        x = position, y = !!sym(y),
                        label = ifelse(
                            is.na(!!sym(y)), " NA ",
                            ifelse(
                                .data[[x]] >= 0,
                                gsub("(\\n|$)", " \\1", !!sym(y)),
                                gsub("(^|\\n)", "\\1 ", !!sym(y))
                            )
                        ),
                        hjust = ifelse(.data[[x]] >= 0, 1, 0)
                    ),
                    color = "black",
                    lineheight = lineheight,
                    angle = label_angle,
                    inherit.aes = FALSE
                )

                p <- p + geom_text(
                    data = ~ dplyr::filter(.x, .data[[x]] < 0), # Adding labels for categories with only negative x axis numbers
                    aes(
                        x = -position, y = !!sym(y), # Position is set to negative as labels are being moved in the opposite direction
                        label = ifelse(
                            is.na(!!sym(y)), " NA ",
                            ifelse(
                                .data[[x]] >= 0,
                                gsub("(\\n|$)", " \\1", !!sym(y)),
                                gsub("(^|\\n)", "\\1 ", !!sym(y))
                            )
                        ),
                        hjust = ifelse(.data[[x]] >= 0, 1, 0)
                    ),
                    color = "black",
                    lineheight = lineheight,
                    angle = label_angle,
                    inherit.aes = FALSE
                )

                # Hide the category axis labels/ticks drawn by plotthis (replaced by the
                # manual labels above). The category axis is the X axis when flipped.
                if (rotated) {
                    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
                } else {
                    p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
                }
            }
            fig <- ggplotly(p)
            if (!is.null(facet.by) && nzchar(facet.by)) {
                fig <- apply_facet_subplot_spacing(
                    fig,
                    spacing = c(isolate_fn(input$subplot.margin.x), isolate_fn(input$subplot.margin.y)),
                    ncol = facet.ncol,
                    nrow = facet.nrow
                )
            }
            fig <- apply_title_layout(fig, input, isolate_fn, title_y = 0.98, title_x = isolate_fn(input$axis.title.horizontal.position))

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
        output$SplitBarPlot <- renderPlotly({
            req(input$x.data, input$y.data)

            fig <- apply_render_margins(generate_SplitBarPlot(), input)

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
                plot_reactive = generate_SplitBarPlot,
                inputs_reactive = AllInputs()
            )
        })

        output$download.source <- create_source_download_handler(
            data_list = plot_source_reactive,
            filename_base = "SplitBarPlot_source"
        )

        return(plot_source_reactive)
    })
}
