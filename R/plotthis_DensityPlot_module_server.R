#' Density Plot Server Module
#'
#' @description
#' Server-side logic for the density plot module. This function manages
#' reactive data processing, dynamic UI generation for color palettes,
#' and the rendering of interactive Plotly density plots.
#'
#' @param id `character` unique ID for the shiny namespace.
#' @param data `reactive` A reactive expression returning a data frame to be plotted.
#'   Values that are not data frames are coerced with [as.data.frame()]; a `NULL`
#'   value is treated as "not ready yet" and the module waits for data.
#' @param hide.inputs `character` vector of input IDs to hide in the UI. Default is NULL.
#' @param hide.tabs `character` vector of tab names to hide within the module. Default is NULL.
#' @param defaults A named list of default values for the inputs. When the reset button is
#'   clicked, inputs are reset to these values rather than hardcoded fallbacks. Typically
#'   the same list passed to the corresponding UI function. An entry may also be a
#'   [shiny::reactive()] or [shiny::reactiveVal()], in which case the input tracks it as the
#'   parent app's state changes; see [setup_reactive_defaults()].
#'
#' @return The `moduleServer` function for the DensityPlot module.
#'
#' @import shiny
#' @import plotly
#' @importFrom plotthis DensityPlot
#' @importFrom colourpicker updateColourInput
#' @importFrom ggplot2 unit
#'
#' @export
#' @author Jacob Martin, Jared Andrews
plotthis_DensityPlotServer <- function(id, data, hide.inputs = NULL, hide.tabs = NULL, defaults = NULL) {
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
                    for (tab.name in hide.tabs) hideTab(inputId = "DensityPlotTabsetPanel", target = tab.name)
                })
            })
        }

        ns <- session$ns

        # Persist manual legend/annotation/colorbar repositioning across rebuilds.
        plot_source <- session$ns("density")
        edit_store <- setup_manual_edits(input, session, plot_source)

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

            group_col <- input$group.by

            # Only return groups when group.by is set to a valid categorical column
            if (!is.null(group_col) && nzchar(group_col) && group_col %in% names(df)) {
                unique(na.omit(as.character(df[[group_col]])))
            } else {
                # No grouping - will show single color picker instead
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
                # No grouping - show single color picker for fill color
                initial_color <- isolate(input$single.fill.color)
                if (is.null(initial_color) || !nzchar(initial_color)) {
                    initial_color <- get_default(
                        defaults, "single.fill.color", default_palette_values[1], is.character
                    )
                }
                return(colourInput(
                    ns("single.fill.color"),
                    label = "Fill color",
                    value = initial_color
                ))
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
            numeric.data <- data()[, vapply(data(), is.numeric, logical(1)), drop = FALSE]
            all.choices <- c("", names(data()))
            char.choices <- c("", names(data())[vapply(data(), function(x) !is.numeric(x), logical(1))])
            num.choices <- names(numeric.data)
            # Reset numeric inputs to defaults derived from data

            # Data
            update_viz_select(session, "x.data",
                selected = get_default(defaults, "x.data", num.choices[1], function(x) x %in% num.choices))
            update_viz_select(session, "group.by",
                selected = get_default(defaults, "group.by", "", function(x) x == "" || x %in% all.choices))
            update_viz_select(session, "facet.by",
                selected = get_default(defaults, "facet.by", "", function(x) x == "" || x %in% all.choices))
            update_viz_select(session, "facet.scale",
                selected = get_default(defaults, "facet.scale", "fixed"))
            updateNumericInput(session, "facet.ncol", value = get_default(defaults, "facet.ncol", NA, is.numeric))
            updateNumericInput(session, "facet.nrow", value = get_default(defaults, "facet.nrow", NA, is.numeric))
            updateMaterialSwitch(session, "facet.by.row",
                value = get_default(defaults, "facet.by.row", TRUE, is.logical))
            update_viz_select(session, "split.by",
                selected = get_default(defaults, "split.by", "", function(x) x == "" || x %in% all.choices))
            updateMaterialSwitch(session, "rotate", value = get_default(defaults, "rotate", FALSE, is.logical))
            updateMaterialSwitch(session, "add.bars",
                value = get_default(defaults, "add.bars", FALSE, is.logical))
            updateNumericInput(session, "bar.height",
                value = get_default(defaults, "bar.height", 0.04, is.numeric))
            updateSliderInput(session, "bar.alpha", value = get_default(defaults, "bar.alpha", 1, is.numeric))
            updateNumericInput(session, "bar.width", value = get_default(defaults, "bar.width", 1, is.numeric))
            updateSliderInput(session, "plot.alpha", value = get_default(defaults, "plot.alpha", 0.5, is.numeric))
            update_viz_select(session, "theme", selected = get_default(defaults, "theme", "theme_this"))
            update_viz_select(session, "position", selected = get_default(defaults, "position", "identity"))
            updateColourInput(session, "single.fill.color",
                value = get_default(defaults, "single.fill.color", default_palette_values[1]))

            # Action Button
            # Group colors
            .reset_group_colors(session, "palette.colours", defaults, palette_groups(), default_palette_values)

            reset_plotly_inputs(session, defaults)
            reset_legend_inputs(session, defaults)

            # Lines
            reset_lines_inputs(session, defaults = defaults)

            # Axes
            reset_axes_inputs(session, defaults)
        })


        observeEvent(input$facet.by, {
            if (.nz_value(input$facet.by)) {
                show_input(session, c("facet.title.font.size", "facet.title.font.color", "facet.title.font.family"))
            } else {
                hide_input(session, c("facet.title.font.size", "facet.title.font.color", "facet.title.font.family"))
            }
        })

        generate_DensityPlot <- reactive({
            isolate_fn <- setup_auto_update_logic(input, params)

            facet.by <- NULL
            if (.nz_value(isolate_fn(input$facet.by))) {
                facet.by <- isolate_fn(input$facet.by)
            }

            group.by <- NULL
            if (.nz_value(isolate_fn(input$group.by))) {
                group.by <- isolate_fn(input$group.by)
            }

            palette_values <- resolve_palette(
                isolate_fn(palette_groups()),
                isolate_fn(palette_store()),
                default_palette_values,
                .default_group_colors(defaults, "palette.colours")
            )

            palcolor_arg <- NULL
            if (!is.null(palette_values) && length(palette_values) > 0) {
                palcolor_arg <- as.list(palette_values)
            } else {
                # No grouping - use single fill color
                single_color <- isolate_fn(input$single.fill.color)
                if (!is.null(single_color) && nzchar(single_color)) {
                    palcolor_arg <- list(single_color)
                }
            }

            # Facet rows and columns na to null
            facet.ncol <- .na_to_null(isolate_fn(input$facet.ncol))
            facet.nrow <- .na_to_null(isolate_fn(input$facet.nrow))

            theme_args <- create_ggplot_axis_style(input, isolate_fn = isolate_fn)
            theme_args$panel.spacing.x <- unit(isolate_fn(input$subplot.margin.x), "pt")
            theme_args$panel.spacing.y <- unit(isolate_fn(input$subplot.margin.y), "pt")

            p <- DensityPlot(
                data = data(),
                x = isolate_fn(input$x.data),
                group_by = group.by,
                facet_by = facet.by,
                facet_scales = isolate_fn(input$facet.scale),
                facet_ncol = facet.ncol,
                facet_nrow = facet.nrow,
                facet_byrow = isolate_fn(input$facet.by.row),
                alpha = isolate_fn(input$plot.alpha),
                flip = isolate_fn(input$rotate),
                add_bars = isolate_fn(input$add.bars),
                bar_height = isolate_fn(input$bar.height),
                bar_alpha = isolate_fn(input$bar.alpha),
                bar_width = isolate_fn(input$bar.width),
                theme = "theme_this",
                theme_args = theme_args,
                palcolor = palcolor_arg,
                position = isolate_fn(input$position)
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
        output$DensityPlot <- renderPlotly({
            req(input$x.data)

            fig <- apply_render_margins(generate_DensityPlot(), input)

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
                plot_reactive = generate_DensityPlot,
                inputs_reactive = AllInputs()
            )
        })

        output$download.source <- create_source_download_handler(
            data_list = plot_source_reactive,
            filename_base = "DensityPlot_source"
        )

        return(plot_source_reactive)
    })
}
