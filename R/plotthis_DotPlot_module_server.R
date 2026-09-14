#' Server logic for DotPlot module
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
#' @return The `moduleServer` function for the DotPlot module.
#'
#' @import shiny
#' @import plotly
#' @importFrom colourpicker updateColourInput
#' @importFrom plotthis DotPlot
#' @importFrom stats na.omit
#' @importFrom shinyjs hide delay
#' @importFrom shinyWidgets updateMaterialSwitch
#'
#' @export
#' @author Jacob Martin, Jared Andrews
plotthis_DotPlotServer <- function(id, data, hide.inputs = NULL, hide.tabs = NULL, defaults = NULL) {
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
                    for (tab.name in hide.tabs) hideTab(inputId = "DotPlotTabsetPanel", target = tab.name)
                })
            })
        }

        ns <- session$ns

        # Persist manual legend/annotation/colorbar repositioning across rebuilds.
        plot_source <- session$ns("dot")
        edit_store <- setup_manual_edits(input, session, plot_source)

        if (is.null(defaults)) defaults <- list()
        if (is.null(defaults[["margin.r"]])) defaults[["margin.r"]] <- 70
        # Reset functionality
        observeEvent(input$reset, {
            char.choices <- c("", names(data())[vapply(data(), function(x) !is.numeric(x), logical(1))])
            num.choices <- c("", names(data())[vapply(data(), is.numeric, logical(1))])
            palette_names <- names(.flatten_palette_options(default_palettes()[["choices"]]))

            # Data
            update_viz_select(session, "x.data",
                selected = get_default(defaults, "x.data", char.choices[2], function(x) x %in% char.choices)
            )
            update_viz_select(session, "y.data",
                selected = get_default(
                    defaults, "y.data", char.choices[min(3, length(char.choices))],
                    function(x) x %in% char.choices
                )
            )
            update_viz_select(session, "size.by",
                selected = get_default(defaults, "size.by", "", function(x) x == "" || x %in% num.choices)
            )
            update_viz_select(session, "fill.by",
                selected = get_default(defaults, "fill.by", "", function(x) x == "" || x %in% num.choices)
            )
            updateNumericInput(session, "fill.cutoff", value = get_default(defaults, "fill.cutoff", NA, is.numeric))
            update_viz_select(session, "fill.cutoff.direction",
                selected = get_default(
                    defaults, "fill.cutoff.direction", "<",
                    function(x) x %in% c("<", "<=", ">", ">=")
                )
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
            update_viz_select(session, "palette.name",
                selected = get_default(
                    defaults, "palette.name", "Spectral",
                    function(x) x %in% palette_names
                )
            )
            updateMaterialSwitch(session, "palreverse", value = get_default(defaults, "palreverse", FALSE, is.logical))
            updateNumericInput(session, "alpha", value = get_default(defaults, "alpha", 1, is.numeric))
            updateColourInput(session, "border.color",
                value = get_default(defaults, "border.color", "black", is.character)
            )
            updateNumericInput(session, "border.size", value = get_default(defaults, "border.size", 0.5, is.numeric))
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
            updateNumericInput(session, "axis.title.font.size",
                value = get_default(defaults, "axis.title.font.size", 18, is.numeric)
            )
            updateNumericInput(session, "title.font.size",
                value = get_default(defaults, "title.font.size", 26, is.numeric)
            )
            reset_axes_inputs(session, defaults)

            # Legend
            updateNumericInput(session, "size.legend.x",
                value = get_default(defaults, "size.legend.x", 1.04, is.numeric)
            )
            updateNumericInput(session, "size.legend.y",
                value = get_default(defaults, "size.legend.y", 0.35, is.numeric)
            )
            reset_legend_inputs(session, defaults)
            updateNumericInput(session, "size.min", value = get_default(defaults, "size.min", 1, is.numeric))
            updateNumericInput(session, "size.max", value = get_default(defaults, "size.max", 6, is.numeric))

            # Plotly
            reset_plotly_inputs(session, defaults)

            # Lines
            reset_lines_inputs(session, defaults = defaults)
        })

        observeEvent(input$facet.by, {
            if (.nz_value(input$facet.by)) {
                show_input(session, c("facet.title.font.size", "facet.title.font.color", "facet.title.font.family"))
            } else {
                hide_input(session, c("facet.title.font.size", "facet.title.font.color", "facet.title.font.family"))
            }
        })

        # The color-scale trimming controls only affect the continuous fill gradient,
        # so only expose them when a fill column is selected.
        observeEvent(input$fill.by, {
            fill.scale.inputs <- c("lower.quantile", "upper.quantile", "lower.cutoff", "upper.cutoff")
            if (.nz_value(input$fill.by)) {
                show_input(session, fill.scale.inputs)
            } else {
                hide_input(session, fill.scale.inputs)
            }
        }, ignoreInit = FALSE)

        generate_DotPlot <- reactive({
            isolate_fn <- setup_auto_update_logic(input, params)

            # Null Values:
            facet.by <- NULL
            if (.nz_value(isolate_fn(input$facet.by))) {
                facet.by <- isolate_fn(input$facet.by)
            }

            size.by <- NULL
            if (.nz_value(isolate_fn(input$size.by))) {
                size.by <- isolate_fn(input$size.by)
            }

            fill.by <- NULL
            if (.nz_value(isolate_fn(input$fill.by))) {
                fill.by <- isolate_fn(input$fill.by)
            }

            # fill_cutoff is only valid when fill_by is provided. plotthis expects a
            # string expression such as "< 18"; combine the numeric value with the
            # selected direction operator.
            fill.cutoff <- NULL
            if (!is.null(fill.by) && !is.na(isolate_fn(input$fill.cutoff))) {
                fill.cutoff <- paste(isolate_fn(input$fill.cutoff.direction), isolate_fn(input$fill.cutoff))
            }

            # Convert NA to NULL for facet.ncol and facet.nrow
            facet.ncol <- .na_to_null(isolate_fn(input$facet.ncol))
            facet.nrow <- .na_to_null(isolate_fn(input$facet.nrow))

            palette_arg <- isolate_fn(input$palette.name)
            if (is.null(palette_arg) || !nzchar(palette_arg)) palette_arg <- "Spectral"

            theme_args <- create_ggplot_axis_style(input, isolate_fn = isolate_fn)

            p <- DotPlot(
                data(),
                x = isolate_fn(input$x.data),
                y = isolate_fn(input$y.data),
                flip = isolate_fn(input$rotate),
                size_by = size.by,
                fill_by = fill.by,
                fill_cutoff = fill.cutoff,
                size_min = isolate_fn(input$size.min),
                size_max = isolate_fn(input$size.max),
                facet_by = facet.by,
                facet_scales = isolate_fn(input$facet.scale),
                facet_ncol = facet.ncol,
                facet_nrow = facet.nrow,
                facet_byrow = isolate_fn(input$facet.by.row),
                palette = palette_arg,
                palreverse = isolate_fn(input$palreverse),
                theme = "theme_this",
                theme_args = theme_args,
                alpha = isolate_fn(input$alpha),
                border_color = isolate_fn(input$border.color),
                border_size = isolate_fn(input$border.size),
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

            # Apply axis styling to all subplot axes (handles faceting)
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

            # Custom Legend:
            # Generates a custom dot plot circle legend based on the number of values in size_values.
            fig <- .custom_legend(
                fig,
                data = data(),
                size_by = size.by,
                gap = 0.04,
                title.size = isolate_fn(input$legend.title.size),
                text.size = isolate_fn(input$legend.text.size),
                start_y = isolate_fn(input$size.legend.y),
                start_x = isolate_fn(input$size.legend.x)
            )

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
        output$DotPlot <- renderPlotly({
            req(input$x.data, input$y.data)

            return_empty <- FALSE
            txt <- c()

            if (input$x.data == input$y.data) {
                return_empty <- TRUE
                txt <- c(txt, "Cannot have the x and y inputs be equal. Please change either input.")
            }

            if (return_empty) {
                fig <- empty_plot(text = txt, plotly = TRUE)
            } else {
                fig <- apply_render_margins(generate_DotPlot(), input)
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
                plot_reactive = generate_DotPlot,
                inputs_reactive = AllInputs()
            )
        })

        output$download.source <- create_source_download_handler(
            data_list = plot_source_reactive,
            filename_base = "DotPlot_source"
        )

        return(plot_source_reactive)
    })
}
