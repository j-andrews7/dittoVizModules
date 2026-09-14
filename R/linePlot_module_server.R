#' Server logic for linePlot module
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
#' @return The `moduleServer` function for the linePlot module.
#'
#' @import shiny
#' @import plotly
#' @importFrom stats na.omit
#' @importFrom colourpicker updateColourInput
#' @importFrom shinyjs hide delay
#'
#' @seealso [VizModules::linePlot()], [VizModules::linePlotInputsUI()],
#' [VizModules::linePlotOutputUI()], [VizModules::linePlotApp()]
#'
#' @export
#' @author Jacob Martin
linePlotServer <- function(id, data, hide.inputs = NULL, hide.tabs = NULL, defaults = NULL) {
    stopifnot(is.reactive(data))
    data <- .require_data_frame(data)
    data_reactive <- data

    # linePlot-specific default for subplot spacing (tighter than the global 0.1),
    # matching the default used in linePlotInputsUI().
    if (is.null(defaults) || is.null(defaults[["subplot.margin"]])) {
        defaults <- c(defaults, list("subplot.margin" = 0.05))
    }

    moduleServer(id, function(input, output, session) {
        params <- setup_reactive_defaults(defaults, input, session)

        # Hide individual inputs if specified

        # Persist manual legend/annotation/colorbar repositioning across rebuilds.
        plot_source <- session$ns("line")
        edit_store <- setup_manual_edits(input, session, plot_source)

        # Axis side(s) whose title is regenerated (not persisted) because a data
        # adjustment is active; set inside generate_linePlot() and read at render.
        regen_keys_rv <- reactiveVal(character(0))

        # x/y variables at the last build; when one changes we drop any persisted
        # manual title text for that axis so it regenerates for the new variable.
        last_axis_val <- reactiveVal(NULL)

        observeEvent(input$x.value, {
            req(input$x.value)
            if (length(input$x.value) > 1 || is.numeric(data()[[input$x.value]])) {
                hide_input(session, c("error.bar.width", "error.bar.colour", "error.bar"))
            } else {
                show_input(session, c("error.bar", "error.bar.width", "error.bar.colour"))
            }
        })

        # Hide individual inputs/tabs if specified. The inputs UI is injected by the
        # parent app via renderUI (and re-injected when the dataset changes), so the
        # hiding must be (re)applied after the controls exist in the DOM rather than
        # once at module initialization.
        if (!is.null(hide.inputs) || !is.null(hide.tabs)) {
            observeEvent(data(), {
                delay(100, {
                    hide_input(session, hide.inputs)
                    for (tab.name in hide.tabs) hideTab(inputId = "linePlotTabsetPanel", target = tab.name)
                })
            })
        }

        default_palette_name <- "dittoColors"
        palette_lookup <- .flatten_palette_options(default_palettes()[["choices"]])
        default_palette_values <- palette_lookup[[default_palette_name]]
        if (is.null(default_palette_values) || length(default_palette_values) == 0) {
            default_palette_values <- if (length(palette_lookup) > 0) palette_lookup[[1]] else character(0)
        }

        palette_groups <- reactive({
            df <- data_reactive()
            if (is.null(df)) {
                return(character(0))
            }

            x_vals <- input$x.value
            y_vals <- input$y.value
            group_col <- input$group.by
            multi_axis <- xor(length(x_vals) > 1, length(y_vals) > 1)

            if (multi_axis) {
                if (length(x_vals) > 1) {
                    return(x_vals)
                }
                if (length(y_vals) > 1) {
                    return(y_vals)
                }
            }

            if (!is.null(group_col) && nzchar(group_col) && group_col %in% names(df)) {
                return(unique(na.omit(as.character(df[[group_col]]))))
            }

            if (!is.null(x_vals) && length(x_vals) > 0) {
                return(x_vals[1])
            }

            if (!is.null(y_vals) && length(y_vals) > 0) {
                return(y_vals[1])
            }

            character(0)
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
            ns <- session$ns
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
            choices <- c("", names(data()))
            # Reset Data columns to default. First and second index of data named list
            update_viz_select(session, "x.value",
                selected = get_default(defaults, "x.value", names(data())[1], function(x) all(x %in% choices)))
            update_viz_select(session, "y.value",
                selected = get_default(defaults, "y.value", names(data())[2], function(x) all(x %in% choices)))
            update_viz_select(session, "plot.mode", selected = get_default(defaults, "plot.mode", "lines"))
            update_viz_select(session, "line.type", selected = get_default(defaults, "line.type", "solid"))
            updateMaterialSwitch(session, "order.by",
                value = get_default(defaults, "order.by", FALSE, is.logical))
            updateMaterialSwitch(session, "flip.x",
                value = get_default(defaults, "flip.x", FALSE, is.logical))
            updateMaterialSwitch(session, "flip.y",
                value = get_default(defaults, "flip.y", FALSE, is.logical))
            update_viz_select(session, "group.by",
                selected = get_default(defaults, "group.by", "", function(x) x == "" || x %in% choices))
            update_viz_select(session, "facet.by",
                selected = get_default(defaults, "facet.by", "", function(x) x == "" || x %in% choices))
            update_viz_select(session, "facet.scales",
                selected = get_default(defaults, "facet.scales", "fixed"))
            updateNumericInput(session, "facet.nrow",
                value = get_default(defaults, "facet.nrow", NA, is.numeric))
            updateNumericInput(session, "facet.ncol",
                value = get_default(defaults, "facet.ncol", NA, is.numeric))
            update_viz_select(session, "x.adjustment", selected = get_default(defaults, "x.adjustment", ""))
            update_viz_select(session, "y.adjustment", selected = get_default(defaults, "y.adjustment", ""))
            updateMaterialSwitch(session, "error.bar",
                value = get_default(defaults, "error.bar", TRUE, is.logical))
            updateNumericInput(session, "error.bar.width",
                value = get_default(defaults, "error.bar.width", 1, is.numeric))
            updateColourInput(session, "error.bar.colour",
                value = get_default(defaults, "error.bar.colour", "#000000"))

            reset_axes_inputs(session, defaults)

            # Plotly
            # Group colors
            .reset_group_colors(session, "palette.colours", defaults, palette_groups(), default_palette_values)

            reset_plotly_inputs(session, defaults)
            reset_legend_inputs(session, defaults)

            # Lines
            reset_lines_inputs(session, defaults = defaults)
        })


        observeEvent(input$facet.by, {
            if (.nz_value(input$facet.by)) {
                show_input(session, c(
                    "facet.title.font.size", "facet.title.font.color", "facet.title.font.family",
                    "facet.nrow", "facet.ncol"
                ))
            } else {
                hide_input(session, c(
                    "facet.title.font.size", "facet.title.font.color", "facet.title.font.family",
                    "facet.nrow", "facet.ncol"
                ))
            }
        })

        # Reactive expression to generate the plot (used by both output and download)
        generate_linePlot <- reactive({
            isolate_fn <- setup_auto_update_logic(input, params)

            d <- data_reactive()

            x_input <- isolate_fn(input$x.value)
            y_input <- isolate_fn(input$y.value)

            # Sets the colouring to the first item in the selected palette unless group.by is selected
            palette_values <- resolve_palette(
                isolate_fn(palette_groups()),
                isolate_fn(palette_store()),
                default_palette_values,
                .default_group_colors(defaults, "palette.colours")
            )

            palette_selection <- palette_values
            if (is.null(palette_selection) || length(palette_selection) == 0) {
                palette_selection <- default_palette_values
            }

            group.by <- NULL
            show_legend <- FALSE
            if (isolate_fn(input$group.by) != "" && length(x_input) == 1 && length(y_input) == 1) {
                group.by <- isolate_fn(input$group.by)
                show_legend <- TRUE
            } else if (length(x_input) > 1 || length(y_input) > 1) {
                show_legend <- TRUE
            }

            # Making multiple lines on the axis. e.g 3x and 1y
            # Determining axis min and max
            # Checking if the axis is a category and non continious
            # And axis ordering
            axis_min_x <- NULL
            axis_max_x <- NULL

            # Order by selected axis
            order_by <- x_input
            if (isolate_fn(input$order.by)) {
                order_by <- y_input
            }

            if (is.numeric(d[, x_input])) {
                d <- d[do.call(order, d[, order_by, drop = FALSE]), ]
            }

            if (is.numeric(d[, y_input])) {
                d <- d[do.call(order, d[, order_by, drop = FALSE]), ]
            }


            x_title <- x_input[1]
            if (length(x_input) > 1) {
                x_title <- "Value"
            }

            y_title <- y_input[1]
            if (length(y_input) > 1) {
                y_title <- "Value"
            }

            y.adjustment <- NULL
            if (.nz_value(isolate_fn(input$y.adjustment))) {
                y.adjustment <- isolate_fn(input$y.adjustment)
            }

            x.adjustment <- NULL
            if (.nz_value(isolate_fn(input$x.adjustment))) {
                x.adjustment <- isolate_fn(input$x.adjustment)
            }

            # Checking that all columns are numeric for x and y adjustment to be available
            if (!all(vapply(d[x_input], is.numeric, logical(1)))) {
                update_viz_select(session, "x.adjustment", selected = "")
                x.adjustment <- NULL
            }
            if (!all(vapply(d[y_input], is.numeric, logical(1)))) {
                update_viz_select(session, "y.adjustment", selected = "")
                y.adjustment <- NULL
            }

            # Reflect any applied data adjustment in the axis titles so they accurately
            # describe the values displayed (e.g. "log2(units)"), matching other modules.
            x_title <- adjusted_axis_label(x_title, NULL, x.adjustment)
            y_title <- adjusted_axis_label(y_title, NULL, y.adjustment)

            # Flag adjustment-derived axis titles so finalize_manual_edits() regenerates
            # rather than persists their text on rebuild.
            regen_keys_rv(c(
                if (!is.null(x.adjustment)) "axis:x",
                if (!is.null(y.adjustment)) "axis:y"
            ))

            # When the x or y variable changes, drop any persisted manual title text for
            # that side so it regenerates for the new variable (position still persists).
            cur_val <- list(x = x_input, y = y_input)
            prev_val <- last_axis_val()
            if (!identical(cur_val, prev_val)) {
                if (is.null(prev_val) || !identical(cur_val$x, prev_val$x)) {
                    reset_axis_title_text(edit_store, "axis:x")
                }
                if (is.null(prev_val) || !identical(cur_val$y, prev_val$y)) {
                    reset_axis_title_text(edit_store, "axis:y")
                }
                last_axis_val(cur_val)
            }

            facet.by <- NULL
            if (.nz_value(isolate_fn(input$facet.by))) {
                facet.by <- isolate_fn(input$facet.by)
            }
            facet.nrow.val <- clean_facet_dim(isolate_fn(input$facet.nrow))
            facet.ncol.val <- clean_facet_dim(isolate_fn(input$facet.ncol))

            fig <- linePlot(
                data = d,
                x = isolate_fn(input$x.value),
                y = isolate_fn(input$y.value),
                plot.mode = isolate_fn(input$plot.mode),
                line.type = isolate_fn(input$line.type),
                colour.group.by = group.by,
                palette.selection = palette_selection,
                show.legend = show_legend,
                facet.by = facet.by,
                facet.scales = isolate_fn(input$facet.scales),
                facet.nrow = facet.nrow.val,
                facet.ncol = facet.ncol.val,
                subplot.margin = c(isolate_fn(input$subplot.margin.x), isolate_fn(input$subplot.margin.y)),
                order.by = order_by,
                axis.showline = isolate_fn(input$axis.showline),
                axis.mirror = isolate_fn(input$axis.mirror),
                axis.linecolor = isolate_fn(input$axis.linecolor),
                axis.linewidth = isolate_fn(input$axis.linewidth),
                axis.tickfont.size = isolate_fn(input$axis.tickfont.size),
                axis.tickfont.color = isolate_fn(input$axis.tickfont.color),
                axis.tickfont.family = isolate_fn(input$axis.tickfont.family),
                axis.tickangle.x = isolate_fn(input$axis.tickangle.x),
                axis.tickangle.y = isolate_fn(input$axis.tickangle.y),
                axis.ticks = isolate_fn(input$axis.ticks),
                axis.tickcolor = isolate_fn(input$axis.tickcolor),
                axis.ticklen = isolate_fn(input$axis.ticklen),
                axis.tickwidth = isolate_fn(input$axis.tickwidth),
                show.grid.x = isolate_fn(input$show.grid.x),
                show.grid.y = isolate_fn(input$show.grid.y),
                title.font.size = isolate_fn(input$title.font.size),
                title.font.family = isolate_fn(input$title.font.family),
                title.font.color = isolate_fn(input$title.font.color),
                title.x.position = isolate_fn(input$axis.title.horizontal.position),
                x.title = x_title,
                y.title = y_title,
                flip.x = isolate_fn(input$flip.x),
                flip.y = isolate_fn(input$flip.y),
                x.adjustment = x.adjustment,
                y.adjustment = y.adjustment,
                error.colour = isolate_fn(input$error.bar.colour),
                error.width = isolate_fn(input$error.bar.width),
                error.bar = isolate_fn(input$error.bar)
            )
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

            config_list <- add_plot_config(download.format = isolate_fn(input$download.format), include.modebar.buttons = FALSE, facet.by = facet.by)
            fig <- do.call(plotly::config, c(list(p = fig), config_list))
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
        output$linePlot <- renderPlotly({
            req(input$x.value, input$y.value)

            d <- data_reactive()
            x_input <- input$x.value
            y_input <- input$y.value


            # Section is for catching errors and displaying an empty plot with a warning message if any error conditions are met.
            # Ensures a clean method for dealing with errors and instructing the user on next steps to resolve the issue
            # Error Prone conditions
            x_is_cat <- length(x_input) == 1 && nzchar(x_input) && !is.numeric(d[[x_input]])
            y_is_cat <- length(y_input) == 1 && nzchar(y_input) && !is.numeric(d[[y_input]])
            multi_axis <- xor(length(x_input) > 1, length(y_input) > 1)
            dual_multiAxis <- length(x_input) > 1 && length(y_input) > 1
            x_pure <- is_pure_type(c(x_input), d)
            y_pure <- is_pure_type(c(y_input), d)

            return_empty <- FALSE
            txt <- c()

            if (x_is_cat && y_is_cat) {
                return_empty <- TRUE
                txt <- c(txt, "X and Y categories cannot both be discrete data types")
            } else if (!x_pure || !y_pure) {
                return_empty <- TRUE
                txt <- c(txt, "Cant have a discrete and non discrete data input on the same axis.")
            } else if (dual_multiAxis) {
                return_empty <- TRUE
                txt <- c(txt, "You cannot have multiple inputs for both X and Y inputs simultaneously")
            } else if (multi_axis && .nz_value(input$group.by)) {
                return_empty <- TRUE
                txt <- c(txt, "You cannot have multiple inputs on x and y axis and group by at the same time")
            }

            if (return_empty) {
                fig <- empty_plot(text = txt, plotly = TRUE)
            } else {
                fig <- apply_render_margins(generate_linePlot(), input)
            }

            fig <- finalize_manual_edits(
                fig, plot_source, edit_store, session,
                regen_keys = isolate(regen_keys_rv())
            )

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
                plot_reactive = generate_linePlot,
                inputs_reactive = AllInputs()
            )
        })

        output$download.source <- create_source_download_handler(
            data_list = plot_source_reactive,
            filename_base = "linePlot_source"
        )

        return(plot_source_reactive)
    })
}
