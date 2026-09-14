#' Server logic for the ComplexHeatmap module
#'
#' @param id The ID for the Shiny module.
#' @param data A `reactive` yielding either a data frame (the matrix's columns
#'   plus any row-annotation columns, all together — the original, single-table
#'   behavior) or `list(matrix = <data.frame>, column_annotations =
#'   <data.frame>)` to additionally enable column annotations, where
#'   `column_annotations` is a per-sample metadata table keyed by a column
#'   matching the matrix's selected column names (see the "Column Key" input).
#'   A `NULL` value (or a list missing `matrix`) is treated as "not ready yet"
#'   and the module waits for data.
#' @param hide.inputs A character vector of input IDs to hide. These will still
#'   be initialized and their values used, but the user will not be able to
#'   see/adjust them in the UI.
#' @param hide.tabs A character vector of tab names to hide. Inputs in these tabs
#'   will still be initialized and used, but not shown in the UI.
#' @param defaults A named list of default values for the inputs. When the reset
#'   button is clicked, inputs are reset to these values rather than hardcoded
#'   fallbacks. Typically the same list passed to the UI function. An entry may also
#'   be a [shiny::reactive()] or [shiny::reactiveVal()], in which case the input tracks
#'   it as the parent app's state changes; see [setup_reactive_defaults()].
#'
#' @return The `moduleServer` function for the ComplexHeatmap module. The
#'   returned reactive yields the source-download bundle (matrix data + inputs).
#'
#' @details The incoming data frame is converted to a numeric matrix using the
#'   selected matrix columns (and optional row-name column). Row/column
#'   annotation tracks configured on the "Annotations" tab are built as
#'   [ComplexHeatmap::rowAnnotation()]/[ComplexHeatmap::columnAnnotation()] (one
#'   per side) and passed as `left_annotation`/`right_annotation`/
#'   `top_annotation`/`bottom_annotation`. The heatmap is built
#'   with [ComplexHeatmap::Heatmap()] and registered for interactivity with
#'   [InteractiveComplexHeatmap::makeInteractiveComplexHeatmap()], which draws it
#'   onto its own device to capture the interactive widget.
#'
#'   Both \pkg{ComplexHeatmap} and \pkg{InteractiveComplexHeatmap} are
#'   Bioconductor packages and are only required at runtime for this module; they
#'   are guarded with [requireNamespace()].
#'
#' @import shiny
#' @importFrom shinyjs delay
#'
#' @seealso [ComplexHeatmap::Heatmap()], [VizModules::ComplexHeatmap_HeatmapInputsUI()],
#' [VizModules::ComplexHeatmap_HeatmapOutputUI()], [VizModules::ComplexHeatmap_HeatmapApp()]
#'
#' @export
#' @author Jacob Martin, Jared Andrews
ComplexHeatmap_HeatmapServer <- function(id, data, hide.inputs = NULL, hide.tabs = NULL, defaults = NULL) {
    stopifnot(is.reactive(data))
    # Not .require_data_frame(data) -- that helper coerces its input straight to
    # one data frame via as.data.frame(), which would mangle the two-table
    # list(matrix=, column_annotations=) shape (see .heatmap_resolve_data()).

    for (pkg in c("ComplexHeatmap", "InteractiveComplexHeatmap", "circlize")) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
            stop(
                "The '", pkg, "' package is required for the ComplexHeatmap module. ",
                "Install it with BiocManager::install('", pkg, "')."
            )
        }
    }

    moduleServer(id, function(input, output, session) {
        params <- setup_reactive_defaults(defaults, input, session)
        ns <- session$ns

        # Normalize `data()` to list(matrix=, column_annotations=) once; every
        # other reactive reads matrix_data()/column_data() rather than data()
        # directly. See .heatmap_resolve_data().
        resolved <- reactive({
            d <- data()
            req(!is.null(d))
            validate(need(
                is.data.frame(d) || (is.list(d) && is.data.frame(d[["matrix"]])),
                "`data` must be a data frame, or a list with a `matrix` data frame."
            ))
            .heatmap_resolve_data(d)
        })
        matrix_data <- reactive(resolved()$matrix)
        column_data <- reactive(resolved()$column_annotations)

        # Hide individual inputs/tabs if specified. UI is (re)injected by the
        # parent app, so hiding is (re)applied after the controls exist.
        if (!is.null(hide.inputs) || !is.null(hide.tabs)) {
            observeEvent(resolved(), {
                delay(100, {
                    hide_input(session, hide.inputs)
                    for (tab.name in hide.tabs) hideTab(inputId = "HeatmapTabsetPanel", target = tab.name)
                })
            })
        }

        # The two filters are free-text, and a textInput reports on every
        # keystroke -- so an undebounced read would redraw the heatmap once per
        # character, on half-typed expressions that are mostly invalid anyway.
        # Drawing a heatmap is expensive (it is rendered to a device and
        # measured), so this is far more costly here than for a cheap plotly
        # figure. Debouncing collapses a burst of typing into one redraw once
        # the user pauses. Select and numeric inputs report discrete choices
        # and need no such treatment.
        #
        # debounce() emits its initial value immediately and then holds the
        # previous value while typing, so startup is unaffected; and if it ever
        # did yield NULL, .heatmap_apply_filter() reads that as "no filter",
        # which is the right fallback.
        filter_debounce_ms <- 700
        row_filter_text <- debounce(reactive(input$row_filter), filter_debounce_ms)
        column_filter_text <- debounce(reactive(input$column_filter), filter_debounce_ms)

        # The frame a Column Filter expression is evaluated against: one row per
        # selected matrix column, carrying the column name plus any per-sample
        # metadata. See .heatmap_column_meta().
        column_meta <- reactive({
            .heatmap_column_meta(column_data(), input$column_key, input$matrix.cols)
        })

        # Matrix columns surviving the Column Filter, in matrix order.
        filtered_cols <- reactive({
            cols <- input$matrix.cols
            validate(need(
                !is.null(cols) && length(cols) >= 1,
                "Select at least one numeric column for the matrix."
            ))

            res <- .heatmap_apply_filter(column_filter_text(), column_meta(), length(cols))
            validate(need(
                !identical(res$status, "invalid"),
                paste(
                    "Column Filter is not a valid expression over:",
                    paste(names(column_meta()), collapse = ", ")
                )
            ))
            out <- cols[res$keep]
            validate(need(length(out) >= 1, "No matrix columns match the Column Filter."))
            out
        })

        # matrix_data() narrowed by the Row Filter. Everything that reads
        # annotation values off the matrix data frame must go through this
        # rather than matrix_data(): row annotations align *positionally* with
        # the matrix rows, so a filter that shifts the rows out from under them
        # would silently relabel every track. See build_heatmap() below.
        filtered_matrix_data <- reactive({
            df <- matrix_data()
            req(df)

            res <- .heatmap_apply_filter(row_filter_text(), df, nrow(df))
            validate(need(
                !identical(res$status, "invalid"),
                paste(
                    "Row Filter is not a valid expression over:",
                    paste(names(df), collapse = ", ")
                )
            ))
            out <- df[res$keep, , drop = FALSE]
            validate(need(nrow(out) >= 1, "No rows match the Row Filter."))
            out
        })

        # Convert the incoming data frame to a numeric matrix per the selected
        # (and filtered) columns, applying the optional row-name column.
        # Surfaces friendly validation messages on bad input.
        heatmap_matrix <- reactive({
            df <- filtered_matrix_data()
            req(df)

            cols <- filtered_cols()
            validate(need(
                all(cols %in% names(df)),
                "One or more selected matrix columns are not in the data."
            ))

            mat <- as.matrix(df[, cols, drop = FALSE])
            validate(need(
                is.numeric(mat),
                "The selected matrix columns must all be numeric."
            ))

            rn.col <- input$rowname.col
            if (!is.null(rn.col) && nzchar(rn.col) && rn.col %in% names(df)) {
                rownames(mat) <- make.unique(as.character(df[[rn.col]]))
            }

            # Guard against clustering failures on all-NA or zero-variance rows.
            validate(need(
                any(!is.na(mat)),
                "The matrix is entirely NA; nothing to plot."
            ))

            mat
        })

        # Row/column annotation color widgets are rendered dynamically, one
        # set per currently-defined multiDynamicInput row (Low/Mid/High colour
        # pickers for a numeric column, a multiColorPicker() for a categorical
        # one) -- see .heatmap_annotation_widget_id() for the id convention
        # shared between these renderUI blocks and the color_lookup closures
        # inside build_heatmap() that read the resulting inputs back.
        #
        # The renderUI()s below key off a *spec* reactiveVal (see
        # .heatmap_annotation_spec()), not the raw rows/data frame directly,
        # and that store only updates when the spec actually changes --
        # otherwise any unrelated data recompute (e.g. filtering the "Data
        # Table" without touching an annotated column's own levels) would
        # rebuild, and so reset, every color widget on every tweak.
        make_annotation_spec_store <- function(rows_fn, df_fn) {
            spec <- reactive(.heatmap_annotation_spec(rows_fn(), df_fn()))
            store <- reactiveVal()
            observe({
                new_spec <- spec()
                if (!identical(new_spec, isolate(store()))) {
                    store(new_spec)
                }
            })
            function() {
                val <- store()
                if (is.null(val)) {
                    val <- spec()
                    store(val)
                }
                val
            }
        }
        row_annotation_rows <- reactive({
            input$row_annotations %||% get_default(defaults, "row_annotations", NULL)
        })
        column_annotation_rows <- reactive({
            input$column_annotations %||% get_default(defaults, "column_annotations", NULL)
        })
        row_annotation_spec <- make_annotation_spec_store(row_annotation_rows, filtered_matrix_data)
        column_annotation_spec <- make_annotation_spec_store(column_annotation_rows, column_data)

        # A widget only rebuilds when its spec (column/type/levels) actually
        # changed (see the reactiveVal store above), but when it does -- e.g.
        # a "Data Table" filter narrows a categorical column down to fewer
        # levels -- seed the new widget from the *outgoing* one's own current
        # value (still live in `input` at this point, since the new UI hasn't
        # reached the client yet) rather than fresh defaults, so a level that
        # survives the change keeps the color the user picked for it. Mirrors
        # setup_group_colors()'s carry-over pattern used for the same reason
        # elsewhere in the package.
        annotation_colors_ui <- function(spec, prefix) {
            if (length(spec) == 0) {
                return(NULL)
            }
            ditto_palette <- .flatten_palette_options(default_palettes()[["choices"]])[["dittoColors"]]
            tagList(lapply(names(spec), function(row_name) {
                s <- spec[[row_name]]
                widget_id <- .heatmap_annotation_widget_id(prefix, row_name)
                if (isTRUE(s$numeric)) {
                    prev_low <- isolate(input[[paste0(widget_id, "_low")]]) %||%
                        get_default(defaults, paste0(widget_id, "_low"), NULL) %||%
                        get_default(defaults, paste0(s$column, "_low"), "#2166AC")
                    prev_mid <- isolate(input[[paste0(widget_id, "_mid")]]) %||%
                        get_default(defaults, paste0(widget_id, "_mid"), NULL) %||%
                        get_default(defaults, paste0(s$column, "_mid"), "#F7F7F7")
                    prev_high <- isolate(input[[paste0(widget_id, "_high")]]) %||%
                        get_default(defaults, paste0(widget_id, "_high"), NULL) %||%
                        get_default(defaults, paste0(s$column, "_high"), "#B2182B")
                    tagList(
                        strong(paste0(s$column, ":")),
                        colourInput(ns(paste0(widget_id, "_low")), "Low Color", value = prev_low),
                        colourInput(ns(paste0(widget_id, "_mid")), "Mid Color", value = prev_mid),
                        colourInput(ns(paste0(widget_id, "_high")), "High Color", value = prev_high)
                    )
                } else {
                    prev_colors <- isolate(input[[widget_id]])
                    default_colors <- .default_group_colors(defaults, widget_id) %||%
                        .default_group_colors(defaults, row_name) %||%
                        .default_group_colors(defaults, s$column)
                    seeded <- resolve_palette(s$levels, prev_colors, ditto_palette, default_colors)
                    multiColorPicker(ns(widget_id), label = paste0(s$column, ":"),
                        groups = s$levels, palette_options = default_palettes()[["choices"]],
                        selected_palette = "dittoColors", colors = seeded, compact = TRUE
                    )
                }
            }))
        }

        output$row_annotation_colors_ui <- renderUI({
            annotation_colors_ui(row_annotation_spec(), "row_ann_color")
        })
        outputOptions(output, "row_annotation_colors_ui", suspendWhenHidden = FALSE)

        output$column_annotation_colors_ui <- renderUI({
            annotation_colors_ui(column_annotation_spec(), "column_ann_color")
        })
        outputOptions(output, "column_annotation_colors_ui", suspendWhenHidden = FALSE)

        # Build (and draw) the Heatmap object from the current inputs.
        build_heatmap <- reactive({
            isolate_fn <- setup_auto_update_logic(input, params)
            raw_mat <- heatmap_matrix()

            # Scaling only affects what's drawn -- row/column annotation values
            # and the source-data download (plot_source_reactive(), which reads
            # heatmap_matrix() directly) always use the unscaled matrix.
            scale_choice <- isolate_fn(input$scale) %||% get_default(defaults, "scale", "None")
            scale_mode <- switch(scale_choice %||% "None",
                "Rows" = "row", "Columns" = "column", "none"
            )
            mat <- .heatmap_scale_matrix(raw_mat, scale_mode)

            min_val <- isolate_fn(input$min_value)
            if (is.null(min_val) || is.na(min_val)) min_val <- suppressWarnings(min(mat, na.rm = TRUE))
            max_val <- isolate_fn(input$max_value)
            if (is.null(max_val) || is.na(max_val)) max_val <- suppressWarnings(max(mat, na.rm = TRUE))
            mid_val <- isolate_fn(input$mid_value)
            if (is.null(mid_val) || is.na(mid_val)) mid_val <- mean(c(min_val, max_val))
            # circlize::colorRamp2() errors on fewer than two distinct breaks,
            # which a constant matrix (or a user typing the same Min/Max Value)
            # would otherwise hit.
            if (isTRUE(min_val == max_val)) {
                min_val <- min_val - 0.5
                max_val <- max_val + 0.5
            }

            low_c <- isolate_fn(input$low_color) %||% get_default(defaults, "low_color", .heatmap_default_colors()[1])
            mid_c <- isolate_fn(input$mid_color) %||% get_default(defaults, "mid_color", .heatmap_default_colors()[2])
            high_c <- isolate_fn(input$high_color) %||% get_default(defaults, "high_color", .heatmap_default_colors()[3])
            cols <- c(low_c, mid_c, high_c)
            if (isTRUE(isolate_fn(input$reverse.palette) %||% get_default(defaults, "reverse.palette", FALSE))) {
                cols <- rev(cols)
            }
            col_fun <- circlize::colorRamp2(c(min_val, mid_val, max_val), cols)

            # Row-annotation values come from the *filtered* matrix data frame.
            # heatmap_matrix() builds mat from exactly these rows, in this
            # order, so the two line up 1:1 positionally -- reading the
            # unfiltered matrix_data() here would relabel every track the
            # moment a Row Filter is set. Column-annotation values are matched
            # by value from the separate column_data() table via the Column Key
            # input, so a narrower set of columns needs no equivalent care.
            row_source_df <- filtered_matrix_data()
            row_key_values <- rownames(raw_mat)
            if (is.null(row_key_values)) row_key_values <- as.character(seq_len(nrow(raw_mat)))

            # Values backing an "Annotation" split, pulled through the same
            # helper the annotation tracks use so a split and a track on one
            # column can never disagree about what that column means.
            split_values <- function(cols, source_df, key_values, key_col) {
                if (is.null(cols) || length(cols) == 0) {
                    return(NULL)
                }
                vals <- lapply(cols, function(cl) {
                    .heatmap_annotation_values(source_df, cl, key_values, key_col)
                })
                names(vals) <- cols
                vals <- Filter(Negate(is.null), vals)
                if (length(vals) == 0) {
                    return(NULL)
                }
                as.data.frame(vals, stringsAsFactors = FALSE, check.names = FALSE)
            }

            # Exactly one of row_km/row_split is ever passed to Heatmap() below
            # (never both) — see .heatmap_resolve_split() for why.
            row_res <- .heatmap_resolve_split(
                isolate_fn(input$row_split_by) %||% get_default(defaults, "row_split_by", "None"),
                isolate_fn(input$row_split_n) %||% get_default(defaults, "row_split_n", NA),
                nrow(mat),
                split_values(
                    isolate_fn(input$row_split_cols) %||% get_default(defaults, "row_split_cols", NULL),
                    row_source_df, row_key_values, NULL
                )
            )
            column_res <- .heatmap_resolve_split(
                isolate_fn(input$column_split_by) %||% get_default(defaults, "column_split_by", "None"),
                isolate_fn(input$column_split_n) %||% get_default(defaults, "column_split_n", NA),
                ncol(mat),
                split_values(
                    isolate_fn(input$column_split_cols) %||% get_default(defaults, "column_split_cols", NULL),
                    column_data(),
                    colnames(raw_mat),
                    isolate_fn(input$column_key) %||% get_default(defaults, "column_key", NULL)
                )
            )

            # Reads a row's dynamically-rendered color widget(s) (see
            # annotation_colors_ui() above) -- built once per axis and reused
            # for both the Left/Right (or Top/Bottom) split below.
            ditto_palette <- .flatten_palette_options(default_palettes()[["choices"]])[["dittoColors"]]

            make_color_lookup <- function(prefix) {
                function(row_name, col, values) {
                    widget_id <- .heatmap_annotation_widget_id(prefix, row_name)
                    if (is.numeric(values)) {
                        low <- isolate_fn(input[[paste0(widget_id, "_low")]]) %||%
                            get_default(defaults, paste0(widget_id, "_low"), NULL) %||%
                            get_default(defaults, paste0(col, "_low"), "#2166AC")
                        mid <- isolate_fn(input[[paste0(widget_id, "_mid")]]) %||%
                            get_default(defaults, paste0(widget_id, "_mid"), NULL) %||%
                            get_default(defaults, paste0(col, "_mid"), "#F7F7F7")
                        high <- isolate_fn(input[[paste0(widget_id, "_high")]]) %||%
                            get_default(defaults, paste0(widget_id, "_high"), NULL) %||%
                            get_default(defaults, paste0(col, "_high"), "#B2182B")
                        .heatmap_annotation_col(values,
                            low_color = low,
                            mid_color = mid,
                            high_color = high
                        )
                    } else {
                        client_colors <- isolate_fn(input[[widget_id]])
                        levels <- sort(unique(as.character(values[!is.na(values)])))
                        default_colors <- .default_group_colors(defaults, widget_id) %||%
                            .default_group_colors(defaults, row_name) %||%
                            .default_group_colors(defaults, col)
                        discrete_cols <- resolve_palette(levels, client_colors, ditto_palette, default_colors)
                        .heatmap_annotation_col(values, discrete_colors = discrete_cols)
                    }
                }
            }

            # Split each axis's rows by side and build one HeatmapAnnotation per
            # side, so each track can be placed independently.
            row_rows <- isolate_fn(input$row_annotations)
            if (is.null(row_rows)) {
                row_rows <- get_default(defaults, "row_annotations", NULL)
            }
            row_color_lookup <- make_color_lookup("row_ann_color")
            left_ann <- .heatmap_build_annotation(
                Filter(function(r) identical(r$side %||% "Left", "Left"), row_rows),
                row_source_df, row_key_values,
                key_col = NULL, which = "row", color_lookup = row_color_lookup
            )
            right_ann <- .heatmap_build_annotation(
                Filter(function(r) identical(r$side, "Right"), row_rows),
                row_source_df, row_key_values,
                key_col = NULL, which = "row", color_lookup = row_color_lookup
            )

            column_rows <- isolate_fn(input$column_annotations)
            if (is.null(column_rows)) {
                column_rows <- get_default(defaults, "column_annotations", NULL)
            }
            column_color_lookup <- make_color_lookup("column_ann_color")
            top_ann <- .heatmap_build_annotation(
                Filter(function(r) identical(r$side %||% "Top", "Top"), column_rows),
                column_data(), colnames(raw_mat),
                key_col = isolate_fn(input$column_key), which = "column", color_lookup = column_color_lookup
            )
            bottom_ann <- .heatmap_build_annotation(
                Filter(function(r) identical(r$side, "Bottom"), column_rows),
                column_data(), colnames(raw_mat),
                key_col = isolate_fn(input$column_key), which = "column", color_lookup = column_color_lookup
            )

            ht <- ComplexHeatmap::Heatmap(
                matrix = mat,
                name = isolate_fn(input$name) %||% get_default(defaults, "name", "value"),
                col = col_fun,
                na_col = isolate_fn(input$na_col) %||% get_default(defaults, "na_col", "grey"),
                show_heatmap_legend = isolate_fn(input$show_heatmap_legend) %||% get_default(defaults, "show_heatmap_legend", TRUE),
                border = isolate_fn(input$border) %||% get_default(defaults, "border", FALSE),
                left_annotation = left_ann,
                right_annotation = right_ann,
                top_annotation = top_ann,
                bottom_annotation = bottom_ann,
                cluster_rows = isolate_fn(input$cluster_rows) %||% get_default(defaults, "cluster_rows", TRUE),
                cluster_columns = isolate_fn(input$cluster_columns) %||% get_default(defaults, "cluster_columns", TRUE),
                clustering_distance_rows = isolate_fn(input$clustering_distance_rows) %||% get_default(defaults, "clustering_distance_rows", "euclidean"),
                clustering_distance_columns = isolate_fn(input$clustering_distance_columns) %||% get_default(defaults, "clustering_distance_columns", "euclidean"),
                clustering_method_rows = isolate_fn(input$clustering_method_rows) %||% get_default(defaults, "clustering_method_rows", "complete"),
                clustering_method_columns = isolate_fn(input$clustering_method_columns) %||% get_default(defaults, "clustering_method_columns", "complete"),
                show_row_dend = isolate_fn(input$show_row_dend) %||% get_default(defaults, "show_row_dend", TRUE),
                show_column_dend = isolate_fn(input$show_column_dend) %||% get_default(defaults, "show_column_dend", TRUE),
                row_km = row_res$km,
                column_km = column_res$km,
                row_split = row_res$split,
                column_split = column_res$split,
                row_gap = grid::unit(isolate_fn(input$row_gap) %||% get_default(defaults, "row_gap", 1), "mm"),
                column_gap = grid::unit(isolate_fn(input$column_gap) %||% get_default(defaults, "column_gap", 1), "mm"),
                row_title = isolate_fn(input$row_title) %||% get_default(defaults, "row_title", ""),
                column_title = isolate_fn(input$column_title) %||% get_default(defaults, "column_title", ""),
                show_row_names = isolate_fn(input$show_row_names) %||% get_default(defaults, "show_row_names", TRUE),
                show_column_names = isolate_fn(input$show_column_names) %||% get_default(defaults, "show_column_names", TRUE),
                row_names_side = isolate_fn(input$row_names_side) %||% get_default(defaults, "row_names_side", "right"),
                column_names_side = isolate_fn(input$column_names_side) %||% get_default(defaults, "column_names_side", "bottom"),
                column_names_rot = isolate_fn(input$column_names_rot) %||% get_default(defaults, "column_names_rot", 90),
                row_names_gp = grid::gpar(fontsize = isolate_fn(input$row_names_fontsize) %||% get_default(defaults, "row_names_fontsize", 12)),
                column_names_gp = grid::gpar(fontsize = isolate_fn(input$column_names_fontsize) %||% get_default(defaults, "column_names_fontsize", 12)),
                row_title_gp = grid::gpar(fontsize = isolate_fn(input$title_fontsize) %||% get_default(defaults, "title_fontsize", 13.2)),
                column_title_gp = grid::gpar(fontsize = isolate_fn(input$title_fontsize) %||% get_default(defaults, "title_fontsize", 13.2))
            )

            # The interactive widget needs a *drawn* heatmap (HeatmapList) to
            # measure cell positions. Draw it onto a throwaway device so the
            # heatmap does not also appear in the R session's plot pane, then
            # return the drawn object. (Passing an un-drawn Heatmap can leave the
            # widget's render stalled on "Making heatmap, please wait...".)
            grDevices::pdf(NULL)
            on.exit(grDevices::dev.off(), add = TRUE)
            ComplexHeatmap::draw(ht)
        })

        # Static counterpart to the InteractiveComplexHeatmap widget, backing
        # ComplexHeatmap_HeatmapStaticOutputUI(). This one is a plain module
        # output, so ns("HeatmapStatic") lines up without any of the root-scope
        # handling makeInteractiveComplexHeatmap() needs below. Only one of the
        # two output forms is ever in the UI for a given id, and the unused
        # renderer simply never runs.
        output$HeatmapStatic <- renderPlot({
            ComplexHeatmap::draw(build_heatmap())
        })

        # makeInteractiveComplexHeatmap() uses `heatmap_id` verbatim as the
        # output slot name and in session/input message keys (see the package
        # source: output[[qq("@{heatmap_id}_heatmap")]], input[[qq(...)]],
        # session$sendCustomMessage(qq("@{heatmap_id}_..."))). It was written for
        # a top-level server, not moduleServer: a module's `output` auto-prefixes
        # the namespace to keys (double-namespacing), while `input` and
        # `sendCustomMessage` do not. To keep both sides consistent with the UI's
        # InteractiveComplexHeatmapOutput(ns("Heatmap")), we register against the
        # session's *root* scope and pass the fully-qualified id ns("Heatmap").
        root_session <- session$rootScope()

        observeEvent(build_heatmap(), {
            h_id <- ns("Heatmap")
            # InteractiveComplexHeatmap requires the matching UI component to have been
            # generated (which registers the heatmap in its environment). It keys that
            # registry by validate_heatmap_id(), which is .heatmap_widget_id()'s
            # normalisation -- so the namespaced id must be normalised before the lookup
            # or `[[` misses on every module-hosted heatmap (every `-` in the namespace
            # is an `_` in the key) and the widget is never made. Only the lookup needs
            # it: makeInteractiveComplexHeatmap() normalises `heatmap_id` itself.
            if (!requireNamespace("InteractiveComplexHeatmap", quietly = TRUE) ||
                is.null(getFromNamespace("shiny_env", "InteractiveComplexHeatmap")$heatmap[[
                    .heatmap_widget_id(h_id)
                ]])) {
                return()
            }
            InteractiveComplexHeatmap::makeInteractiveComplexHeatmap(
                root_session$input, root_session$output, root_session,
                build_heatmap(),
                heatmap_id = h_id
            )
        }, ignoreNULL = TRUE)

        # Reset handler mirroring the UI defaults.
        observeEvent(input$reset, {
            df <- matrix_data()
            num.cols <- names(df)[vapply(df, is.numeric, logical(1))]
            id.choices <- c("", names(df)[vapply(df, function(x) !is.numeric(x), logical(1))])

            update_viz_select(session, "matrix.cols",
                selected = get_default(defaults, "matrix.cols", num.cols, function(x) all(x %in% num.cols)))
            update_viz_select(session, "rowname.col",
                selected = get_default(defaults, "rowname.col", "", function(x) x %in% id.choices))
            updateTextInput(session, "name", value = get_default(defaults, "name", "value"))
            colourpicker::updateColourInput(session, "na_col", value = get_default(defaults, "na_col", "grey"))
            update_viz_select(session, "scale", selected = get_default(defaults, "scale", "None"))

            updateTextInput(session, "row_filter", value = get_default(defaults, "row_filter", ""))
            updateTextInput(session, "column_filter", value = get_default(defaults, "column_filter", ""))

            updateCheckboxInput(session, "reverse.palette", value = get_default(defaults, "reverse.palette", FALSE, is.logical))
            default_cols <- .heatmap_default_colors()
            colourpicker::updateColourInput(session, "low_color", value = get_default(defaults, "low_color", default_cols[1]))
            colourpicker::updateColourInput(session, "mid_color", value = get_default(defaults, "mid_color", default_cols[2]))
            colourpicker::updateColourInput(session, "high_color", value = get_default(defaults, "high_color", default_cols[3]))
            updateNumericInput(session, "min_value", value = get_default(defaults, "min_value", NA, is.numeric))
            updateNumericInput(session, "mid_value", value = get_default(defaults, "mid_value", NA, is.numeric))
            updateNumericInput(session, "max_value", value = get_default(defaults, "max_value", NA, is.numeric))
            updateCheckboxInput(session, "show_heatmap_legend", value = get_default(defaults, "show_heatmap_legend", TRUE, is.logical))
            updateCheckboxInput(session, "border", value = get_default(defaults, "border", FALSE, is.logical))

            updateCheckboxInput(session, "cluster_rows", value = get_default(defaults, "cluster_rows", TRUE, is.logical))
            updateCheckboxInput(session, "cluster_columns", value = get_default(defaults, "cluster_columns", TRUE, is.logical))
            update_viz_select(session, "clustering_distance_rows", selected = get_default(defaults, "clustering_distance_rows", "euclidean"))
            update_viz_select(session, "clustering_distance_columns", selected = get_default(defaults, "clustering_distance_columns", "euclidean"))
            update_viz_select(session, "clustering_method_rows", selected = get_default(defaults, "clustering_method_rows", "complete"))
            update_viz_select(session, "clustering_method_columns", selected = get_default(defaults, "clustering_method_columns", "complete"))
            updateCheckboxInput(session, "show_row_dend", value = get_default(defaults, "show_row_dend", TRUE, is.logical))
            updateCheckboxInput(session, "show_column_dend", value = get_default(defaults, "show_column_dend", TRUE, is.logical))
            update_viz_select(session, "row_split_by", selected = get_default(defaults, "row_split_by", "None"))
            updateNumericInput(session, "row_split_n", value = get_default(defaults, "row_split_n", NA, is.numeric))
            update_viz_select(session, "row_split_cols",
                selected = get_default(defaults, "row_split_cols", character(0)))
            update_viz_select(session, "column_split_by", selected = get_default(defaults, "column_split_by", "None"))
            updateNumericInput(session, "column_split_n", value = get_default(defaults, "column_split_n", NA, is.numeric))
            updateNumericInput(session, "row_gap", value = get_default(defaults, "row_gap", 1, is.numeric))
            updateNumericInput(session, "column_gap", value = get_default(defaults, "column_gap", 1, is.numeric))

            updateTextInput(session, "row_title", value = get_default(defaults, "row_title", ""))
            updateTextInput(session, "column_title", value = get_default(defaults, "column_title", ""))
            updateCheckboxInput(session, "show_row_names", value = get_default(defaults, "show_row_names", TRUE, is.logical))
            updateCheckboxInput(session, "show_column_names", value = get_default(defaults, "show_column_names", TRUE, is.logical))
            update_viz_select(session, "row_names_side", selected = get_default(defaults, "row_names_side", "right"))
            update_viz_select(session, "column_names_side", selected = get_default(defaults, "column_names_side", "bottom"))
            updateNumericInput(session, "column_names_rot", value = get_default(defaults, "column_names_rot", 90, is.numeric))
            updateNumericInput(session, "row_names_fontsize", value = get_default(defaults, "row_names_fontsize", 12, is.numeric))
            updateNumericInput(session, "column_names_fontsize", value = get_default(defaults, "column_names_fontsize", 12, is.numeric))
            updateNumericInput(session, "title_fontsize", value = get_default(defaults, "title_fontsize", 13.2, is.numeric))

            reset_multi_dynamic <- function(inputId, key) {
                default_elements <- get_default(defaults, key, NULL)
                if (is.null(default_elements)) {
                    updateMultiDynamicInput(session, inputId, clear = TRUE)
                } else {
                    updateMultiDynamicInput(session, inputId, elements = default_elements)
                }
            }
            reset_multi_dynamic("row_annotations", "row_annotations")

            col_df <- column_data()
            if (!is.null(col_df)) {
                column.key.choices <- names(col_df)
                update_viz_select(session, "column_key",
                    selected = get_default(
                        defaults, "column_key", column.key.choices[1],
                        function(x) x %in% column.key.choices
                    ))
                reset_multi_dynamic("column_annotations", "column_annotations")
                update_viz_select(session, "column_split_cols",
                    selected = get_default(defaults, "column_split_cols", character(0)))
            }
        })

        # Capture all UI inputs for the source download.
        AllInputs <- reactive(reactiveValuesToList(input))

        # This module's output is not a plotly graph, so neither the Figure
        # Builder's canvas export nor the source download can photograph it in
        # the browser. Both ask the module to draw itself instead, at whatever
        # size the panel is on screen. The ids are namespaced per widget so two
        # heatmap panels in one figure cannot claim each other's clip paths.
        # `res` matches what renderPlot() drew the panel at, so the legends and
        # labels -- which ComplexHeatmap sizes in absolute points -- keep the
        # same share of the canvas they have on screen instead of crowding the
        # cells out. See draw_to_svg().
        heatmap_svg <- function(width, height, res = 72) {
            ht <- build_heatmap()
            if (is.null(ht)) {
                return(NULL)
            }
            draw_to_svg(
                function() ComplexHeatmap::draw(ht),
                width = width, height = height, res = res,
                id_prefix = .heatmap_widget_id(ns("Heatmap"))
            )
        }

        heatmap_png <- function(width, height, res = 72) {
            ht <- build_heatmap()
            if (is.null(ht)) {
                return(NULL)
            }
            draw_to_png(
                function() ComplexHeatmap::draw(ht),
                width = width, height = height, res = res
            )
        }

        # Heatmap-specific source collector. The shared create_source_download_handler()
        # writes object$plot via htmlwidgets::saveWidget(), which only accepts an
        # htmlwidget; a drawn Heatmap is not one, so `plot` is left NULL and we
        # export the underlying matrix and the UI input values instead.
        plot_source_reactive <- reactive({
            mat <- heatmap_matrix()
            inputs <- isolate(AllInputs())
            input_df <- data.frame(
                names = names(inputs),
                values = vapply(inputs, function(v) {
                    if (is.null(v)) "NULL" else paste(as.character(v), collapse = ", ")
                }, character(1)),
                stringsAsFactors = FALSE
            )
            list(
                plot = NULL,
                plot_data = as.data.frame(mat),
                stats = NULL,
                inputs = input_df,
                # On the summary rather than only on the reactive, so the
                # archive picks them up regardless of the order this server
                # wires things together in.
                vector_svg = heatmap_svg,
                raster_png = heatmap_png
            )
        })

        output$download.source <- create_source_download_handler(
            data_list = plot_source_reactive,
            filename_base = "ComplexHeatmap_source"
        )

        # Vector export hook for the Figure Builder's canvas export, which reads
        # the attribute rather than the summary (see figureBuilderServer()).
        attr(plot_source_reactive, "vector_svg") <- heatmap_svg
        attr(plot_source_reactive, "raster_png") <- heatmap_png

        return(plot_source_reactive)
    })
}
