#' Collect vector art for Figure Builder panels the browser cannot export
#'
#' The canvas export reads an SVG straight out of a plotly graph client-side. A
#' panel rendering anything else leaves it nothing to read, so the browser asks
#' the server for those panels instead and this builds the answer.
#'
#' A module opts in by attaching a `vector_svg` attribute to the reactive its
#' server returns -- a `function(width, height, res)` yielding an `<svg>`
#' element drawn at that pixel size. A panel whose module attaches nothing, or
#' which has since been removed, is left out of the reply and simply contributes
#' no artwork to the figure, exactly as it did before.
#'
#' @param panels A list of requests, one per panel, each with `pid` (the panel
#'   id) and `pw`/`ph` (its on-screen size in pixels).
#' @param sources A store of per-panel source reactives keyed by panel id (the
#'   Figure Builder's `panel_sources`).
#'
#' @return An unnamed list of `list(pid = , svg = )`, holding only the panels
#'   that produced artwork.
#'
#' @author Jared Andrews
#' @rdname INTERNAL_figure_builder_panel_svgs
#' @keywords internal
.figure_builder_panel_svgs <- function(panels, sources) {
    rendered <- lapply(panels, function(panel) {
        pid <- panel$pid
        if (is.null(pid)) {
            return(NULL)
        }
        panel_source <- sources[[pid]]
        render <- attr(panel_source, "vector_svg")
        if (is.null(panel_source) || !is.function(render)) {
            return(NULL)
        }
        # One panel that cannot draw itself must not take the whole figure down
        # with it; it is dropped and the rest of the figure still exports.
        svg <- tryCatch(
            render(width = panel$pw, height = panel$ph),
            error = function(e) {
                warning(
                    "Could not render panel '", pid, "' to SVG: ",
                    conditionMessage(e)
                )
                NULL
            }
        )
        if (is.null(svg)) NULL else list(pid = pid, svg = svg)
    })

    unname(Filter(Negate(is.null), rendered))
}


#' Collect every panel's source summary for the Figure Builder's archive
#'
#' Gathers what each panel's module reports about itself and names each entry
#' after the panel's label, so the archive reads the way the canvas looks.
#'
#' Two things are added on the way through. Each summary is tagged with its
#' panel id, because the browser photographs cards by id while the archive names
#' them by the generated label, and [create_source_download_handler()] needs the
#' two to meet. And a module that
#' declares its renderers the older way -- as attributes on the reactive, the
#' `vector_svg` contract in [figureBuilderServer()] -- has them copied onto the
#' summary, so panels the browser cannot photograph still draw themselves.
#'
#' @param ids Panel ids, in canvas order.
#' @param sources The per-panel source reactives, keyed by panel id.
#' @param labels The per-panel labels, keyed by panel id.
#'
#' @return A named list of summaries, one per panel that produced one.
#'
#' @author Jared Andrews
#' @rdname INTERNAL_figure_builder_sources
#' @keywords internal
.figure_builder_sources <- function(ids, sources, labels) {
    out <- lapply(ids, function(p) {
        sr <- sources[[p]]
        if (is.null(sr)) {
            return(NULL)
        }
        # Skip (rather than abort the whole download) if a single panel's
        # source cannot be built.
        src <- tryCatch(sr(), error = function(e) {
            warning(
                "Could not build source for panel '", p, "': ",
                conditionMessage(e)
            )
            NULL
        })
        if (is.null(src)) {
            return(NULL)
        }

        src$svg_key <- p
        if (!is.function(src$vector_svg)) {
            src$vector_svg <- attr(sr, "vector_svg")
        }
        if (!is.function(src$raster_png)) {
            src$raster_png <- attr(sr, "raster_png")
        }
        src
    })

    names(out) <- vapply(ids, function(p) labels[[p]], character(1))
    out[!vapply(out, is.null, logical(1))]
}


#' Server logic for the Figure Builder module
#'
#' Powers the multi-panel **Figure Builder** module rendered by
#' [figureBuilderUI()]. Users can add any VizModules plot module to a free-form
#' A4 canvas, drag and resize each plot, filter each plot's data independently,
#' label panels automatically, and export the whole figure as a single editable
#' SVG (or bundle every plot's source data, HTML plot, and statistics into one
#' `.zip`).
#'
#' Call this from your app's server with the same `id` you passed to
#' [figureBuilderUI()]. Because it is a proper Shiny module, several Figure
#' Builders can coexist on one page, each with its own namespace and canvas.
#'
#' @param id The ID for the Shiny module. Must match the `id` given to
#'   [figureBuilderUI()].
#' @param data_list An optional named list of data frames that seed the dataset
#'   registry. If `NULL` (the default), the bundled example datasets (plus a
#'   `sales_by_product` summary suited to the pie plot) are used. At least one
#'   element is required. An element is either a data frame, or a named list of
#'   data frames for a module that needs companion tables (the `ComplexHeatmap`
#'   module's `list(matrix = , column_annotations = )`); in the latter case only
#'   the primary table is filtered and shown in the panel's table pane.
#' @param module_registry An optional named list describing the plot modules to
#'   offer. If `NULL` (the default), all bundled VizModules modules are offered.
#'   Each entry is itself a list with components: `label` (character, shown in the
#'   picker), `dataset` (character, the dataset name its `defaults` were written
#'   for), `inputs_ui`, `output_ui`, and `server_fn` (the module's three
#'   functions), and `defaults` (a named list of input defaults applied only when
#'   `dataset` is the chosen dataset). `defaults` is passed to both `inputs_ui`
#'   and `server_fn`, so it can seed server-rendered controls such as the group
#'   color picker. An entry may also carry `primary.table`, naming which table of
#'   a multi-table dataset gets filtered (the first by default); its presence is
#'   also what marks the module as able to take a multi-table dataset at all, so
#'   modules without it are handed the primary table alone and any dataset stays
#'   usable with any module.
#'
#'   A module whose output is not a plotly graph can still contribute to the SVG
#'   figure export by attaching a `vector_svg` attribute to the reactive its
#'   server returns: a `function(width, height, res)` yielding an `<svg>`
#'   element drawn at that pixel size, which is spliced into the figure in place
#'   of the `Plotly.toImage()` result. [draw_to_svg()] builds one from any grid
#'   or base drawing; [ComplexHeatmap_HeatmapServer()] is the worked example. A
#'   panel whose module attaches nothing simply contributes no artwork.
#'
#'   The same renderers feed the source archive, which additionally wants a
#'   `raster_png` counterpart returning PNG bytes (see [draw_to_png()]). Either
#'   can be given as an attribute on the reactive, as here, or as a field on the
#'   summary list the reactive returns -- the latter being order-independent,
#'   since the summary is rebuilt on every download. See
#'   [create_source_download_handler()].
#'
#' @return Invisibly returns `NULL`; called for its side effects (wiring up the
#'   Figure Builder module's reactive logic).
#'
#' @import shiny
#' @importFrom stats aggregate setNames
#'
#' @export
#' @author Jared Andrews
#' @seealso [figureBuilderUI()], [figureBuilderApp()]
#' @examples
#' library(VizModules)
#' if (interactive()) {
#'     ui <- fluidPage(figureBuilderUI("figure_builder"))
#'     server <- function(input, output, session) {
#'         figureBuilderServer("figure_builder")
#'     }
#'     shinyApp(ui, server)
#' }
figureBuilderServer <- function(id, data_list = NULL, module_registry = NULL) {
    # --- Validate / resolve inputs
    if (is.null(data_list)) {
        data_list <- .figure_builder_data()
    }
    stopifnot(is.list(data_list), length(data_list) >= 1)
    # An entry may be a plain data frame or a named list of them, for modules
    # that take companion tables alongside the one that gets filtered (the
    # ComplexHeatmap module's column annotations). .app_entry_parts() is what
    # resolves either shape, and returns NULL for anything holding no table.
    for (entry in data_list) {
        stopifnot(!is.null(.app_entry_parts(entry)))
    }

    if (is.null(module_registry)) {
        module_registry <- .figure_builder_registry()
    }
    stopifnot(is.list(module_registry), length(module_registry) >= 1)

    # The datasets that seed the dataset registry. Users can add their own
    # datasets at runtime via the "Load Data" section (see the server below),
    # which are appended to this catalogue.
    initial_datasets <- data_list

    # Choices for the "add plot" module picker (label shown, key returned).
    module_choices <- setNames(
        names(module_registry),
        vapply(module_registry, function(m) m$label, character(1))
    )

    moduleServer(id, function(input, output, session) {
        ns <- session$ns

        # Registry of datasets available to the "Add Plot" dialog.
        dataset_store <- reactiveVal(initial_datasets)

        # Reactive bookkeeping for the panels currently on the canvas.
        rv <- reactiveValues(
            panel_ids = character(0), # ordered vector of active panel ids
            labels    = list(), # pid -> display label
            counter   = 0L # monotonic id source
        )

        # Per-panel data snapshots (fixed at creation time) and the observers that
        # power each card's remove button. Storing the data in reactiveValues lets
        # us null it out on removal so any lingering reactive reads short-circuit
        # via req() instead of erroring.
        panel_data <- reactiveValues()

        # Per-panel source reactives returned by each module server. Used to bundle
        # every plot's interactive source (plot + data + inputs) into one download.
        panel_sources <- reactiveValues()
        panel_observers <- new.env(parent = emptyenv())

        observeEvent(input$pb_data_add, {
            file <- input$pb_data_file

            if (is.null(file)) {
                showNotification("Choose a file to load first.", type = "warning")
                return(invisible(NULL))
            }

            ext <- tolower(tools::file_ext(file$name))
            df <- tryCatch(
                {
                    switch(ext,
                        csv = utils::read.csv(file$datapath,
                            stringsAsFactors = FALSE, check.names = FALSE
                        ),
                        tsv = utils::read.delim(file$datapath,
                            stringsAsFactors = FALSE, check.names = FALSE
                        ),
                        txt = utils::read.delim(file$datapath,
                            stringsAsFactors = FALSE, check.names = FALSE
                        ),
                        rds = readRDS(file$datapath),
                        stop("Unsupported file type '.", ext, "'.")
                    )
                },
                error = function(e) e
            )

            if (inherits(df, "error")) {
                showNotification(paste("Could not read file:", conditionMessage(df)),
                    type = "error", duration = 8
                )
                return(invisible(NULL))
            }

            if (!is.data.frame(df)) {
                df <- tryCatch(as.data.frame(df, check.names = FALSE),
                    error = function(e) NULL
                )
            }

            if (!is.data.frame(df) || nrow(df) == 0L || ncol(df) == 0L) {
                showNotification("File must contain a non-empty data frame.",
                    type = "error", duration = 8
                )
                return(invisible(NULL))
            }

            nm <- trimws(input$pb_data_name)

            if (!.nz_value(nm)) {
                nm <- tools::file_path_sans_ext(basename(file$name))
            }

            store <- dataset_store()

            # Ensure a unique name so existing datasets are never overwritten.
            base <- nm
            i <- 1L

            while (nm %in% names(store)) {
                i <- i + 1L
                nm <- paste0(base, " (", i, ")")
            }

            store[[nm]] <- df
            dataset_store(store)
            updateTextInput(session, "pb_data_name", value = "")

            showNotification(
                sprintf(
                    "Added dataset '%s' (%d rows x %d cols).",
                    nm, nrow(df), ncol(df)
                ),
                type = "message"
            )
        })

        observeEvent(input$pb_orientation, {
            # shinyjs namespaces the `id` for us inside a module, so pass bare ids
            # (wrapping in ns() here would double-namespace and silently no-op).
            if (identical(input$pb_orientation, "landscape")) {
                shinyjs::removeClass("pb_canvas", "a4-portrait")
                shinyjs::addClass("pb_canvas", "a4-landscape")
            } else {
                shinyjs::removeClass("pb_canvas", "a4-landscape")
                shinyjs::addClass("pb_canvas", "a4-portrait")
            }
        })

        observeEvent(input$pb_add, {
            showModal(modalDialog(
                title = "Add a Plot",
                viz_select_input(ns("pb_new_module"), "Plot type:",
                    choices = module_choices
                ),
                viz_select_input(ns("pb_new_dataset"), "Dataset:",
                    choices = names(dataset_store())
                ),
                footer = tagList(
                    modalButton("Cancel"),
                    actionButton(ns("pb_add_confirm"), "Add",
                        class = "btn-primary"
                    )
                ),
                easyClose = TRUE
            ))
        })

        # Suggest the module's preferred dataset when the plot type changes.
        observeEvent(input$pb_new_module, {
            mod <- module_registry[[input$pb_new_module]]
            if (!is.null(mod) && mod$dataset %in% names(dataset_store())) {
                update_viz_select(session, "pb_new_dataset",
                    selected = mod$dataset
                )
            }
        })

        # --- Create a panel
        observeEvent(input$pb_add_confirm, {
            datasets <- dataset_store()
            mod_key <- input$pb_new_module
            ds_name <- input$pb_new_dataset
            mod <- module_registry[[mod_key]]
            req(mod, ds_name %in% names(datasets))
            removeModal()

            rv$counter <- rv$counter + 1L
            pid <- paste0("panel", rv$counter)
            data_snapshot <- datasets[[ds_name]]
            panel_data[[pid]] <- data_snapshot

            label <- sprintf("%s #%d (%s)", mod$label, rv$counter, ds_name)
            # Only apply built-in defaults when the dataset they target is chosen.
            defaults <- if (identical(ds_name, mod$dataset)) mod$defaults else list()

            # Any dataset can be paired with any module, so a multi-table dataset
            # chosen for a module that knows nothing about companion tables is
            # reduced to its primary table. A registry entry declares it can take
            # the whole thing by naming a `primary.table`.
            multi_table <- !is.null(mod$primary.table)
            module_data <- if (multi_table) {
                data_snapshot
            } else {
                .app_entry_parts(data_snapshot)$primary
            }

            # Hide empty-state hints once the first panel is added. shinyjs
            # namespaces these ids itself, so pass them bare.
            shinyjs::hide("pb_canvas_empty")
            shinyjs::hide("pb_controls_empty")
            shinyjs::hide("pb_table_empty")

            # 1) Plot card on the canvas (draggable via the hover toolbar's grip,
            #    resizable from the corner). The toolbar only appears on hover and
            #    is excluded from the SVG export, so the card stays free of chrome.
            card <- div(
                id = ns(paste0(pid, "_card")),
                class = "viz-panel-card",
                # Pin every new card to the top-left of the canvas. `position` is
                # forced inline (with !important) so nothing in the cascade or any
                # jQuery UI wrapper can drop the card back into normal document flow
                # where the cards would stack vertically down the page.
                style = "position:absolute !important; top:20px; left:20px;",
                div(
                    class = "viz-panel-toolbar",
                    span(
                        class = "viz-panel-drag", title = label,
                        icon("grip-vertical")
                    ),
                    tags$button(
                        id = ns(paste0(pid, "_remove")),
                        class = "viz-panel-remove action-button",
                        type = "button", title = "Remove plot",
                        icon("times")
                    )
                ),
                # Live panel label (a, b, c, ...). Text is filled in client-side
                # from the "Panel labels" control and reorders as cards are moved;
                # it is excluded from the SVG export (which draws its own labels).
                div(class = "viz-panel-label"),
                div(class = "viz-panel-body", mod$output_ui(ns(pid), resizable = FALSE))
            )
            insertUI(
                selector = paste0("#", ns("pb_canvas")), where = "beforeEnd",
                ui = shinyjqui::jqui_draggable(
                    shinyjqui::jqui_resizable(card),
                    options = list(handle = ".viz-panel-drag", containment = "parent")
                ),
                immediate = TRUE
            )

            # 2) Controls for this panel (hidden until selected in the dropdown).
            insertUI(
                selector = paste0("#", ns("pb_controls_container")), where = "beforeEnd",
                ui = div(
                    id = ns(paste0(pid, "_controls")),
                    class = "pb-controls-pane",
                    style = "display:none;",
                    mod$inputs_ui(ns(pid), module_data, defaults = defaults)
                ),
                immediate = TRUE
            )

            # 3) Data-filter table for this panel (hidden until selected).
            insertUI(
                selector = paste0("#", ns("pb_table_container")), where = "beforeEnd",
                ui = div(
                    id = ns(paste0(pid, "_table")),
                    class = "pb-table-pane",
                    style = "display:none;",
                    dataFilterUI(ns(paste0(pid, "_filter")))
                ),
                immediate = TRUE
            )

            # 4) Wire up the servers. The dataset is fixed; the filter feeds the
            #    plot. Reads short-circuit once the panel's data is removed. The
            #    sub-module servers are called with bare ids so they namespace
            #    themselves under this module's namespace prefix (the wrapper
            #    pattern described in `vignette("custom-modules")`), while their
            #    UI functions above are given `ns(pid)` to match.
            # The entry as supplied (a data frame, or a list of tables) ...
            panel_entry <- reactive({
                .app_entry_parts(req(panel_data[[pid]]), primary = mod$primary.table)
            })
            # ... of which only the primary table is filtered and shown in the
            # panel's table pane; companion tables ride along untouched, as in
            # createModuleApp().
            panel_reactive <- reactive(req(panel_entry())$primary)

            filtered_primary <- dataFilterServer(paste0(pid, "_filter"), panel_reactive)
            filtered <- if (multi_table) {
                # Hand the module back the shape it was given, filtered rows
                # swapped in.
                reactive(panel_entry()$rebuild(filtered_primary()))
            } else {
                filtered_primary
            }

            # The module server returns a reactive yielding its interactive source
            # (plot + data + inputs); keep it so we can bundle every panel together.
            panel_sources[[pid]] <- mod$server_fn(pid, data = filtered, defaults = defaults)

            # 5) Per-panel remove handler (tracked so it can be destroyed on remove).
            panel_observers[[pid]] <- observeEvent(
                input[[paste0(pid, "_remove")]],
                {
                    remove_panel(pid)
                },
                ignoreInit = TRUE
            )

            # 6) Register the panel and focus it in both dropdowns.
            rv$labels[[pid]] <- label
            rv$panel_ids <- c(rv$panel_ids, pid)
            refresh_selectors(selected = pid)
        })

        remove_panel <- function(pid) {
            if (!pid %in% rv$panel_ids) {
                return(invisible(NULL))
            }
            # Destroy the jQuery UI interactions before pulling the DOM nodes so the
            # remaining cards are never disturbed by orphaned handlers.
            shinyjqui::jqui_draggable(paste0("#", ns(paste0(pid, "_card"))),
                operation = "destroy"
            )
            shinyjqui::jqui_resizable(paste0("#", ns(paste0(pid, "_card"))),
                operation = "destroy"
            )
            removeUI(selector = paste0("#", ns(paste0(pid, "_card"))), immediate = TRUE)
            removeUI(selector = paste0("#", ns(paste0(pid, "_controls"))), immediate = TRUE)
            removeUI(selector = paste0("#", ns(paste0(pid, "_table"))), immediate = TRUE)

            # Tear down the panel's bookkeeping so its controls/table cannot linger.
            if (!is.null(panel_observers[[pid]])) {
                panel_observers[[pid]]$destroy()
                rm(list = pid, envir = panel_observers)
            }
            panel_data[[pid]] <- NULL
            panel_sources[[pid]] <- NULL
            rv$labels[[pid]] <- NULL
            rv$panel_ids <- setdiff(rv$panel_ids, pid)

            if (length(rv$panel_ids) == 0L) {
                shinyjs::show("pb_canvas_empty")
                shinyjs::show("pb_controls_empty")
                shinyjs::show("pb_table_empty")
            }
            refresh_selectors()
        }

        # Keep both selectors in sync with the active panels
        refresh_selectors <- function(selected = NULL) {
            choices <- setNames(
                rv$panel_ids,
                vapply(rv$panel_ids, function(p) rv$labels[[p]], character(1))
            )
            pick <- function(current) {
                if (!is.null(selected)) {
                    return(selected)
                }
                if (!is.null(current) && current %in% rv$panel_ids) {
                    return(current)
                }
                if (length(rv$panel_ids)) rv$panel_ids[[1]] else NULL
            }
            update_viz_select(session, "pb_controls_select",
                choices = choices, selected = pick(isolate(input$pb_controls_select))
            )
            update_viz_select(session, "pb_table_select",
                choices = choices, selected = pick(isolate(input$pb_table_select))
            )
        }

        # Swap visible controls
        observeEvent(input$pb_controls_select,
            {
                sel <- input$pb_controls_select
                for (p in rv$panel_ids) {
                    if (identical(p, sel)) {
                        shinyjs::show(paste0(p, "_controls"))
                    } else {
                        shinyjs::hide(paste0(p, "_controls"))
                    }
                }
            },
            ignoreNULL = FALSE
        )

        # Swap visible table
        observeEvent(input$pb_table_select,
            {
                sel <- input$pb_table_select
                for (p in rv$panel_ids) {
                    if (identical(p, sel)) {
                        shinyjs::show(paste0(p, "_table"))
                    } else {
                        shinyjs::hide(paste0(p, "_table"))
                    }
                }
            },
            ignoreNULL = FALSE
        )

        # Summary download
        # Bundle every panel's interactive summary (plot + data + inputs) into a
        # single .zip. We collect each panel's summary reactive (returned by its
        # module server) and hand a named list of summaries to
        # create_source_download_handler, which writes one set of files per panel.
        output$download.source <- create_source_download_handler(
            data_list = reactive({
                ids <- rv$panel_ids

                validate(need(
                    length(ids) > 0,
                    "Add at least one plot before downloading."
                ))

                .figure_builder_sources(ids, panel_sources, rv$labels)
            }),
            filename_base = "panel_source"
        )

        # Vector art for panels the client-side export cannot read. The canvas
        # export pulls an SVG straight out of a plotly graph in the browser; a
        # module that renders anything else (the ComplexHeatmap module's static
        # heatmap) leaves it nothing to work with, so the browser asks here
        # instead and the module redraws itself onto an SVG device at the panel's
        # on-screen size. A module opts in by attaching a `vector_svg` function
        # to the reactive its server returns; panels whose module attaches
        # nothing are answered with an empty list and simply contribute no
        # artwork, exactly as before.
        observeEvent(input$pb_svg_request, {
            request <- input$pb_svg_request
            req(request$nonce)

            session$sendCustomMessage("vizmodules-pb-svg", list(
                nonce = request$nonce,
                panels = .figure_builder_panel_svgs(request$panels, panel_sources)
            ))
        })

        invisible(NULL)
    })
}
