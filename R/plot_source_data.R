#' Find columns referenced by a plotly figure's trace attributes
#'
#' Walks a `plotly` object's `x$attrs` (the per-trace arguments captured at
#' trace-construction time, e.g. `~mpg` formulas or literal vectors/lists such
#' as parcoords `dimensions`) to determine which columns of the plot's source
#' data.frame are actually rendered. This works generically across both
#' `ggplotly()`-converted figures (which encode mappings as `.data[["col"]]`
#' formulas) and figures built directly with `plot_ly()`/`add_trace()` (which
#' may use `~col` formulas, literal `label = "col"` entries, or, as a last
#' resort, raw data vectors matched back to `full_data` by value).
#'
#' @param plot A `plotly` object.
#' @param full_data The `data.frame` returned by `plotly_data(plot)`.
#'
#' @return A character vector of column names in `full_data` referenced by
#'   `plot`.
#'
#' @author Jared Andrews
#' @keywords internal
#' @rdname INTERNAL_plotted_vars_from_attrs
.plotted_vars_from_attrs <- function(plot, full_data) {
    cols <- names(full_data)
    attrs <- tryCatch(plot$x$attrs, error = function(e) NULL)
    if (is.null(attrs) || length(cols) == 0) {
        return(character(0))
    }

    tokens <- character(0)
    literal_vecs <- list()

    # Recurse through formulas/calls/lists, collecting every symbol and string
    # literal encountered (candidate column names) plus any literal data
    # vectors (fallback for values passed with no accompanying name).
    walk <- function(e) {
        if (inherits(e, "formula")) {
            # Quosures are formulas but deprecate `[[` subsetting; strip the
            # class so plain language-object indexing is used instead.
            class(e) <- setdiff(class(e), "quosure")
            walk(e[[length(e)]])
        } else if (is.call(e)) {
            for (a in as.list(e)) walk(a)
        } else if (is.symbol(e)) {
            tokens <<- c(tokens, as.character(e))
        } else if (is.character(e)) {
            tokens <<- c(tokens, e)
        } else if (is.list(e)) {
            for (el in e) walk(el)
        } else if (is.atomic(e) && length(e) > 1) {
            literal_vecs[[length(literal_vecs) + 1]] <<- e
        }
    }
    for (trace_attrs in attrs) {
        for (v in trace_attrs) walk(v)
    }

    found <- intersect(cols, unique(tokens))

    remaining <- setdiff(cols, found)
    if (length(remaining) && length(literal_vecs) && nrow(full_data) > 0) {
        for (vec in literal_vecs) {
            if (length(remaining) == 0) break
            if (length(vec) != nrow(full_data)) next
            for (col in remaining) {
                same <- tryCatch(
                    isTRUE(all.equal(unname(vec), unname(full_data[[col]]), check.attributes = FALSE)),
                    error = function(e) FALSE
                )
                if (same) {
                    found <- c(found, col)
                    remaining <- setdiff(remaining, col)
                }
            }
        }
    }
    unique(found)
}

#' Find columns referenced by module UI inputs
#'
#' Complements [.plotted_vars_from_attrs()] for columns that never make it
#' into the built `plotly` figure, most notably `split.by`/`facet.by`
#' variables (faceting is resolved before the `ggplot`-to-`plotly` conversion,
#' so the facet column name is lost from the figure entirely). All plot
#' modules name their column-selecting inputs with a consistent convention
#' (e.g. `x.by`, `color.by`, `x.value`, `x.data`, `labels`, `theta`, `group`,
#' `dimensions`), so inputs matching that convention are checked against the
#' plot's source columns.
#'
#' @param ui_inputs A named list of UI input values (see `inputs_reactive` in
#'   [collect_source_data()]).
#' @param cols A character vector of the plot's source data.frame column
#'   names.
#'
#' @return A character vector of column names in `cols` referenced by
#'   `ui_inputs`.
#'
#' @author Jared Andrews
#' @keywords internal
#' @rdname INTERNAL_plotted_vars_from_inputs
.plotted_vars_from_inputs <- function(ui_inputs, cols) {
    if (is.null(ui_inputs) || length(ui_inputs) == 0 || length(cols) == 0) {
        return(character(0))
    }
    selector_names <- c("labels", "values", "theta", "r", "group", "dimensions", "var")
    is_selector <- grepl("\\.(by|value|data)$", names(ui_inputs)) | names(ui_inputs) %in% selector_names
    char_inputs <- Filter(function(v) is.character(v) && length(v) > 0, ui_inputs[is_selector])
    intersect(unique(unlist(char_inputs, use.names = FALSE)), cols)
}

#' Collect plot and source data for download
#'
#' Collects the plot object, its underlying data, statistical testing details (if applied),
#' and optional UI input values into a single list for downstream download
#' generation.
#'
#' @param plot_reactive A reactive expression returning a `plotly` plot object.
#' @param stats_reactive Optional. A reactive expression (e.g. a
#'   [shiny::reactiveVal()]) returning a `data.frame` of statistical test results.
#'   When `NULL` or when the reactive returns `NULL`, no statistics data is
#'   included.
#' @param inputs_reactive Optional. A reactive expression returning a named
#'   list of UI input values. When `NULL` or when it returns `NULL`, no UI
#'   input data is included.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{plot}{The `plotly` plot object.}
#'   \item{plot_data}{A `data.frame` of the plot's underlying data, limited to
#'     the columns and rows actually rendered (see Details).}
#'   \item{stats}{A `data.frame` of statistical test results, or `NULL`.}
#'   \item{inputs}{A `data.frame` of UI input names and values, or `NULL`.}
#' }
#'
#' @details `plot_data` is scoped down from the plot's full source data.frame
#'   (as returned by [plotly::plotly_data()]) in two ways:
#'   \itemize{
#'     \item{Columns are limited to those actually mapped in the plot (x, y,
#'       color/fill, shape, size, labels/values, facets, etc.), detected by
#'       inspecting the built `plotly` figure's trace attributes and, for
#'       columns that don't survive conversion to `plotly` (namely
#'       `split.by`/`facet.by`), the UI input values.}
#'     \item{Rows are limited to those with complete data across the detected
#'       columns, since rows with `NA` in a plotted aesthetic are dropped by
#'       the underlying plotting functions.}
#'   }
#'   If no plotted columns can be detected, the full source data.frame is
#'   returned unchanged.
#'
#' @author Jacob Martin, Jared Andrews
#' @export
#' @examples
#' \dontrun{
#' # Example usage in a Shiny app
#' library(shiny)
#' library(plotly)
#' library(VizModules)
#'
#' ui <- fluidPage(
#'     plotlyOutput("my_plot"),
#'     downloadButton("download_data", "Download Plot and Data")
#' )
#'
#' server <- function(input, output) {
#'     plot_reactive <- reactive({
#'        plot_ly(mtcars, x = ~mpg, y = ~hp, type = "scatter", mode = "markers")
#'     })
#'
#'     data_list <- collect_source_data(plot_reactive)
#'     output$my_plot <- renderPlotly(plot_reactive())
#'     output$download_data <- create_source_download_handler(reactive(data_list))
#' }
#'
#' shinyApp(ui, server)
#' }
collect_source_data <- function(plot_reactive,
                                stats_reactive = NULL,
                                inputs_reactive = NULL) {

    plot <- plot_reactive()
    full_data <- as.data.frame(plotly_data(plot))
    stats <- NULL

    if (!is.null(stats_reactive)) {
        stats_df <- tryCatch(stats_reactive(), error = function(e) NULL)
        if (!is.null(stats_df)) {
            stats <- as.data.frame(stats_df) 
        }
    }

    ui_inputs <- tryCatch(isolate(inputs_reactive), error = function(e) {
        message("ERROR: ", e$message)
        NULL
    })

    plotted_vars <- intersect(
        names(full_data),
        unique(c(
            .plotted_vars_from_attrs(plot, full_data),
            .plotted_vars_from_inputs(ui_inputs, names(full_data))
        ))
    )

    if (length(plotted_vars) > 0) {
        keep_rows <- stats::complete.cases(full_data[, plotted_vars, drop = FALSE])
        plot_data <- full_data[keep_rows, plotted_vars, drop = FALSE]
    } else {
        plot_data <- full_data
    }

    inp <- data.frame(
        names  = names(ui_inputs),
        values = vapply(ui_inputs, function(x) {
            if (is.null(x) || length(x) == 0) ""
            else if (length(x) > 1) paste(x, collapse = ", ")
            else as.character(x)
        }, character(1))
    )

    data_list <- list("plot" = plot, "plot_data" = plot_data, "stats" = stats, "inputs" = inp)
    data_list
}


#' HTML dependency for the source download's image capture
#'
#' Ships the script that photographs a module's live plotly graph when its
#' source download is clicked, so the archive can carry an image of the plot as
#' it actually looks rather than only the interactive HTML.
#'
#' @return An `htmltools::htmlDependency` object.
#'
#' @importFrom htmltools htmlDependency
#'
#' @author Jared Andrews
#' @rdname INTERNAL_source_export_dependency
#' @keywords internal
.source_export_dependency <- function() {
    htmlDependency(
        name = "viz-source-export",
        version = as.character(utils::packageVersion("VizModules")),
        src = "src",
        package = "VizModules",
        script = "sourceExport.js"
    )
}


#' Validate one reported image dimension
#'
#' The browser reports the on-screen size of each plot it photographs, which is
#' the size the server draws at when it has to render a panel itself. Anything
#' unusable -- a hidden tab measuring zero, a graph that never laid out -- is
#' reported as `NA` so the caller can fall back.
#'
#' @param x A reported dimension, in pixels.
#'
#' @return A positive numeric scalar of at least 10, or `NA_real_`.
#'
#' @author Jared Andrews
#' @rdname INTERNAL_source_image_dim
#' @keywords internal
.source_image_dim <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    if (length(x) != 1L || is.na(x) || !is.finite(x) || x < 10) {
        return(NA_real_)
    }
    x
}


#' Pick the size to draw a source image at
#'
#' @param img One entry of [.decode_source_images()]'s result, or `NULL`.
#' @param fallback Length-2 numeric used for any dimension the browser did not
#'   report. Only reached when the browser said nothing at all -- its request
#'   timed out, or the handler is wired to a button that does not carry the
#'   capture markup.
#'
#' @return A length-2 numeric, width then height.
#'
#' @author Jared Andrews
#' @rdname INTERNAL_source_image_size
#' @keywords internal
.source_image_size <- function(img, fallback = c(1000, 700)) {
    w <- .source_image_dim(img$width)
    h <- .source_image_dim(img$height)
    c(
        if (is.na(w)) fallback[[1]] else w,
        if (is.na(h)) fallback[[2]] else h
    )
}


#' Read the images the browser captured off the live plots
#'
#' Turns the payload sent by `sourceExport.js` into a list keyed by the name
#' each image belongs to, ready for [.write_source_zip()] to match against its
#' summaries. Anything malformed is dropped rather than failing the download:
#' the archive is still worth having without a picture in it.
#'
#' @param images The `images` element of the browser's payload: a list of
#'   entries with `key`, optionally `svg` (markup) and `png` (base64), and the
#'   on-screen `width`/`height`.
#' @param max_bytes Largest single image accepted. A dense scatter plot can
#'   produce megabytes of markup, and the archive is not the place to discover
#'   that; oversized images are dropped on the browser side too.
#'
#' @return A named list, one entry per key, of
#'   `list(svg = , png = , width = , height = )`. `svg` is a character scalar or
#'   `NULL`; `png` is a raw vector or `NULL`.
#'
#' @importFrom jsonlite base64_dec
#'
#' @author Jared Andrews
#' @rdname INTERNAL_decode_source_images
#' @keywords internal
.decode_source_images <- function(images, max_bytes = 8e6) {
    out <- list()
    if (is.null(images) || !length(images)) {
        return(out)
    }

    usable <- function(x) {
        is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x) &&
            nchar(x, type = "bytes") <= max_bytes
    }

    for (img in images) {
        key <- img$key
        if (!is.character(key) || length(key) != 1L || is.na(key) ||
            !nzchar(key)) {
            next
        }

        png <- NULL
        if (usable(img$png)) {
            png <- tryCatch(base64_dec(img$png), error = function(e) NULL)
            # base64_dec() answers garbage with an empty vector rather than an
            # error, and an empty vector is not an image.
            if (!is.raw(png) || length(png) == 0) {
                png <- NULL
            }
        }

        out[[key]] <- list(
            svg = if (usable(img$svg)) img$svg else NULL,
            png = png,
            # TRUE when the PNG is a screenshot scraped off a renderPlot output
            # rather than a capture of a plotly graph -- see
            # [.write_source_images()], which prefers a redraw over one of these.
            png_fallback = isTRUE(img$png_fallback),
            width = .source_image_dim(img$width),
            height = .source_image_dim(img$height)
        )
    }

    out
}


#' Write one summary's plot images into the archive directory
#'
#' Each image comes from the browser when it could photograph the plot, and from
#' the module itself otherwise. A module supplies its own by putting a
#' `vector_svg` and/or `raster_png` function on its summary (or, for the older
#' contract, on the reactive it returns); [draw_to_svg()] and [draw_to_png()]
#' build one from any grid or base drawing.
#'
#' A capture of a plotly graph wins, because it is the plot as the user
#' actually has it on screen -- every plotly-layer edit included -- whereas a
#' server-side redraw only knows what the module can rebuild. The one exception
#' is the screenshot the browser can scrape off a non-plotly `renderPlot`
#' output, which carries no such advantage and is pinned to the on-screen
#' device's resolution: a module's own `raster_png` displaces that. So the
#' order is plotly capture, then the module's renderer, then the screenshot.
#'
#' @param dir Directory to write into.
#' @param safe The summary's sanitised name, used as the filename stem.
#' @param img The matching entry of [.decode_source_images()]'s result, or
#'   `NULL` when the browser sent nothing for this summary.
#' @param vector_svg,raster_png Optional `function(width, height, res)` the
#'   module supplies to draw itself. `vector_svg` returns `<svg>` markup,
#'   `raster_png` a raw vector of PNG bytes.
#' @param res Pixels per inch passed to those functions.
#'
#' @return Invisibly `NULL`; called for the files it writes. A format that
#'   cannot be produced or written warns and is skipped, so one bad image never
#'   costs the user the rest of the archive.
#'
#' @author Jared Andrews
#' @rdname INTERNAL_write_source_images
#' @keywords internal
.write_source_images <- function(dir, safe, img, vector_svg = NULL,
                                 raster_png = NULL, res = 72) {
    size <- .source_image_size(img)

    render <- function(fn, what) {
        tryCatch(
            fn(width = size[[1]], height = size[[2]], res = res),
            error = function(e) {
                warning(
                    "Could not render '", safe, "' to ", what, ": ",
                    conditionMessage(e)
                )
                NULL
            }
        )
    }

    svg <- img$svg
    if (is.null(svg) && is.function(vector_svg)) {
        svg <- render(vector_svg, "SVG")
    }
    if (is.character(svg) && length(svg) == 1L && !is.na(svg) && nzchar(svg)) {
        # draw_to_svg() and the Figure Builder both deal in fragments, which
        # carry no namespace; a file on its own needs one restated or nothing
        # will open it.
        tryCatch(
            writeLines(
                .svg_standalone(svg),
                file.path(dir, paste0(safe, "_plot.svg")),
                useBytes = TRUE
            ),
            error = function(e) {
                warning(
                    "Could not write '", safe, "_plot.svg': ",
                    conditionMessage(e)
                )
            }
        )
    }

    png <- img$png
    # A screenshot the browser scraped off a renderPlot output is whatever
    # resolution the on-screen device happened to use, so a module that can
    # redraw itself wins over one -- unlike a plotly capture, which is the plot
    # as the user actually has it and wins over everything. A module that
    # cannot redraw is still better served by the screenshot than by nothing,
    # so it is only displaced once the redraw actually produces something.
    if ((is.null(png) || isTRUE(img$png_fallback)) && is.function(raster_png)) {
        redrawn <- render(raster_png, "PNG")
        if (!is.null(redrawn)) {
            png <- redrawn
        }
    }
    if (is.raw(png) && length(png) > 0) {
        tryCatch(
            writeBin(png, file.path(dir, paste0(safe, "_plot.png"))),
            error = function(e) {
                warning(
                    "Could not write '", safe, "_plot.png': ",
                    conditionMessage(e)
                )
            }
        )
    }

    invisible(NULL)
}


#' Build the source download archive
#'
#' The body of [create_source_download_handler()]'s download, split out so it
#' can be exercised without a browser or a running app.
#'
#' @param file Path to write the `.zip` to.
#' @param data_list_value Either a single summary from [collect_source_data()]
#'   or a named list of them. A summary may additionally carry `svg_key`
#'   (naming the image the browser captured for it) and `vector_svg` /
#'   `raster_png` (see [.write_source_images()]). Summary names become the
#'   archive's filename stems, so they must be distinct -- the Figure Builder's
#'   generated labels are unique by construction.
#' @param images Captured images, as returned by [.decode_source_images()].
#' @param fallback_svg,fallback_png Renderers used for any summary that does not
#'   carry its own, taken from the `vector_svg` / `raster_png` attributes of the
#'   reactive handed to [create_source_download_handler()].
#'
#' @return Invisibly `NULL`; called for the archive it writes.
#'
#' @importFrom htmlwidgets saveWidget
#' @importFrom shinyjqui jqui_resizable
#' @importFrom zip zip
#' @importFrom utils write.csv
#'
#' @author Jacob Martin, Jared Andrews
#' @rdname INTERNAL_write_source_zip
#' @keywords internal
.write_source_zip <- function(file, data_list_value, images = NULL,
                              fallback_svg = NULL, fallback_png = NULL) {
    # Use a fresh temporary directory that lives for the duration of the
    # download. Creating it here (rather than when the handler is built)
    # ensures the directory still exists when the files are written.
    tmp <- tempfile("vizmodules_source_")
    dir.create(tmp)
    on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

    # A single source (e.g. from one plot) is a flat named list with a
    # top-level "plot" element. Wrap it so a single source and a list
    # of sources (one per panel) can be written by the same loop.
    if ("plot" %in% names(data_list_value)) {
        data_list_value <- list("Data" = data_list_value)
    }

    for (x in names(data_list_value)) {
        object <- data_list_value[[x]]
        if (is.null(object)) {
            next
        }

        # Sanitise the (possibly user-facing) name so it is safe to use
        # as part of a file path.
        safe <- gsub("[^A-Za-z0-9._-]+", "_", x)

        if (!is.null(object$stats)) {
            write.csv(object$stats, file.path(tmp, paste0(safe, "_stats_data.csv")), row.names = FALSE)
        }

        if (!is.null(object$plot)) {
            # saveWidget() shells out to pandoc to inline the widget's assets.
            # On a machine without it the archive used to fail outright, taking
            # the data and the images down with the one file that needed it.
            tryCatch(
                saveWidget(
                    widget = jqui_resizable(object$plot),
                    file = file.path(tmp, paste0(safe, "_plot.html")),
                    selfcontained = TRUE
                ),
                error = function(e) {
                    warning(
                        "Could not write '", safe, "_plot.html' (a ",
                        "self-contained widget needs pandoc installed): ",
                        conditionMessage(e)
                    )
                }
            )
        }

        # Which captured image belongs to this summary. The browser knows a
        # Figure Builder panel only by its id, while the summary is named after
        # the label the user can edit, so the Figure Builder tags each entry
        # with the id; a lone module has the one summary this loop names "Data".
        key <- if (is.character(object$svg_key) && length(object$svg_key) == 1L) {
            object$svg_key
        } else {
            "Data"
        }

        .write_source_images(
            tmp, safe, images[[key]],
            vector_svg = if (is.function(object$vector_svg)) {
                object$vector_svg
            } else {
                fallback_svg
            },
            raster_png = if (is.function(object$raster_png)) {
                object$raster_png
            } else {
                fallback_png
            }
        )

        if (!is.null(object$plot_data)) {
            write.csv(object$plot_data, file.path(tmp, paste0(safe, "_plot_data.csv")), row.names = FALSE)
        }

        if (!is.null(object$inputs)) {
            write.csv(object$inputs, file.path(tmp, paste0(safe, "_ui_inputs.csv")), row.names = FALSE)
        }
    }

    files_to_zip <- list.files(tmp, full.names = FALSE)

    if (length(files_to_zip) == 0) {
        stop("No files were created to zip.")
    }

    zip(zipfile = file, files = files_to_zip, root = tmp, mode = "cherry-pick")

    invisible(NULL)
}


#' Park the browser's captured images until the download asks for them
#'
#' A download handler's `content()` cannot wait on the browser: Shiny serves one
#' session on one thread, so blocking there to ask for an image would deadlock
#' the very message it is waiting for. The capture therefore happens *before*
#' the download starts -- the script intercepts the button's first click, sends
#' what it photographed here, and only re-fires the click once this replies.
#'
#' @param store An environment the images are parked in, as `store$images`.
#' @param session The module session whose input carries the payload.
#' @param output_id The download output's id within that session. The browser
#'   sends to `<output_id>_images`.
#'
#' @return Invisibly `NULL`; called to set up the observer.
#'
#' @importFrom shiny observeEvent
#'
#' @author Jared Andrews
#' @rdname INTERNAL_stage_source_images
#' @keywords internal
.stage_source_images <- function(store, session, output_id) {
    input_id <- paste0(output_id, "_images")

    # Defaults on purpose: ignoreNULL already suppresses the empty first value,
    # while ignoreInit would additionally swallow the first real payload
    # whenever the observer's first flush is the one carrying it.
    observeEvent(session$input[[input_id]], {
        payload <- session$input[[input_id]]
        nonce <- payload$nonce
        if (is.null(nonce)) {
            return(invisible(NULL))
        }

        store$images <- tryCatch(
            .decode_source_images(payload$images),
            error = function(e) {
                warning(
                    "Could not read the captured plot images: ",
                    conditionMessage(e)
                )
                NULL
            }
        )

        # Answer even when nothing could be read: the browser holds the download
        # until this lands or its own timeout fires.
        session$sendCustomMessage(
            "vizmodules-source-images",
            list(nonce = nonce)
        )
    })

    invisible(NULL)
}


#' Create download handler for plot with source data
#'
#' Generates a Shiny [downloadHandler()] that bundles the interactive plot,
#' images of it, and its supporting data into a single `.zip` archive.
#'
#' @param data_list A reactive returning either a single summary list produced
#'   by [collect_source_data()] (with elements `plot`, `plot_data`,
#'   `stats`, and `inputs`), or a named list of such summaries (one per plot).
#'   When a named list of summaries is supplied, each summary is written to its
#'   own set of files (prefixed with the list name) so several plots can be
#'   bundled into a single archive.
#' @param filename_base `character(1)`. Base name for the downloaded `.zip`
#'   file without extension. The final filename takes the form
#'   `<filename_base>_<Sys.Date()>.zip`.
#' @param images `logical(1)`. Whether to include `.svg` and `.png` images of
#'   each plot. Requires the button to carry the markup [module_tack_ui()] gives
#'   it; a hand-rolled [shiny::downloadButton()] without it simply gets no
#'   images.
#' @param output_id `character(1)`. The id this handler is assigned to within
#'   its module, used to find the images the browser sends. Only needs changing
#'   if the handler is assigned to something other than `output$download.source`.
#' @param session The module session. Defaults to the calling module's, which is
#'   what every in-package call site wants.
#'
#' @return A `downloadHandler` object suitable for assignment to a Shiny
#' output.
#'
#' @details The archive holds, per plot: the interactive plot as self-contained
#'   HTML (`<name>_plot.html`), an `<name>_plot.svg` and `<name>_plot.png` of
#'   it, and CSVs of the plot data, the statistics, and the UI inputs.
#'
#'   The images are photographed in the browser, off the graph the user is
#'   looking at, so they carry every edit made after the figure was built --
#'   reference lines, statistical brackets, restyled axes and legends, dragged
#'   annotations. The capture happens between the button's click and the
#'   download itself, which is why the button pauses briefly before the archive
#'   arrives. When it cannot be done -- the capture fails, the round trip times
#'   out, the plot sits on a hidden tab -- the archive still downloads, without
#'   the images.
#'
#'   A module whose output is not a plotly graph has nothing for the browser to
#'   photograph and draws itself instead, by putting a `vector_svg` and/or
#'   `raster_png` function of `(width, height, res)` on its summary list, or on
#'   the reactive passed as `data_list`. [draw_to_svg()] and [draw_to_png()]
#'   build one from any grid or base drawing; [ComplexHeatmap_HeatmapServer()]
#'   is the worked example.
#'
#'   A summary may also carry `svg_key`, naming the captured image that belongs
#'   to it. The Figure Builder uses this because the browser knows a panel only
#'   by its id, while the summary is named after the panel's display label.
#'
#'   One caveat worth passing on to users: a plot drawn with WebGL (the
#'   `dittoViz` scatter plot's WebGL toggle) can only be photographed as a
#'   raster, so its points arrive as an embedded image inside an otherwise
#'   vector SVG. Turning WebGL off gives a fully editable file.
#'
#' @importFrom shiny downloadHandler getDefaultReactiveDomain
#'
#' @author Jacob Martin, Jared Andrews
#' @seealso [collect_source_data()], [module_tack_ui()]
#' @export
#' @examples
#' \dontrun{
#' # Example usage in a Shiny app
#' library(shiny)
#' library(plotly)
#' library(VizModules)
#' ui <- fluidPage(
#'     plotlyOutput("my_plot"),
#'     downloadButton("download_data", "Download Plot and Data")
#' )
#'
#' server <- function(input, output) {
#'     plot_reactive <- reactive({
#'         plot_ly(mtcars, x = ~mpg, y = ~hp, type = "scatter", mode = "markers")
#'     })
#'
#'     data_list <- collect_source_data(plot_reactive)
#'     output$my_plot <- renderPlotly(plot_reactive())
#'     output$download_data <- create_source_download_handler(reactive(data_list))
#' }
#'
#' shinyApp(ui, server)
#' }
create_source_download_handler <- function(data_list,
                                           filename_base = "source_data",
                                           images = TRUE,
                                           output_id = "download.source",
                                           session = getDefaultReactiveDomain()) {
    # Where the browser's photographs wait for the download to collect them.
    # A plain environment rather than a reactiveVal: nothing reacts to it, and
    # the download must consume-and-clear so a capture that never arrived can
    # not ship the *previous* download's images.
    store <- new.env(parent = emptyenv())
    store$images <- NULL

    if (isTRUE(images) && !is.null(session)) {
        .stage_source_images(store, session, output_id)
    }

    downloadHandler(
        filename = function() {
            paste0(filename_base, "_", Sys.Date(), ".zip")
        },
        content = function(file) {
            staged <- store$images
            store$images <- NULL

            .write_source_zip(
                file, data_list(), staged,
                # The older contract, kept for modules that attach their
                # renderers to the reactive rather than to the summary.
                fallback_svg = attr(data_list, "vector_svg"),
                fallback_png = attr(data_list, "raster_png")
            )
        }
    )
}
