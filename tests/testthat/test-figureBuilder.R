test_that("figureBuilderUI namespaces its ids and canvas markup", {
    ui <- figureBuilderUI("figure_builder")
    html <- as.character(ui)
    # The bundled CSS/JS is hoisted into the document head, so pull that slot too.
    head_html <- as.character(htmltools::renderTags(ui)$head)

    # Every control id must be namespaced so the module can be embedded/reused.
    expect_true(grepl("figure_builder-pb_canvas", html))
    expect_true(grepl("figure_builder-pb_add", html))
    expect_true(grepl("figure_builder-download.source", html))

    # Canvas is styled/targeted by class (not a hardcoded id) so the JS/CSS work
    # under any namespace and for multiple instances.
    expect_true(grepl("pb-canvas a4-portrait", html))
    expect_true(grepl("pb-canvas-scroll", html))
    expect_true(grepl("\\.pb-canvas", head_html))

    # The SVG download uses a delegated, class-based handler (no inline onclick
    # to a global function, which would collide across instances). The bundled
    # JS wires that handler up and resolves each button's sibling canvas.
    expect_true(grepl("pb-download-svg", html))
    expect_false(grepl("pbDownloadSVG\\(\\)", html))
    expect_true(grepl("addEventListener\\('click'", head_html))
    expect_true(grepl("pbCanvasForControl", head_html))

    # Panel labels render live on the canvas: the menu is tagged so a delegated
    # change handler can find it, the bundled JS assigns/reorders labels, and the
    # label element is styled by class.
    expect_true(grepl("pb-label-case", html))
    expect_true(grepl("pbAssignLabels", head_html))
    expect_true(grepl("\\.viz-panel-label", head_html))
})

test_that("figureBuilderUI can omit its header title", {
    with_title <- as.character(figureBuilderUI("fb", title = "My Builder"))
    expect_true(grepl("My Builder", with_title))

    no_title <- as.character(figureBuilderUI("fb", title = NULL))
    expect_false(grepl("<h2", no_title))
})

test_that("figureBuilderServer adds and removes panels within its namespace", {
    seen_defaults <- NULL
    # A minimal fake module keeps the test independent of the plot modules.
    fake_reg <- list(
        demo = list(
            label = "Demo", dataset = "d1",
            inputs_ui = function(id, data, defaults = NULL) {
                shiny::tags$div(id = shiny::NS(id)("inp"), "controls")
            },
            output_ui = function(id, resizable = TRUE) {
                shiny::tags$div(id = shiny::NS(id)("out"), "plot")
            },
            server_fn = function(id, data, defaults = NULL) {
                seen_defaults <<- defaults
                shiny::moduleServer(id, function(input, output, session) {
                    shiny::reactive(list(data = data()))
                })
            },
            defaults = list(palette.colours = c(x = "#FF0000"))
        )
    )
    dl <- list(d1 = data.frame(a = 1:3, b = 4:6))

    shiny::testServer(
        figureBuilderServer,
        args = list(data_list = dl, module_registry = fake_reg),
        {
            session$setInputs(pb_orientation = "landscape")
            session$setInputs(pb_add = 1)
            session$setInputs(pb_new_module = "demo", pb_new_dataset = "d1")
            session$setInputs(pb_add_confirm = 1)

            expect_equal(rv$panel_ids, "panel1")
            expect_equal(rv$labels[["panel1"]], "Demo #1 (d1)")
            expect_equal(seen_defaults, list(palette.colours = c(x = "#FF0000")))

            session$setInputs(pb_controls_select = "panel1")
            session$setInputs(pb_table_select = "panel1")

            # The remove button's input arrives under the module namespace.
            session$setInputs(panel1_remove = 1)
            expect_length(rv$panel_ids, 0)
        }
    )
})

test_that("figureBuilderServer validates its inputs", {
    expect_error(
        figureBuilderServer("fb", data_list = list()),
        "length"
    )
    # An entry holding no data frame at all is still rejected.
    expect_error(
        figureBuilderServer("fb", data_list = list(a = 1:3)),
        "app_entry_parts"
    )
    expect_error(
        figureBuilderServer("fb", data_list = list(a = list(b = 1:3))),
        "app_entry_parts"
    )
})

test_that("figureBuilderServer accepts a multi-table dataset entry", {
    entry <- list(
        matrix = data.frame(gene = c("a", "b"), s1 = 1:2),
        column_annotations = data.frame(sample = "s1", condition = "ctrl")
    )
    expect_silent(
        figureBuilderApp(data_list = list(two_table = entry), return_components = TRUE)
    )
})

test_that("figureBuilderApp returns UI and server components", {
    parts <- figureBuilderApp(
        data_list = list(d1 = data.frame(a = 1:3)),
        return_components = TRUE
    )
    expect_named(parts, c("ui", "server"))
    expect_true(is.function(parts$server))
    expect_true(grepl("figure_builder-pb_canvas", as.character(parts$ui)))
})

test_that("the ComplexHeatmap module is offered when its dependencies are present", {
    reg <- .figure_builder_registry()
    dat <- .figure_builder_data()

    # The two-table dataset ships regardless, so any module can still be paired
    # with it (reduced to its matrix).
    expect_true("example_heatmap" %in% names(dat))
    expect_named(dat$example_heatmap, c("matrix", "column_annotations"))

    available <- all(vapply(
        c("ComplexHeatmap", "InteractiveComplexHeatmap", "circlize"),
        requireNamespace, logical(1),
        quietly = TRUE
    ))
    expect_equal("heatmap" %in% names(reg), available)

    skip_if_not(available)
    entry <- reg$heatmap
    expect_equal(entry$dataset, "example_heatmap")
    expect_equal(entry$primary.table, "matrix")
    # The static output, not the InteractiveComplexHeatmap widget: a figure
    # panel has no room for the widget's border and control strip.
    expect_identical(entry$output_ui, ComplexHeatmap_HeatmapStaticOutputUI)
    expect_identical(entry$server_fn, ComplexHeatmap_HeatmapServer)
    expect_true(all(entry$defaults$matrix.cols %in% names(example_heatmap_matrix)))
})

test_that("a multi-table dataset is reduced for modules that cannot take one", {
    entry <- list(
        matrix = data.frame(gene = c("a", "b"), s1 = c(1, 2), s2 = c(3, 4)),
        column_annotations = data.frame(sample = c("s1", "s2"), condition = c("x", "y"))
    )
    registry <- list(
        plain = list(
            label = "Plain", dataset = "two_table",
            inputs_ui = function(id, data, defaults = NULL, ...) {
                shiny::tags$div(paste(class(data), collapse = ","))
            },
            output_ui = function(id, resizable = TRUE) shiny::tags$div(),
            server_fn = function(id, data, defaults = NULL) {
                shiny::moduleServer(id, function(input, output, session) {
                    shiny::reactive(list(seen = class(data())))
                })
            },
            defaults = list()
        ),
        multi = list(
            label = "Multi", dataset = "two_table", primary.table = "matrix",
            inputs_ui = function(id, data, defaults = NULL, ...) {
                shiny::tags$div(paste(names(data), collapse = ","))
            },
            output_ui = function(id, resizable = TRUE) shiny::tags$div(),
            server_fn = function(id, data, defaults = NULL) {
                shiny::moduleServer(id, function(input, output, session) {
                    shiny::reactive(list(seen = names(data())))
                })
            },
            defaults = list()
        )
    )

    shiny::testServer(
        figureBuilderServer,
        args = list(
            data_list = list(two_table = entry),
            module_registry = registry
        ),
        {
            session$setInputs(pb_new_module = "plain", pb_new_dataset = "two_table")
            session$setInputs(pb_add_confirm = 1)
            # A module with no `primary.table` sees the matrix alone.
            expect_equal(panel_sources[["panel1"]]()$seen, "data.frame")

            session$setInputs(pb_new_module = "multi", pb_new_dataset = "two_table")
            session$setInputs(pb_add_confirm = 2)
            # One that declares it sees both tables, filtering applied to the
            # primary and the companion riding along.
            expect_equal(
                panel_sources[["panel2"]]()$seen,
                c("matrix", "column_annotations")
            )
        }
    )
})


test_that("the figure export collects vector art only from panels that offer it", {
    with_svg <- shiny::reactiveVal(list(plot = NULL))
    attr(with_svg, "vector_svg") <- function(width, height, res = 96) {
        paste0("<svg width=", width, " height=", height, "/>")
    }
    broken <- shiny::reactiveVal(list(plot = NULL))
    attr(broken, "vector_svg") <- function(width, height, res = 96) {
        stop("cannot draw")
    }
    sources <- list(
        vector = with_svg,
        # A plotly module returns a bare reactive with no renderer attached.
        plotly = shiny::reactiveVal(list(plot = NULL)),
        broken = broken
    )

    request <- list(
        list(pid = "vector", pw = 480, ph = 380),
        list(pid = "plotly", pw = 480, ph = 380),
        list(pid = "removed", pw = 100, ph = 100),
        list(pw = 100, ph = 100)
    )

    out <- .figure_builder_panel_svgs(request, sources)

    # Only the panel that offered a renderer answers; the plotly panel (which
    # the browser handles itself), a panel removed since the request, and a
    # malformed entry are all simply left out.
    expect_length(out, 1L)
    expect_null(names(out))
    expect_equal(out[[1]]$pid, "vector")
    expect_equal(out[[1]]$svg, "<svg width=480 height=380/>")

    # A panel that cannot draw itself warns and is dropped, rather than taking
    # the whole figure down.
    expect_warning(
        dropped <- .figure_builder_panel_svgs(
            list(list(pid = "broken", pw = 480, ph = 380)), sources
        ),
        "cannot draw"
    )
    expect_length(dropped, 0L)

    expect_length(.figure_builder_panel_svgs(list(), sources), 0L)
})

test_that("a ComplexHeatmap panel works end to end on the canvas", {
    skip_if_not_installed("ComplexHeatmap")
    skip_if_not_installed("InteractiveComplexHeatmap")
    skip_if_not_installed("circlize")

    sample_cols <- setdiff(
        names(example_heatmap_matrix),
        c("gene", "pathway", "mean_expression")
    )

    shiny::testServer(figureBuilderServer, args = list(), {
        session$setInputs(
            pb_new_module = "heatmap", pb_new_dataset = "example_heatmap"
        )
        session$setInputs(pb_add_confirm = 1)

        expect_equal(rv$panel_ids, "panel1")
        # Both tables are kept, so the module's column annotations, splits and
        # filters have the metadata they need.
        expect_named(panel_data[["panel1"]], c("matrix", "column_annotations"))

        # Drive the panel's own controls; they live under the panel's namespace.
        panel_inputs <- list(sample_cols, "gene", "sample", "", "", TRUE)
        names(panel_inputs) <- paste0("panel1-", c(
            "matrix.cols", "rowname.col", "column_key",
            "row_filter", "column_filter", "auto.update"
        ))
        do.call(session$setInputs, panel_inputs)

        # The source-data bundle still works, with plot = NULL as before.
        source <- panel_sources[["panel1"]]()
        expect_named(
            source,
            c(
                "plot", "plot_data", "stats", "inputs",
                # Nothing here for the browser to photograph, so the summary
                # carries the renderers the archive draws the images with.
                "vector_svg", "raster_png"
            )
        )
        expect_null(source$plot)
        expect_equal(nrow(source$plot_data), nrow(example_heatmap_matrix))
        expect_true(is.function(source$vector_svg))
        expect_true(is.function(source$raster_png))

        # ... and the panel contributes real vector art to the figure export.
        exported <- .figure_builder_panel_svgs(
            list(list(pid = "panel1", pw = 480, ph = 380)), panel_sources
        )
        expect_length(exported, 1L)
        expect_true(startsWith(exported[[1]]$svg, "<svg "))
        expect_true(grepl('width="480"', exported[[1]]$svg, fixed = TRUE))
        expect_true(grepl(
            example_heatmap_matrix$gene[1], exported[[1]]$svg,
            fixed = TRUE
        ))

        # A plotly panel alongside it is still handled client-side, so the
        # server contributes nothing for it.
        suppressWarnings({
            session$setInputs(pb_new_module = "bar", pb_new_dataset = "example_bar")
            session$setInputs(pb_add_confirm = 2)
        })
        expect_length(
            .figure_builder_panel_svgs(
                list(
                    list(pid = "panel1", pw = 480, ph = 380),
                    list(pid = "panel2", pw = 480, ph = 380)
                ),
                panel_sources
            ),
            1L
        )
    })
})


test_that("figureBuilderUI points its source download at its own canvas", {
    html <- as.character(figureBuilderUI("fb"))

    expect_true(grepl("viz-source-download", html, fixed = TRUE))
    expect_true(grepl('data-viz-source-ns="fb-"', html, fixed = TRUE))
    # Named explicitly so two builders on one page cannot photograph each
    # other's panels.
    expect_true(grepl('data-viz-canvas="fb-pb_canvas"', html, fixed = TRUE))

    deps <- vapply(htmltools::findDependencies(figureBuilderUI("fb")),
        function(d) d$name, character(1))
    expect_true("viz-source-export" %in% deps)
})


test_that(".figure_builder_sources tags each summary with its panel id", {
    sources <- list(
        panel1 = reactive(list(plot = NULL, plot_data = data.frame(a = 1))),
        panel2 = reactive(list(plot = NULL, plot_data = data.frame(b = 2)))
    )
    labels <- list(panel1 = "Box #1 (mtcars)", panel2 = "Bar #2 (mtcars)")

    out <- isolate(.figure_builder_sources(c("panel1", "panel2"), sources, labels))

    # Named for the reader, keyed for the browser: the archive uses the label
    # the user can edit, while the capture only ever knows the panel id.
    expect_named(out, c("Box #1 (mtcars)", "Bar #2 (mtcars)"))
    expect_equal(out[["Box #1 (mtcars)"]]$svg_key, "panel1")
    expect_equal(out[["Bar #2 (mtcars)"]]$svg_key, "panel2")
})


test_that(".figure_builder_sources forwards renderers given as attributes", {
    svg_fn <- function(width, height, res = 72) "<svg/>"
    png_fn <- function(width, height, res = 72) as.raw(1:4)

    sr <- reactive(list(plot = NULL, plot_data = data.frame(a = 1)))
    attr(sr, "vector_svg") <- svg_fn
    attr(sr, "raster_png") <- png_fn

    out <- isolate(.figure_builder_sources("panel1", list(panel1 = sr),
        list(panel1 = "Heatmap #1")))

    # The older contract puts them on the reactive; the archive reads the
    # summary, so they are copied across.
    expect_identical(out[["Heatmap #1"]]$vector_svg, svg_fn)
    expect_identical(out[["Heatmap #1"]]$raster_png, png_fn)
})


test_that(".figure_builder_sources drops a panel it cannot build", {
    sources <- list(
        panel1 = reactive(stop("panel is broken")),
        panel2 = reactive(list(plot = NULL, plot_data = data.frame(b = 2)))
    )
    labels <- list(panel1 = "Broken #1", panel2 = "Bar #2")

    expect_warning(
        out <- isolate(.figure_builder_sources(c("panel1", "panel2"), sources, labels)),
        "panel is broken"
    )

    # One bad panel must not cost the user the rest of the canvas.
    expect_named(out, "Bar #2")
})
