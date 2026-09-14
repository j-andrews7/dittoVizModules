test_that("collect_source_data handles zero-length (non-NULL) UI input values", {
    fig <- plotly::plot_ly(mtcars, x = ~mpg, y = ~hp, type = "scatter", mode = "markers")

    # A multi-select input with nothing chosen can report `character(0)`
    # rather than `NULL`, which previously made `names`/`values` mismatch in
    # length and crash `data.frame()`.
    result <- collect_source_data(
        plot_reactive = function() fig,
        inputs_reactive = list(x.by = "mpg", y.by = "hp", hover.data = character(0), title = "t")
    )

    expect_equal(nrow(result$inputs), length(result$inputs$names))
    expect_equal(result$inputs$values[result$inputs$names == "hover.data"], "")
})

test_that("collect_source_data limits plot_data to plotted columns and rows", {
    df <- mtcars
    df$carname <- rownames(df)
    df$gear_f <- factor(df$gear)

    fig <- plotly::ggplotly(
        dittoViz::scatterPlot(df, x.by = "mpg", y.by = "hp", color.by = "cyl", shape.by = "gear_f", data.out = FALSE)
    )

    result <- collect_source_data(
        plot_reactive = function() fig,
        inputs_reactive = list(x.by = "mpg", y.by = "hp", color.by = "cyl", shape.by = "gear_f", split.by = "")
    )

    expect_setequal(names(result$plot_data), c("mpg", "hp", "cyl", "gear_f"))
    expect_equal(nrow(result$plot_data), nrow(df))
})

test_that("collect_source_data recovers split.by/facet columns from UI inputs", {
    df <- mtcars
    df$am <- factor(df$am)

    fig <- plotly::ggplotly(
        dittoViz::scatterPlot(df, x.by = "mpg", y.by = "hp", color.by = "cyl", split.by = "am", data.out = FALSE)
    )

    result <- collect_source_data(
        plot_reactive = function() fig,
        inputs_reactive = list(x.by = "mpg", y.by = "hp", color.by = "cyl", split.by = "am")
    )

    expect_true("am" %in% names(result$plot_data))
    expect_setequal(names(result$plot_data), c("mpg", "hp", "cyl", "am"))
})

test_that("collect_source_data drops rows with NA in a plotted column", {
    df <- mtcars
    df$hp[c(2, 5)] <- NA

    fig <- plotly::ggplotly(
        dittoViz::scatterPlot(df, x.by = "mpg", y.by = "hp", color.by = "cyl", data.out = FALSE)
    )

    result <- collect_source_data(
        plot_reactive = function() fig,
        inputs_reactive = list(x.by = "mpg", y.by = "hp", color.by = "cyl")
    )

    expect_equal(nrow(result$plot_data), nrow(df) - 2)
    expect_false(anyNA(result$plot_data$hp))
})

test_that("collect_source_data falls back to full data when no plotted vars are detected", {
    fig <- plotly::plot_ly(mtcars)

    result <- collect_source_data(plot_reactive = function() fig, inputs_reactive = list(title = "My Plot"))

    expect_setequal(names(result$plot_data), names(mtcars))
    expect_equal(nrow(result$plot_data), nrow(mtcars))
})

test_that("collect_source_data scopes dumbbellPlot data to the plotted x/y columns", {
    ddata <- data.frame(
        School = c("MIT", "Stanford", "Harvard"),
        Women = c(152, 96, 112),
        Men = c(95, 151, 165),
        Extra = c(1, 2, 3)
    )
    fig <- dumbbellPlot(
        data = ddata, x = c("Women", "Men"), y = "School",
        colour.by = "X variables", palette.selection = c("green", "blue")
    )

    result <- collect_source_data(
        plot_reactive = function() fig,
        inputs_reactive = list(x.value = c("Women", "Men"), y.value = "School", colour.by = "X variables")
    )

    expect_setequal(names(result$plot_data), c("School", "Women", "Men"))
})

test_that("collect_source_data scopes parallelCoordinatesPlot data to the selected dimensions", {
    fig <- parallelCoordinatesPlot(data = mtcars, dimensions = c("mpg", "cyl", "hp"), color.by = "mpg")

    result <- collect_source_data(
        plot_reactive = function() fig,
        inputs_reactive = list(dimensions = c("mpg", "cyl", "hp"), color.by = "mpg")
    )

    expect_setequal(names(result$plot_data), c("mpg", "cyl", "hp"))
    expect_equal(nrow(result$plot_data), nrow(mtcars))
})


# --- Images captured in the browser -------------------------------------------

test_that(".decode_source_images reads a payload into per-key images", {
    skip_if_not(isTRUE(capabilities("png")))

    png_raw <- draw_to_png(function() plot(1:5), 200, 150)
    payload <- list(
        list(
            key = "Data", svg = "<svg><g/></svg>",
            png = jsonlite::base64_enc(png_raw),
            width = 640, height = 480
        )
    )

    out <- .decode_source_images(payload)

    expect_named(out, "Data")
    expect_equal(out$Data$svg, "<svg><g/></svg>")
    # The bytes must survive the trip, or the PNG in the archive is corrupt.
    expect_equal(out$Data$png, png_raw)
    expect_equal(out$Data$width, 640)
    expect_equal(out$Data$height, 480)
})


test_that(".decode_source_images drops what it cannot use", {
    # No key, so there is no summary it could belong to.
    expect_length(.decode_source_images(list(list(svg = "<svg/>"))), 0)
    expect_length(.decode_source_images(list(list(key = "", svg = "<svg/>"))), 0)

    # Nothing at all.
    expect_equal(.decode_source_images(NULL), list())
    expect_equal(.decode_source_images(list()), list())

    # Present but empty fields are not images.
    one <- .decode_source_images(list(list(key = "a", svg = "", png = "")))
    expect_null(one$a$svg)
    expect_null(one$a$png)

    # Undecodable base64 loses the PNG without taking the entry with it.
    # base64_dec() answers garbage with an empty vector rather than an error,
    # which must still read as "no image" downstream.
    bad <- .decode_source_images(list(list(key = "a", svg = "<svg/>", png = "!!!!")))
    expect_equal(bad$a$svg, "<svg/>")
    expect_null(bad$a$png)
})


test_that(".decode_source_images enforces the per-image size cap", {
    skip_if_not(isTRUE(capabilities("png")))

    png_raw <- draw_to_png(function() plot(1:5), 200, 150)
    b64 <- jsonlite::base64_enc(png_raw)
    cap <- nchar(b64, type = "bytes") + 10

    out <- .decode_source_images(
        list(list(key = "h", svg = strrep("x", cap + 50), png = b64)),
        max_bytes = cap
    )

    # The oversized one goes; the other is still worth having.
    expect_null(out$h$svg)
    expect_equal(out$h$png, png_raw)
})


test_that(".source_image_size falls back per dimension", {
    expect_equal(.source_image_size(list(width = 800, height = 600)), c(800, 600))

    # Nothing reported at all: the browser never answered.
    expect_equal(.source_image_size(NULL), c(1000, 700))

    # A dimension that cannot be drawn at falls back on its own, so a half-
    # usable report is not thrown away wholesale.
    expect_equal(.source_image_size(list(width = 800, height = 2)), c(800, 700))
    expect_equal(.source_image_size(list(width = NA, height = 600)), c(1000, 600))
    expect_equal(.source_image_size(list(width = -5, height = "wide")), c(1000, 700))
})


# --- Writing the images -------------------------------------------------------

test_that(".write_source_images prefers the browser's capture over a redraw", {
    skip_if_not(isTRUE(capabilities("png")))

    dir <- withr::local_tempdir()
    png_raw <- draw_to_png(function() plot(1:5), 200, 150)
    called <- FALSE

    .write_source_images(
        dir, "x",
        img = list(svg = "<svg viewBox='0 0 10 10'><g/></svg>", png = png_raw),
        vector_svg = function(width, height, res) {
            called <<- TRUE
            "<svg/>"
        },
        raster_png = function(width, height, res) {
            called <<- TRUE
            png_raw
        }
    )

    # The capture is the plot as the user actually has it, so a module that
    # could also draw itself is not asked to.
    expect_false(called)
    expect_true(file.exists(file.path(dir, "x_plot.svg")))
    expect_true(file.exists(file.path(dir, "x_plot.png")))
    expect_equal(readBin(file.path(dir, "x_plot.png"), "raw", n = 1e6), png_raw)
    # Written as a file on its own, so it needs the namespace a fragment lacks.
    expect_true(startsWith(readLines(file.path(dir, "x_plot.svg"), n = 1), "<?xml"))
})


test_that(".write_source_images falls back to the module's own renderers", {
    skip_if_not(isTRUE(capabilities("png")))

    dir <- withr::local_tempdir()
    seen <- NULL

    .write_source_images(
        dir, "x", img = NULL,
        vector_svg = function(width, height, res) {
            seen <<- c(width, height, res)
            draw_to_svg(function() plot(1:3), width, height, res = res)
        },
        raster_png = function(width, height, res) {
            draw_to_png(function() plot(1:3), width, height, res = res)
        }
    )

    # No size came from the browser, so the documented fallback is used.
    expect_equal(seen, c(1000, 700, 72))
    expect_true(file.exists(file.path(dir, "x_plot.svg")))
    expect_true(file.exists(file.path(dir, "x_plot.png")))
})


test_that(".write_source_images survives a renderer that fails", {
    skip_if_not(isTRUE(capabilities("png")))

    dir <- withr::local_tempdir()
    png_raw <- draw_to_png(function() plot(1:5), 200, 150)

    expect_warning(
        .write_source_images(
            dir, "x", img = NULL,
            vector_svg = function(width, height, res) stop("boom"),
            raster_png = function(width, height, res) png_raw
        ),
        "boom"
    )

    # One format failing must not cost the other.
    expect_false(file.exists(file.path(dir, "x_plot.svg")))
    expect_true(file.exists(file.path(dir, "x_plot.png")))
})


test_that(".write_source_images writes nothing when there is nothing to write", {
    dir <- withr::local_tempdir()
    .write_source_images(dir, "x", img = NULL)
    expect_length(list.files(dir), 0)
})


# --- Building the archive -----------------------------------------------------

test_that(".write_source_zip puts images beside the data", {
    skip_if_not(isTRUE(capabilities("png")))

    png_raw <- draw_to_png(function() plot(1:5), 200, 150)
    f <- withr::local_tempfile(fileext = ".zip")

    # plot = NULL keeps saveWidget() (and therefore pandoc) out of it.
    .write_source_zip(
        f,
        list(A = list(
            plot = NULL, plot_data = data.frame(a = 1:3),
            stats = data.frame(p = 0.01), inputs = data.frame(names = "x", values = "1"),
            svg_key = "p1"
        )),
        images = list(p1 = list(svg = "<svg viewBox='0 0 10 10'><g/></svg>", png = png_raw))
    )

    expect_setequal(
        zip::zip_list(f)$filename,
        c("A_plot.svg", "A_plot.png", "A_plot_data.csv", "A_stats_data.csv", "A_ui_inputs.csv")
    )
})


test_that(".write_source_zip keys a lone summary on Data", {
    f <- withr::local_tempfile(fileext = ".zip")

    # A single summary arrives unwrapped and is named "Data" by the writer, so
    # that is the key the browser is told to send for it.
    .write_source_zip(
        f,
        list(plot = NULL, plot_data = data.frame(a = 1), stats = NULL, inputs = NULL),
        images = list(Data = list(svg = "<svg><g/></svg>"))
    )

    expect_setequal(zip::zip_list(f)$filename, c("Data_plot.svg", "Data_plot_data.csv"))
})


test_that(".write_source_zip still refuses to write an empty archive", {
    expect_error(
        .write_source_zip(withr::local_tempfile(fileext = ".zip"), list()),
        "No files were created to zip"
    )
})


test_that(".write_source_zip sanitises a summary name into the filenames", {
    f <- withr::local_tempfile(fileext = ".zip")
    .write_source_zip(
        f,
        stats::setNames(
            list(list(plot = NULL, plot_data = data.frame(a = 1))),
            "Violin #1 (mtcars)"
        ),
        images = NULL
    )
    # Every run of disallowed characters collapses to one underscore, so the
    # trailing ")" leaves a separator of its own before the file's own suffix.
    expect_setequal(zip::zip_list(f)$filename, "Violin_1_mtcars__plot_data.csv")
})


# --- Staging ------------------------------------------------------------------

test_that(".stage_source_images parks a payload for the download to collect", {
    store <- new.env(parent = emptyenv())

    srv <- function(id) {
        moduleServer(id, function(input, output, session) {
            .stage_source_images(store, session, "download.source")
            invisible(NULL)
        })
    }

    shiny::testServer(srv, {
        session$setInputs(`download.source_images` = list(
            nonce = "n1",
            images = list(list(key = "Data", svg = "<svg/>", width = 400, height = 300))
        ))
        expect_equal(store$images$Data$svg, "<svg/>")
        expect_equal(store$images$Data$width, 400)

        # Without a nonce there is nobody to answer, so it is ignored rather
        # than clearing what is already staged.
        session$setInputs(`download.source_images` = list(images = list()))
        expect_equal(store$images$Data$svg, "<svg/>")

        # A fresh capture replaces the last one.
        session$setInputs(`download.source_images` = list(
            nonce = "n2",
            images = list(list(key = "Data", svg = "<svg id='2'/>"))
        ))
        expect_equal(store$images$Data$svg, "<svg id='2'/>")
    })
})


test_that("create_source_download_handler stages images and consumes them once", {
    skip_if_not(isTRUE(capabilities("png")))

    srv <- function(id) {
        moduleServer(id, function(input, output, session) {
            handler <- create_source_download_handler(
                data_list = reactive(list(
                    plot = NULL, plot_data = data.frame(a = 1:3),
                    stats = NULL, inputs = NULL
                )),
                filename_base = "test_source"
            )
            output$download.source <- handler
            handler
        })
    }

    shiny::testServer(srv, {
        # A download handler only runs its content() when the browser fetches
        # the URL, which testServer cannot do, so reach the closure directly.
        content <- environment(environment(session$returned)$renderFunc)$content
        skip_if(!is.function(content), "shiny's downloadHandler internals changed")

        session$setInputs(`download.source_images` = list(
            nonce = "n1",
            images = list(list(key = "Data", svg = "<svg><g/></svg>", width = 400, height = 300))
        ))

        f <- withr::local_tempfile(fileext = ".zip")
        content(f)
        expect_true("Data_plot.svg" %in% zip::zip_list(f)$filename)

        # A second download with no fresh capture must not re-ship the first
        # one's picture -- by then the plot may be something else entirely.
        f2 <- withr::local_tempfile(fileext = ".zip")
        content(f2)
        expect_false("Data_plot.svg" %in% zip::zip_list(f2)$filename)
        expect_true("Data_plot_data.csv" %in% zip::zip_list(f2)$filename)
    })
})


# --- Screenshot vs. redraw ----------------------------------------------------
# For a module whose output is not a plotly graph, the browser can still scrape
# the base64 PNG Shiny inlines for a renderPlot output. That is a screenshot at
# the on-screen device's resolution, not a capture of a live graph, so a module
# that can redraw itself should win over it -- which it did not, leaving
# raster_png unreachable for the ComplexHeatmap module.

test_that(".decode_source_images carries the screenshot flag", {
    flagged <- .decode_source_images(
        list(list(key = "a", png = jsonlite::base64_enc(as.raw(1:8)), png_fallback = TRUE))
    )
    expect_true(flagged$a$png_fallback)

    # A plotly capture is not flagged, and neither is a payload from an older
    # client that predates the flag.
    plain <- .decode_source_images(
        list(list(key = "a", png = jsonlite::base64_enc(as.raw(1:8))))
    )
    expect_false(plain$a$png_fallback)
})


test_that(".write_source_images prefers a redraw over a screenshot", {
    skip_if_not(isTRUE(capabilities("png")))

    dir <- withr::local_tempdir()
    shot <- draw_to_png(function() plot(1:5), 200, 150)
    redrawn <- draw_to_png(function() plot(1:5), 400, 300)
    called <- FALSE

    .write_source_images(
        dir, "x",
        img = list(png = shot, png_fallback = TRUE),
        raster_png = function(width, height, res) {
            called <<- TRUE
            redrawn
        }
    )

    expect_true(called)
    expect_equal(readBin(file.path(dir, "x_plot.png"), "raw", n = 1e7), redrawn)
})


test_that(".write_source_images keeps an unflagged capture over a redraw", {
    skip_if_not(isTRUE(capabilities("png")))

    dir <- withr::local_tempdir()
    captured <- draw_to_png(function() plot(1:5), 200, 150)
    called <- FALSE

    .write_source_images(
        dir, "x",
        img = list(png = captured),
        raster_png = function(width, height, res) {
            called <<- TRUE
            as.raw(1:8)
        }
    )

    # A Plotly.toImage() result is the plot as the user actually has it, so it
    # still beats anything the server can rebuild.
    expect_false(called)
    expect_equal(readBin(file.path(dir, "x_plot.png"), "raw", n = 1e7), captured)
})


test_that(".write_source_images falls back to a screenshot when it must", {
    skip_if_not(isTRUE(capabilities("png")))

    shot <- draw_to_png(function() plot(1:5), 200, 150)

    # No renderer at all: a screenshot beats no picture.
    dir <- withr::local_tempdir()
    .write_source_images(dir, "x", img = list(png = shot, png_fallback = TRUE))
    expect_equal(readBin(file.path(dir, "x_plot.png"), "raw", n = 1e7), shot)

    # A renderer that fails must not cost the user the screenshot either.
    dir2 <- withr::local_tempdir()
    expect_warning(
        .write_source_images(
            dir2, "x",
            img = list(png = shot, png_fallback = TRUE),
            raster_png = function(width, height, res) stop("boom")
        ),
        "boom"
    )
    expect_equal(readBin(file.path(dir2, "x_plot.png"), "raw", n = 1e7), shot)

    # So must one that simply declines to draw.
    dir3 <- withr::local_tempdir()
    .write_source_images(
        dir3, "x",
        img = list(png = shot, png_fallback = TRUE),
        raster_png = function(width, height, res) NULL
    )
    expect_equal(readBin(file.path(dir3, "x_plot.png"), "raw", n = 1e7), shot)
})
