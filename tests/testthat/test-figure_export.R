test_that(".strip_svg_prolog drops everything before the root element", {
    doc <- paste(
        '<?xml version="1.0" encoding="UTF-8"?>',
        '<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "svg11.dtd">',
        "<!-- Created by cairo -->",
        '<svg width="10pt"><g/></svg>',
        sep = "\n"
    )
    expect_equal(.strip_svg_prolog(doc), '<svg width="10pt"><g/></svg>')

    # Already a bare fragment: unchanged.
    expect_equal(.strip_svg_prolog("<svg/>"), "<svg/>")

    expect_null(.strip_svg_prolog("no markup here"))
    expect_null(.strip_svg_prolog(""))
    expect_null(.strip_svg_prolog(NA_character_))
    expect_null(.strip_svg_prolog(NULL))
})

test_that(".svg_set_px_size restates the outer size without touching the viewBox", {
    svg <- "<svg width='225.00pt' height='150.00pt' viewBox='0 0 225.00 150.00'><g/></svg>"
    out <- .svg_set_px_size(svg, 300, 200)

    expect_true(grepl('width="300"', out, fixed = TRUE))
    expect_true(grepl('height="200"', out, fixed = TRUE))
    # The viewBox is what does the scaling, so it must survive intact.
    expect_true(grepl("viewBox='0 0 225.00 150.00'", out, fixed = TRUE))
    expect_false(grepl("225.00pt", out, fixed = TRUE))
    # Only the root element is rewritten.
    expect_true(grepl("<g/></svg>", out, fixed = TRUE))

    # Without a viewBox, width/height *are* the user units, so rewriting them
    # would rescale the drawing rather than restate its size.
    plain <- "<svg width='100' height='50'><g/></svg>"
    expect_equal(.svg_set_px_size(plain, 300, 200), plain)

    # An attribute that is missing gets added.
    bare <- "<svg viewBox='0 0 10 10'><g/></svg>"
    out2 <- .svg_set_px_size(bare, 30, 20)
    expect_true(grepl('width="30"', out2, fixed = TRUE))
    expect_true(grepl('height="20"', out2, fixed = TRUE))
})

test_that(".svg_namespace_ids rewrites definitions and references together", {
    svg <- paste0(
        "<svg><defs><clipPath id='cp1'><rect/></clipPath>",
        "<g id=\"glyph-0\"/></defs>",
        "<g clip-path='url(#cp1)'><use xlink:href=\"#glyph-0\"/></g></svg>"
    )
    out <- .svg_namespace_ids(svg, "panel1")

    expect_true(grepl("id='panel1-cp1'", out, fixed = TRUE))
    expect_true(grepl("url(#panel1-cp1)", out, fixed = TRUE))
    expect_true(grepl('id="panel1-glyph-0"', out, fixed = TRUE))
    expect_true(grepl('xlink:href="#panel1-glyph-0"', out, fixed = TRUE))
    # Nothing left pointing at the un-prefixed name.
    expect_false(grepl("url(#cp1)", out, fixed = TRUE))

    # No prefix, no change.
    expect_equal(.svg_namespace_ids(svg, NULL), svg)
    expect_equal(.svg_namespace_ids(svg, ""), svg)

    # Prefixes are sanitised into something id-safe.
    expect_true(grepl(
        "id='mod-hm-cp1'",
        .svg_namespace_ids(svg, "mod hm"), fixed = TRUE
    ))
})

test_that("draw_to_svg renders a drawing at a pixel size with unique ids", {
    a <- draw_to_svg(function() plot(1:10), 300, 200, id_prefix = "panelA")
    b <- draw_to_svg(function() plot(1:10), 300, 200, id_prefix = "panelB")

    expect_true(startsWith(a, "<svg "))
    expect_true(grepl('width="300"', a, fixed = TRUE))
    expect_true(grepl('height="200"', a, fixed = TRUE))

    # Two panels drawing the same thing mint the same raw ids, which is exactly
    # the collision the prefixing exists to prevent.
    ids <- function(x) unlist(regmatches(x, gregexpr("id='[^']+", x)))
    expect_length(intersect(ids(a), ids(b)), 0L)

    # Every internal reference still resolves to a definition in its own panel.
    refs <- unlist(regmatches(a, gregexpr("url[(]#[^)]+[)]", a)))
    if (length(refs)) {
        targets <- sub("[)]$", "", sub("^url[(]#", "", refs))
        expect_true(all(targets %in% sub("^id='", "", ids(a))))
    }

    # A non-numeric size is refused outright rather than handed to a device.
    expect_null(draw_to_svg(function() plot(1:3), NA, 200))
    expect_null(draw_to_svg(function() plot(1:3), 300, "wide"))

    # A card dragged down to nothing opens a device but leaves no room to draw
    # in. The error surfaces to the caller (figureBuilderServer() turns it into
    # a warning and drops that panel) rather than being silently swallowed.
    expect_error(draw_to_svg(function() plot(1:3), 0, 0), "margins")
})

test_that("draw_to_svg leaves no device open when the drawing fails", {
    before <- length(grDevices::dev.list())
    expect_error(
        draw_to_svg(function() stop("boom"), 300, 200),
        "boom"
    )
    expect_equal(length(grDevices::dev.list()), before)
})

test_that("draw_to_svg draws on the canvas the panel was rendered at", {
    # A panel on the Figure Builder canvas is drawn by shiny::renderPlot(), which
    # works at res = 72 -- so its pixel box is that many points of canvas. The
    # exporter has to use the same figure, because ComplexHeatmap sizes legends,
    # row labels and titles in absolute points: on a smaller canvas they keep
    # their size and the heatmap body, the only flexible element, absorbs the
    # whole shortfall. Exporting at 96 squeezed a legend-heavy heatmap's cells
    # down to a fraction of a point.
    expect_equal(eval(formals(draw_to_svg)$res), 72)
    expect_equal(
        eval(formals(draw_to_svg)$res),
        eval(formals(shiny::renderPlot)$res)
    )

    svg <- draw_to_svg(function() plot(1:3), 480, 380)
    # At 72dpi one user unit is one pixel, so the viewBox matches the panel box.
    # `.` does not cross newlines, so match the attribute rather than the doc.
    vb <- as.numeric(strsplit(gsub("viewBox='|'", "",
        regmatches(svg, regexpr("viewBox='[^']+'", svg))), " ")[[1]])
    expect_equal(vb[3:4], c(480, 380))
    expect_true(grepl('width="480"', svg, fixed = TRUE))
    expect_true(grepl('height="380"', svg, fixed = TRUE))

    # A smaller canvas really does move the furniture relative to the drawing,
    # which is the failure this default exists to prevent.
    small <- draw_to_svg(function() plot(1:3), 480, 380, res = 96)
    expect_false(identical(small, svg))
})


test_that(".svg_standalone restates what a fragment leaves out", {
    # svglite is asked for a fragment, which is why this is needed at all.
    frag <- "<svg width='10' height='10' viewBox='0 0 10 10'><g/></svg>"
    out <- .svg_standalone(frag)

    expect_true(startsWith(out, "<?xml version="))
    expect_true(grepl('xmlns="http://www.w3.org/2000/svg"', out, fixed = TRUE))
    # Nothing references xlink, so declaring it would be noise.
    expect_false(grepl("xmlns:xlink", out, fixed = TRUE))
    expect_true(grepl("<g/></svg>", out, fixed = TRUE))

    # Declared only when the fragment actually uses it.
    used <- .svg_standalone("<svg><use xlink:href=\"#g0\"/></svg>")
    expect_true(grepl('xmlns:xlink="http://www.w3.org/1999/xlink"', used, fixed = TRUE))

    # A document that is already complete is left exactly as it is.
    whole <- paste0(
        "<?xml version=\"1.0\"?>\n",
        "<svg xmlns=\"http://www.w3.org/2000/svg\"><g/></svg>"
    )
    expect_equal(.svg_standalone(whole), whole)
    expect_equal(.svg_standalone(.svg_standalone(frag)), .svg_standalone(frag))

    # Nothing usable to work with: handed back untouched rather than mangled.
    expect_equal(.svg_standalone("no markup here"), "no markup here")
    expect_null(.svg_standalone(NULL))
    expect_equal(.svg_standalone(NA_character_), NA_character_)
})


test_that("draw_to_svg output is only openable as a file once made standalone", {
    # The regression this guards: writing draw_to_svg()'s result straight to a
    # .svg gives a file no browser or vector editor will open, because svglite
    # is asked for a fragment and fragments carry no namespace.
    svg <- draw_to_svg(function() plot(1:10), 300, 200)
    skip_if(is.null(svg))

    if (requireNamespace("svglite", quietly = TRUE)) {
        expect_false(grepl("xmlns=", svg, fixed = TRUE))
    }

    standalone <- .svg_standalone(svg)
    expect_true(startsWith(standalone, "<?xml version="))
    expect_true(grepl("xmlns=", standalone, fixed = TRUE))
})


test_that("draw_to_png renders a drawing to PNG bytes", {
    skip_if_not(isTRUE(capabilities("png")))

    out <- draw_to_png(function() plot(1:10), 300, 200)
    expect_type(out, "raw")
    expect_gt(length(out), 0)
    # The PNG signature, so this is a real image rather than an empty file.
    expect_equal(out[1:4], as.raw(c(0x89, 0x50, 0x4e, 0x47)))

    # scale raises the pixel count without changing the layout size, so a
    # denser render is a bigger file.
    big <- draw_to_png(function() plot(1:10), 300, 200, scale = 3)
    expect_gt(length(big), length(out))
})


test_that("draw_to_png refuses a size it cannot draw at", {
    skip_if_not(isTRUE(capabilities("png")))

    expect_null(draw_to_png(function() plot(1:3), NA, 200))
    expect_null(draw_to_png(function() plot(1:3), 300, "wide"))
    expect_null(draw_to_png(function() plot(1:3), 300, 200, res = 0))
    expect_null(draw_to_png(function() plot(1:3), 300, 200, scale = -1))

    # A card dragged to nothing clamps to 1px rather than erroring on the
    # device, and then fails the way draw_to_svg() does -- on the drawing, which
    # the caller is better placed to handle. The png device grumbles about the
    # clamped size on the way past; that is the device, not a problem here.
    suppressWarnings(
        expect_error(draw_to_png(function() plot(1:3), 0, 0), "margins")
    )
})


test_that("draw_to_png leaves no device open when the drawing fails", {
    skip_if_not(isTRUE(capabilities("png")))

    before <- length(grDevices::dev.list())
    expect_error(draw_to_png(function() stop("boom"), 300, 200), "boom")
    expect_equal(length(grDevices::dev.list()), before)
})


test_that("draw_to_svg returns NULL when no SVG device is reachable", {
    # svglite is only a Suggest, and its fallback -- grDevices::svg() -- is the
    # cairo device. On a headless build with neither, opening the device used to
    # error out of an export that could otherwise have delivered everything
    # else. draw_to_png() has always answered this case with NULL.
    testthat::local_mocked_bindings(
        requireNamespace = function(...) FALSE,
        capabilities = function(...) FALSE,
        .package = "base"
    )

    expect_null(draw_to_svg(function() plot(1:3), 300, 200))
})


test_that("draw_to_svg still uses cairo when svglite is unavailable", {
    skip_if_not(isTRUE(capabilities("cairo")))

    testthat::local_mocked_bindings(
        requireNamespace = function(...) FALSE,
        .package = "base"
    )

    svg <- draw_to_svg(function() plot(1:3), 300, 200)
    expect_type(svg, "character")
    # The cairo device converts text to glyph paths, so this is still a valid
    # drawing -- just not one whose labels stay editable.
    expect_true(startsWith(svg, "<svg"))
})
