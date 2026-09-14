# Tests for organize_inputs() grid-flow behavior.

test_that("organize_inputs drops NULL inputs so they do not create empty cells", {
    ui <- organize_inputs(tagList(div("a"), NULL, div("b")), columns = 2)
    n_cells <- length(gregexpr("vizmodules-input-cell", as.character(ui))[[1]])
    expect_equal(n_cells, 2)
})

test_that("organize_inputs flows a uniform input block into one cell per visible input", {
    ns <- NS("x")
    block <- uniform_axes_inputs_ui(ns)
    expected <- sum(!vapply(block, is.null, logical(1)))

    ui <- organize_inputs(block, columns = 2)
    n_cells <- length(gregexpr("vizmodules-input-cell", as.character(ui))[[1]])
    expect_equal(n_cells, expected)
})

test_that("organize_inputs keeps a tooltip-wrapped input as a single cell", {
    ui <- organize_inputs(
        tagList(shinyBS::tipify(numericInput("a", "A", 1), "tip")),
        columns = 2
    )
    n_cells <- length(gregexpr("vizmodules-input-cell", as.character(ui))[[1]])
    expect_equal(n_cells, 1)
})


test_that("module_tack_ui marks the download button for the image capture", {
    ui <- module_tack_ui(NS("m"))
    html <- as.character(ui)

    expect_true(grepl("viz-source-download", html, fixed = TRUE))
    # The prefix is how the script works out which plot on the page is this
    # module's; every output of the module is named with it.
    expect_true(grepl('data-viz-source-ns="m-"', html, fixed = TRUE))
    expect_true(grepl('id="m-download.source"', html, fixed = TRUE))

    deps <- vapply(htmltools::findDependencies(ui), function(d) d$name, character(1))
    expect_true("viz-source-export" %in% deps)
})


test_that("module_tack_ui carries a nested module's full namespace", {
    # A module inside the Figure Builder is namespaced twice over, and the
    # script matches on the whole prefix.
    html <- as.character(module_tack_ui(NS(NS("fb")("panel1"))))
    expect_true(grepl('data-viz-source-ns="fb-panel1-"', html, fixed = TRUE))
})


# --- CSS containment ----------------------------------------------------------
# Every stylesheet this package ships is loaded into the *host* document, so a
# selector that is not anchored on a class the package owns restyles the host
# app. That is not hypothetical: multiColorPicker.css styled `.selectize-dropdown`
# generically, which broke every stock selectInput() dropdown on the page, and
# the Figure Builder styled `.well`, which shiny::sidebarPanel() renders.

# Prefixes this package owns. Anything else at the head of a selector reaches
# markup the package did not create.
viz_owned_prefix <- paste0(
    "^(",
    "\\.multi-color-picker|\\.mc-|",
    "\\.multi-dynamic-input|\\.mdi-|",
    "\\.vizmodules-|\\.viz-|",
    "\\.pb-|\\.a4-|",
    "\\.data-filter|\\.df-",
    ")"
)

# Split a stylesheet into individual selectors, dropping at-rules and comments.
viz_selectors <- function(css) {
    css <- paste(css, collapse = "\n")
    css <- gsub("/\\*.*?\\*/", "", css)
    groups <- regmatches(css, gregexpr("[^{}]+(?=\\{)", css, perl = TRUE))[[1]]
    sels <- trimws(unlist(strsplit(groups, ",", fixed = TRUE)))
    sels <- gsub("\\s+", " ", sels)
    sels <- sels[nzchar(sels)]
    # An at-rule's prelude (@media ...) is not a selector.
    sels[!startsWith(sels, "@")]
}


test_that("bundled stylesheets only style markup this package owns", {
    files <- list.files(
        system.file("src", package = "VizModules"),
        pattern = "\\.css$", full.names = TRUE
    )
    skip_if(length(files) == 0, "package not installed with inst/src")

    offenders <- character()
    for (f in files) {
        sels <- viz_selectors(readLines(f, warn = FALSE))
        bad <- sels[!grepl(viz_owned_prefix, sels)]
        if (length(bad)) {
            offenders <- c(offenders, paste0(basename(f), ": ", bad))
        }
    }
    expect_equal(offenders, character())
})


test_that("inline stylesheets only style markup this package owns", {
    for (fn in c(".data_filter_css", ".figure_builder_css")) {
        sels <- viz_selectors(get(fn)())
        bad <- sels[!grepl(viz_owned_prefix, sels)]
        expect_equal(bad, character(), info = fn)
    }
})


test_that("the containment check can actually fail", {
    # A tripwire that cannot fire pins nothing.
    leaky <- ".selectize-dropdown .option { padding: 0 }\n.mc-thing { color: red }"
    sels <- viz_selectors(leaky)
    expect_length(sels, 2)
    expect_length(sels[!grepl(viz_owned_prefix, sels)], 1)
})


test_that("the colour picker tags its own dropdown", {
    js <- readLines(
        system.file("src/multiColorPicker.js", package = "VizModules"),
        warn = FALSE
    )
    js <- paste(js, collapse = "\n")

    # The dropdown is parented to <body>, so this marker is the only thing the
    # stylesheet can scope to. Selectize replaces its default dropdownClass
    # wholesale, so its own class has to be restated alongside ours.
    expect_true(grepl("dropdownClass", js, fixed = TRUE))
    expect_true(grepl("mc-palette-dropdown", js, fixed = TRUE))
    expect_true(grepl("selectize-dropdown mc-palette-dropdown", js, fixed = TRUE))

    # ...and the stylesheet has to actually use it.
    css <- paste(readLines(
        system.file("src/multiColorPicker.css", package = "VizModules"),
        warn = FALSE
    ), collapse = "\n")
    expect_true(grepl(".mc-palette-dropdown", css, fixed = TRUE))
})


# --- The control grid ---------------------------------------------------------

test_that("organize_inputs keeps its layout out of inline styles", {
    ui <- organize_inputs(
        tagList(numericInput("a", "A", 1), numericInput("b", "B", 2)),
        columns = 2
    )
    html <- as.character(ui)

    # The negative-margin row idiom assumed a parent with matching padding; in a
    # sidebar with less than that, the grid overhung and the sidebar scrolled.
    expect_false(grepl("margin-left: -15px", html, fixed = TRUE))
    expect_false(grepl("margin-right: -15px", html, fixed = TRUE))
    # Per-cell padding went with it -- the gap is on the container now.
    expect_false(grepl("padding-left: 15px", html, fixed = TRUE))

    # Only the column count stays inline, because it varies per call. Inline
    # styles cannot be overridden without !important, so everything else lives
    # in the stylesheet.
    expect_true(grepl("--viz-input-columns: 2;", html, fixed = TRUE))

    deps <- vapply(htmltools::findDependencies(ui), function(d) d$name, character(1))
    expect_true("viz-modules" %in% deps)
})


test_that("organize_inputs marks its tabset so the tab strip can wrap", {
    ui <- organize_inputs(
        list(
            Data = tagList(numericInput("a", "A", 1)),
            Axes = tagList(numericInput("b", "B", 2))
        ),
        columns = 1
    )
    expect_true(grepl("vizmodules-input-tabs", as.character(ui), fixed = TRUE))
})


test_that("the Figure Builder scopes its layout styles", {
    html <- as.character(figureBuilderUI("fb"))
    # `.pb-app .well` rules are inert without this marker on the layout.
    expect_true(grepl("pb-app", html, fixed = TRUE))
})
