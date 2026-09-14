## Helper to create mock plotly objects for testing
make_plotly <- function(data = list(), layout = list()) {
    fig <- list(x = list(data = data, layout = layout))
    class(fig) <- "plotly"
    fig
}

# ─── .hide_jitter_from_legend ─────────────────────────────────────────────────

test_that(".hide_jitter_from_legend hides scatter marker traces", {
    fig <- make_plotly(data = list(
        list(type = "box", showlegend = TRUE, name = "Group A"),
        list(type = "box", showlegend = TRUE, name = "Group B"),
        list(type = "scatter", mode = "markers", showlegend = TRUE, name = "Jitter A"),
        list(type = "scatter", mode = "markers", showlegend = TRUE, name = "Jitter B"),
        list(type = "scatter", mode = "lines", showlegend = TRUE, name = "Line")
    ))

    result <- VizModules:::.hide_jitter_from_legend(fig)

    expect_s3_class(result, "plotly")
    expect_true(result$x$data[[1]]$showlegend)
    expect_true(result$x$data[[2]]$showlegend)
    expect_false(result$x$data[[3]]$showlegend)
    expect_false(result$x$data[[4]]$showlegend)
    expect_true(result$x$data[[5]]$showlegend)
})

test_that(".hide_jitter_from_legend preserves trace count", {
    fig <- make_plotly(data = list(
        list(type = "box", showlegend = TRUE),
        list(type = "scatter", mode = "markers", showlegend = TRUE),
        list(type = "scatter", mode = "markers", showlegend = TRUE)
    ))

    result <- VizModules:::.hide_jitter_from_legend(fig)
    expect_equal(length(result$x$data), 3)
})

test_that(".hide_jitter_from_legend works with real BoxPlot", {
    p <- plotthis::BoxPlot(
        data = data.frame(
            x = rep(c("A", "B", "C"), 10),
            y = rnorm(30),
            group = rep(c("G1", "G2"), 15)
        ),
        x = "x", y = "y", group_by = "group", add_point = TRUE
    )
    fig <- plotly::ggplotly(p)
    result <- VizModules:::.hide_jitter_from_legend(fig)

    expect_s3_class(result, "plotly")
    scatter_markers <- vapply(result$x$data, function(trace) {
        !is.null(trace$type) && trace$type == "scatter" &&
            !is.null(trace$mode) && trace$mode == "markers"
    }, logical(1))

    for (i in which(scatter_markers)) {
        expect_false(result$x$data[[i]]$showlegend,
            info = sprintf("Scatter marker trace %d should have showlegend=FALSE", i)
        )
    }
})

test_that(".hide_jitter_from_legend handles empty data", {
    fig <- make_plotly(data = list())
    result <- VizModules:::.hide_jitter_from_legend(fig)
    expect_s3_class(result, "plotly")
    expect_equal(length(result$x$data), 0)
})

test_that(".hide_jitter_from_legend handles traces without type", {
    fig <- make_plotly(data = list(
        list(showlegend = TRUE, name = "No Type"),
        list(type = "scatter", mode = "markers", showlegend = TRUE)
    ))

    result <- VizModules:::.hide_jitter_from_legend(fig)
    expect_true(result$x$data[[1]]$showlegend)
    expect_false(result$x$data[[2]]$showlegend)
})

test_that(".hide_jitter_from_legend rejects non-plotly objects", {
    expect_error(
        VizModules:::.hide_jitter_from_legend(list(x = list(data = list()))),
        "plotly"
    )
})

test_that(".hide_jitter_from_legend with mtcars dataset", {
    p <- plotthis::BoxPlot(
        data = data.frame(
            x = factor(mtcars$cyl), y = mtcars$mpg, group = factor(mtcars$vs)
        ),
        x = "x", y = "y", group_by = "group", add_point = TRUE
    )
    fig <- plotly::ggplotly(p)
    result <- VizModules:::.hide_jitter_from_legend(fig)

    box_traces <- sum(vapply(result$x$data, function(t) {
        !is.null(t$type) && t$type == "box"
    }, logical(1)))
    scatter_markers <- sum(vapply(result$x$data, function(t) {
        !is.null(t$type) && t$type == "scatter" && !is.null(t$mode) && t$mode == "markers"
    }, logical(1)))

    expect_gt(box_traces, 0)
    expect_gt(scatter_markers, 0)

    for (trace in result$x$data) {
        if (!is.null(trace$type) && trace$type == "scatter" &&
            !is.null(trace$mode) && trace$mode == "markers") {
            expect_false(trace$showlegend)
        }
        if (!is.null(trace$type) && trace$type == "box") {
            expect_true(trace$showlegend)
        }
    }
})

# ─── .align_box_positions ───────────────────────────────────────

# "blood" carries all three groups, the other two only A and B. This uneven
# coverage is what #356 is about: ggplot dodges two slots at lung/liver while
# plotly.js reserves three everywhere.
.uneven_group_data <- function(seed = 42) {
    withr::with_seed(seed, {
        df <- rbind(
            data.frame(tissue = "blood", grp = rep(c("A", "B", "C"), each = 8)),
            data.frame(tissue = "lung", grp = rep(c("A", "B"), each = 8)),
            data.frame(tissue = "liver", grp = rep(c("A", "B"), each = 8))
        )
        df$val <- rnorm(nrow(df))
        df$tissue <- factor(df$tissue, levels = c("blood", "lung", "liver"))
        df$grp <- factor(df$grp, levels = c("A", "B", "C"))
        df
    })
}

# The x each box trace sits at, one entry per distinct position, named by trace.
.box_trace_positions <- function(fig) {
    out <- list()
    for (trace in fig$x$data) {
        if (is.null(trace$type) || trace$type != "box") next
        out[[trace$name]] <- sort(unique(as.numeric(trace$x)))
    }
    out
}

test_that(".align_box_positions dodges only the groups present at each x", {
    fig <- plotly::ggplotly(plotthis::BoxPlot(
        .uneven_group_data(),
        x = "tissue", y = "val", group_by = "grp"
    ))

    # Before: every box trace still carries the raw category index.
    expect_equal(.box_trace_positions(fig)$A, c(1, 2, 3))

    result <- VizModules:::.align_box_positions(fig, dodge.width = 1, box.width = 0.8)
    pos <- .box_trace_positions(result)

    # blood splits three ways, lung and liver two.
    expect_equal(pos$A, c(1 - 1 / 3, 2 - 0.25, 3 - 0.25), tolerance = 1e-8)
    expect_equal(pos$B, c(1, 2 + 0.25, 3 + 0.25), tolerance = 1e-8)
    expect_equal(pos$C, 1 + 1 / 3, tolerance = 1e-8)
    expect_equal(result$x$layout$boxmode, "overlay")
})

test_that(".align_box_positions puts boxes over their jitter points (#356)", {
    fig <- plotly::ggplotly(plotthis::BoxPlot(
        .uneven_group_data(),
        x = "tissue", y = "val", group_by = "grp", add_point = TRUE
    ))
    result <- VizModules:::.align_box_positions(
        fig,
        dodge.width = VizModules:::.PLOTTHIS_DODGE_WIDTH, box.width = 0.8
    )

    # Jitter traces were never moved, so their clusters are ggplot's own
    # positions. Every box has to sit at the centre of the matching cluster.
    for (trace in result$x$data) {
        if (is.null(trace$type) || trace$type != "scatter") next
        if (is.null(trace$mode) || trace$mode != "markers") next

        boxes <- .box_trace_positions(result)[[trace$name]]
        expect_false(is.null(boxes))
        cluster <- vapply(as.numeric(trace$x), function(v) boxes[which.min(abs(boxes - v))], numeric(1))
        centres <- vapply(split(as.numeric(trace$x), cluster), mean, numeric(1))

        expect_equal(as.numeric(centres), as.numeric(names(centres)),
            tolerance = 0.05,
            info = sprintf("jitter cluster centres for group %s", trace$name)
        )
    }
})

test_that(".align_box_positions scales offsets with dodge.width and sets width", {
    fig <- plotly::ggplotly(plotthis::BoxPlot(
        .uneven_group_data(),
        x = "tissue", y = "val", group_by = "grp"
    ))

    wide <- VizModules:::.align_box_positions(fig, dodge.width = 1, box.width = 0.8)
    narrow <- VizModules:::.align_box_positions(fig, dodge.width = 0.5, box.width = 0.8)

    # Halving the dodge halves every offset from the category centre.
    expect_equal(
        .box_trace_positions(narrow)$A - c(1, 2, 3),
        (.box_trace_positions(wide)$A - c(1, 2, 3)) / 2,
        tolerance = 1e-8
    )

    # One width for the whole figure, taken from the most crowded x position.
    widths <- vapply(
        Filter(function(tr) identical(tr$type, "box"), wide$x$data),
        function(tr) tr$width, numeric(1)
    )
    expect_equal(unname(widths), rep(1 / 3 * 0.8, 3), tolerance = 1e-8)
})

test_that(".align_box_positions leaves boxes centred when there is no grouping", {
    df <- .uneven_group_data()
    fig <- plotly::ggplotly(plotthis::BoxPlot(df, x = "tissue", y = "val"))
    result <- VizModules:::.align_box_positions(fig, dodge.width = 1, box.width = 0.8)

    # Ungrouped, plotthis emits one box trace per x category, so every position
    # has a single occupant and nothing should be dodged off the tick.
    positions <- sort(unlist(lapply(result$x$data, function(trace) {
        if (is.null(trace$type) || trace$type != "box") NULL else unique(as.numeric(trace$x))
    })))
    expect_equal(positions, c(1, 2, 3), tolerance = 1e-8)
})

test_that(".align_box_positions dodges each facet panel and x position on its own", {
    df <- .uneven_group_data()
    # Group C lands in one panel only, and there only at "blood", so that panel
    # mixes a three-way position with two-way ones.
    df$panel <- ifelse(df$grp == "C", "p1", rep(c("p1", "p2"), length.out = nrow(df)))
    fig <- plotly::ggplotly(plotthis::BoxPlot(
        df,
        x = "tissue", y = "val", group_by = "grp", facet_by = "panel"
    ))
    result <- VizModules:::.align_box_positions(fig, dodge.width = 1, box.width = 0.8)

    boxes <- Filter(function(tr) identical(tr$type, "box"), result$x$data)
    axes <- vapply(boxes, function(tr) tr$xaxis %||% "x", character(1))
    expect_gt(length(unique(axes)), 1)

    # Whatever the occupancy, the boxes at one position must land on the centres
    # of however many slots that position splits into.
    for (ax in unique(axes)) {
        xs <- unlist(lapply(boxes[axes == ax], function(tr) unique(as.numeric(tr$x))))
        for (p in unique(round(xs))) {
            here <- sort(xs[round(xs) == p] - p)
            n <- length(here)
            expect_equal(here, (seq_len(n) - 0.5) / n - 0.5,
                tolerance = 1e-8,
                info = sprintf("axis %s, position %s", ax, p)
            )
        }
    }

    # The panel holding group C has a three-way position; the other panel does not.
    occupancy <- vapply(unique(axes), function(ax) {
        xs <- unlist(lapply(boxes[axes == ax], function(tr) unique(as.numeric(tr$x))))
        max(table(round(xs)))
    }, numeric(1))
    expect_equal(sort(unname(occupancy)), c(2, 3))
})

test_that(".align_box_positions is a no-op without box traces", {
    fig <- make_plotly(data = list(
        list(type = "scatter", mode = "markers", x = c(1, 2, 3))
    ))
    result <- VizModules:::.align_box_positions(fig, dodge.width = 1)
    expect_equal(result$x$data[[1]]$x, c(1, 2, 3))
    expect_null(result$x$layout$boxmode)
})

test_that(".align_box_positions rejects non-plotly objects", {
    expect_error(VizModules:::.align_box_positions(list(), dodge.width = 1))
})

test_that(".box_num falls back when a numeric control is blank", {
    expect_equal(VizModules:::.box_num(0.4, 0.3), 0.4)
    expect_equal(VizModules:::.box_num(NA_real_, 0.3), 0.3)
    expect_equal(VizModules:::.box_num(NULL, 0.3), 0.3)
    expect_equal(VizModules:::.box_num(c(1, 2), 0.3), 0.3)
    expect_equal(VizModules:::.box_num("x", 0.3), 0.3)
})

# ─── parse_numeric_list ──────────────────────────────────────────────────────

test_that("parse_numeric_list parses comma-separated numbers", {
    expect_equal(VizModules::parse_numeric_list("1, 5, 8"), c(1, 5, 8))
    expect_equal(VizModules::parse_numeric_list("3.14"), 3.14)
    expect_equal(VizModules::parse_numeric_list("-1, 0, 2.5"), c(-1, 0, 2.5))
})

test_that("parse_numeric_list returns NULL for empty/invalid input", {
    expect_null(VizModules::parse_numeric_list(NULL))
    expect_null(VizModules::parse_numeric_list(""))
    expect_null(VizModules::parse_numeric_list("   "))
    expect_null(VizModules::parse_numeric_list("abc, def"))
})

test_that("parse_numeric_list drops non-numeric values", {
    expect_equal(VizModules::parse_numeric_list("1, abc, 3"), c(1, 3))
})

# ─── recycle_line_style ──────────────────────────────────────────────────────

test_that("recycle_line_style returns default when values is NULL or empty", {
    expect_equal(VizModules::recycle_line_style(NULL, 3, "red"), rep("red", 3))
    expect_equal(VizModules::recycle_line_style(character(0), 2, 1), rep(1, 2))
})

test_that("recycle_line_style returns values unchanged when length matches", {
    expect_equal(VizModules::recycle_line_style(c("a", "b", "c"), 3, "x"), c("a", "b", "c"))
})

test_that("recycle_line_style recycles first value when length mismatch", {
    expect_equal(VizModules::recycle_line_style(c("a", "b"), 4, "x"), rep("a", 4))
    expect_equal(VizModules::recycle_line_style(c(1, 2, 3), 2, 0), rep(1, 2))
})

# ─── linetype_to_dash ───────────────────────────────────────────────────────

test_that("linetype_to_dash maps all known linetypes", {
    expect_equal(VizModules::linetype_to_dash("solid"), "solid")
    expect_equal(VizModules::linetype_to_dash("dashed"), "dash")
    expect_equal(VizModules::linetype_to_dash("dotted"), "dot")
    expect_equal(VizModules::linetype_to_dash("dotdash"), "dashdot")
    expect_equal(VizModules::linetype_to_dash("longdash"), "longdash")
    expect_equal(VizModules::linetype_to_dash("twodash"), "longdashdot")
})

test_that("linetype_to_dash is case-insensitive and defaults to solid", {
    expect_equal(VizModules::linetype_to_dash("SOLID"), "solid")
    expect_equal(VizModules::linetype_to_dash("Dashed"), "dash")
    expect_equal(VizModules::linetype_to_dash("unknown"), "solid")
})

# ─── adjust_column_values ───────────────────────────────────────────────────

test_that("adjust_column_values applies log2 transformation", {
    df <- data.frame(x = c(1, 2, 4, 8))
    result <- VizModules::adjust_column_values(df, x.col = "x", x.adj.fun = "log2")
    expect_true("x.adj" %in% names(result))
    expect_equal(result$x.adj, c(0, 1, 2, 3))
})

test_that("adjust_column_values applies transformations to multiple axes", {
    df <- data.frame(x = c(1, 10, 100), y = c(2, 4, 8))
    result <- VizModules::adjust_column_values(df,
        x.col = "x", y.col = "y",
        x.adj.fun = "log10", y.adj.fun = "sqrt"
    )
    expect_equal(result$x.adj, c(0, 1, 2))
    expect_equal(result$y.adj, sqrt(c(2, 4, 8)))
})

test_that("adjust_column_values returns unchanged df for NULL/empty fun", {
    df <- data.frame(x = 1:3)
    expect_identical(VizModules::adjust_column_values(df, x.col = "x", x.adj.fun = NULL), df)
    expect_identical(VizModules::adjust_column_values(df, x.col = "x", x.adj.fun = ""), df)
})

test_that("adjust_column_values ignores non-numeric columns", {
    df <- data.frame(x = letters[1:3], stringsAsFactors = FALSE)
    result <- VizModules::adjust_column_values(df, x.col = "x", x.adj.fun = "log2")
    expect_false("x.adj" %in% names(result))
})

test_that("adjust_column_values handles invalid expression gracefully", {
    df <- data.frame(x = 1:3)
    result <- VizModules::adjust_column_values(df, x.col = "x", x.adj.fun = "{{invalid")
    expect_identical(result, df)
})

# ─── add_plot_config ────────────────────────────────────────────────────────

test_that("add_plot_config returns default config without facet", {
    config <- VizModules::add_plot_config()
    # Axis titles are rendered as draggable annotations, so native axis-title
    # text editing is disabled even in the non-faceted configuration.
    expect_false(config$edits$axisTitleText)
    expect_true(config$edits$titleText)
    expect_false(config$displaylogo)
    expect_equal(config$toImageButtonOptions$format, "png")
    expect_true(length(config$modeBarButtonsToAdd) > 0)
})

test_that("add_plot_config with facet.by disables axisTitleText editing", {
    config <- VizModules::add_plot_config(facet.by = "group")
    expect_false(config$edits$axisTitleText)
    expect_true(config$edits$titleText)
})

test_that("add_plot_config respects download format and filename", {
    config <- VizModules::add_plot_config(download.format = "svg", filename = "my_plot")
    expect_equal(config$toImageButtonOptions$format, "svg")
    expect_equal(config$toImageButtonOptions$filename, "my_plot")
})

test_that("add_plot_config excludes modebar buttons when requested", {
    config <- VizModules::add_plot_config(include.modebar.buttons = FALSE)
    expect_null(config$modeBarButtonsToAdd)
})

# ─── apply_subplot_axis_styling ─────────────────────────────────────────────

test_that("apply_subplot_axis_styling returns NULL/empty fig unchanged", {
    expect_null(VizModules::apply_subplot_axis_styling(NULL, list(), list()))

    fig_no_x <- list(y = 1)
    expect_identical(VizModules::apply_subplot_axis_styling(fig_no_x, list(), list()), fig_no_x)
})

test_that("apply_subplot_axis_styling applies style to single axes", {
    fig <- make_plotly(layout = list(
        xaxis = list(title = "X"),
        yaxis = list(title = "Y")
    ))

    result <- VizModules::apply_subplot_axis_styling(
        fig,
        xaxis_style = list(showgrid = FALSE),
        yaxis_style = list(showgrid = TRUE)
    )

    # plotly::layout() stores updates in layoutAttrs
    layout_update <- result$x$layoutAttrs[[1]]
    expect_false(layout_update$xaxis$showgrid)
    expect_true(layout_update$yaxis$showgrid)
    # Existing properties preserved
    expect_equal(layout_update$xaxis$title, "X")
})

test_that("apply_subplot_axis_styling applies style to multiple subplot axes", {
    fig <- make_plotly(layout = list(
        xaxis = list(title = "X1"),
        xaxis2 = list(title = "X2"),
        yaxis = list(title = "Y1"),
        yaxis2 = list(title = "Y2")
    ))

    result <- VizModules::apply_subplot_axis_styling(
        fig,
        xaxis_style = list(linecolor = "red"),
        yaxis_style = list(linecolor = "blue")
    )

    layout_update <- result$x$layoutAttrs[[1]]
    expect_equal(layout_update$xaxis$linecolor, "red")
    expect_equal(layout_update$xaxis2$linecolor, "red")
    expect_equal(layout_update$yaxis$linecolor, "blue")
    expect_equal(layout_update$yaxis2$linecolor, "blue")
})

test_that("apply_subplot_axis_styling handles empty layout names", {
    fig <- make_plotly(layout = list())
    result <- VizModules::apply_subplot_axis_styling(fig, list(a = 1), list(b = 2))
    expect_s3_class(result, "plotly")
})

# ─── axis_titles_as_annotations ─────────────────────────────────────────────

test_that("axis_titles_as_annotations converts single-panel titles to annotations", {
    fig <- plotly::plot_ly(x = 1:3, y = 1:3, type = "scatter", mode = "lines") |>
        plotly::layout(xaxis = list(title = "Weight"), yaxis = list(title = "MPG"))

    result <- VizModules::axis_titles_as_annotations(fig)
    built <- plotly::plotly_build(result)

    ann_text <- vapply(built$x$layout$annotations, function(a) a$text, character(1))
    expect_true("Weight" %in% ann_text)
    expect_true("MPG" %in% ann_text)

    # Native axis titles cleared so they do not render twice
    expect_identical(built$x$layout$xaxis$title$text, "")
    expect_identical(built$x$layout$yaxis$title$text, "")

    # Annotations are paper-anchored and the y title is rotated
    x_ann <- Filter(function(a) identical(a$text, "Weight"), built$x$layout$annotations)[[1]]
    y_ann <- Filter(function(a) identical(a$text, "MPG"), built$x$layout$annotations)[[1]]
    expect_identical(x_ann$xref, "paper")
    expect_identical(x_ann$yref, "paper")
    expect_equal(y_ann$textangle, -90)
})

test_that("axis_titles_as_annotations preserves the native axis title font", {
    fig <- plotly::plot_ly(x = 1:3, y = 1:3, type = "scatter") |>
        plotly::layout(
            xaxis = list(title = list(text = "Cyl", font = list(size = 18, color = "red"))),
            yaxis = list(title = list(text = "MPG", font = list(size = 18)))
        )

    built <- plotly::plotly_build(VizModules::axis_titles_as_annotations(fig))
    x_ann <- Filter(function(a) identical(a$text, "Cyl"), built$x$layout$annotations)[[1]]
    expect_equal(x_ann$font$size, 18)
    expect_equal(x_ann$font$color, "red")
})

test_that("axis_titles_as_annotations preserves pre-existing annotations", {
    fig <- plotly::plot_ly(x = 1:3, y = 1:3, type = "scatter") |>
        plotly::layout(
            xaxis = list(title = "X"), yaxis = list(title = "Y"),
            annotations = list(list(x = 1, y = 1, text = "stat", showarrow = FALSE))
        )

    built <- plotly::plotly_build(VizModules::axis_titles_as_annotations(fig))
    ann_text <- vapply(built$x$layout$annotations, function(a) a$text, character(1))
    expect_true(all(c("stat", "X", "Y") %in% ann_text))
})

test_that("axis_titles_as_annotations leaves multi-panel figures unchanged", {
    # A subplot figure has secondary axes (xaxis2/yaxis2); its shared titles
    # are already draggable annotations, so the helper must not alter it.
    fig <- plotly::plotly_build(plotly::subplot(
        plotly::plot_ly(x = 1:3, y = 1:3, type = "scatter"),
        plotly::plot_ly(x = 1:3, y = 3:1, type = "scatter"),
        nrows = 1
    ))
    n_before <- length(fig$x$layout$annotations)
    result <- VizModules::axis_titles_as_annotations(fig)
    expect_equal(length(result$x$layout$annotations), n_before)
})

test_that("axis_titles_as_annotations is a no-op without axis titles", {
    fig <- plotly::plot_ly(x = 1:3, y = 1:3, type = "scatter")
    built <- plotly::plotly_build(VizModules::axis_titles_as_annotations(fig))
    expect_null(built$x$layout$annotations)
})

test_that("axis_titles_as_annotations returns NULL input unchanged", {
    expect_null(VizModules::axis_titles_as_annotations(NULL))
})


# ─── apply_legend_styling ───────────────────────────────────────────────────

test_that("apply_legend_styling sets legend title and text font sizes", {
    fig <- plotly::plot_ly(x = 1:3, y = 1:3, type = "scatter", mode = "lines")
    built <- plotly::plotly_build(
        VizModules::apply_legend_styling(fig, title.size = 20, text.size = 9)
    )
    expect_equal(built$x$layout$legend$font$size, 9)
    expect_equal(built$x$layout$legend$title$font$size, 20)
})

test_that("apply_legend_styling ignores NULL/NA sizes", {
    fig <- plotly::plot_ly(x = 1:3, y = 1:3, type = "scatter")
    # Both NULL -> figure returned unchanged (no legend args added).
    expect_identical(
        VizModules::apply_legend_styling(fig, title.size = NULL, text.size = NULL),
        fig
    )
    # Only text.size supplied -> title font untouched.
    built <- plotly::plotly_build(
        VizModules::apply_legend_styling(fig, text.size = 11)
    )
    expect_equal(built$x$layout$legend$font$size, 11)
    expect_null(built$x$layout$legend$title$font$size)
})

test_that("apply_legend_styling returns NULL input unchanged", {
    expect_null(VizModules::apply_legend_styling(NULL, title.size = 12))
})

test_that("apply_legend_styling preserves existing legend position", {
    fig <- plotly::plot_ly(x = 1:3, y = 1:3, type = "scatter") |>
        plotly::layout(legend = list(x = 0.8, y = 0.2, orientation = "h"))
    built <- plotly::plotly_build(
        VizModules::apply_legend_styling(fig, text.size = 14)
    )
    expect_equal(built$x$layout$legend$x, 0.8)
    expect_equal(built$x$layout$legend$y, 0.2)
    expect_equal(built$x$layout$legend$orientation, "h")
    expect_equal(built$x$layout$legend$font$size, 14)
})

test_that("apply_legend_styling styles continuous colorbar legends", {
    # Numeric colour mappings render a colorbar rather than a categorical
    # legend, so the title/tick fonts live on the trace's marker$colorbar.
    fig <- plotly::plot_ly(
        x = 1:3, y = 1:3, type = "scatter", mode = "markers",
        marker = list(
            color = c(1, 2, 3),
            colorbar = list(title = "value")
        )
    )
    built <- plotly::plotly_build(
        VizModules::apply_legend_styling(fig, title.size = 18, text.size = 8)
    )
    cb <- NULL
    for (tr in built$x$data) {
        if (!is.null(tr$marker$colorbar)) {
            cb <- tr$marker$colorbar
            break
        }
    }
    expect_false(is.null(cb))
    title_size <- if (is.list(cb$title)) cb$title$font$size else cb$titlefont$size
    expect_equal(title_size, 18)
    expect_equal(cb$tickfont$size, 8)
})


# ─── apply_facet_subplot_spacing ────────────────────────────────────────────

test_that("apply_facet_subplot_spacing supports separate horizontal/vertical spacing", {
    grid <- plotly::subplot(
        plotly::plot_ly(x = 1:3, y = 1:3, type = "scatter"),
        plotly::plot_ly(x = 1:3, y = 3:1, type = "scatter"),
        plotly::plot_ly(x = 1:3, y = 2:4, type = "scatter"),
        plotly::plot_ly(x = 1:3, y = 4:2, type = "scatter"),
        nrows = 2
    )

    result <- VizModules:::apply_facet_subplot_spacing(
        grid, spacing = c(0.2, 0.05), ncol = 2, nrow = 2
    )

    layout_names <- names(result$x$layout)
    x_axes <- layout_names[grepl("^xaxis[0-9]*$", layout_names)]
    y_axes <- layout_names[grepl("^yaxis[0-9]*$", layout_names)]

    x_starts <- sort(unique(round(vapply(
        x_axes, function(a) result$x$layout[[a]]$domain[1], numeric(1)
    ), 6)))
    x_ends <- sort(unique(round(vapply(
        x_axes, function(a) result$x$layout[[a]]$domain[2], numeric(1)
    ), 6)))
    # Horizontal gap between the two columns equals spacing[1] = 0.2.
    expect_equal(x_starts[2] - x_ends[1], 0.2, tolerance = 1e-6)

    y_starts <- sort(unique(round(vapply(
        y_axes, function(a) result$x$layout[[a]]$domain[1], numeric(1)
    ), 6)))
    y_ends <- sort(unique(round(vapply(
        y_axes, function(a) result$x$layout[[a]]$domain[2], numeric(1)
    ), 6)))
    # Vertical gap between the two rows equals spacing[2] = 0.05.
    expect_equal(y_starts[2] - y_ends[1], 0.05, tolerance = 1e-6)
})

test_that("apply_facet_subplot_spacing treats a single value as both directions", {
    grid <- plotly::subplot(
        plotly::plot_ly(x = 1:3, y = 1:3, type = "scatter"),
        plotly::plot_ly(x = 1:3, y = 3:1, type = "scatter"),
        plotly::plot_ly(x = 1:3, y = 2:4, type = "scatter"),
        plotly::plot_ly(x = 1:3, y = 4:2, type = "scatter"),
        nrows = 2
    )

    single <- VizModules:::apply_facet_subplot_spacing(grid, spacing = 0.1, ncol = 2, nrow = 2)
    vec <- VizModules:::apply_facet_subplot_spacing(grid, spacing = c(0.1, 0.1), ncol = 2, nrow = 2)

    get_domains <- function(fig, prefix) {
        nms <- names(fig$x$layout)
        axes <- nms[grepl(paste0("^", prefix, "[0-9]*$"), nms)]
        lapply(axes, function(a) fig$x$layout[[a]]$domain)
    }
    expect_equal(get_domains(single, "xaxis"), get_domains(vec, "xaxis"))
    expect_equal(get_domains(single, "yaxis"), get_domains(vec, "yaxis"))
})



test_that(".compute_linear_fit returns data frame for global fit", {
    df <- data.frame(x = 1:10, y = 2 * (1:10) + 1)
    result <- VizModules:::.compute_linear_fit(df, "x", "y")

    expect_s3_class(result, "data.frame")
    expect_true(all(c("x", "y") %in% names(result)))
    expect_equal(nrow(result), 100)
    # Check fit is close to y = 2x + 1
    expect_equal(result$y[1], 2 * result$x[1] + 1, tolerance = 0.01)
})

test_that(".compute_linear_fit returns named list for grouped fit", {
    df <- data.frame(
        x = rep(1:10, 2),
        y = c(1:10, 2 * (1:10)),
        g = rep(c("A", "B"), each = 10)
    )
    result <- VizModules:::.compute_linear_fit(df, "x", "y", group.col = "g")

    expect_type(result, "list")
    expect_true(all(c("A", "B") %in% names(result)))
    expect_s3_class(result$A, "data.frame")
    expect_s3_class(result$B, "data.frame")
})

test_that(".compute_linear_fit returns NULL with fewer than 2 points", {
    df <- data.frame(x = 1, y = 1)
    expect_null(VizModules:::.compute_linear_fit(df, "x", "y"))
})

test_that(".compute_linear_fit handles NAs in data", {
    df <- data.frame(x = c(1, NA, 3, 4, 5), y = c(2, 4, NA, 8, 10))
    result <- VizModules:::.compute_linear_fit(df, "x", "y")
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 100)
})

test_that(".compute_linear_fit treats empty group.col like NULL", {
    df <- data.frame(x = 1:5, y = 1:5)
    result_null <- VizModules:::.compute_linear_fit(df, "x", "y", group.col = NULL)
    result_empty <- VizModules:::.compute_linear_fit(df, "x", "y", group.col = "")
    expect_equal(nrow(result_null), nrow(result_empty))
})

# ─── .compute_loess_fit ──────────────────────────────────────────────────────

test_that(".compute_loess_fit returns data frame for global fit", {
    set.seed(42)
    df <- data.frame(x = 1:20, y = sin(1:20) + rnorm(20, sd = 0.1))
    result <- VizModules:::.compute_loess_fit(df, "x", "y")

    expect_s3_class(result, "data.frame")
    expect_true(all(c("x", "y") %in% names(result)))
    expect_equal(nrow(result), 100)
})

test_that(".compute_loess_fit returns NULL with fewer than 4 points", {
    df <- data.frame(x = 1:3, y = 1:3)
    expect_null(VizModules:::.compute_loess_fit(df, "x", "y"))
})

test_that(".compute_loess_fit returns named list for grouped fit", {
    set.seed(42)
    df <- data.frame(
        x = rep(1:20, 2),
        y = c(sin(1:20), cos(1:20)) + rnorm(40, sd = 0.1),
        g = rep(c("A", "B"), each = 20)
    )
    result <- VizModules:::.compute_loess_fit(df, "x", "y", group.col = "g")

    expect_type(result, "list")
    expect_true(all(c("A", "B") %in% names(result)))
})

test_that(".compute_loess_fit removes groups with insufficient data", {
    df <- data.frame(
        x = c(1:20, 1, 2),
        y = c(sin(1:20), 1, 2),
        g = c(rep("A", 20), "B", "B")
    )
    result <- VizModules:::.compute_loess_fit(df, "x", "y", group.col = "g")

    expect_true("A" %in% names(result))
    expect_false("B" %in% names(result))
})

# ─── add_hlines ─────────────────────────────────────────────────────────────

test_that("add_hlines returns empty list for NULL/empty intercepts", {
    fig <- make_plotly()
    expect_equal(VizModules::add_hlines(fig, NULL), list())
    expect_equal(VizModules::add_hlines(fig, numeric(0)), list())
})

test_that("add_hlines creates correct shape for single line", {
    fig <- make_plotly(data = list(list(type = "scatter", x = 1:5, y = 1:5)))
    shapes <- VizModules::add_hlines(fig, intercepts = 3)

    expect_equal(length(shapes), 1)
    expect_equal(shapes[[1]]$type, "line")
    expect_equal(shapes[[1]]$y0, 3)
    expect_equal(shapes[[1]]$y1, 3)
    expect_equal(shapes[[1]]$x0, 0)
    expect_equal(shapes[[1]]$x1, 1)
    expect_equal(shapes[[1]]$line$color, "#000000")
})

test_that("add_hlines creates multiple shapes with per-line styling", {
    fig <- make_plotly(data = list(list(type = "scatter", x = 1:5, y = 1:5)))
    shapes <- VizModules::add_hlines(fig,
        intercepts = c(1, 5),
        colors = c("red", "blue"), widths = c(2, 3)
    )

    expect_equal(length(shapes), 2)
    expect_equal(shapes[[1]]$line$color, "red")
    expect_equal(shapes[[2]]$line$color, "blue")
    expect_equal(shapes[[1]]$line$width, 2)
    expect_equal(shapes[[2]]$line$width, 3)
})

# ─── add_vlines ─────────────────────────────────────────────────────────────

test_that("add_vlines returns empty list for NULL/empty intercepts", {
    fig <- make_plotly()
    expect_equal(VizModules::add_vlines(fig, NULL), list())
    expect_equal(VizModules::add_vlines(fig, numeric(0)), list())
})

test_that("add_vlines creates correct shape for single line", {
    fig <- make_plotly(data = list(list(type = "scatter", x = 1:5, y = 1:5)))
    shapes <- VizModules::add_vlines(fig, intercepts = 2)

    expect_equal(length(shapes), 1)
    expect_equal(shapes[[1]]$x0, 2)
    expect_equal(shapes[[1]]$x1, 2)
    expect_equal(shapes[[1]]$y0, 0)
    expect_equal(shapes[[1]]$y1, 1)
})

# ─── add_ablines ───────────────────────────────────────────────────────────

test_that("add_ablines returns empty list for NULL slopes or intercepts", {
    fig <- make_plotly()
    expect_equal(VizModules::add_ablines(fig, NULL, c(0)), list())
    expect_equal(VizModules::add_ablines(fig, c(1), NULL), list())
    expect_equal(VizModules::add_ablines(fig, numeric(0), c(0)), list())
})

test_that("add_ablines creates y = mx + b line", {
    fig <- make_plotly(
        data = list(list(type = "scatter", x = c(0, 10), y = c(0, 10))),
        layout = list(xaxis = list(range = c(0, 10)))
    )
    shapes <- VizModules::add_ablines(fig, slopes = 2, intercepts = 1)

    expect_equal(length(shapes), 1)
    # y0 = intercept + slope * x0 = 1 + 2*0 = 1
    expect_equal(shapes[[1]]$y0, 1 + 2 * shapes[[1]]$x0)
    expect_equal(shapes[[1]]$y1, 1 + 2 * shapes[[1]]$x1)
})

test_that("add_ablines recycles shorter slopes/intercepts vector", {
    fig <- make_plotly(
        data = list(list(type = "scatter", x = 1:5, y = 1:5)),
        layout = list(xaxis = list(range = c(0, 5)))
    )
    # 2 slopes, 1 intercept -> intercept recycled to length 2
    shapes <- VizModules::add_ablines(fig, slopes = c(1, 2), intercepts = 0)
    expect_equal(length(shapes), 2)
})

# ─── add_reference_lines ────────────────────────────────────────────────────

test_that("add_reference_lines adds horizontal lines to figure", {
    fig <- make_plotly(data = list(list(type = "scatter", x = 1:5, y = 1:5)))
    result <- VizModules::add_reference_lines(fig, hline.intercepts = "2, 4")

    expect_true(length(result$x$layout$shapes) >= 2)
    expect_equal(result$x$layout$shapes[[1]]$y0, 2)
    expect_equal(result$x$layout$shapes[[2]]$y0, 4)
})

test_that("add_reference_lines adds vertical lines to figure", {
    fig <- make_plotly(data = list(list(type = "scatter", x = 1:5, y = 1:5)))
    result <- VizModules::add_reference_lines(fig, vline.intercepts = "3")

    expect_true(length(result$x$layout$shapes) >= 1)
    expect_equal(result$x$layout$shapes[[1]]$x0, 3)
})

test_that("add_reference_lines returns figure unchanged with no lines", {
    fig <- make_plotly(data = list(list(type = "scatter", x = 1:5, y = 1:5)))
    result <- VizModules::add_reference_lines(fig)
    expect_null(result$x$layout$shapes)
})

test_that("add_reference_lines preserves existing shapes", {
    existing_shape <- list(type = "rect", x0 = 0, x1 = 1, y0 = 0, y1 = 1)
    fig <- make_plotly(
        data = list(list(type = "scatter", x = 1:5, y = 1:5)),
        layout = list(shapes = list(existing_shape))
    )
    result <- VizModules::add_reference_lines(fig, hline.intercepts = "5")

    expect_true(length(result$x$layout$shapes) >= 2)
    expect_equal(result$x$layout$shapes[[1]]$type, "rect")
})

# ─── .calculate_range ────────────────────────────────────────────────────────

test_that(".calculate_range returns correct min/max for numeric column", {
    df <- data.frame(val = c(2, 5, 10))
    result <- VizModules:::.calculate_range(df, data_col_y = "val", axis_scale_factor = 1.1)

    expect_equal(result$min, 2)
    expect_equal(result$max, 10 * 1.1)
})

test_that(".calculate_range returns NULL for missing or non-numeric column", {
    df <- data.frame(val = c("a", "b", "c"))
    expect_null(VizModules:::.calculate_range(df, data_col_y = "val", axis_scale_factor = 1))
    expect_null(VizModules:::.calculate_range(df, data_col_y = "", axis_scale_factor = 1))
    expect_null(VizModules:::.calculate_range(df, data_col_y = "nonexistent", axis_scale_factor = 1))
})

test_that(".calculate_range handles all NA values", {
    df <- data.frame(val = c(NA_real_, NA_real_))
    result <- VizModules:::.calculate_range(df, data_col_y = "val", axis_scale_factor = 1)
    expect_equal(result$min, 0)
    expect_equal(result$max, 1)
})

test_that(".calculate_range works with x column", {
    df <- data.frame(x = c(1, 3, 5))
    result <- VizModules:::.calculate_range(df, data_col_x = "x", axis_scale_factor = 1)
    expect_equal(result$min, 1)
    expect_equal(result$max, 5)
})

test_that(".calculate_range works in grouping mode", {
    df <- data.frame(
        vals = c(10, 20, 30, 5, 2, 1),
        grp = c("A", "A", "A", "B", "B", "B")
    )
    result <- VizModules:::.calculate_range(df,
        data_col_x = "grp", data_col_y = "vals",
        axis_scale_factor = 1, grouping = TRUE
    )

    expect_equal(result$min, 0)
    expect_equal(result$max, 60)
})

test_that(".calculate_range spans every column of a multi-column selection", {
    df <- data.frame(a = c(2, 5, 10), b = c(-3, 0, 4))

    result <- VizModules:::.calculate_range(df, data_col_y = c("a", "b"), axis_scale_factor = 1.1)

    # Both columns share one axis, so the limits must fit the widest of them.
    expect_equal(result$min, -3)
    expect_equal(result$max, 10 * 1.1)
})

test_that(".calculate_range ignores blank names and rejects non-numeric ones in a selection", {
    df <- data.frame(a = c(2, 5, 10), b = c("x", "y", "z"), stringsAsFactors = FALSE)

    dropped <- VizModules:::.calculate_range(df, data_col_y = c("a", ""), axis_scale_factor = 1)
    expect_equal(dropped$min, 2)
    expect_equal(dropped$max, 10)

    expect_null(VizModules:::.calculate_range(df, data_col_y = c("a", "b"), axis_scale_factor = 1))
    expect_null(VizModules:::.calculate_range(df, data_col_y = character(0), axis_scale_factor = 1))
    expect_null(VizModules:::.calculate_range(df, data_col_y = NA_character_, axis_scale_factor = 1))
})

test_that(".calculate_range sums a multi-column selection when stacked", {
    df <- data.frame(
        a = c(10, 20, 5, 2),
        b = c(1, 2, 3, 4),
        grp = c("A", "A", "B", "B")
    )

    result <- VizModules:::.calculate_range(df,
        data_col_x = "grp", data_col_y = c("a", "b"),
        axis_scale_factor = 1, grouping = TRUE
    )

    # Stacked bars total both columns within each x group: A = 10+20+1+2.
    expect_equal(result$min, 0)
    expect_equal(result$max, 33)
})

# ─── .multivar_long_df ───────────────────────────────────────────────────────

test_that(".multivar_long_df stacks columns the way dittoViz does internally", {
    df <- data.frame(grp = c("A", "B"), a = c(1, 2), b = c(3, 4), stringsAsFactors = FALSE)

    long <- VizModules:::.multivar_long_df(df, c("a", "b"))

    expect_equal(nrow(long), 4)
    expect_equal(long$var.which, c("a", "a", "b", "b"))
    expect_equal(long$var.multi, c(1, 2, 3, 4))
    # The original columns ride along so grouping/faceting variables stay usable.
    expect_equal(long$grp, c("A", "B", "A", "B"))
})

# ─── empty_plot ─────────────────────────────────────────────────────────────

test_that(".empty_plot returns ggplot by default", {
    p <- VizModules::empty_plot(text = "No data")
    expect_s3_class(p, "ggplot")
})

test_that("empty_plot returns plotly when requested", {
    p <- VizModules::empty_plot(text = "No data", plotly = TRUE)
    expect_s3_class(p, "plotly")
})

test_that("empty_plot works with NULL text", {
    p <- VizModules::empty_plot(text = NULL)
    expect_s3_class(p, "ggplot")
})

# ─── is_pure_type ────────────────────────────────────────────────────────────

test_that("is_pure_type returns TRUE for all numeric columns", {
    df <- data.frame(a = 1:3, b = 4:6)
    expect_true(is_pure_type(c("a", "b"), df))
})

test_that("is_pure_type returns TRUE for all categorical columns", {
    df <- data.frame(a = letters[1:3], b = factor(c("x", "y", "z")), stringsAsFactors = FALSE)
    expect_true(is_pure_type(c("a", "b"), df))
})

test_that("is_pure_type returns FALSE for mixed numeric and categorical", {
    df <- data.frame(num = 1:3, cat = letters[1:3], stringsAsFactors = FALSE)
    expect_false(is_pure_type(c("num", "cat"), df))
})

test_that("is_pure_type returns TRUE for single column", {
    df <- data.frame(a = 1:3)
    expect_true(is_pure_type("a", df))
})

test_that("is_pure_type returns TRUE for empty or nonexistent columns", {
    df <- data.frame(a = 1:3)
    expect_true(is_pure_type("", df))
    expect_true(is_pure_type("nonexistent", df))
    expect_true(is_pure_type(character(0), df))
})

# ─── get_documentation ───────────────────────────────────────────────────────

test_that("get_documentation returns named list for valid function", {
    result <- VizModules::get_documentation("stats::lm", selected = c("formula"))
    expect_type(result, "list")
    expect_true(nzchar(result$formula))
})

test_that("get_documentation capitalizes when cap = TRUE", {
    result <- VizModules::get_documentation("stats::lm", selected = c("formula"), cap = TRUE)
    first_char <- substring(result$formula, 1, 1)
    expect_equal(first_char, toupper(first_char))
})

# ─── create_axis_styles ────────────────────────────────────────────────────

test_that("create_axis_styles returns expected structure for x axis", {
    mock_input <- list(
        axis.title.font.size = 14,
        axis.title.font.family = "Arial",
        axis.title.font.color = "black",
        axis.tickfont.size = 12,
        axis.tickfont.color = "#333",
        axis.tickfont.family = "Arial",
        axis.tickangle.x = -45,
        axis.tickangle.y = 0,
        axis.ticks = "outside",
        axis.tickcolor = "black",
        axis.ticklen = 5,
        axis.tickwidth = 1,
        show.grid.x = TRUE,
        show.grid.y = FALSE,
        grid.color = "#CCCCCC",
        axis.showline = TRUE,
        axis.mirror = FALSE,
        axis.linecolor = "black",
        axis.linewidth = 1
    )

    result <- VizModules::create_axis_styles(mock_input, axis_side = "x", isolate_fn = identity)

    expect_equal(result$title$font$size, 14)
    expect_equal(result$title$font$family, "Arial")
    expect_equal(result$tickangle, -45)
    expect_true(result$showgrid)
    expect_equal(result$gridcolor, "#CCCCCC")
})

test_that("create_axis_styles returns expected structure for y axis", {
    mock_input <- list(
        axis.title.font.size = 14,
        axis.title.font.family = "Arial",
        axis.title.font.color = "black",
        axis.tickfont.size = 12,
        axis.tickfont.color = "#333",
        axis.tickfont.family = "Arial",
        axis.tickangle.x = 0,
        axis.tickangle.y = -90,
        axis.ticks = "outside",
        axis.tickcolor = "black",
        axis.ticklen = 5,
        axis.tickwidth = 1,
        show.grid.x = TRUE,
        show.grid.y = FALSE,
        grid.color = "#CCCCCC",
        axis.showline = TRUE,
        axis.mirror = FALSE,
        axis.linecolor = "black",
        axis.linewidth = 1
    )

    result <- VizModules::create_axis_styles(mock_input, axis_side = "y", isolate_fn = identity)

    expect_equal(result$tickangle, -90)
    expect_false(result$showgrid)
    expect_equal(result$gridcolor, "#CCCCCC")
})

test_that("create_axis_styles excludes line props when ggplot.axis.styling is TRUE", {
    mock_input <- list(
        axis.title.font.size = 14, axis.title.font.family = "Arial",
        axis.title.font.color = "black", axis.tickfont.size = 12,
        axis.tickfont.color = "#333", axis.tickfont.family = "Arial",
        axis.tickangle.x = 0, axis.tickangle.y = 0,
        axis.ticks = "outside", axis.tickcolor = "black",
        axis.ticklen = 5, axis.tickwidth = 1,
        show.grid.x = TRUE, show.grid.y = TRUE,
        grid.color = "#CCCCCC",
        axis.showline = TRUE, axis.mirror = TRUE,
        axis.linecolor = "red", axis.linewidth = 2
    )

    result <- VizModules::create_axis_styles(mock_input,
        axis_side = "x",
        isolate_fn = identity, ggplot.axis.styling = TRUE
    )
    expect_null(result$showline)
    expect_null(result$mirror)

    result2 <- VizModules::create_axis_styles(mock_input,
        axis_side = "x",
        isolate_fn = identity, ggplot.axis.styling = FALSE
    )
    expect_true(result2$showline)
    expect_true(result2$mirror)
    expect_equal(result2$linecolor, "red")
})

# ─── create_ggplot_axis_style ───────────────────────────────────────────────

test_that("create_ggplot_axis_style returns full border when showline + mirror", {
    mock_input <- list(
        axis.showline = TRUE,
        axis.mirror = TRUE,
        axis.linecolor = "red",
        axis.linewidth = 2
    )
    result <- VizModules::create_ggplot_axis_style(mock_input, isolate_fn = identity)

    expect_true(inherits(result$panel.border, "element_rect"))
    expect_true(inherits(result$axis.line, "element_blank"))
})

test_that("create_ggplot_axis_style returns axis lines only when showline but no mirror", {
    mock_input <- list(
        axis.showline = TRUE,
        axis.mirror = FALSE,
        axis.linecolor = "blue",
        axis.linewidth = 1
    )
    result <- VizModules::create_ggplot_axis_style(mock_input, isolate_fn = identity)

    expect_true(inherits(result$axis.line, "element_line"))
    expect_true(inherits(result$panel.border, "element_blank"))
})

test_that("create_ggplot_axis_style returns no borders when showline is FALSE", {
    mock_input <- list(
        axis.showline = FALSE,
        axis.mirror = FALSE,
        axis.linecolor = "black",
        axis.linewidth = 1
    )
    result <- VizModules::create_ggplot_axis_style(mock_input, isolate_fn = identity)

    expect_true(inherits(result$panel.border, "element_blank"))
    expect_true(inherits(result$axis.line, "element_blank"))
})

# ─── .custom_legend ───────────────────────────────────────────────────────────

test_that(".custom_legend returns the figure unchanged when size_by is missing", {
    fig <- make_plotly()
    data <- data.frame(cell_type = c("A", "B"), pct_expressed = c(10, 20))

    expect_identical(VizModules:::.custom_legend(fig, data, size_by = NULL), fig)
    expect_identical(VizModules:::.custom_legend(fig, data, size_by = ""), fig)
    expect_identical(VizModules:::.custom_legend(fig, data, size_by = "absent"), fig)
})

test_that(".custom_legend returns the figure unchanged for non-numeric size_by", {
    fig <- make_plotly()
    data <- data.frame(cell_type = c("A", "B"), pct_expressed = c(10, 20))

    expect_identical(VizModules:::.custom_legend(fig, data, size_by = "cell_type"), fig)
})

test_that(".custom_legend appends size-legend annotations for numeric size_by", {
    data <- data.frame(
        cell_type = rep(c("A", "B"), each = 3),
        pct_expressed = c(5, 25, 50, 10, 40, 90)
    )

    fig <- plotly::plot_ly(
        data = data, x = ~cell_type, y = ~pct_expressed, type = "scatter", mode = "markers"
    )

    result <- VizModules:::.custom_legend(fig, data,
        size_by = "pct_expressed",
        size_values = c(10, 20, 30, 40, 50)
    )

    expect_s3_class(result, "plotly")
    built <- plotly::plotly_build(result)
    # 1 title annotation + 5 circle glyphs + 5 numeric labels
    expect_equal(length(built$x$layout$annotations), 11)
    ann_text <- vapply(built$x$layout$annotations, function(a) a$text, character(1))
    expect_true("pct_expressed" %in% ann_text)
})

test_that(".custom_legend applies legend title and label font sizes to annotations", {
    data <- data.frame(
        cell_type = rep(c("A", "B"), each = 3),
        pct_expressed = c(5, 25, 50, 10, 40, 90)
    )
    fig <- plotly::plot_ly(
        data = data, x = ~cell_type, y = ~pct_expressed, type = "scatter", mode = "markers"
    )

    result <- VizModules:::.custom_legend(fig, data,
        size_by = "pct_expressed",
        size_values = c(10, 20, 30, 40, 50),
        title.size = 22, text.size = 9
    )
    built <- plotly::plotly_build(result)
    anns <- built$x$layout$annotations

    title_ann <- Filter(function(a) identical(a$text, "pct_expressed"), anns)
    expect_equal(title_ann[[1]]$font$size, 22)

    # Numeric label annotations carry the requested text size.
    label_anns <- Filter(
        function(a) !is.null(a$font$size) && grepl("^[0-9.]+$", a$text), anns
    )
    expect_true(length(label_anns) >= 5)
    expect_true(all(vapply(label_anns, function(a) a$font$size, numeric(1)) == 9))
})

test_that(".custom_legend derives circle sizes from marker sizes when size_values is NULL", {
    data <- data.frame(
        cell_type = rep(c("A", "B"), each = 3),
        pct_expressed = c(5, 25, 50, 10, 40, 90)
    )
    fig <- plotly::plot_ly(
        data = data, x = ~cell_type, y = ~pct_expressed, type = "scatter", mode = "markers",
        marker = list(size = ~pct_expressed)
    )

    result <- VizModules:::.custom_legend(fig, data, size_by = "pct_expressed")
    built <- plotly::plotly_build(result)
    anns <- built$x$layout$annotations
    circle_text <- Filter(function(a) grepl("font-size", a$text), anns)
    expect_equal(length(circle_text), 5)

    sizes <- as.numeric(sub(".*font-size:([0-9.]+)px.*", "\\1", vapply(
        circle_text, function(a) a$text, character(1)
    )))
    # The glyph font-sizes are the marker pixel diameters scaled up by the
    # circle-glyph ink ratio so the rendered circles match the plotted dots.
    msizes <- VizModules:::.extract_marker_sizes(fig)
    ratio <- VizModules:::.CIRCLE_GLYPH_DIAMETER_RATIO
    expect_equal(min(sizes), min(msizes) / ratio)
    expect_equal(max(sizes), max(msizes) / ratio)
    # Sizes increase monotonically.
    expect_false(is.unsorted(sizes))
})

test_that(".custom_legend does not duplicate label annotations", {
    data <- data.frame(
        cell_type = rep(c("A", "B"), each = 3),
        pct_expressed = c(5, 25, 50, 10, 40, 90)
    )
    fig <- plotly::plot_ly(
        data = data, x = ~cell_type, y = ~pct_expressed, type = "scatter", mode = "markers"
    )

    result <- VizModules:::.custom_legend(fig, data,
        size_by = "pct_expressed",
        size_values = c(10, 20, 30, 40, 50)
    )
    built <- plotly::plotly_build(result)
    texts <- vapply(built$x$layout$annotations, function(a) a$text, character(1))
    # Each of the 5 numeric break labels appears exactly once (no duplicates).
    labels <- texts[grepl("^[0-9.]+$", texts)]
    expect_equal(length(labels), 5)
    expect_equal(length(unique(labels)), 5)

    # Annotations live in the built layout, so a second build does not double them.
    rebuilt <- plotly::plotly_build(built)
    expect_equal(
        length(rebuilt$x$layout$annotations),
        length(built$x$layout$annotations)
    )
})

test_that(".custom_legend strips the size variable from a combined legend title", {
    data <- data.frame(
        cell_type = rep(c("A", "B"), each = 3),
        pct_expressed = c(5, 25, 50, 10, 40, 90)
    )
    fig <- plotly::plot_ly(
        data = data, x = ~cell_type, y = ~pct_expressed, type = "scatter", mode = "markers"
    )
    fig$x$layout$legend$title$text <- "cell_type<br />pct_expressed"

    result <- VizModules:::.custom_legend(fig, data,
        size_by = "pct_expressed",
        size_values = c(10, 20, 30, 40, 50)
    )
    expect_equal(result$x$layout$legend$title$text, "cell_type")

    # A standalone (already-merged) title is left untouched.
    fig2 <- fig
    fig2$x$layout$legend$title$text <- "pct_expressed"
    result2 <- VizModules:::.custom_legend(fig2, data,
        size_by = "pct_expressed",
        size_values = c(10, 20, 30, 40, 50)
    )
    expect_equal(result2$x$layout$legend$title$text, "pct_expressed")
})

test_that(".custom_legend start_y lowers the legend column", {
    data <- data.frame(
        cell_type = rep(c("A", "B"), each = 3),
        pct_expressed = c(5, 25, 50, 10, 40, 90)
    )
    fig <- plotly::plot_ly(
        data = data, x = ~cell_type, y = ~pct_expressed, type = "scatter", mode = "markers"
    )

    high <- plotly::plotly_build(VizModules:::.custom_legend(fig, data,
        size_by = "pct_expressed", size_values = c(10, 20, 30, 40, 50), start_y = 0.95
    ))
    low <- plotly::plotly_build(VizModules:::.custom_legend(fig, data,
        size_by = "pct_expressed", size_values = c(10, 20, 30, 40, 50), start_y = 0.45
    ))
    max_y <- function(b) max(vapply(b$x$layout$annotations, function(a) a$y, numeric(1)))
    expect_true(max_y(low) < max_y(high))

    # An invalid start_y falls back to the default placement.
    fallback <- plotly::plotly_build(VizModules:::.custom_legend(fig, data,
        size_by = "pct_expressed", size_values = c(10, 20, 30, 40, 50), start_y = NA
    ))
    expect_equal(max_y(fallback), max_y(high))
})

test_that(".custom_legend start_x shifts the legend column horizontally", {
    data <- data.frame(
        cell_type = rep(c("A", "B"), each = 3),
        pct_expressed = c(5, 25, 50, 10, 40, 90)
    )
    fig <- plotly::plot_ly(
        data = data, x = ~cell_type, y = ~pct_expressed, type = "scatter", mode = "markers"
    )

    default <- plotly::plotly_build(VizModules:::.custom_legend(fig, data,
        size_by = "pct_expressed", size_values = c(10, 20, 30, 40, 50)
    ))
    shifted <- plotly::plotly_build(VizModules:::.custom_legend(fig, data,
        size_by = "pct_expressed", size_values = c(10, 20, 30, 40, 50), start_x = 1.2
    ))
    max_x <- function(b) max(vapply(b$x$layout$annotations, function(a) a$x, numeric(1)))
    expect_true(max_x(shifted) > max_x(default))

    # An invalid start_x falls back to the default placement.
    fallback <- plotly::plotly_build(VizModules:::.custom_legend(fig, data,
        size_by = "pct_expressed", size_values = c(10, 20, 30, 40, 50), start_x = NA
    ))
    expect_equal(max_x(fallback), max_x(default))
})

test_that(".custom_legend offsets numeric labels by a fixed pixel xshift", {
    data <- data.frame(
        cell_type = rep(c("A", "B"), each = 3),
        pct_expressed = c(5, 25, 50, 10, 40, 90)
    )
    fig <- plotly::plot_ly(
        data = data, x = ~cell_type, y = ~pct_expressed, type = "scatter", mode = "markers"
    )
    size_values <- c(10, 20, 30, 40, 50)

    built <- plotly::plotly_build(VizModules:::.custom_legend(fig, data,
        size_by = "pct_expressed", size_values = size_values
    ))
    anns <- built$x$layout$annotations
    label_anns <- Filter(function(a) grepl("^[0-9.]+$", a$text), anns)
    expect_equal(length(label_anns), length(size_values))

    # Labels are anchored at the circle x (paper) and offset purely in pixels,
    # so the marker-to-label spacing is independent of plot width.
    expect_true(all(vapply(label_anns, function(a) isTRUE(a$xanchor == "left"), logical(1))))
    expect_true(all(vapply(label_anns, function(a) !is.null(a$xshift) && a$xshift > 0, logical(1))))

    # The xshift grows with the glyph size (larger circles push labels further).
    shifts <- vapply(label_anns, function(a) a$xshift, numeric(1))
    expect_equal(shifts, sort(shifts))
})

test_that(".extract_marker_sizes collects numeric marker sizes", {
    fig <- plotly::plot_ly(
        x = 1:3, y = 1:3, type = "scatter", mode = "markers",
        marker = list(size = c(4, 8, 12))
    )
    expect_equal(sort(VizModules:::.extract_marker_sizes(fig)), c(4, 8, 12))

    # No marker sizes -> empty numeric vector.
    fig2 <- plotly::plot_ly(x = 1:3, y = 1:3, type = "scatter", mode = "lines")
    expect_equal(VizModules:::.extract_marker_sizes(fig2), numeric(0))
})

# ─── adjusted_axis_label ────────────────────────────────────────────────────

test_that("adjusted_axis_label returns the base label when no adjustment is set", {
    expect_equal(VizModules::adjusted_axis_label("units"), "units")
    expect_equal(VizModules::adjusted_axis_label("units", "", ""), "units")
    expect_equal(VizModules::adjusted_axis_label("units", NA, NULL), "units")
})

test_that("adjusted_axis_label wraps with the data adjustment", {
    expect_equal(VizModules::adjusted_axis_label("units", "z-score"), "z-score(units)")
    expect_equal(
        VizModules::adjusted_axis_label("units", "relative.to.max"),
        "relative.to.max(units)"
    )
})

test_that("adjusted_axis_label wraps with the adjustment function", {
    expect_equal(VizModules::adjusted_axis_label("units", NULL, "log2"), "log2(units)")
})

test_that("adjusted_axis_label nests adjustment then function", {
    expect_equal(
        VizModules::adjusted_axis_label("units", "z-score", "log2"),
        "log2(z-score(units))"
    )
})

test_that(".annotation_edit_key keys axis titles by side and others by text", {
    expect_equal(
        VizModules:::.annotation_edit_key(list(annotationType = "axis", textangle = 0)),
        "axis:x"
    )
    expect_equal(
        VizModules:::.annotation_edit_key(list(annotationType = "axis", textangle = -90)),
        "axis:y"
    )
    expect_equal(VizModules:::.annotation_edit_key(list(text = "p = 0.01")), "text:p = 0.01")
    expect_null(VizModules:::.annotation_edit_key(list(text = "")))
    expect_null(VizModules:::.annotation_edit_key(NULL))
})

test_that(".capture_manual_edits records legend and annotation moves", {
    fig <- list(x = list(layout = list(annotations = list(
        list(text = "X", annotationType = "axis", textangle = 0)
    ))))
    edits <- .capture_manual_edits(
        list(legend = NULL, annotations = list()),
        list(`legend.x` = 0.2, `legend.y` = 0.3, `annotations[0].x` = 0.8),
        fig
    )
    expect_equal(edits$legend$x, 0.2)
    expect_equal(edits$legend$y, 0.3)
    expect_equal(edits$annotations[["axis:x#1"]]$x, 0.8)
})

test_that(".capture_manual_edits disambiguates repeated annotation text", {
    fig <- list(x = list(layout = list(annotations = list(
        list(text = "P"), list(text = "P")
    ))))
    edits <- .capture_manual_edits(
        list(legend = NULL, annotations = list()),
        list(`annotations[0].x` = 0.1, `annotations[1].x` = 0.9),
        fig
    )
    expect_equal(edits$annotations[["text:P#1"]]$x, 0.1)
    expect_equal(edits$annotations[["text:P#2"]]$x, 0.9)
})

test_that(".capture_manual_edits records colorbar drag", {
    edits <- .capture_manual_edits(
        list(legend = NULL, annotations = list()),
        list(`coloraxis.colorbar.x` = 1.2, `coloraxis.colorbar.y` = 0.4),
        NULL
    )
    expect_equal(edits$colorbar$x, 1.2)
    expect_equal(edits$colorbar$y, 0.4)
})

test_that(".capture_manual_edits records legend drag anchors", {
    edits <- .capture_manual_edits(
        list(legend = NULL, annotations = list()),
        list(`legend.x` = 0.2, `legend.y` = 0.3, `legend.xanchor` = "left", `legend.yanchor` = "top"),
        NULL
    )
    expect_equal(edits$legend$xanchor, "left")
    expect_equal(edits$legend$yanchor, "top")
})

test_that(".capture_manual_edits ignores range/zoom keys", {
    edits <- .capture_manual_edits(
        list(legend = NULL, annotations = list()),
        list(`xaxis.range[0]` = 1, `xaxis.range[1]` = 2),
        NULL
    )
    expect_null(edits$legend)
    expect_length(edits$annotations, 0)
})

test_that(".reapply_manual_edits restores positions across index shifts", {
    edits <- list(
        legend = list(x = 0.2, y = 0.3),
        annotations = list("axis:x#1" = list(x = 0.8, y = 0.9))
    )
    fig <- list(x = list(layout = list(
        legend = list(x = 1, y = 1),
        annotations = list(
            list(text = "stat"),
            list(text = "X", annotationType = "axis", textangle = 0, x = 0.5, y = -0.1)
        )
    )))
    out <- .reapply_manual_edits(fig, edits)
    expect_equal(out$x$layout$legend$x, 0.2)
    expect_equal(out$x$layout$annotations[[2]]$x, 0.8)
    expect_equal(out$x$layout$annotations[[2]]$y, 0.9)
})

test_that(".reapply_manual_edits restores dragged arrow offsets", {
    edits <- list(
        legend = NULL,
        annotations = list("text:Pt#1" = list(x = 1, y = 2, ax = 30, ay = -40))
    )
    fig <- list(x = list(layout = list(annotations = list(
        list(text = "Pt", x = 0, y = 0, ax = 20, ay = -20)
    ))))
    out <- .reapply_manual_edits(fig, edits)
    expect_equal(out$x$layout$annotations[[1]]$ax, 30)
    expect_equal(out$x$layout$annotations[[1]]$ay, -40)
})

test_that(".reapply_manual_edits keeps repeated-text annotations independent", {
    edits <- list(
        legend = NULL,
        annotations = list("text:P#1" = list(x = 0.1), "text:P#2" = list(x = 0.9))
    )
    fig <- list(x = list(layout = list(annotations = list(
        list(text = "P", x = 0), list(text = "P", x = 0)
    ))))
    out <- .reapply_manual_edits(fig, edits)
    expect_equal(out$x$layout$annotations[[1]]$x, 0.1)
    expect_equal(out$x$layout$annotations[[2]]$x, 0.9)
})

test_that(".reapply_manual_edits restores a dragged colorbar onto its trace", {
    edits <- list(legend = NULL, annotations = list(), colorbar = list(x = 1.2, y = 0.4))
    fig <- list(x = list(data = list(list(marker = list(colorbar = list(x = 1, y = 0.5))))))
    out <- .reapply_manual_edits(fig, edits)
    expect_equal(out$x$data[[1]]$marker$colorbar$x, 1.2)
    expect_equal(out$x$data[[1]]$marker$colorbar$y, 0.4)
})

test_that(".reapply_manual_edits regenerates an adjusted axis title but keeps its position", {
    edits <- list(legend = NULL, annotations = list(
        "axis:y#1" = list(text = "old label", x = 0.9, y = 0.4)
    ))
    fig <- list(x = list(layout = list(annotations = list(
        list(text = "log2(units)", annotationType = "axis", textangle = -90, x = -0.05, y = 0.5)
    ))))
    out <- .reapply_manual_edits(fig, edits, regen_keys = "axis:y")
    expect_equal(out$x$layout$annotations[[1]]$text, "log2(units)") # regenerated, not clobbered
    expect_equal(out$x$layout$annotations[[1]]$x, 0.9)              # drag position persists
    expect_equal(out$x$layout$annotations[[1]]$y, 0.4)
})

test_that(".reapply_manual_edits persists a manual axis title edit when no adjustment is active", {
    edits <- list(legend = NULL, annotations = list(
        "axis:y#1" = list(text = "My label", x = 0.9)
    ))
    fig <- list(x = list(layout = list(annotations = list(
        list(text = "units", annotationType = "axis", textangle = -90, x = -0.05, y = 0.5)
    ))))
    out <- .reapply_manual_edits(fig, edits) # regen_keys defaults to none
    expect_equal(out$x$layout$annotations[[1]]$text, "My label")
    expect_equal(out$x$layout$annotations[[1]]$x, 0.9)
})

test_that(".reapply_manual_edits only regenerates the named side", {
    edits <- list(legend = NULL, annotations = list(
        "axis:x#1" = list(text = "custom X"),
        "axis:y#1" = list(text = "custom Y")
    ))
    fig <- list(x = list(layout = list(annotations = list(
        list(text = "grp", annotationType = "axis", textangle = 0),
        list(text = "log2(units)", annotationType = "axis", textangle = -90)
    ))))
    out <- .reapply_manual_edits(fig, edits, regen_keys = "axis:y")
    expect_equal(out$x$layout$annotations[[1]]$text, "custom X")    # x not regenerated
    expect_equal(out$x$layout$annotations[[2]]$text, "log2(units)") # y regenerated
})

test_that("build_facet_annotations keys shared axis titles by side, not text", {
    anns <- build_facet_annotations(c("A", "B"), x.title = "grp", y.title = "units", nrows = 1)
    keys <- VizModules:::.annotation_edit_keys(anns)
    expect_true("axis:x#1" %in% keys)
    expect_true("axis:y#1" %in% keys)
    expect_false(any(c("text:grp#1", "text:units#1") %in% keys))
})

test_that("faceted shared-axis-title position survives a label text change", {
    # Position captured on the pre-adjustment faceted figure...
    anns_before <- build_facet_annotations(c("A", "B"), x.title = "grp", y.title = "units")
    fig_before <- list(x = list(layout = list(annotations = anns_before)))
    yi <- which(VizModules:::.annotation_edit_keys(anns_before) == "axis:y#1") - 1L
    rl <- setNames(list(0.02, 0.55), sprintf(c("annotations[%d].x", "annotations[%d].y"), yi))
    edits <- .capture_manual_edits(list(legend = NULL, annotations = list()), rl, fig_before)
    # ...re-applied on the rebuilt faceted figure whose y label changed to log2(units).
    anns_after <- build_facet_annotations(c("A", "B"), x.title = "grp", y.title = "log2(units)")
    out <- .reapply_manual_edits(
        list(x = list(layout = list(annotations = anns_after))),
        edits, regen_keys = "axis:y"
    )
    kk <- VizModules:::.annotation_edit_keys(out$x$layout$annotations)
    ya <- out$x$layout$annotations[[which(kk == "axis:y#1")]]
    expect_equal(ya$x, 0.02) # dragged position kept
    expect_equal(ya$y, 0.55)
    expect_equal(ya$text, "log2(units)") # regenerated label wins
})

test_that("reset_axis_title_text drops text for the named side but keeps position and others", {
    store <- list(edits = shiny::reactiveValues(annotations = list(
        "axis:y#1" = list(text = "custom", x = 0.9, y = 0.4),
        "axis:x#1" = list(text = "keep x"),
        "text:Pt#1" = list(text = "stay", x = 1)
    )))
    shiny::isolate({
        changed <- reset_axis_title_text(store, "axis:y")
        expect_true(changed)
        anns <- store$edits$annotations
        expect_null(anns[["axis:y#1"]]$text)             # text dropped
        expect_equal(anns[["axis:y#1"]]$x, 0.9)          # dragged position kept
        expect_equal(anns[["axis:x#1"]]$text, "keep x")  # other axis untouched
        expect_equal(anns[["text:Pt#1"]]$text, "stay")   # non-axis annotation untouched
    })
})

test_that("reset_axis_title_text removes a text-only entry entirely and reports no-op", {
    store <- list(edits = shiny::reactiveValues(annotations = list(
        "axis:y#1" = list(text = "custom")
    )))
    shiny::isolate({
        expect_true(reset_axis_title_text(store, "axis:y"))
        expect_false("axis:y#1" %in% names(store$edits$annotations))
        # Nothing left to clear -> FALSE, and no error on an empty store.
        expect_false(reset_axis_title_text(store, "axis:y"))
    })
})

test_that(".add_colorbar_listener attaches a render hook to the figure", {
    p <- plotly::plot_ly(x = 1:3, y = 1:3, type = "scatter", mode = "markers")
    out <- .add_colorbar_listener(p, "mod-colorbar.move")
    expect_s3_class(out, "plotly")
    expect_false(is.null(out$jsHooks$render))
})
