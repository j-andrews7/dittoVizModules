test_df <- data.frame(
    group = rep(c("A", "B", "C"), each = 20),
    value = c(rnorm(20, 5), rnorm(20, 8), rnorm(20, 5)),
    facet = rep(c("F1", "F2"), 30),
    color = rep(c("red", "blue"), 30),
    stringsAsFactors = FALSE
)

set.seed(42)

# ─── compute_pairwise_stats ─────────────────────────────────────────────────

test_that("compute_pairwise_stats returns correct structure for wilcox.test", {
    result <- compute_pairwise_stats(
        df = test_df, x = "group", y = "value", test = "wilcox.test"
    )
    expect_s3_class(result, "data.frame")
    expect_true(all(c("group1", "group2", "p.value", "p.adj", "p.signif",
        "test", "facet_level", "x_level") %in% names(result)))
    expect_equal(nrow(result), 3) # C(3,2) = 3 pairs
    expect_true(all(result$test == "wilcox.test"))
})

test_that("compute_pairwise_stats works with t.test", {
    result <- compute_pairwise_stats(
        df = test_df, x = "group", y = "value", test = "t.test"
    )
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 3)
    expect_true(all(result$test == "t.test"))
})

test_that("compute_pairwise_stats works with kruskal.test (omnibus)", {
    result <- compute_pairwise_stats(
        df = test_df, x = "group", y = "value", test = "kruskal.test"
    )
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 1)
    expect_equal(result$group1, "all")
    expect_equal(result$group2, "all")
    expect_true(all(result$test == "kruskal.test"))
})

test_that("compute_pairwise_stats works with anova (omnibus)", {
    result <- compute_pairwise_stats(
        df = test_df, x = "group", y = "value", test = "anova"
    )
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 1)
    expect_equal(result$group1, "all")
    expect_true(all(result$test == "anova"))
})

test_that("compute_pairwise_stats applies p-value adjustment", {
    result <- compute_pairwise_stats(
        df = test_df, x = "group", y = "value",
        test = "wilcox.test", p.adjust.method = "bonferroni"
    )
    # Adjusted p-values should be >= raw p-values
    expect_true(all(result$p.adj >= result$p.value, na.rm = TRUE))
})

test_that("compute_pairwise_stats respects specific pairs", {
    result <- compute_pairwise_stats(
        df = test_df, x = "group", y = "value",
        test = "wilcox.test",
        pairs = list(c("A", "B"))
    )
    expect_equal(nrow(result), 1)
    expect_equal(result$group1, "A")
    expect_equal(result$group2, "B")
})

test_that("compute_pairwise_stats handles per-facet testing", {
    result <- compute_pairwise_stats(
        df = test_df, x = "group", y = "value",
        test = "wilcox.test", facet.by = "facet", per.facet = TRUE
    )
    expect_true(all(c("F1", "F2") %in% result$facet_level))
    # 3 pairs * 2 facets = 6 rows
    expect_equal(nrow(result), 6)
})

test_that("compute_pairwise_stats with per.facet=FALSE returns NA facet_level", {
    result <- compute_pairwise_stats(
        df = test_df, x = "group", y = "value",
        test = "wilcox.test", facet.by = "facet", per.facet = FALSE
    )
    expect_equal(nrow(result), 3)
    expect_true(all(is.na(result$facet_level)))
})

test_that("compute_pairwise_stats with group.by nests within x-levels", {
    result <- compute_pairwise_stats(
        df = test_df, x = "group", y = "value",
        test = "wilcox.test", group.by = "color"
    )
    # 3 x-levels * 1 pair (red vs blue) = 3 rows
    expect_equal(nrow(result), 3)
    expect_true(all(!is.na(result$x_level)))
})

test_that("compute_pairwise_stats returns empty df for single group", {
    single <- test_df[test_df$group == "A", ]
    result <- compute_pairwise_stats(
        df = single, x = "group", y = "value", test = "wilcox.test"
    )
    expect_equal(nrow(result), 0)
})

test_that("compute_pairwise_stats sig.threshold affects p.signif", {
    result <- compute_pairwise_stats(
        df = test_df, x = "group", y = "value",
        test = "wilcox.test", sig.threshold = 1.0
    )
    # With threshold of 1.0, nothing should be "ns"
    expect_true(all(result$p.signif != "ns"))
})

test_that("compute_pairwise_stats paired test works", {
    # Equal-sized groups for paired test
    paired_df <- data.frame(
        group = rep(c("A", "B"), each = 10),
        value = c(rnorm(10, 5), rnorm(10, 8)),
        stringsAsFactors = FALSE
    )
    result <- compute_pairwise_stats(
        df = paired_df, x = "group", y = "value",
        test = "t.test", paired = TRUE
    )
    expect_equal(nrow(result), 1)
    expect_false(is.na(result$p.value))
})

# ─── generate_pair_strings ──────────────────────────────────────────────────

test_that("generate_pair_strings returns correct pairs", {
    result <- generate_pair_strings(test_df, "group")
    expect_equal(length(result), 3) # C(3,2) = 3
    expect_true(all(grepl(" vs ", result)))
})

test_that("generate_pair_strings with group.by uses group levels", {
    result <- generate_pair_strings(test_df, "group", group.by = "color")
    expect_equal(length(result), 1) # C(2,2) = 1 pair: red vs blue
    expect_true(grepl(" vs ", result))
})

test_that("generate_pair_strings returns empty for single level", {
    single <- test_df[test_df$group == "A", ]
    result <- generate_pair_strings(single, "group")
    expect_equal(length(result), 0)
})

# ─── parse_pair_strings ─────────────────────────────────────────────────────

test_that("parse_pair_strings parses valid strings", {
    result <- parse_pair_strings(c("A vs B", "B vs C"))
    expect_type(result, "list")
    expect_equal(length(result), 2)
    expect_equal(result[[1]], c("A", "B"))
    expect_equal(result[[2]], c("B", "C"))
})

test_that("parse_pair_strings returns NULL for empty input", {
    expect_null(parse_pair_strings(NULL))
    expect_null(parse_pair_strings(character(0)))
    expect_null(parse_pair_strings(""))
})

# ─── create_stat_annotations ────────────────────────────────────────────────

test_that("create_stat_annotations returns correct structure", {
    stats_df <- compute_pairwise_stats(
        df = test_df, x = "group", y = "value", test = "wilcox.test"
    )
    result <- create_stat_annotations(
        stats_df = stats_df, fig = NULL, df = test_df,
        x = "group", y = "value"
    )
    expect_type(result, "list")
    expect_true(all(c("annotations", "shapes", "y.max") %in% names(result)))
    expect_true(length(result$annotations) > 0)
    expect_true(length(result$shapes) > 0)
    expect_true(is.numeric(result$y.max))
})

test_that("create_stat_annotations returns empty for NULL stats_df", {
    result <- create_stat_annotations(
        stats_df = NULL, fig = NULL, df = test_df,
        x = "group", y = "value"
    )
    expect_equal(length(result$annotations), 0)
    expect_equal(length(result$shapes), 0)
    expect_null(result$y.max)
})

test_that("create_stat_annotations hide.ns filters results", {
    stats_df <- compute_pairwise_stats(
        df = test_df, x = "group", y = "value",
        test = "wilcox.test", sig.threshold = 0.05
    )
    # With very low threshold, everything should be hidden
    result <- create_stat_annotations(
        stats_df = stats_df, fig = NULL, df = test_df,
        x = "group", y = "value",
        hide.ns = TRUE, sig.threshold = 0.0001
    )
    # Brackets only shown for significant comparisons
    # (may be 0 if none are significant at 0.0001)
    expect_type(result, "list")
})

test_that("create_stat_annotations generates capped bracket shapes", {
    stats_df <- data.frame(
        group1 = "A", group2 = "B",
        p.value = 0.01, p.adj = 0.01, p.signif = "**",
        test = "wilcox.test", facet_level = NA_character_,
        x_level = NA_character_, stringsAsFactors = FALSE
    )
    result <- create_stat_annotations(
        stats_df = stats_df, fig = NULL, df = test_df,
        x = "group", y = "value", bracket.style = "capped"
    )
    # Capped style: 3 shapes per bracket (left tick, horizontal, right tick)
    expect_equal(length(result$shapes), 3)
})

test_that("create_stat_annotations generates flat bracket shapes", {
    stats_df <- data.frame(
        group1 = "A", group2 = "B",
        p.value = 0.01, p.adj = 0.01, p.signif = "**",
        test = "wilcox.test", facet_level = NA_character_,
        x_level = NA_character_, stringsAsFactors = FALSE
    )
    result <- create_stat_annotations(
        stats_df = stats_df, fig = NULL, df = test_df,
        x = "group", y = "value", bracket.style = "flat"
    )
    # Flat style: 1 shape per bracket
    expect_equal(length(result$shapes), 1)
})

test_that("create_stat_annotations displays symbols", {
    stats_df <- data.frame(
        group1 = "A", group2 = "B",
        p.value = 0.001, p.adj = 0.001, p.signif = "***",
        test = "wilcox.test", facet_level = NA_character_,
        x_level = NA_character_, stringsAsFactors = FALSE
    )
    result <- create_stat_annotations(
        stats_df = stats_df, fig = NULL, df = test_df,
        x = "group", y = "value", display = "symbol"
    )
    # Should display the significance symbol
    expect_equal(result$annotations[[1]]$text, "***")
})

test_that("create_stat_annotations includes omnibus annotation", {
    stats_df <- compute_pairwise_stats(
        df = test_df, x = "group", y = "value", test = "kruskal.test"
    )
    result <- create_stat_annotations(
        stats_df = stats_df, fig = NULL, df = test_df,
        x = "group", y = "value"
    )
    # Should have one annotation (omnibus label) and no shapes (no brackets)
    expect_equal(length(result$annotations), 1)
    expect_equal(length(result$shapes), 0)
    expect_true(grepl("Kruskal-Wallis", result$annotations[[1]]$text))
    # Uses paper coordinates
    expect_equal(result$annotations[[1]]$xref, "paper")
    expect_equal(result$annotations[[1]]$yref, "paper")
})

# ─── apply_stat_annotations ─────────────────────────────────────────────────

test_that("apply_stat_annotations adds shapes and annotations to figure", {
    fig <- list(x = list(layout = list(yaxis = list(range = c(0, 10)))))
    class(fig) <- "plotly"

    stat_result <- list(
        annotations = list(list(text = "**", x = 1.5, y = 12)),
        shapes = list(list(type = "line", x0 = 1, x1 = 2, y0 = 11, y1 = 11)),
        y.max = 13
    )

    result <- apply_stat_annotations(fig, stat_result)
    expect_equal(length(result$x$layout$shapes), 1)
    expect_equal(length(result$x$layout$annotations), 1)
    expect_equal(result$x$layout$yaxis$range[2], 13)
    # y-min should have a small buffer below the original value
    expect_true(result$x$layout$yaxis$range[1] < 0)
})

test_that("apply_stat_annotations preserves existing shapes", {
    fig <- list(x = list(layout = list(
        yaxis = list(range = c(0, 10)),
        shapes = list(list(type = "rect")),
        annotations = list(list(text = "existing"))
    )))
    class(fig) <- "plotly"

    stat_result <- list(
        annotations = list(list(text = "new")),
        shapes = list(list(type = "line")),
        y.max = 13
    )

    result <- apply_stat_annotations(fig, stat_result)
    expect_equal(length(result$x$layout$shapes), 2)
    expect_equal(length(result$x$layout$annotations), 2)
})

test_that("apply_stat_annotations updates all y-axes for faceted plots", {
    fig <- list(x = list(layout = list(
        yaxis = list(range = c(0, 10)),
        yaxis2 = list(range = c(0, 10))
    )))
    class(fig) <- "plotly"

    stat_result <- list(annotations = list(), shapes = list(list(type = "line")), y.max = 15)

    result <- apply_stat_annotations(fig, stat_result)
    expect_equal(result$x$layout$yaxis$range[2], 15)
    expect_equal(result$x$layout$yaxis2$range[2], 15)
    # Both axes should have bottom buffer
    expect_true(result$x$layout$yaxis$range[1] < 0)
    expect_true(result$x$layout$yaxis2$range[1] < 0)
})

test_that("apply_stat_annotations is a no-op for empty results", {
    fig <- list(x = list(layout = list(yaxis = list(range = c(0, 10)))))
    class(fig) <- "plotly"

    stat_result <- list(annotations = list(), shapes = list(), y.max = NULL)

    result <- apply_stat_annotations(fig, stat_result)
    expect_equal(result$x$layout$yaxis$range, c(0, 10))
})


test_that("apply_stat_annotations never lowers a requested y-axis maximum", {
    fig <- list(x = list(layout = list(yaxis = list(range = c(0, 20)))))
    class(fig) <- "plotly"
    stat_result <- list(annotations = list(), shapes = list(list(type = "line")), y.max = 8.7)

    # Turning statistics on must not undo a maximum the user chose.
    kept <- apply_stat_annotations(fig, stat_result, y.min = 0, y.max = 20)
    expect_equal(kept$x$layout$yaxis$range[2], 20)

    # ... but a maximum that would clip the brackets is raised to fit them.
    raised <- apply_stat_annotations(fig, stat_result, y.min = 0, y.max = 5)
    expect_equal(raised$x$layout$yaxis$range[2], 8.7)

    # With no maximum supplied, the figure's own top is what gets compared.
    from_fig <- apply_stat_annotations(fig, stat_result, y.min = 0)
    expect_equal(from_fig$x$layout$yaxis$range[2], 20)
})

test_that("stat_bracket_y_max reserves exactly the room the brackets are drawn in", {
    df <- example_iris
    x <- "Species"
    y <- "Sepal.Length"

    drawn_top <- function(hide.ns = FALSE, ...) {
        stats_df <- compute_pairwise_stats(df = df, x = x, y = y, test = "wilcox.test")
        fig <- plotly::plotly_build(
            plotly::plot_ly(data = df, x = df[[x]], y = df[[y]], type = "box")
        )
        create_stat_annotations(
            stats_df = stats_df, fig = fig, df = df, x = x, y = y,
            display = "symbol", hide.ns = hide.ns, ...
        )$y.max
    }

    expect_equal(
        stat_bracket_y_max(df, x = x, y = y),
        drawn_top()
    )
    # Geometry settings feed through the same formula the drawing code uses.
    expect_equal(
        stat_bracket_y_max(df, x = x, y = y, step.increase = 0.3, text.bump = 0.1),
        drawn_top(step.increase = 0.3, text.bump = 0.1)
    )
    # Hiding non-significant brackets must not leave room for ones never drawn.
    expect_equal(
        stat_bracket_y_max(df, x = x, y = y, hide.ns = TRUE),
        drawn_top(hide.ns = TRUE)
    )
    # A single explicit pair needs one level rather than two.
    expect_lt(
        stat_bracket_y_max(df, x = x, y = y, pairs = list(c("setosa", "virginica"))),
        stat_bracket_y_max(df, x = x, y = y)
    )
    expect_gt(stat_bracket_y_max(df, x = x, y = y), max(df[[y]]))
})

test_that("stat_bracket_y_max returns NULL when no brackets would be drawn", {
    df <- example_iris

    expect_null(stat_bracket_y_max(df[0, ], "Species", "Sepal.Length"))
    expect_null(stat_bracket_y_max(df, "nope", "Sepal.Length"))
    expect_null(stat_bracket_y_max(df, "Species", "nope"))
    expect_null(stat_bracket_y_max(df, "Species", ""))
    # One group leaves nothing to compare.
    expect_null(stat_bracket_y_max(df[df$Species == "setosa", ], "Species", "Sepal.Length"))
    # Omnibus tests produce a caption, not brackets.
    expect_null(stat_bracket_y_max(df, "Species", "Sepal.Length", test = "kruskal.test"))
})

test_that("the comparison layout matches what compute_pairwise_stats runs", {
    df <- example_iris

    stats_df <- compute_pairwise_stats(df = df, x = "Species", y = "Sepal.Length")
    layout <- .pairwise_layout(df, "Species", NULL, NULL)

    expect_equal(nrow(layout), nrow(stats_df))
    expect_equal(layout$group1, stats_df$group1)
    expect_equal(layout$group2, stats_df$group2)
})

test_that(".get_x_pos dodges only the groups present at that x category", {
    df <- rbind(
        data.frame(tissue = "blood", grp = c("A", "B", "C")),
        data.frame(tissue = "lung", grp = c("A", "B"))
    )
    df$grp <- factor(df$grp, levels = c("A", "B", "C"))
    x.order <- c("blood", "lung")

    pos <- function(group_label, x_level, dodge.width = 1) {
        VizModules:::.get_x_pos(
            group_label, x_level, x.order, "grp", df,
            x = "tissue", dodge.width = dodge.width
        )
    }

    # blood splits three ways...
    expect_equal(pos("A", "blood"), 1 - 1 / 3, tolerance = 1e-8)
    expect_equal(pos("B", "blood"), 1, tolerance = 1e-8)
    expect_equal(pos("C", "blood"), 1 + 1 / 3, tolerance = 1e-8)
    # ...lung only two, because C is not there. This is the #356 case: the old
    # formula used the global group count and put A/B at 1.6/2.0 instead.
    expect_equal(pos("A", "lung"), 2 - 0.25, tolerance = 1e-8)
    expect_equal(pos("B", "lung"), 2 + 0.25, tolerance = 1e-8)

    # dodge.width scales the offsets, as position_dodge() does.
    expect_equal(pos("A", "lung", dodge.width = 0.5), 2 - 0.125, tolerance = 1e-8)

    # Without a grouping column the label is an x category itself.
    expect_equal(
        VizModules:::.get_x_pos("lung", NA_character_, x.order, NULL, df),
        2
    )
})

test_that(".get_x_pos agrees with the positions .align_box_positions uses", {
    df <- withr::with_seed(7, {
        d <- rbind(
            data.frame(tissue = "blood", grp = rep(c("A", "B", "C"), each = 6)),
            data.frame(tissue = "lung", grp = rep(c("A", "B"), each = 6))
        )
        d$val <- rnorm(nrow(d))
        d$tissue <- factor(d$tissue, levels = c("blood", "lung"))
        d$grp <- factor(d$grp, levels = c("A", "B", "C"))
        d
    })

    fig <- plotly::ggplotly(plotthis::BoxPlot(
        df,
        x = "tissue", y = "val", group_by = "grp"
    ))
    aligned <- VizModules:::.align_box_positions(fig, dodge.width = 1, box.width = 0.8)

    for (trace in aligned$x$data) {
        if (is.null(trace$type) || trace$type != "box") next
        for (x_level in levels(df$tissue)) {
            expected <- VizModules:::.get_x_pos(
                trace$name, x_level, levels(df$tissue), "grp", df,
                x = "tissue", dodge.width = 1
            )
            drawn <- unique(as.numeric(trace$x))
            drawn <- drawn[round(drawn) == match(x_level, levels(df$tissue))]
            if (length(drawn) == 0) next
            expect_equal(drawn, expected, tolerance = 1e-8,
                info = sprintf("group %s at %s", trace$name, x_level)
            )
        }
    }
})

test_that("comparisons against a group absent from an x category are not drawn", {
    df <- withr::with_seed(3, {
        d <- rbind(
            data.frame(tissue = "blood", grp = rep(c("A", "B", "C"), each = 6)),
            data.frame(tissue = "lung", grp = rep(c("A", "B"), each = 6))
        )
        d$val <- rnorm(nrow(d))
        d$tissue <- factor(d$tissue, levels = c("blood", "lung"))
        d$grp <- factor(d$grp, levels = c("A", "B", "C"))
        d
    })

    # All-pairs comparisons include A vs C and B vs C at "lung", where C has no
    # data at all -- those come back with an NA p-value and no box to bracket.
    stats_df <- compute_pairwise_stats(df, x = "tissue", y = "val", group.by = "grp")
    expect_true(any(is.na(stats_df$p.adj)))

    packed <- VizModules:::.assign_bracket_levels(
        stats_df[, c("group1", "group2", "x_level", "facet_level", "p.adj", "p.signif")],
        levels(df$tissue), "grp", df, 0.025,
        x = "tissue", dodge.width = 1
    )

    expect_false(any(is.na(packed$x0_pos)))
    expect_false(any(is.na(packed$x1_pos)))
    expect_false(any(is.na(packed$y_level)))
    expect_false(any(packed$x_level == "lung" & packed$group2 == "C"))
    # The three comparisons at "blood" plus A vs B at "lung" remain.
    expect_equal(nrow(packed), 4)

    # And the whole path builds rather than erroring on the NA positions.
    fig <- dittoViz::yPlot(
        df,
        var = "val", group.by = "tissue", color.by = "grp",
        plots = c("boxplot", "jitter"), do.hover = TRUE
    )
    result <- create_stat_annotations(
        stats_df = stats_df, fig = fig, df = df, x = "tissue", y = "val",
        display = "p.adj", hide.ns = FALSE, group.by = "grp", dodge.width = 1
    )
    expect_length(result$annotations, 4)
})
