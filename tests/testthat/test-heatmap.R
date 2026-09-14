# Tests for the ComplexHeatmap module's internal helpers (R/ComplexHeatmap_Heatmap_helpers.R).
# Heatmap()-building tests are gated on the Bioconductor deps being installed, matching the
# module's own requireNamespace() guards; the pure helpers are tested unconditionally.

# ---- .heatmap_resolve_split() ----------------------------------------------------------------

test_that(".heatmap_resolve_split returns no-op km/split for 'None' or invalid input", {
    expect_equal(.heatmap_resolve_split("None", 3, 10), list(km = 1L, split = NULL))
    expect_equal(.heatmap_resolve_split("K-means", NA, 10), list(km = 1L, split = NULL))
    expect_equal(.heatmap_resolve_split("K-means", 1, 10), list(km = 1L, split = NULL))
    expect_equal(.heatmap_resolve_split(NA, 3, 10), list(km = 1L, split = NULL))
    expect_equal(.heatmap_resolve_split("Bogus", 3, 10), list(km = 1L, split = NULL))
})

test_that(".heatmap_resolve_split never returns both km and split set", {
    km_res <- .heatmap_resolve_split("K-means", 3, 10)
    expect_equal(km_res$km, 3L)
    expect_null(km_res$split)

    split_res <- .heatmap_resolve_split("Hierarchical", 3, 10)
    expect_equal(split_res$km, 1L)
    expect_equal(split_res$split, 3L)
})

test_that(".heatmap_resolve_split clamps k-means one below the dimension, not equal to it", {
    # Empirically confirmed against real ComplexHeatmap: row_km == nrow(mat) errors
    # ("number of cluster centres must lie between 1 and nrow(x)"), row_km == nrow(mat) - 1 does not.
    expect_equal(.heatmap_resolve_split("K-means", 32, 32)$km, 31L)
    expect_equal(.heatmap_resolve_split("K-means", 999, 32)$km, 31L)
    expect_equal(.heatmap_resolve_split("K-means", 31, 32)$km, 31L)
})

test_that(".heatmap_resolve_split clamps hierarchical splits to the dimension (inclusive)", {
    expect_equal(.heatmap_resolve_split("Hierarchical", 32, 32)$split, 32L)
    expect_equal(.heatmap_resolve_split("Hierarchical", 999, 32)$split, 32L)
})

test_that(".heatmap_resolve_split falls back to no-op when k-means clamps below 2", {
    expect_equal(.heatmap_resolve_split("K-means", 5, 2), list(km = 1L, split = NULL))
})

# ---- .heatmap_scale_matrix() ------------------------------------------------------------------

test_that(".heatmap_scale_matrix('none') returns the matrix unchanged", {
    mat <- matrix(1:6, nrow = 2)
    expect_identical(.heatmap_scale_matrix(mat, "none"), mat)
})

test_that(".heatmap_scale_matrix z-scores rows/columns correctly", {
    mat <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, byrow = TRUE)
    rownames(mat) <- c("r1", "r2")
    colnames(mat) <- c("c1", "c2", "c3")

    row_scaled <- .heatmap_scale_matrix(mat, "row")
    expect_equal(unname(row_scaled["r1", ]), as.numeric(scale(mat["r1", ])), tolerance = 1e-8)
    expect_equal(unname(row_scaled["r2", ]), as.numeric(scale(mat["r2", ])), tolerance = 1e-8)

    col_scaled <- .heatmap_scale_matrix(mat, "column")
    expected_col <- scale(mat)
    attr(expected_col, "scaled:center") <- NULL
    attr(expected_col, "scaled:scale") <- NULL
    expect_equal(col_scaled, expected_col, tolerance = 1e-8)

    expect_identical(dimnames(row_scaled), dimnames(mat))
    expect_identical(dimnames(col_scaled), dimnames(mat))
})

test_that(".heatmap_scale_matrix preserves genuine NA but pins zero-variance rows/columns to 0", {
    mat <- matrix(c(5, 5, 5, 1, 2, 3, 5, 5, 5), nrow = 3, byrow = TRUE)
    scaled <- .heatmap_scale_matrix(mat, "row")
    expect_equal(scaled[1, ], c(0, 0, 0))
    expect_equal(scaled[3, ], c(0, 0, 0))
    expect_false(anyNA(scaled))

    mat_na <- matrix(c(1, NA, 3, 4, 5, 6), nrow = 2, byrow = TRUE)
    scaled_na <- .heatmap_scale_matrix(mat_na, "row")
    expect_true(is.na(scaled_na[1, 2]))
    expect_false(anyNA(scaled_na[2, ]))
})

# ---- .heatmap_resolve_data() ------------------------------------------------------------------

test_that(".heatmap_resolve_data normalizes a plain data frame", {
    df <- data.frame(x = 1:3, y = 4:6)
    res <- .heatmap_resolve_data(df)
    expect_identical(res$matrix, df)
    expect_null(res$column_annotations)
})

test_that(".heatmap_resolve_data passes through the list(matrix=, column_annotations=) shape", {
    mat_df <- data.frame(x = 1:3)
    col_df <- data.frame(sample = "s1", condition = "A")
    res <- .heatmap_resolve_data(list(matrix = mat_df, column_annotations = col_df))
    expect_identical(res$matrix, mat_df)
    expect_identical(res$column_annotations, col_df)
})

# ---- .heatmap_default_colors() -------------------------------------------------------------

test_that(".heatmap_default_colors returns the blue/white/red triple", {
    expect_equal(.heatmap_default_colors(), c("#2166AC", "#F7F7F7", "#B2182B"))
})

# ---- .heatmap_annotation_widget_id() -----------------------------------------------------------

test_that(".heatmap_annotation_widget_id sanitizes the row name into a safe HTML id", {
    expect_equal(.heatmap_annotation_widget_id("row_ann_color", "row1"), "row_ann_color_row1")
    expect_equal(.heatmap_annotation_widget_id("row_ann_color", "row annotations1"), "row_ann_color_row_annotations1")
})

# ---- .heatmap_annotation_spec() --------------------------------------------------------------
# This drives whether the dynamically-rendered color widgets get rebuilt (and
# so reset) -- it must stay identical across a data change that doesn't
# actually alter an annotated column's type/levels, e.g. a "Data Table" filter
# that keeps every group, and must differ when it genuinely does.

test_that(".heatmap_annotation_spec is stable across a row-filter that keeps every level", {
    df_full <- data.frame(
        gene = paste0("g", 1:9),
        pathway = rep(c("A", "B", "C"), each = 3),
        score = 1:9
    )
    df_filtered <- df_full[c(1, 4, 7), ]  # one row per level -- still all 3 levels

    rows <- list(r1 = list(column = "pathway"))
    expect_identical(.heatmap_annotation_spec(rows, df_full), .heatmap_annotation_spec(rows, df_filtered))
})

test_that(".heatmap_annotation_spec changes when a level actually disappears", {
    df <- data.frame(pathway = rep(c("A", "B", "C"), each = 3))
    rows <- list(r1 = list(column = "pathway"))
    full_spec <- .heatmap_annotation_spec(rows, df)
    dropped_spec <- .heatmap_annotation_spec(rows, df[df$pathway != "C", , drop = FALSE])
    expect_false(identical(full_spec, dropped_spec))
})

test_that(".heatmap_annotation_spec for a numeric column ignores the actual values (row count only matters via existence)", {
    df <- data.frame(score = c(1, 2, 3, 4, 5))
    rows <- list(r1 = list(column = "score"))
    spec1 <- .heatmap_annotation_spec(rows, df)
    spec2 <- .heatmap_annotation_spec(rows, df[1:2, , drop = FALSE])
    expect_identical(spec1, spec2)
    expect_true(spec1$r1$numeric)
})

test_that(".heatmap_annotation_spec changes when a row's column selection changes", {
    df <- data.frame(pathway = c("A", "B"), score = c(1, 2))
    spec_pathway <- .heatmap_annotation_spec(list(r1 = list(column = "pathway")), df)
    spec_score <- .heatmap_annotation_spec(list(r1 = list(column = "score")), df)
    expect_false(identical(spec_pathway, spec_score))
})

test_that(".heatmap_annotation_spec returns an empty list, not NULL, for empty/invalid input", {
    expect_length(.heatmap_annotation_spec(NULL, data.frame(x = 1)), 0)
    expect_length(.heatmap_annotation_spec(list(), data.frame(x = 1)), 0)
    expect_length(.heatmap_annotation_spec(list(r1 = list(column = "nope")), data.frame(x = 1)), 0)
    expect_length(.heatmap_annotation_spec(list(r1 = list(column = "x")), NULL), 0)
})

# ---- .heatmap_annotation_col() ----------------------------------------------------------------

test_that(".heatmap_annotation_col builds a discrete mapping from explicit discrete_colors", {
    discrete <- c(A = "#FF0000", B = "#00FF00")
    mapping <- .heatmap_annotation_col(c("A", "B", "A", NA), discrete_colors = discrete)
    expect_type(mapping, "character")
    expect_setequal(names(mapping), c("A", "B"))
    expect_equal(unname(mapping[c("A", "B")]), c("#FF0000", "#00FF00"))
})

test_that(".heatmap_annotation_col falls back to grey for a level missing from discrete_colors", {
    mapping <- .heatmap_annotation_col(c("A", "B"), discrete_colors = c(A = "#FF0000"))
    expect_false(anyNA(mapping))
    expect_equal(unname(mapping["B"]), "#999999")
})

test_that(".heatmap_annotation_col builds a continuous mapping for numeric values", {
    mapping <- .heatmap_annotation_col(c(1, 2, 3, NA), low_color = "blue", mid_color = "white", high_color = "red")
    expect_true(is.function(mapping))
})

test_that(".heatmap_annotation_col handles a degenerate (constant) numeric range", {
    mapping <- .heatmap_annotation_col(c(5, 5, 5), low_color = "blue", mid_color = "white", high_color = "red")
    expect_true(is.function(mapping))
})

test_that(".heatmap_annotation_col returns NULL when there are no usable values or colors", {
    expect_null(.heatmap_annotation_col(c(NA_real_, NA_real_), low_color = "blue", mid_color = "white", high_color = "red"))
    expect_null(.heatmap_annotation_col(c(NA_character_, NA_character_), discrete_colors = c(A = "red")))
    expect_null(.heatmap_annotation_col(c(1, 2, 3)))  # numeric but no colors supplied
    expect_null(.heatmap_annotation_col(c("A", "B")))  # categorical but no colors supplied
})

# ---- .heatmap_build_annotation() --------------------------------------------------------------

# A color_lookup stand-in used across these tests, mirroring the closure built
# in ComplexHeatmap_HeatmapServer() (numeric -> low/mid/high, else -> a fixed
# discrete mapping) without needing a live Shiny `input`.
.test_color_lookup <- function(row_name, col, values) {
    if (is.numeric(values)) {
        .heatmap_annotation_col(values, low_color = "blue", mid_color = "white", high_color = "red")
    } else {
        levels <- unique(as.character(values[!is.na(values)]))
        .heatmap_annotation_col(values, discrete_colors = stats::setNames(
            grDevices::rainbow(length(levels)), levels
        ))
    }
}

test_that(".heatmap_build_annotation returns NULL for empty/invalid input without erroring", {
    df <- data.frame(gene = c("g1", "g2"), pathway = c("A", "B"))
    expect_null(.heatmap_build_annotation(NULL, df, c("g1", "g2"), NULL, "row", .test_color_lookup))
    expect_null(.heatmap_build_annotation(list(), df, c("g1", "g2"), NULL, "row", .test_color_lookup))
    expect_null(.heatmap_build_annotation(
        list(r = list(column = "nope", side = "Left")), df, c("g1", "g2"), NULL, "row", .test_color_lookup
    ))
    expect_null(.heatmap_build_annotation(list(r = list(column = "pathway", side = "Left")), NULL, c("g1", "g2"), NULL, "row", .test_color_lookup))
})

test_that(".heatmap_build_annotation builds a rowAnnotation/columnAnnotation object", {
    skip_if_not_installed("ComplexHeatmap")
    skip_if_not_installed("circlize")

    row_df <- data.frame(gene = c("g1", "g2", "g3"), pathway = c("A", "B", "A"), score = c(1, 2, 3))
    row_ann <- .heatmap_build_annotation(
        list(r = list(column = "pathway", side = "Left"), s = list(column = "score", side = "Left")),
        row_df, c("g1", "g2", "g3"), key_col = NULL, which = "row", color_lookup = .test_color_lookup
    )
    expect_s4_class(row_ann, "HeatmapAnnotation")

    col_df <- data.frame(sample = c("s1", "s2"), condition = c("Healthy", "Disease"))
    col_ann <- .heatmap_build_annotation(
        list(c = list(column = "condition", side = "Top")),
        col_df, c("s2", "s1"), key_col = "sample", which = "column", color_lookup = .test_color_lookup
    )
    expect_s4_class(col_ann, "HeatmapAnnotation")

    mat <- matrix(1:6, nrow = 3, dimnames = list(c("g1", "g2", "g3"), c("s1", "s2")))
    ht <- ComplexHeatmap::Heatmap(mat, left_annotation = row_ann, top_annotation = col_ann)
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    expect_no_error(ComplexHeatmap::draw(ht))
})

test_that(".heatmap_build_annotation carries each row's show_legend through per track", {
    df <- data.frame(gene = c("g1", "g2", "g3"), pathway = c("A", "B", "A"), score = c(1, 2, 3))

    ann <- .heatmap_build_annotation(
        list(
            r = list(column = "pathway", side = "Left", show_legend = FALSE),
            s = list(column = "score", side = "Left", show_legend = TRUE)
        ),
        df, c("g1", "g2", "g3"), key_col = NULL, which = "row", color_lookup = .test_color_lookup
    )
    expect_equal(unname(ann@anno_list$pathway@show_legend), FALSE)
    expect_equal(unname(ann@anno_list$score@show_legend), TRUE)
})

test_that(".heatmap_build_annotation shows a legend for a row predating show_legend", {
    df <- data.frame(gene = c("g1", "g2", "g3"), pathway = c("A", "B", "A"))

    ann <- .heatmap_build_annotation(
        list(r = list(column = "pathway", side = "Left")),
        df, c("g1", "g2", "g3"), key_col = NULL, which = "row", color_lookup = .test_color_lookup
    )
    expect_equal(unname(ann@anno_list$pathway@show_legend), TRUE)
})

test_that(".heatmap_build_annotation keeps show_legend aligned when a row is skipped", {
    df <- data.frame(gene = c("g1", "g2", "g3"), pathway = c("A", "B", "A"), score = c(1, 2, 3))

    # The unusable middle row must not shift `score`'s flag onto `pathway`.
    ann <- .heatmap_build_annotation(
        list(
            r = list(column = "pathway", side = "Left", show_legend = TRUE),
            skipped = list(column = "not_a_column", side = "Left", show_legend = FALSE),
            s = list(column = "score", side = "Left", show_legend = FALSE)
        ),
        df, c("g1", "g2", "g3"), key_col = NULL, which = "row", color_lookup = .test_color_lookup
    )
    expect_equal(unname(ann@anno_list$pathway@show_legend), TRUE)
    expect_equal(unname(ann@anno_list$score@show_legend), FALSE)
})

test_that(".heatmap_build_annotation, filtered by side, builds independent left/right annotations", {
    skip_if_not_installed("ComplexHeatmap")
    skip_if_not_installed("circlize")

    df <- data.frame(gene = c("g1", "g2", "g3"), pathway = c("A", "B", "A"), score = c(1, 2, 3))
    rows <- list(
        r1 = list(column = "pathway", side = "Left"),
        r2 = list(column = "score", side = "Right")
    )
    left_rows <- Filter(function(r) identical(r$side %||% "Left", "Left"), rows)
    right_rows <- Filter(function(r) identical(r$side, "Right"), rows)
    expect_named(left_rows, "r1")
    expect_named(right_rows, "r2")

    left_ann <- .heatmap_build_annotation(left_rows, df, df$gene, NULL, "row", .test_color_lookup)
    right_ann <- .heatmap_build_annotation(right_rows, df, df$gene, NULL, "row", .test_color_lookup)
    expect_s4_class(left_ann, "HeatmapAnnotation")
    expect_s4_class(right_ann, "HeatmapAnnotation")

    mat <- matrix(1:9, nrow = 3, dimnames = list(df$gene, c("s1", "s2", "s3")))
    ht <- ComplexHeatmap::Heatmap(mat, left_annotation = left_ann, right_annotation = right_ann)
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    expect_no_error(ComplexHeatmap::draw(ht))
})

test_that("a row without `side` (predating the feature) defaults to Left/Top", {
    rows <- list(r1 = list(column = "pathway"))
    left_rows <- Filter(function(r) identical(r$side %||% "Left", "Left"), rows)
    right_rows <- Filter(function(r) identical(r$side, "Right"), rows)
    expect_length(left_rows, 1)
    expect_length(right_rows, 0)
})

# ---- .heatmap_resolve_split(), "Annotation" method ----------------------------------------------

test_that(".heatmap_resolve_split splits on a single annotation column", {
    sv <- data.frame(pathway = c("A", "A", "B", "B"), stringsAsFactors = FALSE)
    res <- .heatmap_resolve_split("Annotation", NA, 4, sv)

    expect_equal(res$km, 1L)
    expect_s3_class(res$split, "data.frame")
    expect_equal(res$split$pathway, c("A", "A", "B", "B"))
})

test_that(".heatmap_resolve_split nests several annotation columns", {
    sv <- data.frame(
        condition = c("H", "H", "D", "D"),
        batch = c("B1", "B2", "B1", "B2"),
        stringsAsFactors = FALSE
    )
    # Four rows over four distinct combinations is one slice per row, which the
    # degenerate guard rejects.
    expect_equal(.heatmap_resolve_split("Annotation", NA, 4, sv), list(km = 1L, split = NULL))

    # Six rows over the same four combinations is a real grouping.
    sv6 <- sv[c(1, 1, 2, 2, 3, 4), , drop = FALSE]
    res6 <- .heatmap_resolve_split("Annotation", NA, 6, sv6)
    expect_equal(ncol(res6$split), 2L)
    expect_equal(nrow(res6$split), 6L)
})

test_that(".heatmap_resolve_split never returns both km and split for the annotation method", {
    sv <- data.frame(g = c("A", "A", "B", "B"), stringsAsFactors = FALSE)
    # A stale row_split_n left over from a previous K-means selection must not leak through.
    res <- .heatmap_resolve_split("Annotation", 5, 4, sv)

    expect_equal(res$km, 1L)
    expect_false(is.null(res$split))
})

test_that(".heatmap_resolve_split makes NA an explicit annotation slice", {
    sv <- data.frame(g = c("A", NA, "B", NA), stringsAsFactors = FALSE)
    res <- .heatmap_resolve_split("Annotation", NA, 4, sv)

    expect_equal(res$split$g, c("A", "NA", "B", "NA"))
})

test_that(".heatmap_resolve_split keeps a factor's level order for the slice order", {
    # Alphabetically these sort SJ10, SJ115, SJ2; the caller's level order is
    # the point, so it has to survive.
    sv <- data.frame(g = factor(
        c("SJ2", "SJ115", "SJ10", "SJ2"), levels = c("SJ2", "SJ10", "SJ115")
    ))
    res <- .heatmap_resolve_split("Annotation", NA, 4, sv)

    expect_s3_class(res$split$g, "factor")
    expect_equal(levels(res$split$g), c("SJ2", "SJ10", "SJ115"))
})

test_that(".heatmap_resolve_split drops unused factor levels, which would be empty slices", {
    sv <- data.frame(g = factor(
        c("A", "B", "A", "B"), levels = c("A", "B", "never_used")
    ))
    res <- .heatmap_resolve_split("Annotation", NA, 4, sv)

    expect_equal(levels(res$split$g), c("A", "B"))
})

test_that(".heatmap_resolve_split still groups NA when the column is a factor", {
    sv <- data.frame(g = factor(c("A", NA, "B", NA), levels = c("B", "A")))
    res <- .heatmap_resolve_split("Annotation", NA, 4, sv)

    expect_equal(as.character(res$split$g), c("A", "NA", "B", "NA"))
    # The caller's order is kept, with the NA group appended rather than sorted in.
    expect_equal(levels(res$split$g), c("B", "A", "NA"))
})

test_that(".heatmap_resolve_split falls back to no split for unusable annotation values", {
    no_split <- list(km = 1L, split = NULL)

    expect_equal(.heatmap_resolve_split("Annotation", NA, 4, NULL), no_split)
    # Wrong number of rows for the axis.
    expect_equal(.heatmap_resolve_split("Annotation", NA, 4, data.frame(g = c("A", "B"))), no_split)
    # A single group splits nothing.
    expect_equal(
        .heatmap_resolve_split("Annotation", NA, 3, data.frame(g = c("A", "A", "A"))),
        no_split
    )
    # Every row its own slice conveys nothing.
    expect_equal(
        .heatmap_resolve_split("Annotation", NA, 3, data.frame(g = c("A", "B", "C"))),
        no_split
    )
})

test_that(".heatmap_resolve_split leaves the numeric methods untouched", {
    sv <- data.frame(g = c("A", "A", "B", "B"), stringsAsFactors = FALSE)

    # split_values is ignored unless the method is "Annotation".
    expect_equal(.heatmap_resolve_split("K-means", 2, 10, sv), list(km = 2L, split = NULL))
    expect_equal(.heatmap_resolve_split("Hierarchical", 2, 10, sv), list(km = 1L, split = 2L))
    expect_equal(.heatmap_resolve_split("None", 2, 10, sv), list(km = 1L, split = NULL))
})

# ---- .heatmap_annotation_values() ---------------------------------------------------------------

test_that(".heatmap_annotation_values reads row values positionally", {
    df <- data.frame(
        gene = c("a", "b", "c"), pathway = c("P1", "P2", "P1"),
        stringsAsFactors = FALSE
    )

    expect_equal(
        .heatmap_annotation_values(df, "pathway", c("a", "b", "c"), key_col = NULL),
        c("P1", "P2", "P1")
    )
})

test_that(".heatmap_annotation_values matches column values through the key, and reorders", {
    meta <- data.frame(sample = c("S2", "S1"), condition = c("D", "H"), stringsAsFactors = FALSE)

    # key_values are in matrix order, which need not be the metadata's order.
    expect_equal(
        .heatmap_annotation_values(meta, "condition", c("S1", "S2"), key_col = "sample"),
        c("H", "D")
    )
})

test_that(".heatmap_annotation_values returns NULL rather than a misaligned vector", {
    df <- data.frame(g = c("A", "B", "C"), stringsAsFactors = FALSE)

    expect_null(.heatmap_annotation_values(NULL, "g", c("a"), NULL))
    expect_null(.heatmap_annotation_values(df, "missing", c("a", "b", "c"), NULL))
    expect_null(.heatmap_annotation_values(df, "", c("a", "b", "c"), NULL))
    # Row count does not match the axis.
    expect_null(.heatmap_annotation_values(df, "g", c("a", "b"), NULL))
    # Key column absent.
    expect_null(.heatmap_annotation_values(df, "g", c("a", "b", "c"), key_col = "nope"))
})

# ---- .heatmap_column_meta() ---------------------------------------------------------------------

test_that(".heatmap_column_meta synthesises `column` when there is no metadata", {
    meta <- .heatmap_column_meta(NULL, NULL, c("S1", "S2"))

    expect_equal(names(meta), "column")
    expect_equal(meta$column, c("S1", "S2"))
})

test_that(".heatmap_column_meta joins metadata in matrix column order", {
    col_df <- data.frame(
        sample = c("S3", "S1", "S2"),
        condition = c("D", "H", "H"),
        stringsAsFactors = FALSE
    )
    meta <- .heatmap_column_meta(col_df, "sample", c("S1", "S2", "S3"))

    expect_equal(meta$column, c("S1", "S2", "S3"))
    expect_equal(meta$condition, c("H", "H", "D"))
})

test_that(".heatmap_column_meta lets a real `column` field win over the synthetic one", {
    col_df <- data.frame(
        sample = c("S1", "S2"),
        column = c("mine", "also mine"),
        stringsAsFactors = FALSE
    )
    meta <- .heatmap_column_meta(col_df, "sample", c("S1", "S2"))

    expect_equal(meta$column, c("mine", "also mine"))
    expect_equal(sum(names(meta) == "column"), 1L)
})

test_that(".heatmap_column_meta falls back to names alone when the key is unusable", {
    col_df <- data.frame(sample = c("S1", "S2"), condition = c("H", "D"), stringsAsFactors = FALSE)

    expect_equal(names(.heatmap_column_meta(col_df, "nope", c("S1", "S2"))), "column")
    expect_equal(names(.heatmap_column_meta(col_df, "", c("S1", "S2"))), "column")
    expect_equal(names(.heatmap_column_meta(col_df, NULL, c("S1", "S2"))), "column")
})

test_that(".heatmap_column_meta handles no selected columns", {
    expect_equal(nrow(.heatmap_column_meta(NULL, NULL, character(0))), 0L)
    expect_equal(nrow(.heatmap_column_meta(NULL, NULL, NULL)), 0L)
})

# ---- .heatmap_apply_filter() --------------------------------------------------------------------

test_that(".heatmap_apply_filter keeps everything for a blank expression", {
    df <- data.frame(v = 1:3)

    for (blank in list(NULL, "", "   ", NA_character_)) {
        res <- .heatmap_apply_filter(blank, df, 3)
        expect_equal(res$status, "empty")
        expect_equal(res$keep, rep(TRUE, 3))
    }
})

test_that(".heatmap_apply_filter evaluates a valid expression", {
    df <- data.frame(v = c(1, 5, 9), g = c("a", "b", "a"), stringsAsFactors = FALSE)

    res <- .heatmap_apply_filter("v > 4", df, 3)
    expect_equal(res$status, "ok")
    expect_equal(res$keep, c(FALSE, TRUE, TRUE))

    expect_equal(.heatmap_apply_filter('g == "a"', df, 3)$keep, c(TRUE, FALSE, TRUE))
})

test_that(".heatmap_apply_filter treats NA as drop, not keep", {
    df <- data.frame(v = c(1, NA, 9))

    res <- .heatmap_apply_filter("v > 4", df, 3)
    expect_equal(res$status, "ok")
    expect_equal(res$keep, c(FALSE, FALSE, TRUE))
})

test_that(".heatmap_apply_filter reports invalid separately from empty", {
    df <- data.frame(v = 1:3)

    # A blocked call, an unknown symbol, and an unparseable string.
    for (bad in c('system("id")', "nope > 1", "v >")) {
        res <- .heatmap_apply_filter(bad, df, 3)
        expect_equal(res$status, "invalid")
        expect_null(res$keep)
    }
})

test_that(".heatmap_apply_filter rejects a non-logical or wrong-length result", {
    df <- data.frame(v = 1:3)

    # Scalar rather than one value per row.
    expect_equal(.heatmap_apply_filter("is.null(v)", df, 3)$status, "invalid")
    # Numeric rather than logical.
    expect_equal(.heatmap_apply_filter("v + 1", df, 3)$status, "invalid")
})

test_that(".heatmap_apply_filter does not leak safe_eval_filter's warning", {
    df <- data.frame(v = 1:3)

    expect_no_warning(.heatmap_apply_filter('system("id")', df, 3))
})

# ---- Per-annotation label side and size --------------------------------------------------------

.label_test_df <- data.frame(
    g = c("a", "b", "a", "b"), n = c(1, 2, 3, 4), stringsAsFactors = FALSE
)
.label_test_keys <- c("r1", "r2", "r3", "r4")
.label_test_lookup <- function(row_name, col, values) {
    if (is.numeric(values)) {
        .heatmap_annotation_col(values, "blue", "white", "red")
    } else {
        .heatmap_annotation_col(values, discrete_colors = c(a = "#111111", b = "#222222"))
    }
}
.build_label_ann <- function(rows, which = "row") {
    .heatmap_build_annotation(
        rows, .label_test_df, .label_test_keys, NULL, which, .label_test_lookup
    )
}

test_that("each annotation track gets its own label side and size", {
    skip_if_not_installed("ComplexHeatmap")

    ann <- .build_label_ann(list(
        r1 = list(column = "g", label_side = "Top", label_size = 14),
        r2 = list(column = "n", label_side = "Bottom", label_size = 8)
    ))

    expect_equal(ann@anno_list[["g"]]@name_param$side, "top")
    expect_equal(ann@anno_list[["g"]]@name_param$gp$fontsize, 14)
    expect_equal(ann@anno_list[["n"]]@name_param$side, "bottom")
    expect_equal(ann@anno_list[["n"]]@name_param$gp$fontsize, 8)
})

test_that("column annotations take left/right label sides", {
    skip_if_not_installed("ComplexHeatmap")

    ann <- .heatmap_build_annotation(
        list(
            r1 = list(column = "g", label_side = "Left", label_size = 12),
            r2 = list(column = "n", label_side = "Right", label_size = 9)
        ),
        .label_test_df, .label_test_keys, NULL, "column", .label_test_lookup
    )

    expect_equal(ann@anno_list[["g"]]@name_param$side, "left")
    expect_equal(ann@anno_list[["n"]]@name_param$side, "right")
})

test_that("a label side from the wrong axis falls back instead of erroring", {
    skip_if_not_installed("ComplexHeatmap")

    # "left" is a column-annotation side; ComplexHeatmap errors on it for a row
    # annotation, so it must not reach the constructor.
    expect_no_error(ann <- .build_label_ann(list(r1 = list(column = "g", label_side = "Left"))))
    expect_equal(ann@anno_list[["g"]]@name_param$side, "bottom")

    expect_no_error(ann2 <- .heatmap_build_annotation(
        list(r1 = list(column = "g", label_side = "Top")),
        .label_test_df, .label_test_keys, NULL, "column", .label_test_lookup
    ))
    expect_equal(ann2@anno_list[["g"]]@name_param$side, "right")
})

test_that("rows saved before the label fields existed still build", {
    skip_if_not_installed("ComplexHeatmap")

    # A `defaults` list written against the old two-field row_spec.
    ann <- .build_label_ann(list(r1 = list(column = "g", side = "Left")))

    expect_equal(ann@anno_list[["g"]]@name_param$side, "bottom")
    expect_equal(ann@anno_list[["g"]]@name_param$gp$fontsize, 10)
})

test_that("an unusable label size falls back rather than propagating NA", {
    skip_if_not_installed("ComplexHeatmap")

    for (bad in list("abc", NA, -1, 0, NULL)) {
        ann <- .build_label_ann(list(r1 = list(column = "g", label_size = bad)))
        expect_equal(ann@anno_list[["g"]]@name_param$gp$fontsize, 10)
    }
})

test_that("a skipped row does not shift the labels of the rows after it", {
    skip_if_not_installed("ComplexHeatmap")

    # The first row names a column that is not in the data, so it is dropped --
    # its label values must be dropped with it rather than sliding onto `g`.
    ann <- .build_label_ann(list(
        r1 = list(column = "not_a_column", label_side = "Top", label_size = 30),
        r2 = list(column = "g", label_side = "Bottom", label_size = 14)
    ))

    expect_equal(names(ann@anno_list), "g")
    expect_equal(ann@anno_list[["g"]]@name_param$side, "bottom")
    expect_equal(ann@anno_list[["g"]]@name_param$gp$fontsize, 14)
})

test_that("the annotation UI exposes label side and size per row", {
    dat <- list(matrix = example_heatmap_matrix, column_annotations = example_heatmap_column_data)
    html <- paste(as.character(ComplexHeatmap_HeatmapInputsUI("h", dat)), collapse = "")

    expect_true(grepl("label_side", html, fixed = TRUE))
    expect_true(grepl("label_size", html, fixed = TRUE))
    expect_true(grepl("Label Side", html, fixed = TRUE))
    expect_true(grepl("Label Size", html, fixed = TRUE))
})

# ---- Filter pipeline in the module server ------------------------------------------------------
#
# testServer cannot drive this module's *output* (the interactive widget needs a real client), but
# it can drive the filter reactives, which is where the behaviour worth pinning lives.

test_that("the filter reactives narrow the matrix, and are debounced", {
    skip_if_not_installed("ComplexHeatmap")
    skip_if_not_installed("InteractiveComplexHeatmap")
    skip_if_not_installed("circlize")

    df <- example_heatmap_matrix
    sample_cols <- setdiff(names(df), c("gene", "pathway", "mean_expression"))

    shiny::testServer(ComplexHeatmap_HeatmapServer, args = list(data = shiny::reactive(df)), {
        session$setInputs(
            matrix.cols = sample_cols, rowname.col = "gene",
            row_filter = "", column_filter = "", column_key = ""
        )
        expect_equal(nrow(filtered_matrix_data()), nrow(df))
        expect_equal(length(filtered_cols()), length(sample_cols))

        # Typing an expression one keystroke at a time must not take effect until
        # the user pauses -- otherwise every intermediate, unparseable state would
        # redraw the heatmap.
        target <- 'pathway == "Immune"'
        for (i in seq_len(nchar(target))) {
            session$setInputs(row_filter = substr(target, 1, i))
        }
        expect_equal(nrow(filtered_matrix_data()), nrow(df))

        session$elapse(800)
        expect_equal(nrow(filtered_matrix_data()), sum(df$pathway == "Immune"))

        # The column filter sees the synthetic `column` field even with no metadata table.
        session$setInputs(column_filter = 'startsWith(column, "Healthy")')
        session$elapse(800)
        expect_equal(length(filtered_cols()), sum(startsWith(sample_cols, "Healthy")))
    })
})

test_that("a column filter can reach the sample metadata", {
    skip_if_not_installed("ComplexHeatmap")
    skip_if_not_installed("InteractiveComplexHeatmap")
    skip_if_not_installed("circlize")

    df <- example_heatmap_matrix
    col_df <- example_heatmap_column_data
    sample_cols <- setdiff(names(df), c("gene", "pathway", "mean_expression"))
    dat <- list(matrix = df, column_annotations = col_df)

    shiny::testServer(
        ComplexHeatmap_HeatmapServer, args = list(data = shiny::reactive(dat)),
        {
            session$setInputs(
                matrix.cols = sample_cols, rowname.col = "gene",
                row_filter = "", column_filter = "", column_key = "sample"
            )
            session$setInputs(column_filter = 'condition == "Disease"')
            session$elapse(800)

            expected <- col_df$sample[col_df$condition == "Disease"]
            expect_setequal(filtered_cols(), intersect(sample_cols, as.character(expected)))
        }
    )
})

test_that("an invalid filter expression does not silently plot unfiltered data", {
    skip_if_not_installed("ComplexHeatmap")
    skip_if_not_installed("InteractiveComplexHeatmap")
    skip_if_not_installed("circlize")

    df <- example_heatmap_matrix
    sample_cols <- setdiff(names(df), c("gene", "pathway", "mean_expression"))

    shiny::testServer(ComplexHeatmap_HeatmapServer, args = list(data = shiny::reactive(df)), {
        session$setInputs(
            matrix.cols = sample_cols, rowname.col = "gene",
            row_filter = "", column_filter = "", column_key = ""
        )
        # A blocked call must raise rather than fall through to the whole matrix.
        session$setInputs(row_filter = 'system("id")')
        session$elapse(800)
        expect_error(filtered_matrix_data())

        # So must a filter that matches nothing.
        session$setInputs(row_filter = 'pathway == "NoSuchPathway"')
        session$elapse(800)
        expect_error(filtered_matrix_data())

        # Clearing it recovers.
        session$setInputs(row_filter = "")
        session$elapse(800)
        expect_equal(nrow(filtered_matrix_data()), nrow(df))
    })
})

# ---- End-to-end module smoke test --------------------------------------------------------------

test_that("ComplexHeatmap_HeatmapInputsUI builds for both data shapes", {
    df <- example_heatmap_matrix
    col_df <- example_heatmap_column_data

    expect_no_error(ComplexHeatmap_HeatmapInputsUI("h", df))
    expect_no_error(ComplexHeatmap_HeatmapInputsUI("h", list(matrix = df, column_annotations = col_df)))
})

# The app is a thin createModuleApp() wrapper, so its data_list is what to assert on.
.app_data_list <- function(app) {
    get("data_list", envir = environment(app$serverFuncSource()))
}

test_that("ComplexHeatmap_HeatmapApp() defaults to the matrix *and* its metadata", {
    skip_if_not_installed("ComplexHeatmap")
    skip_if_not_installed("InteractiveComplexHeatmap")
    skip_if_not_installed("circlize")

    # Without a metadata table the column annotation/split/filter features are
    # inert, so a bare app() would demo only half the module.
    entry <- .app_data_list(ComplexHeatmap_HeatmapApp())[[1]]

    expect_true(is.list(entry) && !is.data.frame(entry))
    expect_s3_class(entry$matrix, "data.frame")
    expect_s3_class(entry$column_annotations, "data.frame")
    expect_equal(nrow(entry$column_annotations), nrow(example_heatmap_column_data))
    # The join is useless without a key, so it must be seeded too.
    app_env <- environment(ComplexHeatmap_HeatmapApp()$serverFuncSource())
    expect_equal(get("defaults", envir = app_env)$column_key, "sample")
})

test_that("ComplexHeatmap_HeatmapApp() does not attach the bundled metadata to a caller's data", {
    skip_if_not_installed("ComplexHeatmap")
    skip_if_not_installed("InteractiveComplexHeatmap")
    skip_if_not_installed("circlize")

    own <- data.frame(id = c("g1", "g2"), S1 = c(1, 2), S2 = c(3, 4), stringsAsFactors = FALSE)
    entry <- .app_data_list(ComplexHeatmap_HeatmapApp(data_list = list(mine = own)))[[1]]

    # A caller's own matrix must stay a bare data frame -- joining the bundled
    # metadata onto it would match on sample names that do not exist there.
    expect_s3_class(entry, "data.frame")
    expect_equal(entry, own)
})

test_that("ComplexHeatmap_HeatmapApp() attaches supplied column_data to the caller's matrix", {
    skip_if_not_installed("ComplexHeatmap")
    skip_if_not_installed("InteractiveComplexHeatmap")
    skip_if_not_installed("circlize")

    entry <- .app_data_list(ComplexHeatmap_HeatmapApp(
        data_list = list(m = example_heatmap_matrix),
        column_data = example_heatmap_column_data
    ))[[1]]

    expect_equal(names(entry), c("matrix", "column_annotations"))
    expect_equal(entry$matrix, example_heatmap_matrix)
})

# ---- Default annotations and color resolution --------------------------------------------------

test_that("ComplexHeatmap_HeatmapServer renders default annotations on startup without client round-trip", {
    skip_if_not_installed("ComplexHeatmap")
    skip_if_not_installed("InteractiveComplexHeatmap")
    skip_if_not_installed("circlize")

    df <- example_heatmap_matrix
    col_df <- example_heatmap_column_data
    sample_cols <- setdiff(names(df), c("gene", "pathway", "mean_expression"))
    dat <- list(matrix = df, column_annotations = col_df)

    def_rows <- list(
        row1 = list(column = "pathway", side = "Left"),
        row2 = list(column = "mean_expression", side = "Right")
    )
    def_cols <- list(
        col1 = list(column = "condition", side = "Top")
    )

    custom_pathway_colors <- c("Cell Cycle" = "#FF0000", Immune = "#00FF00", Metabolic = "#0000FF")

    defaults <- list(
        matrix.cols = sample_cols,
        rowname.col = "gene",
        column_key = "sample",
        row_annotations = def_rows,
        column_annotations = def_cols,
        pathway = custom_pathway_colors
    )

    shiny::testServer(
        ComplexHeatmap_HeatmapServer,
        args = list(data = shiny::reactive(dat), defaults = defaults),
        {
            session$setInputs(
                matrix.cols = sample_cols,
                rowname.col = "gene",
                column_key = "sample",
                row_filter = "",
                column_filter = "",
                auto.update = TRUE
            )

            # Annotation specs should immediately resolve from defaults
            row_spec <- row_annotation_spec()
            expect_length(row_spec, 2)
            expect_equal(row_spec[["row1"]]$column, "pathway")
            expect_false(row_spec[["row1"]]$numeric)
            expect_setequal(row_spec[["row1"]]$levels, c("Cell Cycle", "Immune", "Metabolic"))

            expect_equal(row_spec[["row2"]]$column, "mean_expression")
            expect_true(row_spec[["row2"]]$numeric)

            col_spec <- column_annotation_spec()
            expect_length(col_spec, 1)
            expect_equal(col_spec[["col1"]]$column, "condition")
            expect_false(col_spec[["col1"]]$numeric)
            expect_setequal(col_spec[["col1"]]$levels, c("Disease", "Healthy"))

            # row_annotation_colors_ui should render immediately
            rendered_row_ui <- output$row_annotation_colors_ui
            expect_false(is.null(rendered_row_ui))
            html_ui <- paste(as.character(rendered_row_ui), collapse = "")
            expect_true(grepl("multi-color-picker", html_ui))
            expect_true(grepl("pathway", html_ui))
            expect_true(grepl("#FF0000", html_ui))

            # build_heatmap should construct a Heatmap with annotations without skipping them
            drawn_ht <- build_heatmap()
            expect_s4_class(drawn_ht, "HeatmapList")
            ht_obj <- drawn_ht@ht_list[[1]]
            expect_false(is.null(ht_obj@left_annotation))
            expect_false(is.null(ht_obj@right_annotation))
            expect_false(is.null(ht_obj@top_annotation))

            # User updates row annotations dynamically via input
            session$setInputs(
                row_annotations = list(
                    row_annotations1 = list(column = "pathway", side = "Right")
                )
            )

            # Left annotation should now be NULL, right annotation should have pathway
            drawn_ht2 <- build_heatmap()
            ht_obj2 <- drawn_ht2@ht_list[[1]]
            expect_null(ht_obj2@left_annotation)
            expect_false(is.null(ht_obj2@right_annotation))
        }
    )
})

test_that("the Filter tab carries its guidance in tooltips, not as on-screen text", {
    dat <- list(matrix = example_heatmap_matrix, column_annotations = example_heatmap_column_data)
    html <- paste(as.character(ComplexHeatmap_HeatmapInputsUI("h", dat)), collapse = "")

    expect_true(grepl("h-row_filter", html, fixed = TRUE))
    expect_true(grepl("h-column_filter", html, fixed = TRUE))
    # helpText() renders a help-block div; the guidance moved into the tooltips.
    expect_false(grepl("help-block", html, fixed = TRUE))

    # Each tooltip must still stand alone: purpose, available fields, vocabulary.
    expect_true(grepl("Leave blank to keep every row", html, fixed = TRUE))
    expect_true(grepl("Leave blank to keep every column", html, fixed = TRUE))
    expect_true(grepl("mean_expression", html, fixed = TRUE))
    # `column` is synthesised, and the metadata fields join alongside it.
    expect_true(grepl("Fields: column, sample, condition, batch", html, fixed = TRUE))
    expect_equal(
        lengths(regmatches(html, gregexpr("startsWith/endsWith", html, fixed = TRUE))), 2L
    )
})

test_that("ComplexHeatmap_HeatmapOutputUI passes compact through to the underlying widget", {
    skip_if_not_installed("InteractiveComplexHeatmap")

    expect_no_error(ComplexHeatmap_HeatmapOutputUI("h", compact = TRUE))
    expect_no_error(ComplexHeatmap_HeatmapOutputUI("h", compact = TRUE, layout = "1|(2-3)"))

    html <- as.character(ComplexHeatmap_HeatmapOutputUI("h", compact = TRUE))
    # Compact mode floats the info panel rather than giving it a static area.
    expect_true(grepl("float", html, ignore.case = TRUE))
})

test_that("a floating info panel is re-parked so it cannot widen the page", {
    skip_if_not_installed("InteractiveComplexHeatmap")

    # InteractiveComplexHeatmap parks the detached panel at right: -10000px,
    # which extends the host document's scrollable width by ~10,000px.
    for (ui in list(
        ComplexHeatmap_HeatmapOutputUI("mod", compact = TRUE),
        ComplexHeatmap_HeatmapOutputUI("mod", output_ui_float = TRUE),
        ComplexHeatmap_HeatmapInfoOutputUI("mod", output_ui_float = TRUE)
    )) {
        deps <- vapply(htmltools::findDependencies(ui), function(d) d$name, character(1))
        expect_true("vizmodules-heatmap-float-output" %in% deps)
        expect_true(grepl(
            'VizModules.heatmapFloatOutput({"id":"mod_Heatmap"});',
            as.character(ui), fixed = TRUE
        ))
    }

    # A static info panel is already contained by its own layout position.
    for (off in list(
        ComplexHeatmap_HeatmapOutputUI("mod"),
        ComplexHeatmap_HeatmapInfoOutputUI("mod")
    )) {
        deps <- vapply(htmltools::findDependencies(off), function(d) d$name, character(1))
        expect_false("vizmodules-heatmap-float-output" %in% deps)
        expect_false(grepl("heatmapFloatOutput", as.character(off), fixed = TRUE))
    }
})

test_that(".heatmap_widget_id mirrors InteractiveComplexHeatmap's id validation", {
    expect_equal(.heatmap_widget_id("heatmap-Heatmap"), "heatmap_Heatmap")
    expect_equal(.heatmap_widget_id("a b.c-d"), "a_b_c_d")
    expect_equal(.heatmap_widget_id("Heatmap"), "Heatmap")
    expect_equal(.heatmap_widget_id("1heatmap"), "v_1heatmap")
})

test_that("the heatmap output UIs fit their container's width unless told not to", {
    skip_if_not_installed("InteractiveComplexHeatmap")

    ui <- ComplexHeatmap_HeatmapOutputUI("mod")
    deps <- vapply(htmltools::findDependencies(ui), function(d) d$name, character(1))
    expect_true("vizmodules-heatmap-fit-width" %in% deps)

    html <- as.character(ui)
    expect_true(grepl('"root":".mod_Heatmap_widget"', html, fixed = TRUE))
    expect_true(grepl('"panels":["heatmap","sub_heatmap"]', html, fixed = TRUE))
    expect_true(grepl('"output":true', html, fixed = TRUE))

    main <- as.character(ComplexHeatmap_HeatmapMainOutputUI("mod"))
    expect_true(grepl('"root":"#mod_Heatmap_heatmap_group"', main, fixed = TRUE))
    expect_true(grepl('"panels":["heatmap"]', main, fixed = TRUE))

    sub <- as.character(ComplexHeatmap_HeatmapSubOutputUI("mod"))
    expect_true(grepl('"root":"#mod_Heatmap_sub_heatmap_group"', sub, fixed = TRUE))
    expect_true(grepl('"panels":["sub_heatmap"]', sub, fixed = TRUE))

    for (off in list(
        ComplexHeatmap_HeatmapOutputUI("mod", fit.width = FALSE),
        ComplexHeatmap_HeatmapMainOutputUI("mod", fit.width = FALSE),
        ComplexHeatmap_HeatmapSubOutputUI("mod", fit.width = FALSE)
    )) {
        expect_false(grepl("heatmapFitWidth", as.character(off), fixed = TRUE))
    }
})

test_that(".heatmap_widget_id matches the key InteractiveComplexHeatmap registers under", {
    skip_if_not_installed("InteractiveComplexHeatmap")
    skip_if_not_installed("ComplexHeatmap")

    # The module server looks the registered heatmap up before calling
    # makeInteractiveComplexHeatmap(). InteractiveComplexHeatmap keys that
    # registry by validate_heatmap_id(), which rewrites every non-word character
    # to "_", so a raw namespaced id never matches and the heatmap is silently
    # never drawn. These two normalisations have to agree.
    for (id in c("mod-Heatmap", "outer-inner-Heatmap", "plain_id", "1leading")) {
        expect_identical(
            .heatmap_widget_id(id),
            getFromNamespace("validate_heatmap_id", "InteractiveComplexHeatmap")(id)
        )
    }
})

test_that("a namespaced heatmap is findable in the registry via .heatmap_widget_id", {
    skip_if_not_installed("InteractiveComplexHeatmap")
    skip_if_not_installed("ComplexHeatmap")

    h_id <- shiny::NS(shiny::NS("methyl_concordance")("pair_heatmap"))("Heatmap")
    invisible(InteractiveComplexHeatmap::originalHeatmapOutput(h_id))
    registry <- getFromNamespace("shiny_env", "InteractiveComplexHeatmap")$heatmap

    expect_false(is.null(registry[[.heatmap_widget_id(h_id)]]))
    # The bug: the raw id is not a key, so the module server's guard returned early.
    expect_null(registry[[h_id]])
})

test_that("heatmap_fit_width attaches the fit script to a hand-built widget", {
    skip_if_not_installed("InteractiveComplexHeatmap")

    ui <- heatmap_fit_width(
        InteractiveComplexHeatmap::InteractiveComplexHeatmapOutput(
            heatmap_id = "ovw_modality_ht", width1 = 1480, height1 = 500
        ),
        heatmap_id = "ovw_modality_ht"
    )
    rendered <- htmltools::renderTags(ui)
    html <- as.character(rendered$html)

    # The root is the class InteractiveComplexHeatmapOutput() puts on the widget,
    # so the script measures the thing that is actually on the page.
    expect_true(grepl(
        '"root":".ovw_modality_ht_widget"', html, fixed = TRUE
    ))
    expect_true(grepl('"panels":["heatmap","sub_heatmap"]', html, fixed = TRUE))
    expect_true(grepl('"output":true', html, fixed = TRUE))
    expect_true(grepl("ovw_modality_ht_widget", html, fixed = TRUE))
    expect_true("vizmodules-heatmap-fit-width" %in%
        vapply(rendered$dependencies, function(d) d$name, character(1)))
})

test_that("heatmap_fit_width honours a narrowed panel set", {
    skip_if_not_installed("InteractiveComplexHeatmap")

    html <- as.character(heatmap_fit_width(
        InteractiveComplexHeatmap::InteractiveComplexHeatmapOutput(heatmap_id = "ht"),
        heatmap_id = "ht", panels = "heatmap", output = FALSE
    ))
    expect_true(grepl('"panels":["heatmap"]', html, fixed = TRUE))
    expect_true(grepl('"output":false', html, fixed = TRUE))
})

test_that("ComplexHeatmap_HeatmapStaticOutputUI renders a plain plot output", {
    skip_if_not_installed("ComplexHeatmap")

    html <- as.character(ComplexHeatmap_HeatmapStaticOutputUI("h", resizable = FALSE))
    # The Figure Builder's card CSS targets a direct-child .shiny-plot-output,
    # so the element must not be wrapped when resizing is off.
    expect_true(grepl('id="h-HeatmapStatic"', html, fixed = TRUE))
    expect_true(grepl("shiny-plot-output", html, fixed = TRUE))
    expect_true(grepl("width:100%", html, fixed = TRUE))
    expect_true(grepl("height:100%", html, fixed = TRUE))

    # None of the InteractiveComplexHeatmap chrome comes along.
    expect_false(grepl("_heatmap_resize", html, fixed = TRUE))
    expect_false(grepl("_heatmap_control", html, fixed = TRUE))

    # Unlike the interactive output, `resizable` is honoured here.
    expect_true(grepl(
        "resizable",
        as.character(ComplexHeatmap_HeatmapStaticOutputUI("h")),
        fixed = TRUE
    ))

    expect_true(grepl(
        'style="width:600px;height:400px;"',
        as.character(ComplexHeatmap_HeatmapStaticOutputUI(
            "h",
            resizable = FALSE, width = "600px", height = "400px"
        )),
        fixed = TRUE
    ))
})

test_that("ComplexHeatmap_HeatmapServer exposes a vector_svg renderer for figure export", {
    skip_if_not_installed("ComplexHeatmap")
    skip_if_not_installed("InteractiveComplexHeatmap")
    skip_if_not_installed("circlize")

    df <- example_heatmap_matrix
    sample_cols <- setdiff(names(df), c("gene", "pathway", "mean_expression"))
    dat <- list(matrix = df, column_annotations = example_heatmap_column_data)

    shiny::testServer(
        ComplexHeatmap_HeatmapServer,
        args = list(
            data = shiny::reactive(dat),
            defaults = list(
                matrix.cols = sample_cols, rowname.col = "gene",
                column_key = "sample"
            )
        ),
        {
            session$setInputs(
                matrix.cols = sample_cols, rowname.col = "gene",
                column_key = "sample", row_filter = "", column_filter = "",
                auto.update = TRUE
            )

            render <- attr(session$getReturned(), "vector_svg")
            expect_true(is.function(render))

            svg <- render(width = 480, height = 380)
            expect_true(is.character(svg))
            # The panel is sized in pixels so it lands at the size the canvas
            # measured, whatever a reader makes of the device's own units.
            expect_true(startsWith(svg, "<svg "))
            expect_true(grepl('width="480"', svg, fixed = TRUE))
            expect_true(grepl('height="380"', svg, fixed = TRUE))
            # Really the heatmap, not a blank device.
            expect_true(grepl(df$gene[1], svg, fixed = TRUE))
            # Ids are namespaced per widget so two panels cannot collide.
            ids <- unlist(regmatches(svg, gregexpr("id='[^']+", svg)))
            if (length(ids)) {
                expect_true(all(grepl("Heatmap-", ids, fixed = TRUE)))
            }
        }
    )
})

test_that("an exported heatmap keeps its cells at the size they were drawn", {
    skip_if_not_installed("ComplexHeatmap")
    skip_if_not_installed("InteractiveComplexHeatmap")
    skip_if_not_installed("circlize")

    df <- example_heatmap_matrix
    sample_cols <- setdiff(names(df), c("gene", "pathway", "mean_expression"))
    dat <- list(matrix = df, column_annotations = example_heatmap_column_data)

    # Annotation tracks on both axes plus a split on each: four legends' worth of
    # furniture, all of it sized in absolute points. This is the arrangement that
    # exposed the export being drawn on a smaller canvas than the panel -- the
    # furniture kept its size and the cells were squeezed to a fraction of a
    # point, so the exported figure looked nothing like the one on screen.
    defaults <- list(
        matrix.cols = sample_cols, rowname.col = "gene", column_key = "sample",
        row_annotations = list(r1 = list(column = "pathway", side = "Left")),
        column_annotations = list(
            c1 = list(column = "condition", side = "Top"),
            c2 = list(column = "batch", side = "Top")
        ),
        row_split_by = "Annotation", row_split_cols = "pathway",
        column_split_by = "Annotation", column_split_cols = "condition"
    )

    shiny::testServer(
        ComplexHeatmap_HeatmapServer,
        args = list(data = shiny::reactive(dat), defaults = defaults),
        {
            session$setInputs(
                matrix.cols = sample_cols, rowname.col = "gene",
                column_key = "sample", row_filter = "", column_filter = "",
                auto.update = TRUE
            )

            svg <- attr(session$getReturned(), "vector_svg")(
                width = 329, height = 399
            )

            # One user unit is one pixel, so the drawing is on the same canvas
            # the panel was rendered at.
            vb <- as.numeric(strsplit(gsub("viewBox='|'", "",
                regmatches(svg, regexpr("viewBox='[^']+'", svg))), " ")[[1]])
            expect_equal(vb[3:4], c(329, 399))

            # The matrix cells are the narrow rects the drawing is mostly made
            # of; their width is what collapses when the canvas is too small.
            rects <- unlist(regmatches(svg, gregexpr("<rect [^>]*>", svg)))
            widths <- suppressWarnings(as.numeric(
                sub(".*width='([0-9.]+)'.*", "\\1", rects)
            ))
            widths <- widths[!is.na(widths) & widths < vb[3] / 10]
            cell <- as.numeric(names(sort(table(widths), decreasing = TRUE))[1])

            # Drawn on the right canvas this is ~7pt; on a 25%-smaller one it
            # fell below a tenth of a point.
            expect_gt(cell, 2)
        }
    )
})
