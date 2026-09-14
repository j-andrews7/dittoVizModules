#' Compute pairwise statistical tests between groups
#'
#' Performs pairwise statistical tests between groups defined by a categorical
#' variable. Supports Wilcoxon rank-sum, t-test, Kruskal-Wallis, and ANOVA.
#' Handles nested grouping (comparing color.by groups within each x-level)
#' and per-facet testing.
#'
#' @param df Data frame containing the data.
#' @param x Character; column name of the categorical x-axis variable.
#' @param y Character; column name of the numeric response variable.
#' @param pairs List of length-2 character vectors specifying group pairs to test.
#'   If NULL (default), tests all unique pairwise combinations.
#' @param test Character; statistical test to use. One of `"wilcox.test"`,
#'   `"t.test"`, `"kruskal.test"`, or `"anova"`.
#' @param p.adjust.method Character; method for p-value adjustment via
#'   [stats::p.adjust()]. Default `"holm"`.
#' @param paired Logical; whether to perform paired tests (only for
#'   `"wilcox.test"` and `"t.test"`). Default FALSE.
#' @param group.by Character or NULL; column for nested grouping. When set,
#'   comparisons are made between levels of `group.by` within each level of `x`.
#' @param facet.by Character or NULL; column for faceting. When set and
#'   `per.facet = TRUE`, tests run independently per facet panel.
#' @param per.facet Logical; if TRUE and `facet.by` is set, run tests
#'   independently per facet panel. Default TRUE.
#' @param sig.threshold Numeric; significance threshold for `*` vs `ns`.
#'   P-values at or below this are labeled `*`; above are labeled `ns`. Default 0.05.
#'   See `sig.levels` for the multi-star thresholds.
#' @param sig.levels Named numeric vector; upper p-value bounds for multi-star
#'   significance symbols. Names are the displayed symbols and values are the
#'   thresholds. Default `c("****" = 0.0001, "***" = 0.001, "**" = 0.01)`.
#'   Any number of levels can be provided. Evaluated from smallest to largest
#'   threshold so the most significant symbol always wins.
#'
#' @return A data.frame with columns: `group1`, `group2`, `p.value`, `p.adj`,
#'   `p.signif`, `test`, `facet_level`, `x_level` (when `group.by` is set).
#'
#' @importFrom stats wilcox.test t.test kruskal.test aov p.adjust as.formula
#'
#' @examples
#' compute_pairwise_stats(
#'     df = example_iris,
#'     x = "Species",
#'     y = "Sepal.Length",
#'     test = "wilcox.test"
#' )
#'
#' # Custom significance levels: only two-star tiers, lower threshold for *
#' compute_pairwise_stats(
#'     df = example_iris,
#'     x = "Species",
#'     y = "Sepal.Length",
#'     test = "wilcox.test",
#'     sig.threshold = 0.01,
#'     sig.levels = c("**" = 0.001, "***" = 0.0001)
#' )
#'
#' @author Jared Andrews, Jacob Martin
#' @export
compute_pairwise_stats <- function(df, x, y,
                                    pairs = NULL,
                                    test = "wilcox.test",
                                    p.adjust.method = "holm",
                                    paired = FALSE,
                                    group.by = NULL,
                                    facet.by = NULL,
                                    per.facet = TRUE,
                                    sig.threshold = 0.05,
                                    sig.levels = c("****" = 0.0001, "***" = 0.001, "**" = 0.01)) {
    .compute_for_subset <- function(sub_df, facet_level = NA_character_) {
        if (test %in% c("kruskal.test", "anova")) {
            return(.compute_omnibus(sub_df, x, y, test, group.by, facet_level))
        }
        .compute_pairwise(sub_df, x, y, test, paired, pairs, group.by, facet_level)
    }

    # Run tests, optionally per facet
    if (!is.null(facet.by) && nzchar(facet.by) && per.facet) {
        facet_levels <- unique(as.character(df[[facet.by]]))
        all_results <- lapply(facet_levels, function(flev) {
            sub <- df[as.character(df[[facet.by]]) == flev, ]
            .compute_for_subset(sub, facet_level = flev)
        })
        stats_df <- do.call(rbind, Filter(Negate(is.null), all_results))
    } else {
        stats_df <- .compute_for_subset(df)
    }

    if (is.null(stats_df) || nrow(stats_df) == 0) {
        return(data.frame(
            group1 = character(0), group2 = character(0),
            p.value = numeric(0), p.adj = numeric(0), p.signif = character(0),
            test = character(0), facet_level = character(0), x_level = character(0),
            stringsAsFactors = FALSE
        ))
    }

    # Adjust p-values across all tests
    stats_df$p.adj <- p.adjust(stats_df$p.value, method = p.adjust.method)
    stats_df$p.signif <- .p_to_signif(stats_df$p.adj, sig.threshold, sig.levels)
    stats_df$p.adjust.method <- p.adjust.method
    stats_df
}


# --- Internal helpers for compute_pairwise_stats -----------------------------

#' Run a single pairwise test on two numeric vectors
#' @noRd
.run_pairwise_test <- function(vals1, vals2, test_type, paired_test) {
    if (length(vals1) < 2 || length(vals2) < 2) {
        return(NA_real_)
    }
    tryCatch(
        {
            result <- switch(test_type,
                "wilcox.test" = wilcox.test(vals1, vals2, paired = paired_test),
                "t.test" = t.test(vals1, vals2, paired = paired_test),
                stop("Unsupported pairwise test: ", test_type)
            )
            result$p.value
        },
        error = function(e) NA_real_
    )
}

#' Convert p-values to significance symbols
#' @noRd
.p_to_signif <- function(p, sig.threshold, sig.levels) {
    out <- ifelse(is.na(p), "NA", ifelse(p <= sig.threshold, "*", "ns"))
    for (nm in names(sort(sig.levels, decreasing = TRUE))) {
        out[!is.na(p) & p <= sig.levels[[nm]]] <- nm
    }
    out
}

#' Compute omnibus tests (Kruskal-Wallis or ANOVA) for a data subset
#' @noRd
.compute_omnibus <- function(sub_df, x, y, test, group.by, facet_level) {
    if (!is.null(group.by) && nzchar(group.by)) {
        x_levels <- unique(as.character(sub_df[[x]]))
        results <- lapply(x_levels, function(xlev) {
            x_sub <- sub_df[as.character(sub_df[[x]]) == xlev, ]
            grp_vals <- as.character(x_sub[[group.by]])
            if (length(unique(grp_vals)) < 2) {
                return(NULL)
            }
            p_val <- tryCatch(
                {
                    if (test == "kruskal.test") {
                        kruskal.test(x_sub[[y]] ~ factor(x_sub[[group.by]]))$p.value
                    } else {
                        summary(aov(as.formula(paste0("`", y, "` ~ factor(`", group.by, "`)")),
                            data = x_sub
                        ))[[1]][["Pr(>F)"]][1]
                    }
                },
                error = function(e) NA_real_
            )
            data.frame(
                group1 = "all", group2 = "all", p.value = p_val,
                test = test, facet_level = facet_level, x_level = xlev,
                stringsAsFactors = FALSE
            )
        })
        do.call(rbind, Filter(Negate(is.null), results))
    } else {
        if (length(unique(as.character(sub_df[[x]]))) < 2) {
            return(NULL)
        }
        p_val <- tryCatch(
            {
                if (test == "kruskal.test") {
                    kruskal.test(sub_df[[y]] ~ factor(sub_df[[x]]))$p.value
                } else {
                    summary(aov(as.formula(paste0("`", y, "` ~ factor(`", x, "`)")),
                        data = sub_df
                    ))[[1]][["Pr(>F)"]][1]
                }
            },
            error = function(e) NA_real_
        )
        data.frame(
            group1 = "all", group2 = "all", p.value = p_val,
            test = test, facet_level = facet_level, x_level = NA_character_,
            stringsAsFactors = FALSE
        )
    }
}


#' Enumerate the comparisons a pairwise run will produce
#'
#' The comparison set depends only on the grouping columns and the user's pair
#' selection, never on the data values, so it can be worked out without running
#' a single test. `.compute_pairwise()` fills these rows in with p-values, and
#' [stat_bracket_y_max()] uses them to work out how much room the brackets need.
#'
#' @return A data frame with `group1`, `group2`, `x_level` and `facet_level`, or
#'   `NULL` when there is nothing to compare.
#' @rdname INTERNAL_pairwise_layout
#' @keywords internal
.pairwise_layout <- function(sub_df, x, pairs, group.by, facet_level = NA_character_) {
    if (!is.null(group.by) && nzchar(group.by)) {
        x_levels <- unique(as.character(sub_df[[x]]))
        grp_levels <- unique(as.character(sub_df[[group.by]]))
        if (length(grp_levels) < 2) {
            return(NULL)
        }

        grp_pairs <- if (!is.null(pairs)) {
            pairs
        } else {
            combn(grp_levels, 2, simplify = FALSE)
        }

        rows <- lapply(x_levels, function(xlev) {
            do.call(rbind, lapply(grp_pairs, function(pr) {
                data.frame(
                    group1 = pr[1], group2 = pr[2],
                    x_level = xlev, facet_level = facet_level,
                    stringsAsFactors = FALSE
                )
            }))
        })
        do.call(rbind, rows)
    } else {
        x_levels <- unique(as.character(sub_df[[x]]))
        if (length(x_levels) < 2) {
            return(NULL)
        }

        test_pairs <- if (!is.null(pairs)) {
            pairs
        } else {
            combn(x_levels, 2, simplify = FALSE)
        }

        do.call(rbind, lapply(test_pairs, function(pr) {
            data.frame(
                group1 = pr[1], group2 = pr[2],
                x_level = NA_character_, facet_level = facet_level,
                stringsAsFactors = FALSE
            )
        }))
    }
}

.compute_pairwise <- function(sub_df, x, y, test, paired, pairs, group.by, facet_level) {
    layout <- .pairwise_layout(sub_df, x, pairs, group.by, facet_level)
    if (is.null(layout) || nrow(layout) == 0) {
        return(NULL)
    }

    nested <- !is.null(group.by) && nzchar(group.by)

    p_vals <- vapply(seq_len(nrow(layout)), function(i) {
        if (nested) {
            x_sub <- sub_df[as.character(sub_df[[x]]) == layout$x_level[i], ]
            vals1 <- x_sub[as.character(x_sub[[group.by]]) == layout$group1[i], y]
            vals2 <- x_sub[as.character(x_sub[[group.by]]) == layout$group2[i], y]
        } else {
            vals1 <- sub_df[as.character(sub_df[[x]]) == layout$group1[i], y]
            vals2 <- sub_df[as.character(sub_df[[x]]) == layout$group2[i], y]
        }
        .run_pairwise_test(vals1, vals2, test, paired)
    }, numeric(1))

    data.frame(
        group1 = layout$group1, group2 = layout$group2, p.value = p_vals,
        test = test, facet_level = layout$facet_level, x_level = layout$x_level,
        stringsAsFactors = FALSE
    )
}


#' Create plotly shapes and annotations for statistical test results
#'
#' Converts results from [compute_pairwise_stats()] into plotly-compatible
#' shapes (brackets) and annotations (text labels). Sorts comparisons so that
#' small-gap brackets are closest to the data and large-gap brackets are higher.
#'
#' @param stats_df Data frame from [compute_pairwise_stats()].
#' @param fig A plotly figure object. Used to detect subplot axis pairs for
#'   faceted plots.
#' @param df The original data frame.
#' @param x Character; x-axis column name.
#' @param y Character; y-axis column name.
#' @param display Character; what to display: `"p.adj"`, `"p.value"`, or
#'   `"symbol"`. Default `"p.adj"`.
#' @param hide.ns Logical; hide non-significant results. Default FALSE.
#' @param sig.threshold Numeric; significance threshold for determining
#'   non-significant results. Default 0.05.
#' @param line.color Character; color for bracket lines. Default `"#000000"`.
#' @param line.width Numeric; width of bracket lines. Default 1.
#' @param bracket.style Character; `"capped"` for ggpubr-style brackets with
#'   vertical ticks, or `"flat"` for a single horizontal line. Default `"capped"`.
#' @param group.by Character or NULL; nested grouping column.
#' @param facet.by Character or NULL; faceting column.
#' @param x.order Character vector; order of x-axis categories. If NULL, derived
#'   from unique values of `x` column.
#' @param font.size Numeric; size of annotation text. Default 12.
#' @param step.increase Numeric; fraction of y-range for spacing between
#'   successive brackets. Default 0.06.
#' @param text.bump Numeric; fraction of y-range for vertical distance of
#'   text above the bracket line. Default 0.04.
#' @param bracket.inset Numeric; fixed amount to inset each bracket endpoint
#'   from the group center position. Creates visual separation between
#'   adjacent brackets at the same y-level. Default 0.025.
#' @param dodge.width Numeric; width the `group.by` levels at one x category are
#'   dodged across, matching the dodge the plot was built with. Brackets between
#'   two `group.by` levels are placed on the same slot centres the boxes sit on,
#'   so this has to be the plot's dodge or they will not line up. Default 1.
#'
#' @return A list with components:
#'   \describe{
#'     \item{annotations}{List of plotly annotation objects.}
#'     \item{shapes}{List of plotly shape objects.}
#'     \item{y.max}{Numeric; maximum y value needed to accommodate all annotations.}
#'   }
#'
#' @importFrom utils combn
#'
#' @examples
#' stats_df <- compute_pairwise_stats(
#'     df = example_iris,
#'     x = "Species",
#'     y = "Sepal.Length",
#'     test = "wilcox.test"
#' )
#'
#' fig <- plotly::plot_ly(
#'     data = example_iris, x = ~Species, y = ~Sepal.Length, type = "box"
#' )
#'
#' stat_result <- create_stat_annotations(
#'     stats_df = stats_df,
#'     fig = fig,
#'     df = example_iris,
#'     x = "Species",
#'     y = "Sepal.Length",
#'     display = "symbol"
#' )
#'
#' names(stat_result)
#'
#' @author Jared Andrews, Jacob Martin
#' @export
create_stat_annotations <- function(stats_df, fig, df, x, y,
                                     display = "p.adj",
                                     hide.ns = FALSE,
                                     sig.threshold = 0.05,
                                     line.color = "#000000",
                                     line.width = 1,
                                     bracket.style = "capped",
                                     group.by = NULL,
                                     facet.by = NULL,
                                     x.order = NULL,
                                     font.size = 12,
                                     step.increase = 0.06,
                                     text.bump = 0.04,
                                     bracket.inset = 0.025,
                                     dodge.width = 1) {
    empty_result <- list(annotations = list(), shapes = list(), y.max = NULL)

    if (is.null(stats_df) || nrow(stats_df) == 0) {
        return(empty_result)
    }

    # Filter non-significant if requested
    if (hide.ns) {
        stats_df <- stats_df[!is.na(stats_df$p.adj) & stats_df$p.adj <= sig.threshold, ]
        if (nrow(stats_df) == 0) {
            return(empty_result)
        }
    }

    # Separate omnibus from pairwise results
    omnibus_df <- stats_df[stats_df$group1 == "all", ]
    pairwise_df <- stats_df[stats_df$group1 != "all", ]

    all_annotations <- list()
    all_shapes <- list()

    # Handle omnibus annotation
    if (nrow(omnibus_df) > 0) {
        all_annotations <- .build_omnibus_annotation(
            omnibus_df, display, font.size, line.color
        )
    }

    if (nrow(pairwise_df) == 0) {
        return(list(annotations = all_annotations, shapes = all_shapes, y.max = NULL))
    }

    # Determine x-axis order
    if (is.null(x.order)) {
        col_data <- df[[x]]
        x.order <- if (is.factor(col_data)) levels(col_data) else unique(as.character(col_data))
    }

    # Y-axis range from data
    v_max <- max(df[[y]], na.rm = TRUE)
    v_range <- v_max - min(df[[y]], na.rm = TRUE)
    v_unit <- v_range * step.increase
    bump <- v_range * text.bump
    tick_height <- v_range * 0.02

    # Build facet axis map
    facet_axis_map <- .build_facet_axis_map(fig, facet.by)

    # Process brackets per facet level
    facet_levels <- unique(pairwise_df$facet_level)
    if (all(is.na(facet_levels))) facet_levels <- NA_character_

    all_annotations <- list()
    all_shapes <- list()
    global_y_max <- v_max

    for (flev in facet_levels) {
        if (is.na(flev)) {
            facet_rows <- pairwise_df[is.na(pairwise_df$facet_level), ]
            xref <- "x"
            yref <- "y"
        } else {
            facet_rows <- pairwise_df[!is.na(pairwise_df$facet_level) &
                pairwise_df$facet_level == flev, ]
            if (length(facet_axis_map) > 0 && flev %in% names(facet_axis_map)) {
                xref <- facet_axis_map[[flev]]$x
                yref <- facet_axis_map[[flev]]$y
            } else {
                xref <- "x"
                yref <- "y"
            }
        }

        if (nrow(facet_rows) == 0) next

        # Position, sort, inset and pack the brackets into non-overlapping levels
        facet_rows <- .assign_bracket_levels(
            facet_rows, x.order, group.by,
            .facet_subset(df, facet.by, flev), bracket.inset,
            x = x, dodge.width = dodge.width
        )

        # Generate bracket shapes and annotations
        bracket_result <- .create_bracket_shapes(
            facet_rows, v_max, v_unit, bump, tick_height,
            display, bracket.style, line.color, line.width,
            font.size, xref, yref
        )
        all_shapes <- c(all_shapes, bracket_result$shapes)
        all_annotations <- c(all_annotations, bracket_result$annotations)
        if (bracket_result$y_max > global_y_max) {
            global_y_max <- bracket_result$y_max
        }
    }

    global_y_max <- global_y_max + v_unit

    # Replicate annotations to extra facet panels when per-facet is disabled
    if (length(facet_axis_map) > 0 && all(is.na(unique(pairwise_df$facet_level)))) {
        replicated <- .replicate_to_facet_panels(
            all_annotations, all_shapes, facet_axis_map
        )
        all_annotations <- replicated$annotations
        all_shapes <- replicated$shapes
    }

    # Re-add omnibus annotation if present
    if (nrow(omnibus_df) > 0) {
        omnibus_annots <- .build_omnibus_annotation(
            omnibus_df, display, font.size, line.color
        )
        all_annotations <- c(omnibus_annots, all_annotations)
    }

    list(annotations = all_annotations, shapes = all_shapes, y.max = global_y_max)
}


# --- Internal helpers for create_stat_annotations ----------------------------

#' Rows of one facet panel, or the whole frame when not faceted
#'
#' `ggplot2` dodges each panel on its own, so which groups are present has to be
#' asked of the panel rather than of the whole data set.
#'
#' @noRd
.facet_subset <- function(df, facet.by, facet_level) {
    if (is.null(facet.by) || length(facet.by) != 1 || is.na(facet.by) ||
        !nzchar(facet.by) || !facet.by %in% names(df) || is.na(facet_level)) {
        return(df)
    }
    df[as.character(df[[facet.by]]) == facet_level, , drop = FALSE]
}

#' Group levels present at one x category, in the order ggplot2 dodges them
#'
#' `ggplot2` orders discrete levels by the factor's levels, or alphabetically
#' for a character column, and dodges only the levels actually present at that
#' x position.
#'
#' @noRd
.groups_at_x <- function(df, x, group.by, x_level) {
    col <- df[[group.by]]
    lv <- if (is.factor(col)) levels(col) else sort(unique(as.character(col)))
    if (is.null(x) || length(x) != 1 || is.na(x) || !x %in% names(df)) {
        return(lv)
    }
    at_x <- as.character(col)[as.character(df[[x]]) == x_level]
    lv[lv %in% at_x]
}

#' Get x-position for a group label on plotly categorical axis
#'
#' Mirrors `ggplot2`'s `position_dodge()`: the levels present at this x category
#' split `dodge.width` between them and each sits at the centre of its slot. It
#' has to agree with [.align_box_positions()], or the brackets do not span the
#' boxes they were computed from.
#'
#' @noRd
.get_x_pos <- function(group_label, x_level, x.order, group.by, df,
                       x = NULL, dodge.width = 1) {
    if (is.null(group.by) || !nzchar(group.by) || is.na(x_level)) {
        return(match(group_label, x.order))
    }
    x_idx <- match(x_level, x.order)
    present <- .groups_at_x(df, x, group.by, x_level)
    n_grps <- length(present)
    if (n_grps <= 1) {
        return(x_idx)
    }
    slot <- match(group_label, present)
    if (is.na(slot)) {
        # The group is not in this x category, so it has no box and no position.
        # .assign_bracket_levels() drops the comparison on the strength of this.
        return(NA_real_)
    }
    x_idx + dodge.width * ((slot - 0.5) / n_grps - 0.5)
}

#' Build omnibus test annotation (ANOVA / Kruskal-Wallis)
#' @noRd
.build_omnibus_annotation <- function(omnibus_df, display, font.size, line.color) {
    test_name <- if (omnibus_df$test[1] == "kruskal.test") "Kruskal-Wallis" else "ANOVA"
    omnibus_labels <- vapply(seq_len(nrow(omnibus_df)), function(i) {
        row <- omnibus_df[i, ]
        p_text <- switch(display,
            "symbol" = row$p.signif,
            "p.value" = if (is.na(row$p.value)) "NA" else format(round(row$p.value, 4), scientific = FALSE),
            "p.adj" = if (is.na(row$p.adj)) "NA" else format(round(row$p.adj, 4), scientific = FALSE)
        )
        facet_prefix <- if (!is.na(row$facet_level)) paste0(row$facet_level, ": ") else ""
        x_prefix <- if (!is.na(row$x_level)) paste0(row$x_level, ": ") else ""
        paste0(facet_prefix, x_prefix, "p=", p_text)
    }, character(1))

    omnibus_text <- paste0(test_name, ": ", paste(omnibus_labels, collapse = "; "))

    list(list(
        text = omnibus_text,
        x = 0.01, y = -0.12,
        xref = "paper", yref = "paper",
        xanchor = "left", yanchor = "top",
        showarrow = FALSE,
        font = list(size = font.size - 1, color = line.color),
        captureevents = TRUE
    ))
}

#' Build facet-level to plotly axis-pair mapping from figure layout
#' @noRd
.build_facet_axis_map <- function(fig, facet.by) {
    facet_axis_map <- list()
    if (is.null(facet.by) || !nzchar(facet.by) || is.null(fig)) {
        return(facet_axis_map)
    }

    layout <- fig$x$layout

    # Collect axes with domain midpoints
    x_axes <- list()
    y_axes <- list()
    for (nm in names(layout)) {
        if (grepl("^xaxis", nm)) {
            ax <- layout[[nm]]
            if (!is.null(ax$domain)) {
                trace_ref <- sub("^xaxis", "x", nm)
                x_axes[[trace_ref]] <- (ax$domain[1] + ax$domain[2]) / 2
            }
        } else if (grepl("^yaxis", nm)) {
            ax <- layout[[nm]]
            if (!is.null(ax$domain)) {
                trace_ref <- sub("^yaxis", "y", nm)
                y_axes[[trace_ref]] <- ax$domain[2]
            }
        }
    }

    # Match strip label annotations to axis pairs
    annots <- layout$annotations
    if (is.null(annots) || length(x_axes) == 0 || length(y_axes) == 0) {
        return(facet_axis_map)
    }

    for (a in annots) {
        if (is.null(a$xref) || a$xref != "paper") next
        if (is.null(a$text) || !nzchar(a$text)) next
        if (isTRUE(a$x == 0) || isTRUE(a$y == 0)) next

        best_x <- NULL
        best_x_dist <- Inf
        for (xref in names(x_axes)) {
            dist <- abs(x_axes[[xref]] - a$x)
            if (dist < best_x_dist) {
                best_x_dist <- dist
                best_x <- xref
            }
        }

        best_y <- NULL
        best_y_dist <- Inf
        for (yref in names(y_axes)) {
            dist <- abs(y_axes[[yref]] - a$y)
            if (dist < best_y_dist) {
                best_y_dist <- dist
                best_y <- yref
            }
        }

        if (!is.null(best_x) && !is.null(best_y)) {
            facet_axis_map[[a$text]] <- list(x = best_x, y = best_y)
        }
    }

    facet_axis_map
}

#' Place one facet's comparisons on the x-axis and stack them into levels
#'
#' Shared by the drawing code and by [stat_bracket_y_max()], so the height the
#' brackets are reserved room for is the height they are actually drawn at.
#'
#' @return `facet_rows` sorted by span, with `x0_draw`/`x1_draw` endpoints and a
#'   `y_level` column.
#' @noRd
.assign_bracket_levels <- function(facet_rows, x.order, group.by, df, bracket.inset,
                                   x = NULL, dodge.width = 1) {
    # Compute x-positions and sort by gap
    pos_args <- list(
        x.order = x.order, group.by = group.by, df = df,
        x = x, dodge.width = dodge.width
    )
    facet_rows$x0_pos <- mapply(
        .get_x_pos, facet_rows$group1, facet_rows$x_level,
        MoreArgs = pos_args
    )
    facet_rows$x1_pos <- mapply(
        .get_x_pos, facet_rows$group2, facet_rows$x_level,
        MoreArgs = pos_args
    )
    # A comparison against a group that has no data at this x category has no box
    # to bracket, and .get_x_pos() reports that as an NA position. Those rows also
    # carry an NA p-value, so drop them rather than drawing an "NA" bracket at a
    # made-up position. Dropping here keeps the height stat_bracket_y_max()
    # reserves equal to the height the brackets are drawn at.
    drawable <- !is.na(facet_rows$x0_pos) & !is.na(facet_rows$x1_pos)
    facet_rows <- facet_rows[drawable, , drop = FALSE]
    if (nrow(facet_rows) == 0) {
        facet_rows$gap <- numeric(0)
        facet_rows$x0_raw <- numeric(0)
        facet_rows$x1_raw <- numeric(0)
        facet_rows$x0_draw <- numeric(0)
        facet_rows$x1_draw <- numeric(0)
        facet_rows$y_level <- integer(0)
        return(facet_rows)
    }

    facet_rows$gap <- abs(facet_rows$x1_pos - facet_rows$x0_pos)
    facet_rows <- facet_rows[order(facet_rows$gap), ]

    # Compute inset endpoints
    facet_rows$x0_raw <- pmin(facet_rows$x0_pos, facet_rows$x1_pos)
    facet_rows$x1_raw <- pmax(facet_rows$x0_pos, facet_rows$x1_pos)
    facet_rows$x0_draw <- facet_rows$x0_raw + bracket.inset
    facet_rows$x1_draw <- facet_rows$x1_raw - bracket.inset

    # Pack brackets into y-levels
    facet_rows$y_level <- .pack_bracket_levels(facet_rows)
    facet_rows
}

#' Pack brackets into non-overlapping y-levels using interval packing
#' @noRd
.pack_bracket_levels <- function(facet_rows) {
    gap_groups <- unique(facet_rows$gap)
    y_levels <- list()
    y_level_out <- rep(NA_integer_, nrow(facet_rows))

    for (gg in gap_groups) {
        group_idx <- which(facet_rows$gap == gg)
        for (idx in group_idx) {
            bx0 <- facet_rows$x0_draw[idx]
            bx1 <- facet_rows$x1_draw[idx]
            placed <- FALSE
            for (lev in seq_along(y_levels)) {
                overlaps <- FALSE
                for (interval in y_levels[[lev]]) {
                    if (bx0 < interval[2] && bx1 > interval[1]) {
                        overlaps <- TRUE
                        break
                    }
                }
                if (!overlaps) {
                    y_levels[[lev]][[length(y_levels[[lev]]) + 1]] <- c(bx0, bx1)
                    y_level_out[idx] <- lev
                    placed <- TRUE
                    break
                }
            }
            if (!placed) {
                y_levels[[length(y_levels) + 1]] <- list(c(bx0, bx1))
                y_level_out[idx] <- length(y_levels)
            }
        }
    }
    y_level_out
}

#' Create bracket shapes and text annotations for a set of comparisons
#' @noRd
.create_bracket_shapes <- function(facet_rows, v_max, v_unit, bump, tick_height,
                                    display, bracket.style, line.color, line.width,
                                    font.size, xref, yref) {
    shapes <- list()
    annotations <- list()
    local_y_max <- v_max

    for (i in seq_len(nrow(facet_rows))) {
        row <- facet_rows[i, ]
        y_bar <- v_max + v_unit * row$y_level

        x0 <- row$x0_draw
        x1 <- row$x1_draw
        x_mid <- (x0 + x1) / 2

        label <- switch(display,
            "symbol" = row$p.signif,
            "p.value" = if (is.na(row$p.value)) "NA" else format(round(row$p.value, 4), scientific = FALSE),
            "p.adj" = if (is.na(row$p.adj)) "NA" else format(round(row$p.adj, 4), scientific = FALSE)
        )

        if (bracket.style == "capped") {
            shapes[[length(shapes) + 1]] <- list(
                type = "line",
                line = list(color = line.color, width = line.width),
                xref = xref, yref = yref,
                x0 = x0, x1 = x0,
                y0 = y_bar - tick_height, y1 = y_bar
            )
            shapes[[length(shapes) + 1]] <- list(
                type = "line",
                line = list(color = line.color, width = line.width),
                xref = xref, yref = yref,
                x0 = x0, x1 = x1,
                y0 = y_bar, y1 = y_bar
            )
            shapes[[length(shapes) + 1]] <- list(
                type = "line",
                line = list(color = line.color, width = line.width),
                xref = xref, yref = yref,
                x0 = x1, x1 = x1,
                y0 = y_bar - tick_height, y1 = y_bar
            )
        } else {
            shapes[[length(shapes) + 1]] <- list(
                type = "line",
                line = list(color = line.color, width = line.width),
                xref = xref, yref = yref,
                x0 = x0, x1 = x1,
                y0 = y_bar, y1 = y_bar
            )
        }

        annotations[[length(annotations) + 1]] <- list(
            text = label,
            x = x_mid, y = y_bar + bump,
            xref = xref, yref = yref,
            showarrow = FALSE,
            font = list(size = font.size, color = line.color)
        )

        top <- y_bar + bump
        if (top > local_y_max) local_y_max <- top
    }

    list(shapes = shapes, annotations = annotations, y_max = local_y_max)
}

#' Replicate annotations/shapes to extra facet panels
#' @noRd
.replicate_to_facet_panels <- function(all_annotations, all_shapes, facet_axis_map) {
    axis_pairs <- unique(lapply(facet_axis_map, function(p) p))
    extra_pairs <- Filter(function(p) !(p$x == "x" && p$y == "y"), axis_pairs)

    if (length(extra_pairs) > 0) {
        base_annotations <- all_annotations
        base_shapes <- all_shapes
        for (pair in extra_pairs) {
            for (a in base_annotations) {
                a$xref <- pair$x
                a$yref <- pair$y
                all_annotations[[length(all_annotations) + 1]] <- a
            }
            for (s in base_shapes) {
                s$xref <- pair$x
                s$yref <- pair$y
                all_shapes[[length(all_shapes) + 1]] <- s
            }
        }
    }

    list(annotations = all_annotations, shapes = all_shapes)
}


#' Y-axis top needed to draw statistical annotation brackets in full
#'
#' Works out how high the significance brackets from [create_stat_annotations()]
#' will reach, so an axis range can reserve room for them up front rather than
#' having the plot drawn with the brackets clipped or pushed outside the panel.
#'
#' @details
#' The brackets are stacked above the data: each packing level sits
#' `step.increase` of the data range above the last, the label sits `text.bump`
#' above its bracket, and a final `step.increase` of clearance is left at the
#' top. Which comparisons land on which level is decided by
#' `.assign_bracket_levels()`, shared with the drawing code, so the two agree
#' exactly.
#'
#' Which comparisons there are to place depends only on the grouping columns and
#' the user's pair selection, so no test needs to be run — except under
#' `hide.ns = TRUE`, where the non-significant brackets are dropped before
#' packing and the tests have to be run to know which those are. That case
#' repeats the work [compute_pairwise_stats()] does at render time; it is
#' skipped for several `y` columns at once, where the comparisons no longer map
#' one-to-one onto a single test run, and the result is then an upper bound.
#'
#' [apply_stat_annotations()] still has the last word on the drawn range, so a
#' bracket is never clipped even where this over- or under-estimates.
#'
#' @param df Data frame the statistics are computed on. For a module that
#'   reshapes its data for testing (e.g. a multi-variable Y selection), pass the
#'   reshaped frame, not the raw one.
#' @param x Character; x-axis column name.
#' @param y Character; y-axis column name(s). Several may be given, in which
#'   case the data range spans all of them.
#' @param pairs List of length-2 character vectors, or `NULL` for all pairwise
#'   combinations.
#' @param group.by Character or `NULL`; nested grouping column.
#' @param facet.by Character or `NULL`; faceting column.
#' @param per.facet Logical; whether tests are run within each facet.
#' @param step.increase Numeric; fraction of the y-range between successive
#'   bracket levels. Default 0.06.
#' @param text.bump Numeric; fraction of the y-range between a bracket and its
#'   label. Default 0.04.
#' @param bracket.inset Numeric; endpoint inset, which affects how tightly
#'   brackets pack onto a level. Default 0.025.
#' @param dodge.width Numeric; width the `group.by` levels at one x category are
#'   dodged across. Match the plot's, or the brackets will not line up with the
#'   boxes. Default 1.
#' @param hide.ns Logical; whether non-significant brackets are dropped before
#'   drawing. When `TRUE` the tests are run so only the surviving comparisons
#'   are counted. Default `FALSE`.
#' @param sig.threshold Numeric; significance cutoff used with `hide.ns`.
#' @param test,p.adjust.method,paired Passed to [compute_pairwise_stats()], and
#'   used only when `hide.ns` is `TRUE`. Match them to the render's settings, or
#'   the wrong comparisons are counted.
#'
#' @return A single number giving the y-axis maximum the brackets need, or
#'   `NULL` when nothing would be drawn.
#'
#' @seealso [create_stat_annotations()], [apply_stat_annotations()]
#'
#' @export
#' @author Jared Andrews
#' @examples
#' # Three species means three comparisons, which stack onto two levels.
#' stat_bracket_y_max(example_iris, x = "Species", y = "Sepal.Length")
#'
#' # Compare against the raw data maximum.
#' max(example_iris$Sepal.Length)
stat_bracket_y_max <- function(df, x, y, pairs = NULL, group.by = NULL,
                               facet.by = NULL, per.facet = TRUE,
                               step.increase = 0.06, text.bump = 0.04,
                               bracket.inset = 0.025,
                               hide.ns = FALSE, sig.threshold = 0.05,
                               test = "wilcox.test", p.adjust.method = "holm",
                               paired = FALSE, dodge.width = 1) {
    if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        return(NULL)
    }
    if (is.null(x) || length(x) != 1 || is.na(x) || !nzchar(x) || !x %in% names(df)) {
        return(NULL)
    }
    # Omnibus tests produce a single caption, not brackets.
    if (length(test) == 1 && test %in% c("kruskal.test", "anova")) {
        return(NULL)
    }

    y <- y[!is.na(y) & nzchar(y)]
    if (length(y) == 0 || !all(y %in% names(df))) {
        return(NULL)
    }
    vals <- unlist(df[, y, drop = FALSE], use.names = FALSE)
    vals <- vals[is.finite(vals)]
    if (length(vals) == 0) {
        return(NULL)
    }

    # Under hide.ns only the significant comparisons are drawn, and there is no
    # way to know which those are without running the tests.
    rows <- NULL
    if (isTRUE(hide.ns) && length(y) == 1) {
        stats_df <- tryCatch(
            compute_pairwise_stats(
                df = df, x = x, y = y, pairs = pairs, test = test,
                p.adjust.method = p.adjust.method, paired = paired,
                group.by = group.by, facet.by = facet.by, per.facet = per.facet,
                sig.threshold = sig.threshold
            ),
            error = function(e) NULL
        )
        if (is.null(stats_df) || nrow(stats_df) == 0) {
            return(NULL)
        }
        keep <- !is.na(stats_df$p.adj) & stats_df$p.adj <= sig.threshold &
            stats_df$group1 != "all"
        rows <- stats_df[keep, c("group1", "group2", "x_level", "facet_level"), drop = FALSE]
        if (nrow(rows) == 0) {
            return(NULL)
        }
    }

    levels_used <- .bracket_level_count(
        df, x, pairs, group.by, facet.by, per.facet, bracket.inset, rows,
        dodge.width = dodge.width
    )
    if (levels_used < 1) {
        return(NULL)
    }

    v_max <- max(vals)
    v_range <- v_max - min(vals)

    # Mirrors create_stat_annotations(): top bracket at v_max + v_unit * level,
    # its label bump above that, then one more v_unit of clearance.
    v_max + v_range * (step.increase * (levels_used + 1) + text.bump)
}

#' Highest bracket level any facet needs
#'
#' @param rows Optional pre-filtered comparison rows (used under `hide.ns`).
#'   When `NULL`, every comparison that would be run is counted.
#' @return An integer; 0 when nothing would be drawn.
#' @noRd
.bracket_level_count <- function(df, x, pairs, group.by, facet.by, per.facet,
                                 bracket.inset, rows = NULL, dodge.width = 1) {
    if (is.null(rows)) {
        faceted <- !is.null(facet.by) && length(facet.by) == 1 && !is.na(facet.by) &&
            nzchar(facet.by) && facet.by %in% names(df) && isTRUE(per.facet)

        rows <- if (faceted) {
            facet_levels <- unique(as.character(df[[facet.by]]))
            per_level <- lapply(facet_levels, function(flev) {
                .pairwise_layout(
                    df[as.character(df[[facet.by]]) == flev, ], x, pairs, group.by, flev
                )
            })
            do.call(rbind, Filter(Negate(is.null), per_level))
        } else {
            .pairwise_layout(df, x, pairs, group.by, NA_character_)
        }
    }

    if (is.null(rows) || nrow(rows) == 0) {
        return(0L)
    }

    col_data <- df[[x]]
    x.order <- if (is.factor(col_data)) levels(col_data) else unique(as.character(col_data))

    max_level <- 0L
    for (flev in unique(rows$facet_level)) {
        facet_rows <- if (is.na(flev)) {
            rows[is.na(rows$facet_level), ]
        } else {
            rows[!is.na(rows$facet_level) & rows$facet_level == flev, ]
        }
        if (nrow(facet_rows) == 0) next

        packed <- .assign_bracket_levels(
            facet_rows, x.order, group.by,
            .facet_subset(df, facet.by, flev), bracket.inset,
            x = x, dodge.width = dodge.width
        )
        levels_here <- suppressWarnings(max(packed$y_level, na.rm = TRUE))
        if (is.finite(levels_here) && levels_here > max_level) {
            max_level <- as.integer(levels_here)
        }
    }

    max_level
}


#' Bracket headroom for a module, read straight off its Stats tab
#'
#' Wraps [stat_bracket_y_max()] with the geometry inputs every module's Stats
#' tab carries, so the three modules that draw significance brackets share one
#' call rather than three copies of the same argument list. Missing inputs fall
#' back to the tab's own defaults, which matters while the tab has yet to be
#' rendered.
#'
#' @param df,x,y,group.by,facet.by,per.facet,dodge.width Passed to
#'   [stat_bracket_y_max()].
#' @param input The Shiny `input` object from inside `moduleServer()`.
#'
#' @return A single number, or `NULL` when no brackets would be drawn.
#'
#' @author Jared Andrews
#' @rdname INTERNAL_stat_bracket_headroom
#' @keywords internal
.stat_bracket_headroom <- function(df, x, y, group.by = NULL, facet.by = NULL,
                                   per.facet = TRUE, input, dodge.width = 1) {
    num_or <- function(value, fallback) {
        if (is.null(value) || length(value) != 1 || is.na(value) || !is.numeric(value)) {
            fallback
        } else {
            value
        }
    }

    chr_or <- function(value, fallback) {
        if (is.null(value) || length(value) != 1 || is.na(value) || !nzchar(value)) {
            fallback
        } else {
            value
        }
    }

    stat_bracket_y_max(
        df = df, x = x, y = y,
        pairs = parse_pair_strings(input$stat.pairs),
        group.by = group.by, facet.by = facet.by, per.facet = per.facet,
        step.increase = num_or(input$stat.step.increase, 0.06),
        text.bump = num_or(input$stat.text.bump, 0.04),
        bracket.inset = num_or(input$stat.bracket.inset, 0.025),
        # The render drops non-significant brackets by default, so counting them
        # here would reserve room for brackets that never appear.
        hide.ns = isTRUE(input$stat.hide.ns),
        sig.threshold = num_or(input$stat.sig.threshold, 0.05),
        test = chr_or(input$stat.test, "wilcox.test"),
        p.adjust.method = chr_or(input$stat.p.adjust, "holm"),
        paired = isTRUE(input$stat.paired),
        dodge.width = dodge.width
    )
}


#' Apply statistical annotation shapes and annotations to a plotly figure
#'
#' Appends the shapes and annotations from [create_stat_annotations()] to
#' an existing plotly figure's layout, raising the y-axis top when the brackets
#' need more room than the requested range gives them.
#'
#' @details
#' The axis is only ever raised, never lowered: a top asked for through `y.max`
#' (or already on the figure) is kept when it is above the brackets, so turning
#' statistics on cannot silently undo a y-axis maximum the user chose. Reserve
#' the room up front with [stat_bracket_y_max()] and this becomes a no-op.
#'
#' @param fig A plotly figure object.
#' @param stat_result List with `annotations`, `shapes`, and `y.max` as returned
#'   by [create_stat_annotations()].
#' @param y.min Numeric or NULL; minimum y-axis value. If NULL, the existing
#'   y-axis range is preserved.
#' @param y.max Numeric or NULL; the maximum the caller asked for. The drawn top
#'   is the larger of this and the height the brackets need. If NULL, the
#'   figure's existing top is used for that comparison.
#'
#' @return The modified plotly figure.
#'
#' @examples
#' stats_df <- compute_pairwise_stats(
#'     df = example_iris,
#'     x = "Species",
#'     y = "Sepal.Length",
#'     test = "wilcox.test"
#' )
#' fig <- plotly::plot_ly(
#'     data = example_iris, x = ~Species, y = ~Sepal.Length, type = "box"
#' )
#' stat_result <- create_stat_annotations(
#'     stats_df = stats_df,
#'     fig = fig,
#'     df = example_iris,
#'     x = "Species",
#'     y = "Sepal.Length",
#'     display = "symbol"
#' )
#' apply_stat_annotations(fig, stat_result)
#'
#' @author Jared Andrews
#' @export
apply_stat_annotations <- function(fig, stat_result, y.min = NULL, y.max = NULL) {
    if (length(stat_result$annotations) == 0 && length(stat_result$shapes) == 0) {
        return(fig)
    }

    # Append new shapes to any existing shapes
    existing_shapes <- fig$x$layout$shapes
    if (is.null(existing_shapes)) existing_shapes <- list()
    fig$x$layout$shapes <- c(existing_shapes, stat_result$shapes)

    # Append new annotations to any existing annotations
    existing_annots <- fig$x$layout$annotations
    if (is.null(existing_annots)) existing_annots <- list()
    fig$x$layout$annotations <- c(existing_annots, stat_result$annotations)

    # Adjust y-axis range to accommodate brackets on ALL y-axes
    if (!is.null(stat_result$y.max)) {
        y_axis_names <- grep("^yaxis", names(fig$x$layout), value = TRUE)
        if (length(y_axis_names) == 0) y_axis_names <- "yaxis"

        for (yax_name in y_axis_names) {
            existing_yaxis <- fig$x$layout[[yax_name]]
            if (is.null(existing_yaxis)) existing_yaxis <- list()

            y_lo <- y.min
            if (is.null(y_lo) && !is.null(existing_yaxis$range)) {
                y_lo <- existing_yaxis$range[1]
            }

            # Raise the top to fit the brackets, but never pull it back down: a
            # maximum the caller asked for is theirs to keep when it already
            # clears them.
            y_hi <- y.max
            if (is.null(y_hi) && !is.null(existing_yaxis$range)) {
                y_hi <- existing_yaxis$range[2]
            }
            y_hi <- if (is.null(y_hi) || !is.finite(y_hi)) {
                stat_result$y.max
            } else {
                max(y_hi, stat_result$y.max)
            }

            if (!is.null(y_lo)) {
                y_span <- y_hi - y_lo
                y_lo <- y_lo - y_span * 0.02
            }
            existing_yaxis$range <- c(y_lo, y_hi)
            fig$x$layout[[yax_name]] <- existing_yaxis
        }
    }

    fig
}


#' Generate comparison pair strings from data columns
#'
#' Creates formatted pair strings for populating the comparison selector UI.
#' Handles both standard x-axis comparisons and nested group.by comparisons.
#'
#' @param df Data frame containing the data.
#' @param x Character; x-axis column name.
#' @param group.by Character or NULL; nested grouping column.
#'
#' @return A character vector of pair strings in "group1 vs group2" format.
#'
#' @importFrom utils combn
#'
#' @examples
#' generate_pair_strings(example_iris, x = "Species")
#'
#' @author Jared Andrews
#' @export
generate_pair_strings <- function(df, x, group.by = NULL) {
    if (!is.null(group.by) && nzchar(group.by) && group.by %in% names(df)) {
        grp_levels <- unique(as.character(df[[group.by]]))
        if (length(grp_levels) < 2) {
            return(character(0))
        }
        pairs_list <- combn(grp_levels, 2, simplify = FALSE)
    } else {
        x_levels <- unique(as.character(df[[x]]))
        if (length(x_levels) < 2) {
            return(character(0))
        }
        pairs_list <- combn(x_levels, 2, simplify = FALSE)
    }
    vapply(pairs_list, paste, character(1), collapse = " vs ")
}


#' Parse pair strings from UI into list of length-2 vectors
#'
#' Converts the "group1 vs group2" strings from the comparison selector
#' back into a list of length-2 character vectors for [compute_pairwise_stats()].
#'
#' @param pair_strings Character vector of pair strings from UI input.
#'
#' @return A list of length-2 character vectors, or NULL if input is empty.
#'
#' @examples
#' parse_pair_strings(c("setosa vs versicolor", "versicolor vs virginica"))
#'
#' @author Jared Andrews
#' @export
parse_pair_strings <- function(pair_strings) {
    if (is.null(pair_strings) || length(pair_strings) == 0 ||
        all(!nzchar(pair_strings))) {
        return(NULL)
    }
    pair_strings <- pair_strings[nzchar(pair_strings)]
    lapply(strsplit(pair_strings, " vs "), trimws)
}
