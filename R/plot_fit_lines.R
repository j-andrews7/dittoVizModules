#' Compute linear regression fit line data
#'
#' Computes predicted values from a linear model for plotting a fit line.
#' Can optionally compute separate fit lines for each group in a grouping variable.
#'
#' @param df Data frame containing the data.
#' @param x.col Character. Name of the column for x-axis values.
#' @param y.col Character. Name of the column for y-axis values.
#' @param group.col Character or NULL. Name of the column to group by.
#'   If NULL, computes a single global fit line.
#' @param n.points Integer. Number of points to generate for the fit line.
#'
#' @return If `group.col` is NULL, a data frame with columns `x` and `y`.
#'   If `group.col` is provided, a named list of data frames (one per group).
#'
#' @importFrom stats lm loess coef predict
#'
#' @author Jared Andrews
#' @rdname INTERNAL_compute_linear_fit
#' @keywords internal
.compute_linear_fit <- function(df, x.col, y.col, group.col = NULL, n.points = 100) {
    compute_for_subset <- function(subset_df) {
        # Remove NA values
        subset_df <- subset_df[!is.na(subset_df[[x.col]]) & !is.na(subset_df[[y.col]]), ]
        if (nrow(subset_df) < 2) {
            return(NULL)
        }

        model <- lm(subset_df[[y.col]] ~ subset_df[[x.col]])

        x_min <- min(subset_df[[x.col]], na.rm = TRUE)
        x_max <- max(subset_df[[x.col]], na.rm = TRUE)
        x_grid <- seq(x_min, x_max, length.out = n.points)
        intercept <- coef(model)[1]
        slope <- coef(model)[2]
        y_grid <- intercept + slope * x_grid

        data.frame(x = x_grid, y = y_grid)
    }

    if (is.null(group.col) || group.col == "") {
        compute_for_subset(df)
    } else {
        groups <- unique(df[[group.col]])
        fits <- lapply(groups, function(g) {
            subset_df <- df[df[[group.col]] == g, ]
            compute_for_subset(subset_df)
        })
        names(fits) <- as.character(groups)
        # Remove NULL entries (groups with insufficient data)
        fits[!vapply(fits, is.null, logical(1))]
    }
}

#' Compute LOESS smooth fit line data
#'
#' Computes predicted values from a LOESS model for plotting a smooth fit line.
#' Can optionally compute separate fit lines for each group in a grouping variable.
#'
#' @param df Data frame containing the data.
#' @param x.col Character. Name of the column for x-axis values.
#' @param y.col Character. Name of the column for y-axis values.
#' @param group.col Character or NULL. Name of the column to group by.
#'   If NULL, computes a single global fit line.
#' @param span Numeric. The span parameter for LOESS smoothing (0 to 1).
#' @param n.points Integer. Number of points to generate for the fit line.
#'
#' @return If `group.col` is NULL, a data frame with columns `x` and `y`.
#'   If `group.col` is provided, a named list of data frames (one per group).
#'
#' @author Jared Andrews
#' @rdname INTERNAL_compute_loess_fit
#' @keywords internal
.compute_loess_fit <- function(df, x.col, y.col, group.col = NULL, span = 0.75, n.points = 100) {
    compute_for_subset <- function(subset_df) {
        # Remove NA values
        subset_df <- subset_df[!is.na(subset_df[[x.col]]) & !is.na(subset_df[[y.col]]), ]
        # LOESS needs at least 4 observations
        if (nrow(subset_df) < 4) {
            return(NULL)
        }

        # Create a local copy with standardized column names for formula
        fit_df <- data.frame(x = subset_df[[x.col]], y = subset_df[[y.col]])

        fit <- tryCatch(
            loess(y ~ x, data = fit_df, span = span),
            error = function(e) NULL
        )
        if (is.null(fit)) {
            return(NULL)
        }

        x_min <- min(fit_df$x, na.rm = TRUE)
        x_max <- max(fit_df$x, na.rm = TRUE)
        x_grid <- seq(x_min, x_max, length.out = n.points)

        y_grid <- predict(fit, newdata = data.frame(x = x_grid))

        data.frame(x = x_grid, y = y_grid)
    }

    if (is.null(group.col) || group.col == "") {
        compute_for_subset(df)
    } else {
        groups <- unique(df[[group.col]])
        fits <- lapply(groups, function(g) {
            subset_df <- df[df[[group.col]] == g, ]
            compute_for_subset(subset_df)
        })
        names(fits) <- as.character(groups)
        # Remove NULL entries (groups with insufficient data)
        fits[!vapply(fits, is.null, logical(1))]
    }
}


#' Add fit line traces to all subplot panels
#'
#' Adds linear or LOESS fit line traces to a plotly figure, handling subplot panels
#' when faceting is applied. Determines subplot axes from existing traces and adds
#' fit lines to each panel.
#'
#' @param fig A plotly figure object.
#' @param df Data frame containing the full dataset.
#' @param x.col Character. Name of the column for x-axis values.
#' @param y.col Character. Name of the column for y-axis values.
#' @param split.by Character vector or NULL. Column name(s) used for faceting.
#' @param group.col Character or NULL. Column name for color grouping.
#' @param color_mapping Named character vector or NULL. Mapping of group names to colors.
#' @param line_color Character. Color for ungrouped fit lines.
#' @param fit_type Character. Type of fit: "linear" or "loess".
#' @param span Numeric. Span parameter for LOESS smoothing (ignored for linear).
#' @param line_width Numeric. Width of the fit lines.
#'
#' @return The modified plotly figure with fit lines added to all subplot panels.
#'
#' @author Jared Andrews, Jacob Martin
#' @keywords internal
#' @rdname INTERNAL_add_fit_lines_to_subplots
.add_fit_lines_to_subplots <- function(fig, df, x.col, y.col, split.by = NULL, group.col = NULL,
                                       color_mapping = NULL, line_color = "#000000",
                                       fit_type = c("linear", "loess"), span = 0.75,
                                       line_width = 3) {
    fit_type <- match.arg(fit_type)

    # Determine facet levels if split.by is provided
    if (!is.null(split.by) && length(split.by) > 0 && all(split.by %in% names(df))) {
        if (length(split.by) == 1) {
            # Preserve factor level order if it's a factor
            if (is.factor(df[[split.by]])) {
                facet_levels <- levels(df[[split.by]])
            } else {
                facet_levels <- unique(as.character(df[[split.by]]))
            }
            df$`.facet_combined` <- as.character(df[[split.by]])
        } else {
            df$`.facet_combined` <- apply(df[, split.by, drop = FALSE], 1, paste, collapse = "_")
            facet_levels <- unique(df$`.facet_combined`)
        }
    } else {
        facet_levels <- NULL
        df$`.facet_combined` <- "all"
    }

    # Extract axis pairs in order by sorting by axis number
    # This ensures x_y comes before x2_y2, etc.
    axis_pairs_raw <- lapply(fig$x$data, function(tr) {
        xaxis <- if (is.null(tr$xaxis)) "x" else tr$xaxis
        yaxis <- if (is.null(tr$yaxis)) "y" else tr$yaxis
        list(x = xaxis, y = yaxis)
    })

    # Get unique pairs while preserving first occurrence order
    seen_keys <- character(0)
    axis_pairs <- list()
    for (pair in axis_pairs_raw) {
        key <- paste0(pair$x, "_", pair$y)
        if (!key %in% seen_keys) {
            seen_keys <- c(seen_keys, key)
            axis_pairs <- c(axis_pairs, list(pair))
        }
    }

    # Sort axis pairs by axis number to match facet order
    # x/y -> 1, x2/y2 -> 2, etc.
    get_axis_num <- function(pair) {
        x_num <- as.numeric(sub("^x", "", pair$x))
        if (is.na(x_num)) x_num <- 1
        x_num
    }
    axis_order <- order(vapply(axis_pairs, get_axis_num, numeric(1)))
    axis_pairs <- axis_pairs[axis_order]

    # If no traces found, default to main axes
    if (length(axis_pairs) == 0) {
        axis_pairs <- list(list(x = "x", y = "y"))
    }

    # Track which trace names we've added to avoid duplicate legend entries
    added_names <- character(0)

    # Process each subplot panel
    for (idx in seq_along(axis_pairs)) {
        pair <- axis_pairs[[idx]]

        # Determine which facet level this panel corresponds to
        if (!is.null(facet_levels) && idx <= length(facet_levels)) {
            facet_val <- facet_levels[idx]
            subset_df <- df[df$`.facet_combined` == facet_val, , drop = FALSE]
        } else if (is.null(facet_levels)) {
            subset_df <- df
        } else {
            # More panels than facet levels - skip
            next
        }

        # Skip if no data in this panel
        if (nrow(subset_df) == 0) {
            next
        }

        # Compute fit data for this panel
        if (fit_type == "linear") {
            fit_data <- .compute_linear_fit(
                df = subset_df,
                x.col = x.col,
                y.col = y.col,
                group.col = group.col
            )
        } else {
            fit_data <- .compute_loess_fit(
                df = subset_df,
                x.col = x.col,
                y.col = y.col,
                group.col = group.col,
                span = span
            )
        }

        if (is.null(fit_data)) {
            next
        }

        fit_name_prefix <- if (fit_type == "linear") "Linear Fit" else "Best Fit"

        if (is.data.frame(fit_data)) {
            # Single global fit line for this panel
            trace_name <- fit_name_prefix
            show_legend <- !trace_name %in% added_names

            fig <- fig |>
                plotly::add_lines(
                    data = fit_data,
                    x = ~x,
                    y = ~y,
                    xaxis = pair$x,
                    yaxis = pair$y,
                    line = list(color = line_color, width = line_width),
                    name = trace_name,
                    legendgroup = trace_name,
                    showlegend = show_legend,
                    inherit = FALSE
                )
            added_names <- c(added_names, trace_name)
        } else {
            # Grouped fit lines (fit_data is a list)
            for (group_name in names(fit_data)) {
                group_fit <- fit_data[[group_name]]
                if (is.null(group_fit) || nrow(group_fit) == 0) {
                    next
                }

                # Get color for this group
                if (!is.null(color_mapping) && group_name %in% names(color_mapping)) {
                    grp_color <- color_mapping[[group_name]]
                } else {
                    grp_color <- line_color
                }

                trace_name <- paste(fit_name_prefix, group_name)
                show_legend <- !trace_name %in% added_names

                fig <- fig |>
                    plotly::add_lines(
                        data = group_fit,
                        x = ~x,
                        y = ~y,
                        xaxis = pair$x,
                        yaxis = pair$y,
                        line = list(color = grp_color, width = line_width),
                        name = trace_name,
                        legendgroup = trace_name,
                        showlegend = show_legend,
                        inherit = FALSE
                    )
                added_names <- c(added_names, trace_name)
            }
        }
    }

    fig
}


#' Compute predicted values from a custom model object
#'
#' Generates a smooth grid of predicted x/y values from any model object that
#' supports [stats::predict()]. The x variable name used in `predict()` must
#' match `x.col` so that `newdata` is constructed correctly.
#'
#' For models that require additional columns in `newdata` (e.g. mixed-effects
#' models from \pkg{lme4} that need random-effect grouping columns), all other
#' numeric columns in `df` are included in `newdata` at their median value and
#' all non-numeric columns at their first level/value. This allows
#' `predict(..., re.form = NA)` (population-level predictions) to succeed for
#' `lmer`/`glmer` models.
#'
#' Note: `lme4::lmer()` must be called with an explicit `data =` argument when
#' fitting the model, otherwise the formula environment cannot be resolved.
#'
#' @param model A fitted model object with a `predict()` method (e.g. `lm`,
#'   `glm`, `nls`, `loess`, `lme4::lmer`, `mgcv::gam`).
#' @param df Data frame containing the x variable.
#' @param x.col Character. Name of the column used as the x predictor.
#' @param n.points Integer. Number of points in the prediction grid. Default 100.
#'
#' @return A `data.frame` with columns `x` and `y`, or `NULL` on failure.
#'
#' @importFrom stats predict median
#'
#' @author Jacob Martin
#' @keywords internal
#' @rdname INTERNAL_compute_custom_model_fit
.compute_custom_model_fit <- function(model, df, x.col, n.points = 100, backend = NULL) {
    x_vals <- df[[x.col]]
    x_vals <- x_vals[is.finite(x_vals)]
    if (length(x_vals) < 2) return(NULL)

    x_grid <- seq(min(x_vals), max(x_vals), length.out = n.points)

    # Building a new data frame based on x data to predict y values and the line coords

    other_cols <- setdiff(names(df), x.col)
    newdata <- setNames(data.frame(x_grid), x.col)
    for (col in other_cols) {
        vals <- df[[col]]
        newdata[[col]] <- if (is.numeric(vals)) {
            median(vals, na.rm = TRUE)
        } else if (is.factor(vals)) {
            factor(levels(vals)[1], levels = levels(vals))
        } else {
            as.character(vals)[1]
        }
    }

    # Use the backend's predict function if provided, otherwise fall back to
    # generic predict with special-casing for lmer/glmer.
    if (!is.null(backend) && is.function(backend$predict)) {
        y_grid <- tryCatch(
            as.numeric(backend$predict(model, newdata)),
            error = function(e) NULL
        )
    } else {
        predict_args <- list(object = model, newdata = newdata)
        if (inherits(model, c("lmerMod", "glmerMod"))) {
            predict_args$re.form <- NA
        }
        y_grid <- tryCatch(
            as.numeric(do.call(predict, predict_args)),
            error = function(e) NULL
        )
    }

    if (is.null(y_grid) || length(y_grid) != n.points) return(NULL)

    data.frame(x = x_grid, y = y_grid)
}


#' Add custom model line traces to all subplot panels
#'
#' Renders a named list of pre-fitted model objects as overlay lines on a
#' plotly scatter figure. Each model is evaluated across the x range of the
#' data using [stats::predict()] and added as a separate `add_lines()` trace.
#' Handles faceted subplots by adding each model line to every panel.
#'
#' Each entry in `custom.models` is a fitted model object (e.g. from [lm()],
#' [glm()], [loess()], or [nls()]). The list name becomes the legend label.
#' Line colour and width are shared across all entries and controlled via the
#' `line_color` and `line_width` arguments.
#'
#' @param fig A plotly figure object.
#' @param df Data frame containing the x variable.
#' @param x.col Character. Name of the column used as the x predictor.
#' @param custom.models Named list of model objects or styled model lists.
#' @param split.by Character vector or NULL. Column name(s) used for faceting
#'   (used only to determine subplot panel count; models are global).
#' @param line_color Character. Default hex color for lines with no per-model
#'   color specified. Default `"#000000"`.
#' @param line_width Numeric. Default line width. Default `2`.
#'
#' @return The modified plotly figure with custom model lines added.
#'
#' @author Jacob Martin
#' @keywords internal
#' @rdname INTERNAL_add_custom_model_lines_to_subplots
.add_custom_model_lines_to_subplots <- function(fig, df, x.col, custom.models,
                                                split.by = NULL,
                                                line_color = "#000000",
                                                line_width = 2,
                                                backend = NULL) {
    if (is.null(custom.models) || length(custom.models) == 0) return(fig)
    if (is.null(names(custom.models)) || any(!nzchar(names(custom.models)))) {
        warning("custom.models must be a named list; unnamed entries will be skipped.")
    }

    # Extract axis pairs from existing traces (mirrors .add_fit_lines_to_subplots)
    axis_pairs_raw <- lapply(fig$x$data, function(tr) {
        xaxis <- if (is.null(tr$xaxis)) "x" else tr$xaxis
        yaxis <- if (is.null(tr$yaxis)) "y" else tr$yaxis
        list(x = xaxis, y = yaxis)
    })
    seen_keys <- character(0)
    axis_pairs <- list()
    for (pair in axis_pairs_raw) {
        key <- paste0(pair$x, "_", pair$y)
        if (!key %in% seen_keys) {
            seen_keys <- c(seen_keys, key)
            axis_pairs <- c(axis_pairs, list(pair))
        }
    }
    get_axis_num <- function(pair) {
        x_num <- suppressWarnings(as.numeric(sub("^x", "", pair$x)))
        if (is.na(x_num)) x_num <- 1
        x_num
    }
    axis_pairs <- axis_pairs[order(vapply(axis_pairs, get_axis_num, numeric(1)))]
    if (length(axis_pairs) == 0) axis_pairs <- list(list(x = "x", y = "y"))

    # Facet subsets so predictions are scoped to each panel's x range
    if (!is.null(split.by) && length(split.by) > 0 && all(split.by %in% names(df))) {
        if (length(split.by) == 1) {
            facet_levels <- if (is.factor(df[[split.by]])) levels(df[[split.by]]) else unique(as.character(df[[split.by]]))
            df$`.facet_combined` <- as.character(df[[split.by]])
        } else {
            df$`.facet_combined` <- apply(df[, split.by, drop = FALSE], 1, paste, collapse = "_")
            facet_levels <- unique(df$`.facet_combined`)
        }
    } else {
        facet_levels <- NULL
        df$`.facet_combined` <- "all"
    }

    added_names <- character(0)

    for (model_name in names(custom.models)) {
        # Each entry is a fitted model object. Colour and width are supplied by
        # the caller (line_color/line_width) rather than per-entry, so a bare
        # model list (e.g. an `lm`, which is itself a list) is never mistaken
        # for a styled config list.
        model   <- custom.models[[model_name]]
        m_color <- line_color
        m_width <- line_width

        for (idx in seq_along(axis_pairs)) {
            pair <- axis_pairs[[idx]]

            subset_df <- if (!is.null(facet_levels) && idx <= length(facet_levels)) {
                df[df$`.facet_combined` == facet_levels[idx], , drop = FALSE]
            } else {
                df
            }

            if (nrow(subset_df) == 0) next

            fit_data <- .compute_custom_model_fit(model, subset_df, x.col, backend = backend)
            if (is.null(fit_data)) next

            show_legend <- !model_name %in% added_names

            fig <- fig |>
                plotly::add_lines(
                    data        = fit_data,
                    x           = ~x,
                    y           = ~y,
                    xaxis       = pair$x,
                    yaxis       = pair$y,
                    line        = list(color = m_color, width = m_width),
                    name        = model_name,
                    legendgroup = model_name,
                    showlegend  = show_legend,
                    inherit     = FALSE
                )
            added_names <- c(added_names, model_name)
        }
    }

    fig
}

#' Safely build a model from a user-supplied formula string
#'
#' Validates and fits a model from a user-typed formula string without
#' evaluating arbitrary code. Intended for interactive contexts (e.g. a Shiny
#' text input) where the formula originates from untrusted user input. The
#' function only ever converts the text into a `formula` object — it never
#' calls `eval(parse(...))` on raw input — and rejects anything that is not a
#' recognised formula built from allow-listed terms.
#'
#' Validation proceeds in stages, returning `NULL` (with a `warning()`) at the
#' first failure:
#' \enumerate{
#'   \item The fit function name is looked up in the model backend registry
#'     via [get_model_backend()].
#'   \item The text is parsed with [parse()] and required to be a single
#'     expression.
#'   \item The expression's abstract syntax tree is walked recursively; only
#'     data-column symbols, a small set of literal keywords, and an allow-list
#'     of math/transform calls are permitted. Calls such as `system`, `eval`,
#'     or `source` are structurally rejected.
#'   \item The text is converted with [stats::as.formula()] and confirmed to be
#'     of class `"formula"`.
#'   \item The model is fitted via the backend's `fit` function inside
#'     [tryCatch()] scoped to `data`, and the returned object's class is
#'     verified against the backend's `validate_classes`.
#' }
#'
#' @param formula_text Character string containing the model formula
#'   (e.g. `"revenue ~ poly(units, 2)"`). Must reference only columns present
#'   in `data`.
#' @param data A `data.frame` whose columns the formula may reference and
#'   against which the model is fitted.
#' @param fit_fn_name Character string naming the model backend. Must be a name
#'   registered via [register_model_backend()] (e.g. `"lm"`, `"glm"`,
#'   `"loess"`, `"nls"`, or any user-registered backend).
#' @param ... Extra arguments forwarded to the backend's `fit` function. These
#'   typically come from additional fields in the [multiDynamicInput()] row
#'   (e.g. `drc_fct = "LL.4"` for a drc backend).
#'
#' @return A fitted model object whose class matches the backend's
#'   `validate_classes`, or `NULL` if the input is empty, unparseable, contains
#'   disallowed terms, or fails to fit.
#'
#' @importFrom stats as.formula
#'
#' @author Jacob Martin
#' @keywords internal
#' @rdname INTERNAL_safe_build_model
.safe_build_model <- function(formula_text, data, fit_fn_name, ...) {
    # `||`, not `|`: with a NULL formula_text the third operand is logical(0),
    # and a vectorized `|` propagates that into `if`, which errors instead of
    # taking the documented NULL return.
    if (is.null(formula_text) || is.null(data) || !nzchar(trimws(formula_text))) {
        return(NULL)
    }

    backend <- get_model_backend(fit_fn_name)
    if (is.null(backend)) {
        warning("Unrecognized model type: ", fit_fn_name,
                ". Registered backends: ", paste(list_model_backends(), collapse = ", "))
        return(NULL)
    }

    parsed <- tryCatch(parse(text = formula_text), error = function(e) NULL)
    if (is.null(parsed) || length(parsed) != 1) {
        warning("Could not parse a single formula expression.")
        return(NULL)
    }

    expr <- parsed[[1]]

    if (!is.call(expr) || !identical(expr[[1]], as.name("~"))) {
        warning("Input must be a model formula (contain '~').")
        return(NULL)
    }

    # The walker is shared with safe_eval_filter() / validate_expression();
    # only the vocabulary differs. This function used to carry its own copy,
    # which drifted: it flattened a call in function position with
    # as.character(), so `y ~ base::log(x)` produced a length-3 vector and an
    # "argument is of length zero" error rather than a clean rejection, while
    # `y ~ log()(x)` flattened to "log" and was accepted.
    if (!.expr_check_node(expr, names(data), .formula_allowed_calls())) {
        warning("Formula contains disallowed terms. Only data columns and ",
                "basic math/transform functions are permitted.")
        return(NULL)
    }

    formula <- tryCatch(stats::as.formula(formula_text), error = function(e) NULL)
    if (is.null(formula) | !inherits(formula, "formula")) {
        warning("Could not construct a valid formula")
        return(NULL)
    }

    model <- tryCatch(backend$fit(formula, data, ...), error = function(e) {
        warning("Model fitting failed: ", conditionMessage(e))
        NULL
    })
    if (is.null(model) || !inherits(model, backend$validate_classes)) {
        return(NULL)
    }
    return(model)
}

#' Model backend registry
#'
#' A pluggable registry that lets any modelling package (drc, mgcv, brms, etc.)
#' be used in the custom-model-lines pipeline without modifying core code. Each
#' **backend** is a small named list that tells the pipeline how to fit a model
#' and how to predict from it.
#'
#' Backends are stored in a package-level environment. The four built-in types
#' (`lm`, `glm`, `loess`, `nls`) are registered automatically when the package
#' loads. Users add new ones with [register_model_backend()].
#'
#' @name model_backends
#' @keywords internal
NULL

# Private registry environment — backends stored by name.
.model_backend_registry <- new.env(parent = emptyenv())


#' Register a model backend
#'
#' Add or replace a model backend in the registry. Once registered, the backend
#' name appears in the model-type dropdown and the custom-model-lines pipeline
#' dispatches through it automatically.
#'
#' A backend is a named list with three required elements:
#' \describe{
#'   \item{`fit`}{A function with signature `function(formula, data, ...)` that
#'     returns a fitted model object. Extra UI fields from the
#'     [multiDynamicInput()] row are forwarded as `...`.}
#'   \item{`predict`}{A function with signature `function(model, newdata)` that
#'     returns a numeric vector of predicted y-values, one per row of
#'     `newdata`.}
#'   \item{`validate_classes`}{Character vector of class names. After fitting,
#'     the pipeline checks `inherits(model, validate_classes)` and rejects the
#'     model if it fails.}
#' }
#'
#' @param name Character string. The name that will appear in the model-type
#'   dropdown (e.g. `"drm"`, `"gam"`).
#' @param backend A named list with elements `fit`, `predict`, and
#'   `validate_classes` as described above.
#'
#' @return Invisibly returns `NULL`. Called for its side effect.
#'
#' @export
#' @author Jacob Martin
#'
#' @examples
#' # Register a custom backend for dose-response curves (requires drc)
#' if (requireNamespace("drc", quietly = TRUE)) {
# '     register_model_backend("drm", list(
# '         fit = function(formula, data, drc_fct = "LL.4", ...) {
# '             fct_map <- list(
# '                 "LL.4" = drc::LL.4, "LL.3" = drc::LL.3,
# '                 "LL.2" = drc::LL.2, "W1.4" = drc::W1.4
# '             )
# '             fct_fn <- fct_map[[drc_fct]]
# '             if (is.null(fct_fn)) stop("Unknown drc family: ", drc_fct)
# '             drc::drm(formula, data = data, fct = fct_fn())
# '         },
# '         predict = function(model, newdata) {
# '             as.numeric(predict(model, newdata = newdata))
# '         },
# '         validate_classes = "drc"
# '     ))
#' }
register_model_backend <- function(name, backend) {
    if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
        stop("`name` must be a single non-empty string.")
    }

    if (!is.list(backend)) {
        stop("`backend` must be a named list with `fit`, `predict`, and `validate_classes`.")
    }

    required <- c("fit", "predict", "validate_classes")
    missing <- setdiff(required, names(backend))

    if (length(missing) > 0) {
        stop("Backend is missing required elements: ", paste(missing, collapse = ", "))
    }

    if (!is.function(backend$fit)) {
        stop("`backend$fit` must be a function(formula, data, ...).")
    }

    if (!is.function(backend$predict)) {
        stop("`backend$predict` must be a function(model, newdata).")
    }

    if (!is.character(backend$validate_classes) || length(backend$validate_classes) == 0) {
        stop("`backend$validate_classes` must be a non-empty character vector.")
    }

    # Optional: extra UI fields this backend contributes to the row_spec.
    # Must be a named list of field specs (same format as row_spec entries).
    if (!is.null(backend$fields) && (!is.list(backend$fields) || is.null(names(backend$fields)))) {
        stop("`backend$fields` must be a named list of field specs or NULL.")
    }

    .model_backend_registry[[name]] <- backend
    invisible(NULL)
}


#' Get a registered model backend
#'
#' Retrieve a model backend from the registry by name.
#'
#' @param name Character string. The backend name (e.g. `"lm"`, `"drm"`).
#'
#' @return The backend list (with `fit`, `predict`, `validate_classes`), or
#'   `NULL` if no backend with that name is registered.
#'
#' @export
#' @author Jacob Martin
#'
#' @examples
#' get_model_backend("lm")
#' get_model_backend("nonexistent")
get_model_backend <- function(name) {
    if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
        return(NULL)
    }

    if (exists(name, envir = .model_backend_registry, inherits = FALSE)) {
        .model_backend_registry[[name]]
    } else {
        NULL
    }
}


#' List registered model backends
#'
#' Returns the names of all currently registered model backends. The built-in
#' backends (`lm`, `glm`, `loess`, `nls`) are always present; any backends
#' added via [register_model_backend()] are included as well.
#'
#' @return A sorted character vector of backend names.
#'
#' @export
#' @author Jacob Martin
#'
#' @examples
#' list_model_backends()
list_model_backends <- function() {
    sort(ls(.model_backend_registry))
}


#' Build a dynamic row_spec from registered backends
#'
#' Constructs the merged `row_spec` for [multiDynamicInput()] by combining the
#' standard model fields (model_type, formula, line_colour, line_width) with
#' any extra `fields` declared by registered backends. Each backend field is
#' tagged with `data-backend` so the client can show/hide it based on the
#' selected model type.
#'
#' @return A named list suitable for the `row_spec` argument of
#'   [multiDynamicInput()].
#'
#' @export
#' @author Jacob Martin
#'
#' @examples
#' build_model_row_spec()
build_model_row_spec <- function() {
    base_spec <- list(
        model_type  = list(type = "select",
            args = list(choices = list_model_backends(), selected = "lm")),
        formula     = list(type = "text",
            args = list(placeholder = "e.g. y ~ poly(x, 2)")),
        line_colour = list(type = "colour",
            args = list(value = "#000000")),
        line_width  = list(type = "numeric",
            args = list(value = 2, min = 0.5, max = 20, step = 0.5))
    )

    # Collect extra fields from all backends, tagging each with its owner
    backend_names <- list_model_backends()
    extra_fields <- list()
    for (bn in backend_names) {
        be <- get_model_backend(bn)
        if (!is.null(be$fields) && length(be$fields) > 0) {
            for (fn in names(be$fields)) {
                spec <- be$fields[[fn]]
                # Tag with the backend name so JS can show/hide
                spec$backend <- bn
                extra_fields[[fn]] <- spec
            }
        }
    }

    c(base_spec, extra_fields)
}


#'
#' Called from `.onLoad()` to seed the registry with the four standard backends.
#'
#' @return Invisibly returns `NULL`.
#'
#' @author Jacob Martin
#' @rdname INTERNAL_register_builtin_backends
#' @keywords internal
.register_builtin_backends <- function() {
    register_model_backend("lm", list(
        fit = function(formula, data, ...) stats::lm(formula, data = data),
        predict = function(model, newdata) as.numeric(stats::predict(model, newdata = newdata)),
        validate_classes = "lm"
    ))

    register_model_backend("glm", list(
        fit = function(formula, data, ...) stats::glm(formula, data = data),
        predict = function(model, newdata) as.numeric(stats::predict(model, newdata = newdata)),
        validate_classes = "glm"
    ))

    register_model_backend("loess", list(
        fit = function(formula, data, ...) stats::loess(formula, data = data),
        predict = function(model, newdata) as.numeric(stats::predict(model, newdata = newdata)),
        validate_classes = "loess"
    ))

    register_model_backend("nls", list(
        fit = function(formula, data, ...) stats::nls(formula, data = data),
        predict = function(model, newdata) as.numeric(stats::predict(model, newdata = newdata)),
        validate_classes = "nls"
    ))

    invisible(NULL)
}
