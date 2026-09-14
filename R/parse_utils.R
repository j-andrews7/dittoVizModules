#' Parse a string indicating a set of vectors to a list of vectors.
#'
#' Used to parse text inputs into a list of vectors.
#'
#' @param x A string indicating a set of vectors.
#'   Supported formats include `"(a, b), (c)"`, `"<a, b>, <c>"`, or brackets.
#'   Should not contain internal quotes around elements.
#'
#' @return A list like `list(c("a", "b", "c"), c("d", "e"))`.
#'   If the input is "", just returns "". If the input is `NULL`, returns `NULL`.
#'
#' @author Jared Andrews
#' @rdname INTERNAL_string_to_list_of_vectors
#' @keywords internal
.string_to_list_of_vectors <- function(x) {
    if (!is.null(x)) {
        if (x != "") {
            # Regex to find content inside [], (), or <>
            matches <- regmatches(x, gregexpr("\\[.*?\\]|\\(.*?\\)|<.*?>", x))[[1]]

            if (length(matches) > 0) {
                # Remove brackets, split, trim
                x <- lapply(matches, function(m) {
                    content <- substr(m, 2, nchar(m) - 1)
                    items <- strsplit(content, ",")[[1]]
                    trimws(items)
                })
            } else {
                # treat as a single vector if no brackets found
                x <- list(trimws(strsplit(x, ",")[[1]]))
            }
        }
    }

    x
}

#' Parse a string delimited by commas, whitespace, or new lines to a vector.
#'
#' Used to parse text inputs into a vector.
#'
#' @param x A string of elements delimited by comma, whitespace, or new lines,
#'   e.g. "a, b c,d, e".
#'
#' @return A vector of strings like `c("a", "b", "c", "d", "e")`.
#'   If the input is "", just returns "". If the input is `NULL`, returns `NULL`.
#'
#' @author Jared Andrews
#' @rdname INTERNAL_string_to_vector
#' @keywords internal
.string_to_vector <- function(x) {
    if (!is.null(x)) {
        if (x != "") {
            # Split string to vector based on commas, whitespace, or new lines
            x <- strsplit(x, ",|\\s|,\\s")[[1]]
        }
    }

    x
}

#' Parse and validate linetype string to a vector
#'
#' Parses a comma-separated string of linetypes and validates each element.
#' Invalid linetypes are replaced with "solid" and a warning is issued.
#'
#' @param x A string of linetypes delimited by commas, e.g. "solid, dashed, dotted".
#'   Valid linetypes are: "solid", "dashed", "dotted", "dotdash", "longdash", "twodash".
#'
#' @return A character vector of validated linetypes.
#'   If the input is "" or NULL, returns "solid".
#'
#' @author Jared Andrews
#' @export
#' @examples
#' string_to_linetypes("solid, dashed, dotted")
#' string_to_linetypes("")
string_to_linetypes <- function(x) {
    valid_linetypes <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")

    if (is.null(x) || identical(x, "")) {
        return("solid")
    }

    # Split string on commas and trim whitespace
    linetypes <- trimws(strsplit(x, ",")[[1]])
    linetypes <- linetypes[linetypes != ""]

    if (length(linetypes) == 0) {
        return("solid")
    }

    # Validate 
    validated <- vapply(linetypes, function(lt) {
        lt_lower <- tolower(lt)
        if (lt_lower %in% valid_linetypes) {
            lt_lower
        } else {
            warning(paste0(
                "Invalid linetype '", lt, "'. Using 'solid' instead. ",
                "Valid options: ", paste(valid_linetypes, collapse = ", ")
            ))
            "solid"
        }
    }, character(1), USE.NAMES = FALSE)

    validated
}

#' Convert NA or empty string to NULL
#'
#' A helper function to convert NA values or empty strings to NULL.
#' Used to handle Shiny input default values which return NA or "" instead of NULL
#' when the input field is empty. numericInput returns NA, textInput returns "".
#'
#' @param x A value that may be NA or an empty string.
#' @return NULL if x is a single NA value or empty string, otherwise x unchanged.
#'
#' @author Jared Andrews
#' @rdname INTERNAL_na_to_null
#' @keywords internal
.na_to_null <- function(x) {
    if (length(x) == 1 && (is.na(x) || identical(x, ""))) {
        return(NULL)
    }
    x
}

#' Negative log10 transformation
#'
#' A helper function for -log10 transformation.
#'
#' @param x Numeric vector to transform.
#' @return `-log10(x)`
#'
#' @author Jared Andrews
#' @rdname INTERNAL_neglog10
#' @keywords internal
neg_log10 <- function(x) {
    -log10(x)
}

#' Require a data frame from a module's data reactive
#'
#' Wraps the `data` reactive every module server receives so that downstream
#' readers always see a data frame. A `NULL` value (which a parent app can emit
#' briefly while switching datasets) becomes a silent [shiny::req()] skip rather
#' than an error cascade through every observer, and anything else is coerced
#' with [as.data.frame()] before use.
#'
#' @param data A `reactive` returning a data frame or an object coercible to one.
#'
#' @return A `reactive` returning a data frame with at least one column.
#'
#' @import shiny
#'
#' @author Jared Andrews
#' @rdname INTERNAL_require_data_frame
#' @keywords internal
.require_data_frame <- function(data) {
    # Callers reassign onto `data`, so resolve the promise before it can recurse.
    force(data)

    reactive({
        d <- data()
        req(!is.null(d))

        if (!is.data.frame(d)) {
            coerced <- tryCatch(as.data.frame(d), error = function(e) NULL)
            validate(need(
                is.data.frame(coerced),
                "'data' must be a data frame or an object coercible to one."
            ))
            d <- coerced
        }

        req(ncol(d) > 0)
        d
    })
}

#' Resolve a color palette for plot groups
#'
#' Maps groups to colors using selected colors or a default palette. Handles
#' named color vectors by matching to group names, fills in missing colors with
#' fallback values, and ensures the output vector is named and matches group length.
#'
#' Colors are layered in order of increasing precedence: `default_palette`,
#' then `manual_colors`, then `selected_colors`. A user's on-screen choice
#' therefore always wins over a caller-supplied mapping, and a caller-supplied
#' mapping wins over the module's stock palette.
#'
#' @param groups A character vector of group names to assign colors to.
#' @param selected_colors A named or unnamed character vector of colors to use.
#'   If named, colors are matched to groups by name. If NULL or empty, uses
#'   `manual_colors` or `default_palette`.
#' @param default_palette A character vector of fallback colors to use when
#'   no other source supplies a color for a group.
#'   Defaults to "#000000" (black) if not provided.
#' @param manual_colors An optional named character vector of caller-supplied
#'   colors, typically taken from a module's `defaults`. Used for groups that
#'   `selected_colors` does not name.
#'
#' @return A named character vector of colors with names corresponding to groups,
#'   or NULL if groups is empty.
#'
#' @export
#' @author Jared Andrews
#' @examples
#' groups <- c("A", "B", "C")
#' colors <- c(A = "#FF0000", B = "#00FF00", C = "#0000FF")
#' resolve_palette(groups, colors)
#' # Returns: c(A = "#FF0000", B = "#00FF00", C = "#0000FF")
#'
#' # Using default palette
#' resolve_palette(groups, NULL, c("#1B9E77", "#D95F02", "#7570B3"))
#' # Returns: c(A = "#1B9E77", B = "#D95F02", C = "#7570B3")
#'
#' # Caller-supplied mapping fills groups the user has not picked
#' resolve_palette(groups, c(A = "#FF0000"), "#CCCCCC", c(B = "#00FF00", C = "#0000FF"))
#' # Returns: c(A = "#FF0000", B = "#00FF00", C = "#0000FF")
resolve_palette <- function(groups, selected_colors = NULL, default_palette = NULL, manual_colors = NULL) {
    if (length(groups) == 0) {
        return(NULL)
    }

    fallback <- if (!is.null(default_palette) && length(default_palette) > 0) default_palette else "#000000"

    colors <- selected_colors
    if (is.null(colors) || length(colors) == 0) {
        colors <- manual_colors
    }
    if (is.null(colors) || length(colors) == 0) {
        colors <- fallback
    }

    if (.has_group_names(colors)) {
        colors <- colors[match(groups, names(colors))]

        if (.has_group_names(manual_colors) && any(is.na(colors))) {
            gaps <- which(is.na(colors))
            colors[gaps] <- manual_colors[match(groups[gaps], names(manual_colors))]
        }
    }

    if (any(is.na(colors))) {
        na_idx <- which(is.na(colors))
        colors[na_idx] <- rep_len(fallback, length(na_idx))
    }

    colors <- rep_len(colors, length(groups))
    stats::setNames(colors[seq_along(groups)], groups)
}

#' Test whether a color vector names its groups
#'
#' @param x A character vector of colors, or NULL.
#'
#' @return `TRUE` when `x` is non-empty and carries at least one non-empty name.
#'
#' @author Jared Andrews
#' @rdname INTERNAL_has_group_names
#' @keywords internal
.has_group_names <- function(x) {
    !is.null(x) && length(x) > 0 && !is.null(names(x)) && any(nzchar(names(x)))
}

#' Extract a caller-supplied group color mapping from `defaults`
#'
#' Looks up `key` in a module's `defaults` and returns it as a named vector of
#' hex colors suitable for [resolve_palette()] and [multiColorPicker()]. Values
#' may be given as R color names (`"red"`) or hex codes; both are normalized to
#' `#RRGGBB`. Anything that is not a fully named character vector is ignored,
#' consistent with [get_default()]'s silent-fallback contract.
#'
#' @param defaults A named list of default values, or `NULL`.
#' @param key Character string — the color input's id, e.g. `"palette.colours"`.
#'
#' @return A named character vector of hex colors, or `NULL` when `defaults`
#'   supplies no usable mapping.
#'
#' @seealso [resolve_palette()], [get_default()]
#'
#' @author Jared Andrews
#' @rdname INTERNAL_default_group_colors
#' @keywords internal
.default_group_colors <- function(defaults, key) {
    colors <- get_default(defaults, key, NULL, function(x) {
        is.character(x) && length(x) > 0 && !is.null(names(x)) && all(nzchar(names(x)))
    })

    if (is.null(colors)) {
        return(NULL)
    }

    colors <- .normalize_hex(colors)
    colors <- colors[nzchar(colors)]

    if (length(colors) == 0) NULL else colors
}

#' Restore a group color picker to its default mapping
#'
#' Used by module Reset buttons. When `defaults` supplies a mapping for `inputId`
#' the picker is set back to it; otherwise the widget resets itself to the stock
#' palette it was built with.
#'
#' @param session The Shiny `session` object from inside `moduleServer()`.
#' @param inputId Character string — the picker's id, without namespacing.
#' @param defaults A named list of default values, or `NULL`.
#' @param groups A character vector of the group levels currently in play.
#' @param default_palette A character vector of fallback colors.
#'
#' @return Invisibly `NULL`; called for its side effect.
#'
#' @author Jared Andrews
#' @rdname INTERNAL_reset_group_colors
#' @keywords internal
.reset_group_colors <- function(session, inputId, defaults, groups, default_palette = NULL) {
    manual <- .default_group_colors(defaults, inputId)
    colors <- if (is.null(manual)) NULL else resolve_palette(groups, NULL, default_palette, manual)

    if (is.null(colors) || length(colors) == 0) {
        updateMultiColorPicker(session, inputId, reset = TRUE)
    } else {
        updateMultiColorPicker(session, inputId, colors = colors)
    }

    invisible(NULL)
}

#' Track the group-to-color mapping a plot should draw with
#'
#' A [multiColorPicker()] is rebuilt by `renderUI()` whenever the group set
#' changes, and the freshly built widget reports its value back on a client
#' round-trip. A plot that depends on the picker's raw `input$<key>` therefore
#' rebuilds when that value lands, even at startup where the reported value is
#' exactly what the server had already seeded the picker with.
#'
#' `setup_group_colors()` gives the module a server-side channel for the mapping
#' instead. It resolves the palette itself (via [resolve_palette()]) as soon as
#' the group set is known, and holds the result in a [shiny::reactiveVal()],
#' which only invalidates on a *changed* value. A rebuilt picker echoing the
#' mapping already in use therefore costs nothing, while a color the user
#' actually picks comes straight through.
#'
#' @details
#' Use it in three places:
#' \itemize{
#'   \item Create the store next to the module's group-levels reactive.
#'   \item Seed it inside the picker's `renderUI()` with the same
#'     `initial_colors` the widget is built from, so the mapping is right even
#'     when the render was deferred (a picker on a hidden tab is suspended).
#'   \item Read `isolate_fn(store())` in the plot reactive, in place of
#'     `isolate_fn(input$<key>)`.
#' }
#'
#' Note that [freezeReactiveValue()] does not cover this case: inside a
#' `renderUI()` it pauses only the readers that run after it in that flush, and
#' at startup the plot output runs first.
#'
#' @param input The Shiny `input` object from inside `moduleServer()`.
#' @param key Character string — the picker's input id, without namespacing,
#'   e.g. `"palette.colours"`.
#' @param groups A `reactive()` yielding the character vector of group levels
#'   currently in play.
#' @param default_palette A character vector of fallback colors.
#' @param defaults A named list of default values, or `NULL`. A named color
#'   mapping stored under `key` seeds groups the user has not picked.
#' @param params Optional reactive-defaults store from
#'   [setup_reactive_defaults()], or `NULL`. When `key` is backed by the store,
#'   the mapping follows it rather than the client input, matching what
#'   [setup_auto_update_logic()] would have done for a direct `input$<key>` read.
#'
#' @return A [shiny::reactiveVal()] holding a named character vector of colors
#'   aligned to the current groups, or `NULL` before any groups exist. Call it
#'   with no arguments to read, and with a value to seed.
#'
#' @seealso [resolve_palette()], [multiColorPicker()], [setup_auto_update_logic()]
#'
#' @importFrom shiny reactiveVal observe
#'
#' @export
#' @author Jared Andrews
#' @examples
#' if (interactive()) {
#'     library(shiny)
#'
#'     server <- function(input, output, session) {
#'         groups <- reactive(levels(as.factor(iris$Species)))
#'         palette_store <- setup_group_colors(
#'             input, "palette.colours", groups,
#'             default_palette = dittoViz::dittoColors()
#'         )
#'
#'         output$palette.selection <- renderUI({
#'             initial_colors <- isolate(palette_store())
#'             multiColorPicker(
#'                 session$ns("palette.colours"),
#'                 groups = groups(), colors = initial_colors
#'             )
#'         })
#'
#'         output$plot <- renderPlot(barplot(1:3, col = palette_store()))
#'     }
#' }
setup_group_colors <- function(input, key, groups, default_palette = NULL,
                               defaults = NULL, params = NULL) {
    store <- reactiveVal(NULL)

    observe({
        levels <- tryCatch(groups(), error = function(e) NULL)
        if (length(levels) == 0) {
            return()
        }

        # A store-backed key is driven by the parent app, not the client input.
        selected <- if (!is.null(params) && params$has(key)) {
            params$get(key)
        } else {
            input[[key]]
        }

        store(resolve_palette(
            levels, selected, default_palette,
            .default_group_colors(defaults, key)
        ))
    })

    store
}

#' Track the axis limits a plot should draw with
#'
#' Modules derive an axis range on the server and push it into their own
#' `y.min`/`y.max` controls with `updateNumericInput()`, which is a client
#' round-trip: the plot renders once with the stale limits and again when the
#' browser echoes the new ones. `setup_axis_range()` gives the plot a
#' server-side value to read instead, held in a [shiny::reactiveVal()] that only
#' invalidates on a real change, so the echo costs nothing while a limit the
#' user types comes straight through.
#'
#' @details
#' Pass `headroom` when something is drawn above the data that the limits have
#' to clear — significance brackets, for instance, via [stat_bracket_y_max()].
#' It is evaluated reactively, and the maximum is raised to meet it whenever the
#' requested one falls short; the control is updated to match, so the number on
#' screen is the limit actually in use rather than one the plot has quietly
#' overridden. The maximum is only ever raised this way, so a larger limit the
#' user chose is left alone.
#'
#' Seed the store alongside any `update*Input()` call that sets the limits (the
#' y-data observer, the Reset button) so the echo arrives as a no-op:
#'
#' ```r
#' y_range_store(list(min = y_range$min, max = y_range$max))
#' updateNumericInput(session, "y.min", value = y_range$min)
#' updateNumericInput(session, "y.max", value = y_range$max)
#' ```
#'
#' @param input The Shiny `input` object from inside `moduleServer()`.
#' @param session The Shiny `session` object from inside `moduleServer()`.
#' @param min_key,max_key Character strings — the limit controls' input ids,
#'   without namespacing.
#' @param headroom Optional function of no arguments returning the smallest
#'   acceptable maximum, or `NULL` for none. Returning `NULL` or a non-finite
#'   value leaves the requested maximum alone.
#' @param params Optional reactive-defaults store from
#'   [setup_reactive_defaults()], or `NULL`. A limit backed by the store follows
#'   it rather than the client input.
#'
#' @return A [shiny::reactiveVal()] holding `list(min = , max = )`. Call it with
#'   no arguments to read, and with a value to seed.
#'
#' @seealso [stat_bracket_y_max()], [setup_group_colors()]
#'
#' @importFrom shiny reactiveVal observe updateNumericInput
#'
#' @export
#' @author Jared Andrews
#' @examples
#' if (interactive()) {
#'     library(shiny)
#'
#'     server <- function(input, output, session) {
#'         y_range_store <- setup_axis_range(
#'             input, session,
#'             headroom = function() {
#'                 if (!isTRUE(input$stats.enabled)) {
#'                     return(NULL)
#'                 }
#'                 stat_bracket_y_max(iris, x = "Species", y = "Sepal.Length")
#'             }
#'         )
#'
#'         output$plot <- renderPlot({
#'             lims <- y_range_store()
#'             plot(iris$Sepal.Length, ylim = c(lims$min, lims$max))
#'         })
#'     }
#' }
setup_axis_range <- function(input, session, min_key = "y.min", max_key = "y.max",
                             headroom = NULL, params = NULL) {
    store <- reactiveVal(NULL)

    observe({
        pick <- function(key) {
            if (!is.null(params) && params$has(key)) params$get(key) else input[[key]]
        }
        lo <- pick(min_key)
        hi <- pick(max_key)

        floor_hi <- if (is.null(headroom)) NULL else tryCatch(headroom(), error = function(e) NULL)
        if (!is.null(floor_hi) && length(floor_hi) == 1 && is.finite(floor_hi) &&
            !.axis_limit_clears(hi, floor_hi)) {
            hi <- floor_hi
            # Keep the control honest about the limit actually in use.
            updateNumericInput(session, max_key, value = hi)
        }

        new <- list(min = lo, max = hi)
        if (!.same_axis_range(isolate(store()), new)) {
            store(new)
        }
    })

    store
}

#' Normalize a module's "no selection" column input to `NULL`
#'
#' Module selects use `""` for "none". Statistics helpers expect `NULL`, and a
#' grouping column that is numeric is a gradient rather than a nesting, which
#' the renders already treat as no grouping.
#'
#' @param value The input value.
#' @param df Optional data frame, needed only for `numeric_is_null`.
#' @param numeric_is_null Logical; when `TRUE`, a numeric column in `df` also
#'   yields `NULL`.
#'
#' @return `value`, or `NULL`.
#'
#' @author Jared Andrews
#' @rdname INTERNAL_blank_to_null
#' @keywords internal
.blank_to_null <- function(value, df = NULL, numeric_is_null = FALSE) {
    if (is.null(value) || length(value) != 1 || is.na(value) || !nzchar(value)) {
        return(NULL)
    }
    if (numeric_is_null && !is.null(df) && value %in% names(df) && is.numeric(df[[value]])) {
        return(NULL)
    }
    value
}

#' Is an axis limit present and already at or above a required minimum?
#'
#' @author Jared Andrews
#' @rdname INTERNAL_axis_limit_clears
#' @keywords internal
.axis_limit_clears <- function(limit, required) {
    !is.null(limit) && length(limit) == 1 && !is.na(limit) &&
        is.numeric(limit) && limit >= required
}

#' Compare two axis-range values for practical equality
#'
#' The limits make a round-trip through the browser as JSON, so the value that
#' comes back can differ from the one sent in the last bits of a double. An
#' exact comparison would treat that echo as a change and rebuild the plot.
#'
#' @author Jared Andrews
#' @rdname INTERNAL_same_axis_range
#' @keywords internal
.same_axis_range <- function(a, b) {
    if (is.null(a) || is.null(b)) {
        return(FALSE)
    }

    same <- function(p, q) {
        if (is.null(p) || is.null(q)) {
            return(is.null(p) && is.null(q))
        }
        if (length(p) != 1 || length(q) != 1) {
            return(identical(p, q))
        }
        if (is.na(p) || is.na(q)) {
            return(is.na(p) && is.na(q))
        }
        isTRUE(all.equal(p, q))
    }

    same(a$min, b$min) && same(a$max, b$max)
}

#' Set up auto-update/isolate logic for reactive contexts
#'
#' A helper function that encapsulates the common pattern of handling auto-update
#' functionality in module servers. When auto-update is disabled, it adds a dependency
#' on the update button. Returns a wrapper function that either isolates reactive
#' expressions or passes them through unchanged.
#'
#' @param input The Shiny input object from the module server,
#'   should have both `auto.update` (boolean) and `update` (button) inputs.
#' @param params Optional reactive-defaults store from [setup_reactive_defaults()],
#'   or `NULL`. When supplied, an `input$<key>` read whose `key` is backed by the
#'   store resolves from the store instead of the client input, so a parameter
#'   driven by the parent app updates in a single render.
#' @return A function that wraps reactive expressions. With `params = NULL` this is
#'   `identity` if auto-update is enabled (expressions will be reactive), or `isolate`
#'   if auto-update is disabled (expressions will not trigger reactivity). With a
#'   store supplied it is a wrapper with the same isolation semantics that additionally
#'   redirects store-backed `input$<key>` reads.
#'
#' @seealso [setup_reactive_defaults()]
#'
#' @details
#' This function consolidates the following common pattern:
#'
#' ```
#' auto_update <- input$auto.update
#' if (!auto_update) {
#'     input$update
#' }
#' isolate_fn <- if (auto_update) identity else isolate
#' ```
#'
#' Usage in a reactive context:
#'
#' ```
#' output$plot <- renderPlotly({
#'     isolate_fn <- setup_auto_update_logic(input)
#'     # Now use isolate_fn to wrap input values
#'     x_val <- isolate_fn(input$x.value)
#' })
#' ```
#'
#' @export
#' @author Jared Andrews
#' @examples
#' if (interactive()) {
#'     library(shiny)
#'     library(plotly)
#'
#'     ui <- fluidPage(
#'         viz_select_input("x_var", "X variable", choices = names(mtcars), selected = "wt"),
#'         viz_select_input("y_var", "Y variable", choices = names(mtcars), selected = "mpg"),
#'         checkboxInput("auto.update", "Auto-update", value = TRUE),
#'         actionButton("update", "Update"),
#'         plotlyOutput("myPlot")
#'     )
#'
#'     server <- function(input, output, session) {
#'         output$myPlot <- renderPlotly({
#'             isolate_fn <- setup_auto_update_logic(input)
#'             x_val <- isolate_fn(input$x_var)
#'             y_val <- isolate_fn(input$y_var)
#'             plot_ly(mtcars, x = ~ .data[[x_val]], y = ~ .data[[y_val]], type = "scatter",
#'                 mode = "markers")
#'         })
#'     }
#'
#'     shinyApp(ui, server)
#' }
setup_auto_update_logic <- function(input, params = NULL) {
    auto_update <- input$auto.update
    req(!is.null(auto_update))

    # If update button is required, add dependency on it
    if (!auto_update) {
        input$update
    }

    if (is.null(params)) {
        return(if (auto_update) identity else isolate)
    }

    function(x) {
        key <- .input_key(substitute(x))

        if (!is.null(key) && params$has(key)) {
            if (auto_update) params$get(key) else isolate(params$get(key))
        } else if (auto_update) {
            x
        } else {
            isolate(x)
        }
    }
}


#' The call names a user-typed expression is allowed to contain
#'
#' Shared by [safe_eval_filter()] and [validate_expression()], which previously
#' each carried their own verbatim copy of this list and of the AST walker in
#' [.expr_check_node()]. Two copies of a security allowlist is one copy too
#' many — widening one and forgetting the other is exactly how a sandbox
#' develops a hole.
#'
#' Every entry is a *pure* function: no I/O, no environment or namespace
#' access, no evaluation, no assignment. That is the property that makes the
#' allowlist safe, and it is the bar any addition must clear. Notably absent
#' and deliberately so: `system`, `file`, `eval`, `parse`, `get`, `assign`,
#' `::`, `$`, `[`, `[[`, `@`, `function`, and `<-`.
#'
#' @return A character vector of permitted call names.
#'
#' @author Jared Andrews
#' @rdname INTERNAL_expr_allowed_calls
#' @keywords internal
.expr_allowed_calls <- function() {
    c(
        # Comparison and logic
        "<", ">", "<=", ">=", "==", "!=",
        "&", "&&", "|", "||", "!", "xor",
        # Membership, construction, missingness
        "%in%", "c", "is.na", "is.null",
        # Grouping and arithmetic
        "(", "-", "+", "*", "/", ":", "%%",
        "abs", "round",
        # String and pattern matching, so name-based filtering (e.g. selecting
        # genes by prefix) is expressible without regex-free contortions.
        "grepl", "startsWith", "endsWith", "substr", "nchar",
        "toupper", "tolower", "trimws"
    )
}


#' The call names a user-typed model formula is allowed to contain
#'
#' The formula counterpart to [.expr_allowed_calls()], for the custom fit-line
#' models in [.safe_build_model()]. A formula needs a different vocabulary --
#' `~` and the model-building operators, plus the handful of transforms that
#' routinely appear on the right-hand side -- but it needs the same walker, so
#' the two lists differ and [.expr_check_node()] does not.
#'
#' The same purity bar applies: every entry is a mathematical transform or a
#' formula operator, with no I/O, environment access, evaluation or assignment.
#'
#' @return A character vector of permitted call names.
#'
#' @author Jared Andrews
#' @rdname INTERNAL_formula_allowed_calls
#' @keywords internal
.formula_allowed_calls <- function() {
    c(
        # Formula and model-building operators
        "~", "+", "-", "*", "/", "^", "(", ":", "I",
        # Transforms commonly applied to a term
        "log", "log2", "log10", "sqrt", "exp", "poly"
    )
}


#' Has a Shiny input reported an actual value?
#'
#' A Shiny input that has not reported yet is `NULL`, and every obvious test
#' against one is `logical(0)` rather than `FALSE` -- `nzchar(NULL)`,
#' `NULL == ""`, `is.na(NULL)` alike. `if (logical(0))` is an
#' `argument is of length zero` error, so each of those reads crashes the
#' reactive it sits in. Module servers read column and size inputs constantly,
#' and [viz_select_input()] is a custom binding that reports late, so the empty
#' value is reachable far more often than it looks.
#'
#' @param x A value from a Shiny input.
#'
#' @return `TRUE` for a length-1, non-`NA` value; `FALSE` for anything else,
#'   `NULL` and `character(0)` included.
#'
#' @author Jared Andrews
#' @rdname INTERNAL_has_value
#' @keywords internal
.has_value <- function(x) {
    !is.null(x) && length(x) == 1L && !is.na(x)
}


#' Is an input's value a usable, non-empty string?
#'
#' [.has_value()] narrowed to the column-selecting inputs, whose "nothing
#' chosen" state is the empty string rather than `NULL`.
#'
#' @param x A value from a Shiny input.
#'
#' @return `TRUE` for a length-1, non-`NA`, non-empty character scalar;
#'   `FALSE` for anything else, `NULL` included.
#'
#' @author Jared Andrews
#' @rdname INTERNAL_nz_value
#' @keywords internal
.nz_value <- function(x) {
    .has_value(x) && nzchar(x)
}


#' Walk a parsed expression and reject anything outside the allowlist
#'
#' The shared guard behind [safe_eval_filter()] and [validate_expression()].
#' Recurses the AST and permits only literals, symbols naming a column of the
#' data (or a bare logical/`NA`/`Inf` constant), and calls whose name is in
#' [.expr_allowed_calls()]. Anything else — an unknown symbol, a call to a
#' function not on the list, a construct that is neither — returns `FALSE`.
#'
#' Note that a function name reaches this as `node[[1L]]` of a call and is
#' checked against the allowlist, never as a symbol, so allowing a call name
#' does not also make it usable as a bare value.
#'
#' @param node A node of a parsed expression, as from [parse()].
#' @param col_names Character vector of column names the expression may refer to.
#' @param allowed Character vector of permitted call names. Defaults to
#'   [.expr_allowed_calls()], the filter/highlight vocabulary; model formulas
#'   pass [.formula_allowed_calls()] instead. The vocabulary is the only thing
#'   that varies between the two -- the walking and the rejection rules are
#'   deliberately shared.
#'
#' @return `TRUE` if every node is permitted, `FALSE` otherwise.
#'
#' @author Jared Andrews
#' @rdname INTERNAL_expr_check_node
#' @keywords internal
.expr_check_node <- function(node, col_names, allowed = .expr_allowed_calls()) {
    if (is.atomic(node) || is.null(node)) {
        return(TRUE)
    }
    if (is.symbol(node)) {
        nm <- as.character(node)
        return(nm %in% col_names || nm %in% c("TRUE", "FALSE", "T", "F", "NA", "NULL", "Inf", "NaN"))
    }
    if (is.call(node)) {
        # A namespaced or extracted call (`base::system`, `x$f`) has a *call*
        # rather than a name in position 1; as.character() on it would flatten
        # to something that could coincidentally match the allowlist, so
        # require a plain length-1 name.
        fn <- node[[1L]]
        if (!is.symbol(fn)) {
            return(FALSE)
        }
        if (!as.character(fn) %in% allowed) {
            return(FALSE)
        }
        for (i in seq_along(node)[-1]) {
            if (!.expr_check_node(node[[i]], col_names, allowed)) {
                return(FALSE)
            }
        }
        return(TRUE)
    }
    if (is.pairlist(node)) {
        return(all(vapply(
            node, .expr_check_node, logical(1),
            col_names = col_names, allowed = allowed
        )))
    }
    FALSE
}


#' Safely evaluate a user-provided filter expression against a data frame
#'
#' Parses the expression text, validates that it only contains allowed
#' operations (comparisons, logical operators, column references, and
#' literals), then evaluates it in a restricted environment containing
#' only the data frame columns. Returns a logical vector suitable for
#' row subsetting, or `NULL` if the input is empty or invalid.
#'
#' **Use this function any time a module evaluates a user-typed expression
#' directly** (e.g., a row-filter text input). Never call `eval(str2expression())`
#' on raw user input — doing so allows arbitrary code execution on the server.
#'
#' @param expr_text Character string containing the filter expression
#'   (e.g., `"Sepal.Length > 5 & Species == 'setosa'"`).
#' @param data A `data.frame` whose columns are made available for the expression.
#' @return A logical vector the same length as `nrow(data)`, or `NULL` if the
#'   input is empty, unparseable, or contains disallowed operations.
#'
#' @export
#' @author Jared Andrews
#' @examples
#' safe_eval_filter("Sepal.Length > 5", iris)
#' safe_eval_filter("Sepal.Length > 5 & Species == 'setosa'", iris)
#' safe_eval_filter("", iris) # NULL
#' safe_eval_filter("system('echo pwned')", iris) # NULL + warning
safe_eval_filter <- function(expr_text, data) {
    if (is.null(expr_text) || !nzchar(trimws(expr_text))) {
        return(NULL)
    }

    parsed <- tryCatch(parse(text = expr_text), error = function(e) NULL)
    if (is.null(parsed) || length(parsed) == 0) {
        warning("Could not parse filter expression.")
        return(NULL)
    }

    # One statement only. Checking parsed[[1L]] and evaluating the same node is
    # safe, but it silently throws away everything after a `;` or a newline --
    # so a two-clause filter would quietly return the mask for its first clause
    # alone. Saying so beats guessing on the user's behalf.
    if (length(parsed) > 1) {
        warning("Filter expression must be a single statement.")
        return(NULL)
    }

    # Walk the AST and ensure only allowlisted operations are used. The
    # allowlist and the walker are shared with validate_expression() -- see
    # .expr_allowed_calls() / .expr_check_node().
    expr <- parsed[[1L]]
    if (!.expr_check_node(expr, names(data))) {
        warning(
            "Filter expression contains disallowed operations. ",
            "Only column references, comparisons, and logical operators are permitted."
        )
        return(NULL)
    }

    # Evaluate in a restricted environment with only data columns
    env <- list2env(as.list(data), parent = baseenv())
    tryCatch(
        eval(expr, envir = env),
        error = function(e) {
            warning("Filter expression error: ", e$message)
            NULL
        }
    )
}

#' Safely resolve an adjustment function name to an actual function
#'
#' Validates that the provided function name is in the allowed list before
#' converting it to a function reference. Returns `NULL` for empty strings
#' or unrecognized names.
#'
#' **Use this instead of `eval(str2expression())` when resolving function names
#' from user input** (e.g., a dropdown that selects a transformation like
#' `"log2"` or `"sqrt"`).
#'
#' @param fn_name Character string — name of the adjustment function.
#'   Currently allowed values: `"log2"`, `"log"`, `"log10"`, `"neg_log10"`,
#'   `"log1p"`, `"as.factor"`, `"abs"`, `"sqrt"`.
#' @return The corresponding function, or `NULL` if `fn_name` is empty or
#'   not in the allowed list.
#'
#' @export
#' @author Jared Andrews
#' @examples
#' safe_resolve_adj_fxn("log2") # returns log2
#' safe_resolve_adj_fxn("") # NULL
#' safe_resolve_adj_fxn("system") # warning + NULL
safe_resolve_adj_fxn <- function(fn_name) {
    if (is.null(fn_name) || !nzchar(trimws(fn_name))) {
        return(NULL)
    }

    allowed <- c("log2", "log", "log10", "neg_log10", "log1p", "as.factor", "abs", "sqrt")
    if (!fn_name %in% allowed) {
        warning("Unrecognized adjustment function: ", fn_name)
        return(NULL)
    }

    match.fun(fn_name)
}

#' Validate a user-provided expression string for safety
#'
#' Parses the expression text and walks the AST to ensure it only contains
#' allowed operations (comparisons, logical operators, column references, and
#' literals). Returns the original string if valid, or `NULL` if the input
#' is empty, unparseable, or contains disallowed operations. This is useful
#' when the expression string must be passed through to a downstream function
#' (e.g., `plotthis::BoxPlot(highlight = ...)`) rather than evaluated directly.
#'
#' **Use this when a module passes a user-typed expression string to an
#' external plotting function** that will evaluate it internally. The string
#' is validated but not executed by this function.
#'
#' @param expr_text Character string containing the expression to validate
#'   (e.g., `"group == 'A' & value > 10"`).
#' @param col_names Character vector of allowed column/symbol names
#'   (typically `names(data)`).
#' @return The original `expr_text` string if safe, or `NULL`.
#'
#' @export
#' @author Jared Andrews
#' @examples
#' validate_expression("Sepal.Length > 5", names(iris))
#' validate_expression("system('echo pwned')", names(iris)) # NULL + warning
#' validate_expression("", names(iris)) # NULL
validate_expression <- function(expr_text, col_names) {
    if (is.null(expr_text) || !nzchar(trimws(expr_text))) {
        return(NULL)
    }

    parsed <- tryCatch(parse(text = expr_text), error = function(e) NULL)
    if (is.null(parsed) || length(parsed) == 0) {
        warning("Could not parse expression.")
        return(NULL)
    }

    # One statement only, and this one is load-bearing: unlike
    # safe_eval_filter(), which evaluates the node it checked, this returns the
    # *original string* for a caller to evaluate. Checking parsed[[1L]] and
    # handing back the whole text would let everything after a `;` through
    # entirely unexamined.
    if (length(parsed) > 1) {
        warning("Expression must be a single statement.")
        return(NULL)
    }

    # Allowlist and walker shared with safe_eval_filter() -- see
    # .expr_allowed_calls() / .expr_check_node().
    expr <- parsed[[1L]]
    if (!.expr_check_node(expr, col_names)) {
        warning(
            "Expression contains disallowed operations. ",
            "Only column references, comparisons, and logical operators are permitted."
        )
        return(NULL)
    }

    expr_text
}

#' Extract parameter documentation from an R function help page
#'
#' Parses the Rd documentation for a given function and extracts
#' parameter descriptions for specified parameter names.
#'
#' @param package_name A string in the format "package::function" indicating
#'   which function's documentation to parse.
#' @param type The type of documentation section to extract. Currently only
#'   "param" is supported.
#' @param selected A list of parameter names to extract. Note that co-documented
#'   parameters (e.g., `x.by` and `y.by`) should be grouped together in a vector
#'   within the list or an error will be thrown by `extract_roc_text`.
#' @param cap Logical; if TRUE, capitalize the first letter of each description.
#'
#' @return A named list where names are parameter names and values are
#'   their documentation strings. Returns empty strings for parameters
#'   not found in the documentation.
#'
#' @importFrom roclang extract_roc_text
#' 
#' @author Jacob Martin, Jared Andrews
#' @export
get_documentation <- function(package_name, type = "param", selected = NULL, cap = FALSE) {
    docs <- lapply(selected, function(s) {
        doc <- tryCatch(
            extract_roc_text(package_name, type = type, select = s, capitalize = cap),
            error = function(e) NA_character_
        )

        if (length(doc) == 0 || all(is.na(doc))) {
            return("")
        }

        doc |>
            gsub("\\\\n", " ", x = _) |>
            gsub("\\\\", "", x = _) |>
            gsub("code\\{([^}]+)\\}", "`\\1`", x = _) |>
            gsub("\n", " ", x = _) |>
            trimws(x = _)
    })

    # Expand co-documented parameters (e.g., c("x.by", "y.by")) into
    # separate named elements sharing the same documentation string.
    result <- list()
    for (i in seq_along(selected)) {
        param_names <- selected[[i]]
        for (nm in param_names) {
            result[[nm]] <- docs[[i]]
        }
    }
    result
}
