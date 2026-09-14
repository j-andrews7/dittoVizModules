---
name: vizmodules-custom-module
description: Build a custom Shiny wrapper module on top of a VizModules plot module, or extend one from outside the package - adding your own inputs, filtering or transforming the data before it reaches the plot, driving a base module's parameters from your wrapper's state, registering a custom model-line backend with register_model_backend(), or adding multiColorPicker()/multiDynamicInput() widgets. Use whenever wrapping, extending, subclassing, or composing a VizModules module, or when a hand-rolled plotly output needs the same manual-edit persistence the built-in modules have. The wrapper lives in your own app or package; adding a module to the VizModules package source is vizmodules-new-module, and plain use of an existing module in an app is vizmodules-app.
license: MIT
---

# Wrapping and extending a VizModules module

## The namespace rule — get this right first

This is the one mistake that produces an app which *looks* fine and silently does
nothing. Shiny namespacing is the whole difficulty of wrapping a module.

```r
myModuleUI <- function(id) {
    ns <- NS(id)
    tagList(
        checkboxInput(ns("filter_setosa"), "Setosa only"),   # YOUR input: ns()
        dittoViz_scatterPlotInputsUI(id, iris)               # BASE module: bare id
    )
}

myModuleOutput <- function(id) dittoViz_scatterPlotOutputUI(id)   # bare id

myModuleServer <- function(id, data_reactive) {
    # 1. Your logic goes INSIDE moduleServer() — that is what gives you input$filter_setosa.
    filtered <- moduleServer(id, function(input, output, session) {
        reactive({
            df <- req(data_reactive())
            if (isTRUE(input$filter_setosa)) df <- df[df$Species == "setosa", ]
            df
        })
    })

    # 2. The base server goes OUTSIDE it. This is the critical line.
    dittoViz_scatterPlotServer(id, filtered)
}
```

Three rules, and they are not symmetric:

| Thing | Namespacing |
|---|---|
| Your own inputs, in your UI | `ns("name")` |
| Base `*InputsUI()` / `*OutputUI()`, in your UI | bare `id` |
| Your own logic, in your server | inside `moduleServer(id, ...)` |
| Base `*Server()`, in your server | **outside** `moduleServer()`, bare `id` |

Calling the base server *inside* `moduleServer()` double-namespaces it: it looks for
`id-id-x.by` while the UI created `id-x.by`. Nothing errors. The controls just have no
effect. If a user reports "the inputs do nothing", check this first.

## Driving a base module parameter from your wrapper

Pass a `reactive()` as that entry of `defaults`. Do **not** reach for `update*Input()` —
it is an async client round-trip, so the plot renders twice per change.

```r
colouredServer <- function(id, data_reactive) {
    colour_col <- moduleServer(id, function(input, output, session) {
        reactive(if (isTRUE(input$by_species)) "Species" else "")
    })

    dittoViz_scatterPlotServer(id, data_reactive, defaults = list(color.by = colour_col))
}
```

The colour mapping follows the checkbox, renders once, and the control stays
user-editable. Hiding a control instead (`hide.inputs`) fixes it to one value — use that
when the parameter should never move.

**The key must be an input the module actually reads.** A `defaults` entry for a key no
module exposes is a silent no-op — nothing errors, the plot just ignores it. Check the
key against `references/../module-inventory.md` or the module's own
**Plot parameters and defaults** roxygen section before relying on it.

> Known trap: `main` (plot title) is **not** exposed by any module — every server passes
> `main = NULL` and none reads `input$main`, so `defaults = list(main = ...)` is a silent
> no-op. To drive a title, act on the plotly figure directly (e.g. `plotlyProxy()` against
> the base module's output id, or `layout(title = ...)` in a hand-built figure). Older
> installs' vignettes used `main` as their reactive-defaults worked example; that was
> corrected to `color.by`, but the example is still wrong wherever it survives.

## Updating your *own* inputs from the server

Freeze first, or the plot renders twice:

```r
observeEvent(input$stat.x, {
    pairs <- generate_pair_strings(data(), input$stat.x)
    if (length(pairs) > 0) {
        freezeReactiveValue(input, "stat.pairs")     # inside the same if branch
        update_viz_select(session, "stat.pairs", choices = c("", pairs), selected = "")
    }
})
```

- Freeze **only** when you will definitely update — a frozen input that never receives a value leaves the plot suspended.
- Never wrap `renderPlotly()` in a catch-all `tryCatch()`. The pause is a silent condition; swallowing it turns a clean pause into a blank plot. Use `req()` and explicit `if` branches.
- Freezing does **not** work for an input rebuilt by `renderUI()`. Use a server-side store instead — `setup_group_colors()` for colour pickers, `setup_axis_range()` for axis limits. See `references/reactive-and-rerender.md`.
- **Free text is the opposite problem: debounce it.** `textInput()`/`textAreaInput()` report on every keystroke, so a base module reading one directly is rebuilt once per character — and an expression input spends most of those renders on text that cannot parse yet. `query <- debounce(reactive(input$query), 700)`, created once in the server body (inside a reactive it rebuilds the timer and does nothing). It emits its initial value immediately, so there is no blank first render. Select, numeric, and checkbox inputs need nothing.

## Other extension points

- **Manual layout edits** on a hand-rolled plotly output — two calls, `setup_manual_edits()` and `finalize_manual_edits()`. Inherited for free if you delegate to a base module. See `references/manual-edits.md`.
- **Model-line backends** — `register_model_backend(name, backend)` adds a fitting engine to the scatter module's Model Type dropdown. See `references/model-backends.md`.
- **Widgets** — `multiColorPicker()` and `multiDynamicInput()` are exported and usable in any Shiny app. See `references/custom-inputs.md`, which also covers styling a widget of your own: a stylesheet goes into the *host* document unscoped, so every selector must be anchored on a class you invented.
- **Runtime show/hide** — `hide_input(session, ids)` / `show_input(session, ids)`, not `shinyjs::hide()`; the VizModules helpers reflow the grid.
- **User-typed expressions** — `safe_eval_filter()`, `validate_expression()`, `safe_resolve_adj_fxn()`. Never `eval(parse())` on user input.

## Where the vignettes will mislead you

`vignette("custom-modules")` is right about the namespace rule and wrong about two
things next to it. Both were corrected in the package after agents hit them, but older
installs still carry them:

- **`hide.inputs`/`hide.tabs` are `*Server()` arguments, not `*InputsUI()` arguments.** Every `*InputsUI()` has the signature `(id, data, defaults = NULL, title = NULL, columns = 2)` and none takes `...`, so passing `hide.inputs` to the UI is an unused-argument **error**, not a no-op.
- **`main` is not exposed by any module**, so a `defaults = list(main = ...)` example cannot work. See the trap note above.

When wrapping, forward `hide.tabs` rather than replacing it, so a caller can hide more
without un-hiding what you enforce:

```r
dittoViz_scatterPlotServer(id, filtered,
    defaults  = defaults,
    hide.tabs = union(hide.tabs, "Plotly"))
```

## Driving a plotly figure directly

If you must reach the figure itself (a title, say, which no module exposes), send **one**
`plotlyProxy()` message per real change — not one per input change. Re-asserting on every
control change desynchronises Shiny's output pipeline and leaves the plot rendering one
change behind. Shiny's re-render also purges plotly's client-side event listeners, so a
one-time `gd.on(...)` hook dies after the first redraw; re-register on `shiny:value`.

## Verifying your work

`shiny::testServer()` covers namespacing, filtering, and what reaches the base server —
which is the whole substance of a wrapper. It cannot drive the plot: the mock session
never renders the `renderUI()`-built colour picker and never registers plotly events, so
the render dead-ends in a silent `req()`. Confirm that by running the stock base server
as a control; it fails identically.

Booting a real browser costs several times what `testServer()` does and, for a wrapper,
usually tells you nothing new. Reserve it for behaviour that genuinely only exists in the
client, such as a `plotlyProxy()` interaction.

## Practices

1. Keep each wrapper to one cohesive concern; design so it could itself be wrapped.
2. Document what columns or data shape your wrapper requires.
3. Pass data in as a `reactive()`, and return one where it is useful.
4. Base module servers return their source-data reactive — capture it if your wrapper needs to expose downloads.
5. If inputs seem to have no effect, it is the namespace rule. It is almost always the namespace rule.
6. If an *unrelated* control in your app starts rendering oddly once a module is on the page, suspect CSS rather than data — the package's stylesheets load into your document. See the styling section of `references/custom-inputs.md`.
