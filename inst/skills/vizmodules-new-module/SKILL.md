---
name: vizmodules-new-module
description: Add a brand-new plot module to the VizModules R package itself - wrapping a dittoViz, plotthis, or native plotting function as the standard InputsUI/OutputUI/Server/App quartet, with the required roxygen sections, reactive-defaults and manual-edit wiring, uniform input helpers, gallery registration, and tests. Use when contributing a module to VizModules, following the adding-a-new-module checklist, or when asked to implement a planned module such as dittoViz freqPlot, barPlot, ridgePlot, or scatterHexPlot. Only for modules that live in the VizModules package source under R/; a wrapper module in your own app or package is vizmodules-custom-module, and merely using an existing module is vizmodules-app.
license: MIT
---

# Adding a module to VizModules

Work from the templates in `templates/` — they encode the whole contract. Copy them,
rename, and fill in. Do **not** read an existing 900-line module server to infer the
pattern; the templates are that pattern, distilled.

## Name and files

| Wrapping | Module name | Files |
|---|---|---|
| `dittoViz::freqPlot` | `dittoViz_freqPlot` | `R/dittoViz_freqPlot_module_{ui,server,app}.R` |
| `plotthis::RidgePlot` | `plotthis_RidgePlot` | `R/plotthis_RidgePlot_module_{ui,server,app}.R` |
| a new native function | `myPlot` | `R/myPlot.R` first, then `R/myPlot_module_{ui,server,app}.R` |

Four exports per module: `<name>InputsUI()`, `<name>OutputUI()`, `<name>Server()`,
`<name>App()`. Signatures are fixed:

```r
<name>InputsUI(id, data, defaults = NULL, title = NULL, columns = 2)
<name>OutputUI(id, resizable = TRUE)
<name>Server(id, data, hide.inputs = NULL, hide.tabs = NULL, defaults = NULL)
<name>App(data_list = NULL, defaults = NULL, hide.inputs = NULL, hide.tabs = NULL)
```

If you are adding a brand-new plotting function, define, document, and test **that**
first. Only wrap it once it is stable.

## The eight things that are easy to get wrong

1. **Three roxygen `@section` blocks on the UI function** are mandatory: parameters not implemented, parameters and defaults, parameters implementing new functionality. See `references/roxygen-sections.md`.
2. **Reactive defaults**: `params <- setup_reactive_defaults(defaults, input, session)` must be the *first* statement of the `moduleServer()` body, and `isolate_fn <- setup_auto_update_logic(input, params)` the first line of the generate reactive. Every read must stay in the literal `isolate_fn(input$key)` form — `isolate_fn(as.numeric(input$size))` cannot be recognised and silently loses reactive-default support. Convert outside the call.
3. **Manual edits**: `plot_source <- session$ns("<short>")` + `edit_store <- setup_manual_edits(...)` near the top, and `finalize_manual_edits()` as the last thing `renderPlotly()` does.
4. **Freeze before you update your own inputs**, or the plot renders twice. For `renderUI()`-rebuilt inputs use `setup_group_colors()` / `setup_axis_range()` instead — freezing does not work there.
5. **Debounce any free-text input the plot reads.** `textInput()` reports on every keystroke, so an undebounced read rebuilds the plot once per character — and for an expression input, most of those characters are a state that cannot parse. `debounce(reactive(input$key), 700)`, created once in the server body. It emits its initial value immediately, so startup is unaffected. Select/numeric/checkbox inputs report discrete choices and need nothing.
6. **Never `eval(parse())` / `eval(str2expression())` on user input.** Use `safe_eval_filter()`, `validate_expression()`, or `safe_resolve_adj_fxn()`. A publicly deployed app otherwise executes arbitrary code.
7. **Reuse the uniform input helpers** rather than writing your own Axes/Legend/Lines/Plotly controls. See `references/uniform-helpers.md`.
8. **Any CSS you add lands in the host app's document.** There is no scoping, and every module pulls a colour picker in, so one bare selector against a Bootstrap/selectize/DT class restyles apps that merely embedded a plot. Anchor every rule on a class this package invented. See `references/css-containment.md`.

## Before you write anything: what shape is the plot function?

The single biggest source of rework. Ask first:

1. **Does it plot columns of the input, or a summary it computes itself?** `dittoViz::freqPlot` tabulates per-sample frequencies and plots *those*. If yours summarises, then axis limits, statistics, point annotations, and the source download must all read the **summary** frame (via the function's own `data.only` / `data.out` argument), not the incoming data. Getting this wrong is invisible until the numbers disagree with the picture.
2. **Does it force its own faceting or grouping?** `freqPlot` always facets on the frequency variable, so there is no `split.by` to expose and statistics can only ever be per-facet — pooling across facets compares non-comparable quantities. Hide the control rather than leaving a dead one.
3. **Does any bundled dataset have the shape it needs?** Often not. `freqPlot` needs samples nested inside groups; without that the demo app opens on one point per group and the underlying function warns. Adding a dataset is normal and has precedent (`example_heatmap_matrix` shipped with the ComplexHeatmap module): generator into `data-raw/generate_example_data.R`, docs into `R/data.R`, name into `R/globals.R`, entry in `_pkgdown.yml`.
4. **Check the upstream function for its own bugs before working around them.** `freqPlot(data.only = TRUE)` returns before applying its own `vars.use` subsetting, so the summary silently disagrees with the plot. Pin anything like that with a test.

## Traps inside this package

- **`.blank_to_null()` returns `NULL` for anything not length 1.** A multi-select feeding it reads as "no selection", so every facet is drawn. Handle multi-value inputs explicitly.
- **Named palettes do not apply to dittoViz ridgeplots.** They fill by an internal composite column, so a named vector matches nothing and ggplot2 silently drops every colour to grey. Drop the names for that layer.
- **`boxgap`/`boxgroupgap` are not in plotly 4.12.1's layout schema.** The warning is pre-existing and package-wide (`dittoViz_yPlot` and `plotthis_BoxPlot` emit it identically). Match the siblings rather than diverging one module.
- **Freeze only what you will actually update, and never at startup.** An unconditional freeze on an input the generate reactive always reads suspends the plot forever, waiting for an echo that never comes. Use `ignoreInit = TRUE`.

## Verifying your work

`shiny::testServer()` cannot drive the plot output — the mock session never renders the
`renderUI()`-built colour picker and never registers plotly events, so it dead-ends in a
silent `req()`. Test the summarisation helpers, the UI payload, and the generate reactive
directly instead; that is why the package's existing tests are shaped the way they are.

Finish with `devtools::document()`, `devtools::test()`, then `devtools::check()`. If
pandoc is missing, `check()` fails at the vignette-rebuild stage for **all** vignettes,
including untouched ones — re-run with `vignettes = FALSE` and say that stage is
unverified rather than chasing it.

## Style

- `viz_select_input()` / `update_viz_select()`, never `selectInput()` / `updateSelectInput()`. It renders a virtualised dropdown, so a column with tens of thousands of levels stays usable. Empty string means "no selection" and displays as `(none)`.
- Every default read goes through `get_default(defaults, key, fallback, validator)`.
- `@importFrom pkg fun` in the roxygen header, then bare `fun()` in the body. Avoid `pkg::fun()` except in `@examples` and vignettes.
- Wrap any non-obvious input in `shinyBS::tipify(..., placement = "top", options = list(container = "body"))`. Self-explanatory labels ("Plot Title") need none. `get_documentation()` pulls the upstream package's parameter text for tooltips.
- Capitalise the first word of every input label; keep labels short and put detail in the tooltip.
- 4-space indent, 120-character lines (enforced by `.lintr`). Use `vapply()`/`lapply()`, never `sapply()`.
- Never edit `NAMESPACE` by hand — regenerate with `devtools::document()`.

## Finishing

- Register the module in `inst/apps/module-gallery/app.R` (its own tab, small sample dataset) **and** in `.figure_builder_registry()` (`R/figureBuilder_module_app.R`) — two registries, different shapes.
- Add `tests/testthat/test-<plot>.R`; cover a new plotting function directly, and the module with `testServer` where feasible.
- Add the exports to `_pkgdown.yml` and an entry to `NEWS.md`.
- Run `devtools::document()`, then `devtools::test()` and `devtools::check()`.
- Prove multi-instance behaviour: two instances with different ids must be independent.

`references/checklist.md` is the full tick-list if you want to audit at the end.
