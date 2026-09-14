# Full checklist

Audit against this before opening a PR.

## Naming and organisation

- [ ] Module named for what it wraps: `dittoViz_<plotName>`, `plotthis_<PlotName>`, or the bare plot name for a native function.
- [ ] Three files: `R/<module>_module_ui.R`, `R/<module>_module_server.R`, `R/<module>_module_app.R`.
- [ ] Four exports: `<module>InputsUI()`, `<module>OutputUI()`, `<module>Server()`, `<module>App()`.
- [ ] Signatures match the package-wide contract exactly (see SKILL.md).
- [ ] Broadly useful internal helpers go in the existing helper files; module-specific ones stay in the module file.

## A new plotting function (only if you added one)

- [ ] Defined under `R/` with full roxygen: inputs, returns, examples.
- [ ] Arguments consistent with the existing plot functions — data first, `...` last, the same palette/palcolor patterns.
- [ ] Assumptions about input shape documented (e.g. a pre-summarised table for pies).
- [ ] `testthat` coverage for happy paths and invalid inputs.
- [ ] Stable before you wrap it. Build the module second.

## Documentation

- [ ] All three `@section` blocks on the UI function (see `roxygen-sections.md`).
- [ ] Every upstream argument appears in exactly one of the first two sections.
- [ ] `ggplotly()` conversion quirks that change or drop functionality are noted.
- [ ] `@param`, `@return`, `@author`, `@export`, `@seealso`, `@examples` on every exported function.
- [ ] `@importFrom pkg fun` for everything the file calls; no `pkg::fun()` in bodies (examples and vignettes excepted).
- [ ] `devtools::document()` run; `NAMESPACE` never hand-edited.

## Server wiring

- [ ] `params <- setup_reactive_defaults(defaults, input, session)` is the first statement of the `moduleServer()` body.
- [ ] `isolate_fn <- setup_auto_update_logic(input, params)` is the first line of the generate reactive.
- [ ] Every read is the literal `isolate_fn(input$key)` form; conversions happen outside the call.
- [ ] `plot_source <- session$ns("<short>")` and `setup_manual_edits()` near the top.
- [ ] `finalize_manual_edits()` is the last thing `renderPlotly()` does.
- [ ] `freezeReactiveValue()` immediately before every `update*Input()` on the module's own inputs, inside the same `if` branch.
- [ ] `renderUI()`-rebuilt inputs use a server-side store (`setup_group_colors()`, `setup_axis_range()`), not a freeze.
- [ ] No catch-all `tryCatch()` around `renderPlotly()`.
- [ ] `hide.inputs` / `hide.tabs` applied inside `observeEvent(data(), delay(100, ...))`, not once at init.
- [ ] A reset observer that covers every input, calling each `reset_*_inputs()` counterpart you used.
- [ ] The server returns its source-data reactive.

## Inputs and style

- [ ] `viz_select_input()` / `update_viz_select()` everywhere; no `selectInput()` / `updateSelectInput()`.
- [ ] Every default goes through `get_default()`, with a validator where the type matters.
- [ ] Uniform helpers reused rather than hand-rolled controls (see `uniform-helpers.md`).
- [ ] Non-obvious inputs wrapped in `tipify(..., placement = "top", options = list(container = "body"))`.
- [ ] Input labels capitalised, concise, loosely matching the upstream parameter names.
- [ ] 4-space indent, 120-character lines. `vapply()`/`lapply()`, never `sapply()`.

## Security

- [ ] No `eval(parse())` or `eval(str2expression())` on user input, anywhere.
- [ ] Free-text filter expressions go through `safe_eval_filter(expr_text, data)`.
- [ ] Expressions passed downstream for evaluation go through `validate_expression(expr_text, col_names)`.
- [ ] Function names from a dropdown or text box go through `safe_resolve_adj_fxn(fn_name)`.

All three share one whitelist: comparisons (`<` `>` `<=` `>=` `==` `!=`), logical
operators (`&` `&&` `|` `||` `!`), `%in%`, `c()`, `is.na()`, `is.null()`, arithmetic
(`-` `+` `*` `/` `:` `%%`), grouping parentheses, column names, and literals. Anything
else — `system()`, `file.remove()`, `library()` — is rejected with a warning.
`safe_resolve_adj_fxn()` additionally allows only `log2`, `log`, `log10`, `neg_log10`,
`log1p`, `as.factor`, `abs`, `sqrt`.

## App, gallery and figure builder

- [ ] `<module>App()` is a thin `createModuleApp()` wrapper — no duplicated import, filtering, or dataset-switching logic.
- [ ] A sensible bundled example dataset as the default `data_list`.
- [ ] The module added to `inst/apps/module-gallery/app.R` in its own tab with a small sample dataset.
- [ ] The module added to `.figure_builder_registry()` in `R/figureBuilder_module_app.R` — a *second* registry, easily missed, with its own shape (`label`, `dataset`, the trio, `defaults`).
- [ ] A module whose output is not plotly supplies `vector_svg` and `raster_png` functions of `(width, height, res)` -- on the summary list for the source download's images, and `vector_svg` also as an attribute on the reactive for the Figure Builder's canvas export -- or its panels export as a bare label and its archive carries no picture (see `ComplexHeatmap_HeatmapServer()`, `draw_to_svg()` and `draw_to_png()`).
- [ ] An example that uses the module **twice** to prove multi-instance behaviour; each instance holds independent state.

## CSS (only if you added any)

See `css-containment.md`. Skip this section entirely if you wrote no stylesheet and no
`tags$style()` block.

- [ ] Every selector is anchored on a class this package invented (`.mc-`, `.mdi-`, `.vizmodules-`, `.viz-`, `.pb-`, …). Nothing bare against `.selectize-dropdown`, `.option`, `.well`, `.nav-tabs`, `.form-group`, `.control-label`, `.btn` or any other Bootstrap/Shiny/selectize/DT class — those reach the whole host page.
- [ ] Anything parented to `<body>` (a dropdown, a popover) carries its own marker class, since a `.my-widget .thing` selector cannot reach it.
- [ ] Each selector verified against the rendered DOM — a rule targeting a class your widget never emits does nothing for you and everything to everyone else.
- [ ] No layout in inline `style=`; per-instance values passed as CSS custom properties instead.
- [ ] No dependence on the parent's padding (no Bootstrap negative-margin rows); `gap` for spacing, `min-width: 0` on flex children.
- [ ] Stylesheet in `inst/src/`, served by an `htmlDependency()`, attached to the widget's markup with `attachDependencies()` so it survives `insertUI()`.
- [ ] The containment test in `tests/testthat/test-ui_utils.R` passes, with your prefix added to its owned list.

## Tests

- [ ] `tests/testthat/test-<plot>.R` exists.
- [ ] `testthat` coverage of the base plotting function's core behaviour where one was added.
- [ ] `testServer` coverage of the module server where feasible.
- [ ] Headless and deterministic; randomness seeded.

## Before submitting

- [ ] `devtools::document()`
- [ ] `devtools::test()`
- [ ] `devtools::check()`
- [ ] Exports added to `_pkgdown.yml` under the right reference section.
- [ ] An entry in `NEWS.md`.
- [ ] UI text and tooltips mention any missing or altered plot features.
