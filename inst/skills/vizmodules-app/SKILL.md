---
name: vizmodules-app
description: Build or modify a Shiny app that uses VizModules plot modules (dittoViz_scatterPlot, dittoViz_yPlot, dittoViz_freqPlot, plotthis_BoxPlot/ViolinPlot/BarPlot/AreaPlot/DensityPlot/DotPlot/Histogram/SplitBarPlot, linePlot, piePlot, radarPlot, dumbbellPlot, parallelCoordinatesPlot, ComplexHeatmap_Heatmap). Use when wiring a plot module into an app, pre-filling controls with defaults, hiding inputs or tabs, enabling the Stats tab, adding the dataFilter table or figure builder, using createModuleApp(), or letting users download plot source data. Covers the R package VizModules. Not for building a wrapper module around a base module (use vizmodules-custom-module), not for adding a module to the VizModules package source (use vizmodules-new-module), and not for repository or documentation maintenance on the package itself.
license: MIT
---

# Using VizModules in a Shiny app

## The contract

Every plot module is a trio of functions sharing one `id`:

```r
<module>InputsUI(id, data, defaults = NULL, title = NULL, columns = 2)  # controls; takes a plain data.frame
<module>OutputUI(id, resizable = TRUE)                                  # the plotly output
<module>Server(id, data, hide.inputs = NULL, hide.tabs = NULL, defaults = NULL)  # data is a reactive()
```

Every module has that exact server signature. `InputsUI` gets a **plain data frame** (it
needs column names at UI-build time); `Server` gets a **`reactive()`**. Place `InputsUI`
and `OutputUI` anywhere in the layout — they are separate on purpose.

```r
ui <- fluidPage(sidebarLayout(
    sidebarPanel(plotthis_ViolinPlotInputsUI("v", example_rnaseq,
        defaults = list(x.data = "cell_type", y.data = "log2_cpm", group.by = "condition"))),
    mainPanel(plotthis_ViolinPlotOutputUI("v"))
))
server <- function(input, output, session) {
    plotthis_ViolinPlotServer("v", data = reactive(example_rnaseq))
}
```

## Look up the keys — do not guess them

`defaults` keys are **input IDs**, and they are *not* uniform across modules: the
x/y mapping is `x.data`/`y.data` on the plotthis modules, `x.by`/`y.by` on
`dittoViz_scatterPlot`, `x.value`/`y.value` on `linePlot`/`dumbbellPlot`, and `var` on
`dittoViz_yPlot`. The colour key is `palette.colours` on most modules but `color.panel`,
`slice.colors`, or `trace.colors` on three of them.

**Read `references/module-inventory.md` before writing any `defaults` or `hide.*`
vector.** It has every module's mapping keys, tab names, colour key, and stats support in
one table. An unrecognised key is *silently ignored* — `get_default()` falls back rather
than erroring — so a typo looks like "the default didn't work".

For the long tail of inputs beyond the mapping keys, the module's own help page
(`?plotthis_ViolinPlotInputsUI`) has a **Plot parameters and defaults** section listing
every wired argument with its UI label and default.

## Two modules break the pattern

Both take the standard trio and the standard `defaults`/`hide.inputs`/`hide.tabs`
arguments, but everything around them differs. Check which one you are on before applying
generic advice.

**`ComplexHeatmap_Heatmap`** — its output is **not** plotly. It renders through
`InteractiveComplexHeatmap`, so there is no `Plotly` tab, no `download.format`, no
draggable annotations, and the source-data export works differently. Its Bioconductor
dependencies (`ComplexHeatmap`, `InteractiveComplexHeatmap`, `circlize`) are `Suggests`,
not hard dependencies — the module errors with a `BiocManager::install()` message when
they are missing, so guard it with `requireNamespace()` if your app must run without them.
Its `data` may also be a **list** rather than a data frame:

```r
ComplexHeatmap_HeatmapServer("h", data = reactive(list(
    matrix             = example_heatmap_matrix,     # numeric columns + optional row-name column
    column_annotations = example_heatmap_column_data # one row per sample column
)))
```

The bare data frame form works too; the list is only needed for column annotation tracks.
The output can be split across `ComplexHeatmap_HeatmapMainOutputUI()`,
`ComplexHeatmap_HeatmapSubOutputUI()`, and `ComplexHeatmap_HeatmapInfoOutputUI()` to place
the heatmap, the sub-heatmap, and the click/brush info panel separately;
`ComplexHeatmap_HeatmapOutputUI()` passes `...` through to
`InteractiveComplexHeatmapOutput()` (`layout`, `compact`, …).

`ComplexHeatmap_HeatmapStaticOutputUI()` is the fourth option: a plain `plotOutput()` of the
same heatmap, backed by the same server call. Use it wherever the widget's chrome is in the
way — `InteractiveComplexHeatmap` puts a grey border around its heatmap panel, a control tab
strip beneath it, and a fixed pixel width on the whole thing, and no argument turns any of
that off (`compact = TRUE` only drops the sub-heatmap). It fills its container, honours
`resizable`, and needs `ComplexHeatmap` but not `InteractiveComplexHeatmap`. The cost is the
interactivity: no cell hover/click, no sub-heatmap, no brush info panel. It is what the
Figure Builder uses.

**`dittoViz_freqPlot`** — it plots a **computed summary**, not columns of the input. It
tabulates how often each level of `var` occurs within each `sample.by` value, one facet per
level. So `y.min`/`y.max`, the Stats tab, the point annotations (points are samples), and
the source download all describe that frequency table. It also needs samples nested inside
groups; on a table without that nesting every group collapses to one point.
`example_composition` is the bundled dataset shaped for it.

## Three arguments do most of the work

| Argument | Where | Effect |
|---|---|---|
| `defaults` | `InputsUI` **and** `Server` | Pre-fill a control. Pass the same list to both. |
| `hide.inputs` | `Server` | Hide one control. Value still initialises and still reaches the plot. |
| `hide.tabs` | `Server` | Hide a whole tab. Same — inputs stay live. |

Hiding never disables. `defaults = list(color.by = "cyl"), hide.inputs = "color.by"` is
the idiom for "fixed to this, user cannot change it".

A `defaults` entry may be a `reactive()`/`reactiveVal()` instead of a fixed value, so a
control follows app state. That is the **only** correct parent→child channel — do not
call `update*Input()` from the parent. See `references/defaults-and-hiding.md` for the
reactive-defaults setup (the UI must be built inside `renderUI()`) and its semantics.

## Building blocks

- **`createModuleApp(inputs_ui_fn, output_ui_fn, server_fn, data_list, defaults, hide.inputs, hide.tabs, show.table, title)`** — a complete app with file upload, a filterable table, and dataset switching. Every `*App()` (`plotthis_BarPlotApp()`, …) is a thin wrapper around it. Reach for this before hand-rolling an app shell.
- **`dataFilterUI(id)` / `dataFilterServer(id, data, factor.char.cols, page.length, col.visibility, hide.columns, filter.max.options)`** — a DT table whose filtered rows come back as a reactive you feed straight to one or more plot modules.
- **`figureBuilderUI(id)` / `figureBuilderServer(id, data_list, module_registry)`** — the multi-panel figure builder, embeddable like any module. `figureBuilderApp()` launches it standalone.
- **Stats tab** — `dittoViz_yPlot`, `plotthis_BoxPlot`, `plotthis_ViolinPlot`, and `dittoViz_freqPlot` only. Pre-fill it through `defaults` with the `stats.enabled` / `stat.*` keys; see `references/stats-tab.md`.
- **Source-data download** — `collect_source_data()` + `create_source_download_handler()`. Bundles the interactive HTML, an SVG and PNG of the plot, and the data/stats/inputs as CSVs. Do not hand-roll `write.csv()`/`zip()`; see `references/source-data-export.md`.

## Traps that cost real time

Every one of these was hit by an agent building an app against this package. None of
them errors loudly; each looks like the module misbehaving.

- **A hand-built app needs `shinyjs::useShinyjs()` in its UI.** `hide.inputs`/`hide.tabs` go through `runjs()`. Without it the app renders fine and the hiding silently does nothing. `createModuleApp()` includes it for you; your own `fluidPage()` does not.
- **`create_source_download_handler()` wants pandoc for the HTML.** It calls `saveWidget(selfcontained = TRUE)`; with no pandoc on `PATH` the `_plot.html` is skipped with a warning and the rest of the archive — the CSVs, the SVG and the PNG, none of which need pandoc — still downloads. RStudio bundles pandoc; a bare server does not. (Before 0.5.0.9000 this failed the whole download with HTTP 500.)
- **`stats.enabled = TRUE` can still draw nothing.** `stat.hide.ns` defaults to `TRUE`, so if every comparison is non-significant the tests run and no brackets appear. That looks broken. Set `stat.hide.ns = FALSE` while checking your wiring.
- **`main` is not a real key.** No module exposes a plot title. `defaults = list(main = ...)` is silently ignored — see `references/defaults-and-hiding.md`.
- **An unknown `defaults` key never errors.** `get_default()` falls back. If a default "doesn't work", suspect the key name first.
- **`manual.colors` no longer exists.** `dittoViz_scatterPlotServer()`'s `manual.colors` argument was removed — it hard-overrode the colour picker, so the colours could not be edited. Seed a palette with a *named* vector under the module's colour key instead (`defaults = list(color.panel = c(setosa = "red"))`); see `references/defaults-and-hiding.md`.

## Verifying your work

`shiny::testServer()` cannot drive a plotly output: the mock session never renders
`renderUI()`-built inputs (the colour picker) and never registers plotly events, so the
plot reactive dead-ends in a silent `req()`. That is a limit of the harness, not your
app — the package's own tests assert on internal reactives for the same reason.

So: use `testServer()` for data and input wiring, and check the rest by building the UI
and inspecting the markup. Booting a real browser is rarely worth it for an app whose
plot comes from a base module that already has its own tests.

## Rules

1. `data` reaches `*Server()` as a `reactive()`. Passing a bare data frame is an error.
2. `InputsUI` needs a real data frame at call time. If the dataset is chosen at runtime, build the controls inside `renderUI()`.
3. One `id` per module instance; reuse the same `id` across the trio. Two instances need two ids.
4. Verify any signature you are unsure of against `?<function>` rather than guessing — 151 functions are exported and the naming is not always predictable.
5. If a feature the user asked for is not exposed by a module, say so explicitly instead of inventing an argument.
