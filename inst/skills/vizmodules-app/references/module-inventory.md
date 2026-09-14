# VizModules module inventory

Function names follow `<Module>InputsUI` / `<Module>OutputUI` / `<Module>Server` /
`<Module>App` for every row. Note the capitalisation: the dittoViz modules are
`dittoViz_scatterPlot*` and `dittoViz_yPlot*` (lowercase plot name), the plotthis ones
are `plotthis_BoxPlot*` etc. (capitalised).

## Column-mapping keys and colour keys

These are the `defaults` / `hide.inputs` keys you will actually use. **They are not
uniform across modules** — check the row before writing a key.

| Module | Wraps | Mapping keys | Colour key | Stats tab |
|---|---|---|---|---|
| `dittoViz_scatterPlot` | `dittoViz::scatterPlot` | `x.by` `y.by` `color.by` `shape.by` `size.by` `split.by` | `color.panel` | — |
| `dittoViz_yPlot` | `dittoViz::yPlot` | `var` `group.by` `color.by` `shape.by` `split.by` `plots` `y.min` `y.max` | `palette.colours` | **yes** |
| `plotthis_AreaPlot` | `plotthis::AreaPlot` | `x.data` `y.data` `group.by` `facet.by` | `palette.colours` | — |
| `plotthis_BarPlot` | `plotthis::BarPlot` | `x.data` `y.data` `group.by` `fill.by` `split.by` `facet.by` `y.min` `y.max` | `palette.colours` | — |
| `plotthis_BoxPlot` | `plotthis::BoxPlot` | `x.data` `y.data` `group.by` `facet.by` `y.min` `y.max` | `palette.colours` | **yes** |
| `plotthis_DensityPlot` | `plotthis::DensityPlot` | `x.data` `group.by` `facet.by` | `palette.colours` | — |
| `plotthis_DotPlot` | `plotthis::DotPlot` | `x.data` `y.data` `fill.by` `size.by` `facet.by` | `palette.name` (continuous) | — |
| `plotthis_Histogram` | `plotthis::Histogram` | `x.data` `group.by` `facet.by` | `palette.colours` | — |
| `plotthis_SplitBarPlot` | `plotthis::SplitBarPlot` | `x.data` `y.data` `fill.by` `split.by` `facet.by` `x.min` `x.max` | `palette.colours` | — |
| `linePlot` | native (`linePlot()`) | `x.value` `y.value` `group.by` `facet.by` | `palette.colours` | — |
| `dumbbellPlot` | native (`dumbbellPlot()`) | `x.value` `y.value` `colour.by` `facet.by` | `palette.colours` | — |
| `piePlot` | native (`piePlot()`) | `labels` `values` | `slice.colors` | — |
| `radarPlot` | native (`radarPlot()`) | `r` `theta` `group` | `trace.colors` | — |
| `parallelCoordinatesPlot` | native | `dimensions` `color.by` | `palette.colours` | — |
| `ComplexHeatmap_Heatmap` | `ComplexHeatmap::Heatmap` | `matrix.cols` `rowname.col` `row_filter` `column_filter` `row_annotations` `column_annotations` `column_key` `row_split_by` `row_split_cols` `column_split_by` `column_split_cols` | `low_color`/`mid_color`/`high_color` | — |
| `dittoViz_freqPlot` | `dittoViz::freqPlot` | `var` `sample.by` `group.by` `color.by` `vars.use` `plots` `scale` `max.normalize` `y.min` `y.max` | `palette.colours` | **yes** |

`ComplexHeatmap_Heatmap` is the odd one out: its output is **not** plotly. It renders
through `InteractiveComplexHeatmap`, so plotly-specific advice does not apply to it — or
through `ComplexHeatmap_HeatmapStaticOutputUI()`, a plain `plotOutput()` of the same heatmap
with none of the widget's border/control-strip/fixed-width chrome, which is what the Figure
Builder uses. It's
also the odd one out on colour — `low_color`/`mid_color`/`high_color` are plain scalar
colour inputs for the value scale, not a `defaults` group-colour key like every other row's
"Colour key" column means. `row_annotations`/`column_annotations` are `multiDynamicInput()`
row lists (`column` + `side` per row), not simple vectors, and each row's own colour
widget(s) are keyed dynamically off its row name rather than one stable `defaults` key.

`ComplexHeatmap_Heatmap` is also the only module with a built-in row/column filter, on its
**Filter** tab. `row_filter` is an expression over the matrix data frame's columns;
`column_filter` is an expression over one row per matrix column, carrying a synthetic
`column` field (the column name) plus every `column_annotations` field joined via
`column_key` — so `column %in% c("S1", "S2")` works with no metadata table and
`condition == "Disease"` works once one is supplied. Both go through `safe_eval_filter()`,
which permits comparisons, `&`/`|`/`!`, `%in%`, `is.na()`, arithmetic, and the string
helpers `grepl`/`startsWith`/`endsWith`/`substr`/`nchar`/`toupper`/`tolower`/`trimws`.
Filtering runs before everything else, so `scale`, the annotation tracks, the splits, and
the source download all describe the filtered matrix.

Its `*_split_by` inputs take a fourth method, `"Annotation"`, which groups rows/columns by
the values of `row_split_cols`/`column_split_cols` rather than by a derived clustering.
Several columns give nested slices. This is also the cheap path: with `cluster_rows = FALSE`
it groups without computing a distance matrix at all.

`dittoViz_freqPlot` is the other odd one out: it does **not** plot columns of the incoming
data. It tabulates how often each level of `var` occurs within each `sample.by` value and
plots those per-sample frequencies, one facet per level. So `y.min`/`y.max`, the statistics,
the point annotations (points are *samples*), and the source download all describe that
summarised frequency table, not the input rows. `scale` picks percent vs count.

The colour key takes a **named character vector** mapping group level to colour, e.g.
`defaults = list(palette.colours = c(Healthy = "#0072B2", Disease = "red"))`. Unnamed
groups fall back to the stock palette; the user can still edit every colour.

## Tab names for `hide.tabs`

| Module | Tabs |
|---|---|
| `dittoViz_scatterPlot` | Data, Adjustments, Points, Colors, Facet, Annotations, Legend, Trajectory, Lines, Axes, Plotly, Extras |
| `dittoViz_yPlot` | Data, Adjustments, Jitter, Box, Violin, Ridge, Stats, Facet, Annotations, Legend, Axes, Lines, Plotly |
| `plotthis_BoxPlot` | Data, Adjustments, Highlight, Facet, Stats, Legend, Axes, Lines, Plotly |
| `plotthis_AreaPlot`, `plotthis_DotPlot`, `linePlot`, `dumbbellPlot` | Data, Facet, Aesthetics, Legend, Axes, Lines, Plotly |
| `plotthis_DensityPlot`, `plotthis_Histogram` | Data, Facet, Aesthetics, Rug, Legend, Axes, Lines, Plotly |
| `plotthis_BarPlot`, `plotthis_SplitBarPlot` | Data, Facet, Aesthetics, Adjustments, Legend, Axes, Lines, Plotly |
| `piePlot` | Data, Aesthetics, Labels, Plotly |
| `parallelCoordinatesPlot` | Data, Aesthetics, Labels, Title, Plotly |
| `radarPlot` | Data, Aesthetics, Axes, Plotly (plus per-trace styling tabs) |
| `dittoViz_freqPlot` | Data, Scale, Jitter, Box, Violin, Ridge, Stats, Facet, Annotations, Legend, Plotly, Axes, Lines |
| `ComplexHeatmap_Heatmap` | Matrix, Filter, Colors, Clustering, Labels, Annotations |

## Shared tab input keys

Inputs on the Axes / Legend / Lines / Plotly tabs come from shared helpers, so they use
the same keys in every module. Full lists are on their help pages —
`?uniform_axes_inputs_ui`, `?uniform_legend_inputs_ui`, `?uniform_lines_inputs_ui`,
`?uniform_plotly_inputs_ui`. The ones you will reach for most:

- Axes: `axis.title.font.size` (18), `title.font.size` (26), `axis.showline` (TRUE), `show.grid.x` / `show.grid.y` (TRUE), `axis.tickfont.size` (12), `axis.tickangle.x` (0)
- Legend: `legend.title.size`, `legend.text.size`
- Lines: `hline.intercepts`, `vline.intercepts`, `abline.slopes` — comma-separated strings, with matching `*.colors` / `*.widths` / `*.linetypes` / `*.opacities`
- Plotly: `download.format`, `subplot.margin.x`, `subplot.margin.y`

## Bundled example datasets

`example_bar`, `example_composition`, `example_demographics`,
`example_heatmap_column_data`, `example_heatmap_matrix`, `example_iris`, `example_markers`,
`example_mtcars`, `example_population`, `example_rnaseq`, `example_sales`,
`example_school_earnings`, `example_skills`.

`example_rnaseq` (288 x 7) is the richest for grouped comparisons:
`cell_type` (factor: CD4 T, CD8 T, B Cell, NK Cell, Monocyte, pDC), `gene` (factor),
`condition` (factor: Healthy, Disease), `replicate` (factor: Rep1-3), `log2_cpm`
(numeric), `avg_expression` (numeric), `neg_log10_pval` (numeric).

`example_heatmap_matrix` (30 x 15) is genes (rows) x samples (columns), shaped for
`ComplexHeatmap_Heatmap`: `gene` (id), `pathway` (factor row annotation), `mean_expression`
(numeric row annotation), plus 12 sample columns (`Healthy_1..6`, `Disease_1..6`).
`example_heatmap_column_data` (12 x 4) is its companion sample-metadata table
(`sample`, `condition`, `batch`, `library_size`) for demonstrating column annotations —
pass both together as `data = list(matrix = example_heatmap_matrix, column_annotations =
example_heatmap_column_data)`.

`example_composition` (1800 x 7) is the dataset shaped for `dittoViz_freqPlot`:
`cell_id`, `sample` (factor: twelve donors `P01`-`P12`, 150 cells each), `condition`
(factor: Healthy/Disease, six donors each), `batch` (factor: B1/B2, crossed with
`condition` so it works as `color.by` without confounding), `cell_type` (factor, the
frequency variable), `n_genes`, `percent_mito`. The nesting of samples inside groups is
what `freqPlot()` needs — on a table without it every group collapses to a single point
and the underlying function warns.

Each `*App()` opens on a dataset chosen to suit it: scatter/line/area/pie/parallel →
`example_sales`; yPlot/box/density/histogram → `example_demographics`; bar and
split bar → `example_bar`; dot → `example_markers`; radar → `example_skills`; dumbbell →
`example_school_earnings`; heatmap → `example_heatmap_matrix`; freqPlot →
`example_composition`.

`dittoViz_freqPlotApp()` additionally seeds `defaults = list(var = "cell_type",
sample.by = "sample", group.by = "condition")`, but **only** when it falls back to the
bundled dataset — pass your own `data_list` and it opens on columns chosen from that.

## Not yet wrapped

`dittoViz::barPlot` and `dittoViz::scatterHexPlot` have no module. If a user asks for one, say so rather than inventing a function name.
