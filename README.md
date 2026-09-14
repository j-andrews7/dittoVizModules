# VizModules

<!-- badges: start -->
[![R-CMD-check](https://github.com/j-andrews7/VizModules/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/j-andrews7/VizModules/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/j-andrews7/VizModules/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/j-andrews7/VizModules/actions/workflows/pkgdown.yaml)
<!-- badges: end -->

This package utilizes various viz packages (currently [dittoViz](https://github.com/dtm2451/dittoViz), [plotthis](https://github.com/pwwang/plotthis), and [ComplexHeatmap](https://bioconductor.org/packages/ComplexHeatmap/) along with native plotting functions) to create interactivity-first Shiny modules for common plot types, designed to serve as building blocks for Shiny apps and as the basis for more complex/specialized modules.

These modules contain all possible functionality for each plot with some additional parameters that make use of the interactive features of plotly, e.g. interactive text annotations, arbitrary shape annotations, multiple download formats, etc.

The modules provide comprehensive plot control for app users, allowing for convenient aesthetic customizations and publication-quality images.
They also provide developers a way to dramatically save time and reduce complexity of their plotting code or a flexible base to build more specialized Shiny modules upon.

## Install

```r
# CRAN
install.packages("VizModules")

# Development version
remotes::install_github("j-andrews7/VizModules")
```

## Quick Start

- Explore the hosted [example gallery](https://j-andrews7-vizmodules.share.connect.posit.cloud/).
- Run the same gallery locally after installation: `shiny::runApp(system.file("apps/module-gallery", package = "VizModules"))`
- Check out the hosted [Figure Builder app](https://j-andrews7-vizmodulesfigbuilder.share.connect.posit.cloud/) for a demo of how the modules can be used together to build a free-form, multi-pane figure.
- Run the Figure Builder app locally: `VizModules::figureBuilderApp()`
- See the vignette for a full walkthrough of using the modules in your own apps: [`vignette("quick-start", package = "VizModules")`][18]

### Using Modules in Your Own App

To use a module in your own app, simply call the `*InputsUI()`, `*OutputUI()`, and `*Server()` functions for the module you want to use. For example, to use the ScatterPlot module from dittoViz, you would do something like this:

```r
library(VizModules)

ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            dittoViz_scatterPlotInputsUI(
                "cars",
                mtcars,
                defaults = list(
                    x.by = "wt",
                    y.by = "mpg",
                    color.by = "cyl"
                )
            )
        ),
        mainPanel(dittoViz_scatterPlotOutputUI("cars"))
    )
)

server <- function(input, output, session) {
    dittoViz_scatterPlotServer(
        "cars",
        data = reactive(mtcars)
    )
}

shinyApp(ui, server)
```

Every module uses the same trio of functions: `*InputsUI()` for controls, `*OutputUI()` for the plot, and `*Server()` for the logic. The separation of InputsUI and OutputUI allows you to place input controls and the actual plot wherever you'd like.

Use `defaults` to pre-fill inputs, and `hide.inputs`/`hide.tabs` to hide controls while keeping their values so you can enforce app-level defaults without exposing them. A `defaults` entry can also be a `reactive()`, so an input follows your app's state without an extra re-render.

Modules built on plotting functions from other packages expose most of the underlying arguments. The module input help pages (e.g., `?dittoViz_scatterPlotInputsUI`, `?plotthis_AreaPlotInputsUI`) list what is wired through and any omissions; cross-reference the underlying plot docs (`?dittoViz::scatterPlot`, `?plotthis::AreaPlot`, etc.) to see the full parameter set.

### Example Apps for Each Module

Every module has a corresponding `*App()` function that creates a complete Shiny app showcasing the module's functionality with example data. For instance, `plotthis_BarPlotApp()` creates an app BarPlot module. You can run these apps directly to explore the module's features and see how it works in a full Shiny context.

```r
library(VizModules)
# Using built-in example data (or upload your own file in the app)
plotthis_BarPlotApp()

# Providing your own data
df <- data.frame(
    category = c("A", "B", "C"),
    value = c(10, 20, 15),
    group = c("X", "Y", "X")
)

plotthis_BarPlotApp(data = df)
```

## App Factory

Every built-in `*App()` convenience function (e.g. `plotthis_BarPlotApp()`, `linePlotApp()`) is a thin wrapper around `createModuleApp()` with sensible default data. You can also pass your own custom wrapper module functions to `createModuleApp()` for rapid prototyping after defining the UI and server functions. All `*App()` wrappers and `createModuleApp()` accept `defaults`, `hide.inputs`, and `hide.tabs` so you can pre-fill or hide controls when testing a module in isolation; see [`vignette("defaults-and-hiding", package = "VizModules")`][28].

```r
library(VizModules)

app <- createModuleApp(
    inputs_ui_fn = plotthis_BarPlotInputsUI,
    output_ui_fn = plotthis_BarPlotOutputUI,
    server_fn    = plotthis_BarPlotServer,
    data_list    = list("cars" = mtcars),
    title        = "My Bar Plot"
)

runApp(app)
```


## Figure Builder App

The **Figure Builder** is now a fully reusable, namespaced Shiny module (`figureBuilderUI()` / `figureBuilderServer()`) that turns the plot modules into a free-form figure builder. It can be launched as a standalone app, embedded inside a larger app, or even instantiated more than once on a single page. Launch the standalone app with `figureBuilderApp()`:

```r
library(VizModules)

# Launch with the bundled example datasets and all modules
figureBuilderApp()

# Or seed it with your own datasets
figureBuilderApp(data_list = list("iris" = iris, "mtcars" = mtcars))
```

`figureBuilderApp()` accepts `data_list` to seed datasets, `module_registry` to add custom modules, and `return_components = TRUE` to get separate `ui`/`server` objects instead of a `shinyApp()`. See `?figureBuilderApp` for details.

Or try the [hosted example](https://j-andrews7-vizmodulesfigbuilder.share.connect.posit.cloud/).

The Figure Builder is also a self-contained Shiny module, so you can embed it in a larger app (and even use more than one instance on a page) with `figureBuilderUI()` / `figureBuilderServer()`, just like the plot modules:

```r
library(VizModules)

ui <- fluidPage(
    figureBuilderUI("figure_builder")
)

server <- function(input, output, session) {
    figureBuilderServer("figure_builder")
}

shinyApp(ui, server)
```

It allows you to interactively compose complicated figures using the modules in a single page:

- **Add plots on demand.** Click *Add Plot* to drop any VizModule onto the canvas, choosing both the plot type and the dataset it should use.
- **Load your own data.** Use the *Load Data* section to upload a `CSV`, `TSV` or `RDS` file. Uploaded datasets are added to the dataset list so you can build plots from your own data alongside the bundled examples. Each plot can use a different dataset if desired.
- **Drag and resize.** Each plot lives on its own card. Hover a card to reveal a small toolbar with a drag handle (to reposition it) and a remove button, and resize it from its corner (via `shinyjqui`) — resizing adjusts the plot in both directions. The toolbar stays out of the way otherwise, so cards remain clean and chrome-free in the SVG export.
- **A4 canvas.** The canvas is sized to an A4 page (switchable between portrait and landscape), making it easy to lay plots out for a composite figure.
- **Swappable controls.** A single dropdown swaps the visible plot's input controls in and out, so only one control set is shown at a time while every plot keeps its own settings.
- **Swappable filtering table.** A matching dropdown swaps the visible plot's filterable data table, mirroring the controls behaviour. Filtering a plot's table subsets only that plot's data.
- **Download as SVG.** Click *Download Full Figure (SVG)* to export the whole canvas as a single vector SVG, with every plot positioned as it appears on the page. This allows downstream editing as necessary, though the hope is that you'll be 95% of the way there with the formatting and syling options available in each module.
- **Automatic panel labels.** Use the *Panel labels* dropdown to add panel letters (`A`, `B`, `C`, … or lowercase `a`, `b`, `c`, …) to the top-left of each panel. Labels now render live on the canvas as soon as they are chosen (and renumber as panels are added, removed, or dragged), in addition to appearing in the SVG export. Labels are ordered the way a reader scans a figure — top-to-bottom by row, then left-to-right within a row. Choose *None* to leave the figure unlabelled.
- **Download source data.** Click *Download Summary* to download a single `.zip` containing all plot data (plot + data + the inputs used to build it + statistical testing information (if applied)) for every plot on the canvas, with one set of files per panel.


## Building Custom Wrapper Modules

The modules in **VizModules** are designed to be composed and extended. You can build higher-level modules that add custom logic while reusing the full functionality of the base modules. Many internal helpers for axes, faceting, and layouts are now exported to support this, along with the `defaults`/`hide.inputs`/`hide.tabs` controls covered in [`vignette("defaults-and-hiding", package = "VizModules")`][28].

For more details, see [`vignette("custom-modules", package = "VizModules")`.][17]

## Custom Shiny Inputs

Beyond the standard `shiny::*Input` widgets, **VizModules** ships two reusable custom Shiny inputs that are used throughout the package and are available for your own apps: `multiColorPicker()` and `multiDynamicInput()`. See [`vignette("custom-shiny-inputs", package = "VizModules")`][27] for full details.

**`multiColorPicker()`** assigns a color to each level of a discrete variable, either by applying a named palette to every group at once or by fine-tuning individual groups with a color picker and an editable hex field. It returns a named character vector of hex colors keyed by group and can be updated from the server with `updateMultiColorPicker()`.

![](man/figures/multiColorPicker.png)

**`multiDynamicInput()`** is a general-purpose widget that lets users dynamically add and remove rows of heterogeneous inputs (e.g. a color, a numeric, and a select per row). It returns a named list of rows and can be updated from the server with `updateMultiDynamicInput()`.

![](man/figures/multiDynamicInput.png)

## Modules Provided

Currently, **VizModules** contains a functional Shiny module for the following visualization functions:

### `dittoViz`

* `dittoViz_scatterPlot` - x/y coordinate plots with additional color and shape encodings (wraps `dittoViz::scatterPlot`). Supports overlaying fit lines, including **multiple custom model lines** defined interactively: add a row per model, each with its own R model formula (e.g. `revenue ~ poly(units, 2)`), fitting function (`lm`, `glm`, `loess`, `nls`), line colour, and width, see [`vignette("custom-model-lines", package = "VizModules")`][26]. 
* `dittoViz_yPlot` - Multi-variate Y-axis plots (boxplot, jitter, violinplots - wraps `dittoViz::yPlot`).
* `dittoViz_freqPlot` - Box/jitter plots for discrete observation frequencies per sample/group (wraps `dittoViz::freqPlot`).

### `plotthis`

* `plotthis_AreaPlot` - Stacked area charts (wraps `plotthis::AreaPlot`).
* `plotthis_BoxPlot` - Box plots (wraps `plotthis::BoxPlot`).
* `plotthis_BarPlot` - Bar charts (wraps `plotthis::BarPlot`).
* `plotthis_SplitBarPlot` - Split bar charts (wraps `plotthis::SplitBarPlot`).
* `plotthis_DensityPlot` - Density plots (wraps `plotthis::DensityPlot`).
* `plotthis_DotPlot` - Dot plots (wraps `plotthis::DotPlot`).
* `plotthis_Histogram` - Histograms (wraps `plotthis::Histogram`).

### `ComplexHeatmap`

* `ComplexHeatmap_Heatmap` - Interactive heatmaps with row/column annotation tracks, clustering, and sub-heatmap zoom (wraps `ComplexHeatmap::Heatmap`). This is the one module whose output is **not** plotly - it renders through [InteractiveComplexHeatmap](https://bioconductor.org/packages/InteractiveComplexHeatmap/), so the plotly-specific controls (the Plotly tab, download formats, draggable annotations) do not apply. It is also the one module whose `data` may be a list rather than a data frame: pass `data = list(matrix = <data.frame>, column_annotations = <data.frame>)` when you want column annotation tracks, where the companion table carries one row per sample column. `ComplexHeatmap`, `InteractiveComplexHeatmap`, and `circlize` are `Suggests` rather than hard dependencies, so install them with `BiocManager::install(c("ComplexHeatmap", "InteractiveComplexHeatmap", "circlize"))` before using this module.
  * The interactive output can also be split across `ComplexHeatmap_HeatmapMainOutputUI()`, `ComplexHeatmap_HeatmapSubOutputUI()`, and `ComplexHeatmap_HeatmapInfoOutputUI()` if you want the main heatmap, the sub-heatmap, and the click/brush info panel in separate places in your layout.

### Plotting Functions Defined in VizModules

Via direct implementation with plotly.

* `linePlot` - Line plots
* `piePlot` - Pie and donut plots
* `radarPlot` - Radar plots
* `parallelCoordinatesPlot` - Parallel coordinate plots
* `dumbbellPlot` - Dumbbell plots

## Statistical Testing

The **BoxPlot**, **yPlot**, and **freqPlot** modules include a **Stats** tab that adds pairwise statistical testing with bracket annotations directly on the plotly figure. On **freqPlot** the tests are always run within each facet (the `stat.per.facet` control is hidden), since each facet is a different level of the frequency variable and pooling across them would compare non-comparable quantities. The underlying helpers (`compute_pairwise_stats()`, `create_stat_annotations()`, `apply_stat_annotations()`, `generate_pair_strings()`, `parse_pair_strings()`) are exported so you can add the same bracket annotations to any custom plotly figure. See [`vignette("statistical-testing", package = "VizModules")`][29].

### Export Summary Data

`collect_source_data()` collects the interactive plot as HTML, its plot data, pairwise testing statistics (if applied), and UI input values into a single list, and `create_source_download_handler()` turns that into a compact zip folder of summary data for the output plot. The zip also carries an SVG and a PNG of each plot, captured in the browser so they match what is on screen — every reference line, statistical bracket, and dragged annotation included. `create_source_download_handler()` also accepts a named list of summaries (one per plot), which is how the Figure Builder bundles every plot on the canvas into one download.

### Supported Tests

- **Pairwise**: Wilcoxon rank-sum test, t-test (paired or unpaired)
- **Omnibus**: Kruskal-Wallis, ANOVA

### Features

- Bracket annotations with capped or flat style, placed via an interval packing algorithm to minimize vertical space
- Multiple display modes: adjusted p-values, raw p-values, or significance symbols (`*`, `**`, `***`, `****`)
- P-value correction via any `p.adjust` method (Holm, Bonferroni, BH, etc.)
- Configurable significance threshold, bracket spacing, inset, and line styling
- Per-facet testing: run tests independently within each facet panel, or across the full dataset
- Nested grouping: compare `group.by` levels within each x-axis category
- Omnibus test results shown as a draggable text annotation
- Download computed statistics as a CSV with metadata header (correction method, threshold, symbol legend)

### Data Format for Paired Tests

When using paired tests (Wilcoxon signed-rank or paired t-test), each group must have the **same number of observations** in corresponding order. Data should be sorted so that paired samples align row-by-row within each group.

## Modules Planned

### `dittoViz`

* **scatterHex** - hexbin plots encoding density/frequency information along x/y coordinates.
* **barPlot** - compositional barplots.

[dittoViz](https://github.com/dtm2451/dittoViz) is under active development, so additional modules may be added as more visualization functions are added.

## Contributing a New Module

To contribute a new module to the package, see the vignette for clear guidelines: [`vignette("adding-a-new-module", package = "VizModules")`][16]


## Available Modules

[linePlot:][1]

![](man/figures/LinePlot.png)

[plotthis_AreaPlot:][2]

[(Source Plotting Function)][19]

![](man/figures/AreaPlot.png)

[plotthis_BoxPlot:][3]

[(Source Plotting Function)][20]

![](man/figures/BoxPlot.png)

[plotthis_DensityPlot:][4]

[(Source Plotting Function)][21]

![](man/figures/DensityPlot.png)

[dumbbellPlot:][5]

![](man/figures/DumbellPlot.png)

[plotthis_Histogram:][6]

[(Source Plotting Function)][21]

![](man/figures/HistogramPlot.png)

[parallelCoordinatesPlot:][7]

![](man/figures/ParallelPlot.png)

[piePlot:][8]

![](man/figures/PiePlot.png)

[radarPlot:][9]

![](man/figures/RadarPlot.png)

[dittoViz_ScatterPlot:][10]

[(Source Plotting Function)][22]

![](man/figures/ScatterPlot.png)

[plotthis_SplitBarPlot:][11]

[(Source Plotting Function)][23]

![](man/figures/SplitBarPlot.png)

[dittoViz_yPlot:][14]

[(Source Plotting Function)][22]

![](man/figures/yPlot.png)

[plotthis_DotPlot:][24]

[(Source Plotting Function)][25]

![](man/figures/DotPlot.png)

[dittoViz_freqPlot:][31]

[(Source Plotting Function)][22]

![](man/figures/FreqPlot.png)

[ComplexHeatmap_Heatmap:][32]

[(Source Plotting Function)][33]

![](man/figures/Heatmap.png)

[figureBuilder:][30]

![](man/figures/Figure_builder.png)

## AI Usage Statement
The developers made use of AI tools (e.g. GitHub Copilot, Claude Code) for code generation, documentation writing, and test creation.
AI assistance was used to accelerate development after the initial module scaffolding and structure was in place, but all AI-generated content was reviewed and edited by human eyeballs/hands to ensure accuracy and quality.
Our own hands are all over this project, and we are invested in it. 
Any inaccuracies, bugs, or issues are attributable to us, and we welcome contributions to help improve the package.

Generative AI tools (GitHub Copilot, ChatGPT, Claude, Gemini, Cursor, etc.) are **explicitly welcome** for building Shiny apps with these modules in addition to creating new modules. To do so, we recommend the use of the skills provided by the package or prefixing prompts with the below to aid LLM usage.

### Agent Skills (GitHub Copilot, OpenAI Codex, Claude Code, and compatible tools)

The package provides three [Agent Skills](https://agentskills.io) that give an agent the
package's conventions without it having to read the vignettes first. Install them into a
project with:

```r
VizModules::use_vizmodules_skills(".")
```

That writes `vizmodules-app`, `vizmodules-custom-module`, and `vizmodules-new-module`
into `.agents/skills/` by default, where GitHub Copilot and OpenAI Codex discover them
automatically. Pass `client = "copilot"` for `.github/skills/` (also read by GitHub
Copilot) or `client = "claude"` for `.claude/skills/` (Claude Code); call the function
more than once with different `client` values to install into several locations at once.
`vizmodules-app` in particular carries a generated inventory of every module's
column-mapping keys (`x.data` vs `x.by` vs `x.value` vs `var`), colour key, tab names,
and stats keys, which is what an agent otherwise spends its budget grepping for.

In rough benchmarking, `vizmodules-app` saves ~40-60% of token usage versus just chucking an agent at the docs/repo/prompt below and generates a functional app in about half the time. The other skills show more variable and modest savings (~10-20% fewer tokens), but they tend to avoid common pitfalls and better utilize some of the more advanced features. Skills are difficult to benchmark, as the benefits are context-dependent and vary with the request.

For tools that cannot read local skill files, the prompt below does the same job less
efficiently.

### LLM Instructions

Copy the prompt below into your LLM or save it in a file (Copilot, ChatGPT, Claude, Gemini, Cursor, etc.) before asking it to build a Shiny app with **VizModules**. It points the model to the authoritative, locally-installed sources of truth so it can use the package correctly.

> You are helping me build a Shiny application using the installed R package **VizModules**, which provides interactivity-first, plotly-based Shiny modules for common plot types. Before writing code, ground yourself in the package's own documentation rather than guessing at the API.
>
> **Core concept.** Every module is a trio of functions that share an `id`: `*InputsUI(id, ...)` renders the controls, `*OutputUI(id)` renders the plotly output, and `*Server(id, data, ...)` holds the logic. `InputsUI` and `OutputUI` are separate so controls and plot can be placed anywhere in the layout. `data` is passed to the server as a `reactive()`. Use the `defaults` argument to pre-fill inputs and `hide.inputs`/`hide.tabs` to lock values while hiding their controls.
>
> **Where to look (all available after `install.packages`/`remotes::install_github`):**
> - `vignette("quick-start", package = "VizModules")` — start here: end-to-end walkthrough of wiring `*InputsUI()`, `*OutputUI()`, and `*Server()` into an app, using `defaults`, and the example `*App()` functions.
> - `vignette("custom-modules", package = "VizModules")` — how to **extend existing modules** by building wrapper modules (adding custom logic/inputs while reusing a base module). Follow the namespace pattern: process namespaced inputs *inside* `moduleServer()`, then call the base `*Server()` *outside* it with the bare `id` to avoid double-namespacing.
> - `vignette("adding-a-new-module", package = "VizModules")` — how to **author a brand-new module** from scratch (the InputsUI/OutputUI/Server contract, conventions, and helpers).
> - `vignette("defaults-and-hiding", package = "VizModules")` — using `defaults`, `hide.inputs`, and `hide.tabs` to pre-fill or hide controls.
> - `vignette("statistical-testing", package = "VizModules")` — the Stats tab and the exported `compute_pairwise_stats()` / `create_stat_annotations()` / `apply_stat_annotations()` helpers.
> - `vignette("custom-model-lines", package = "VizModules")` — the pluggable model-line backend registry (`register_model_backend()`).
> - `vignette("custom-shiny-inputs", package = "VizModules")` — the reusable `multiDynamicInput()` widget.
> - The README — overview, install, the full list of available modules, the App Factory (`createModuleApp()`), statistical-testing features, and summary-data export.
> - Per-function help pages via `?` — e.g. `?dittoViz_scatterPlotInputsUI`, `?plotthis_BarPlotServer`, `?createModuleApp`. Module help pages document exactly which underlying arguments are wired through and any omissions. Cross-reference the underlying plotting docs (`?dittoViz::scatterPlot`, `?plotthis::AreaPlot`, etc.) for the complete parameter set. Browse all docs with `help(package = "VizModules")` or the pkgdown site: <https://j-andrews7.github.io/VizModules/reference/>.
> - `NEWS.md` (`news(package = "VizModules")`) — newest features and changes.
>
> **Available modules:** `dittoViz_scatterPlot`, `dittoViz_yPlot`, `dittoViz_freqPlot`, `plotthis_AreaPlot`, `plotthis_BoxPlot`, `plotthis_BarPlot`, `plotthis_SplitBarPlot`, `plotthis_DensityPlot`, `plotthis_DotPlot`, `plotthis_Histogram`, `ComplexHeatmap_Heatmap`, plus the natively-implemented `linePlot`, `piePlot`, `radarPlot`, `parallelCoordinatesPlot`, and `dumbbellPlot`. Each has a matching `*App()` function (e.g. `plotthis_BarPlotApp()`) you can run to see it in action.
>
> **Optional building blocks** (inspect their source/help in the installed package's `R/` directory or via `?`):
> - Data table / filtering module — `?dataFilterUI`, `?dataFilterServer`.
> - Statistical testing helpers (pairwise + omnibus brackets on plotly figures) — see `?compute_pairwise_stats`, `?apply_stat_annotations`, and the README "Statistical Testing" section; supported by the BoxPlot, yPlot, and freqPlot modules.
> - Summary-data export — `?collect_source_data` and `?create_source_download_handler`.
> - App factory — `?createModuleApp` (every `*App()` is a thin wrapper around it).
>
> **Rules:** All plots are plotly-based except `ComplexHeatmap_Heatmap`, which renders through `InteractiveComplexHeatmap` and needs its Bioconductor dependencies installed; prefer the documented module arguments over hand-rolled plotting. Verify function signatures against the installed help pages before using them, and tell me explicitly if a feature you need is not exposed by a module.


[1]: https://j-andrews7.github.io/VizModules/reference/linePlotApp.html
[2]: https://j-andrews7.github.io/VizModules/reference/plotthis_AreaPlotApp.html
[3]: https://j-andrews7.github.io/VizModules/reference/plotthis_BoxPlotApp.html
[4]: https://j-andrews7.github.io/VizModules/reference/plotthis_DensityPlotApp.html
[5]: https://j-andrews7.github.io/VizModules/reference/dumbbellPlotApp.html
[6]: https://j-andrews7.github.io/VizModules/reference/plotthis_HistogramApp.html
[7]: https://j-andrews7.github.io/VizModules/reference/parallelCoordinatesPlotApp.html
[8]: https://j-andrews7.github.io/VizModules/reference/piePlotApp.html
[9]: https://j-andrews7.github.io/VizModules/reference/radarPlotApp.html
[10]: https://j-andrews7.github.io/VizModules/reference/dittoViz_scatterPlotApp.html
[11]: https://j-andrews7.github.io/VizModules/reference/plotthis_SplitBarPlotApp.html
[14]: https://j-andrews7.github.io/VizModules/reference/dittoViz_yPlotApp.html
[15]: https://j-andrews7.github.io/VizModules/reference/plotthis_BarPlotApp.html
[16]: https://j-andrews7.github.io/VizModules/articles/adding-a-new-module.html
[17]: https://j-andrews7.github.io/VizModules/articles/custom-modules.html
[18]: https://j-andrews7.github.io/VizModules/articles/quick-start.html
[19]: https://pwwang.github.io/plotthis/reference/AreaPlot.html
[20]: https://pwwang.github.io/plotthis/reference/boxviolinplot.html
[21]: https://pwwang.github.io/plotthis/reference/densityhistoplot.html
[22]: https://cran.r-project.org/package=dittoViz
[23]: https://pwwang.github.io/plotthis/reference/barplot.html
[24]: https://j-andrews7.github.io/VizModules/reference/plotthis_DotPlotApp.html
[25]: https://pwwang.github.io/plotthis/reference/dotplot.html

[26]: https://j-andrews7.github.io/VizModules/articles/custom-model-lines.html
[27]: https://j-andrews7.github.io/VizModules/articles/custom-shiny-inputs.html
[28]: https://j-andrews7.github.io/VizModules/articles/defaults-and-hiding.html
[29]: https://j-andrews7.github.io/VizModules/articles/statistical-testing.html

[30]: https://j-andrews7.github.io/VizModules/reference/figureBuilderApp.html
[31]: https://j-andrews7.github.io/VizModules/reference/dittoViz_freqPlotApp.html
[32]: https://j-andrews7.github.io/VizModules/reference/ComplexHeatmap_HeatmapApp.html
[33]: https://jokergoo.github.io/ComplexHeatmap-reference/book/
