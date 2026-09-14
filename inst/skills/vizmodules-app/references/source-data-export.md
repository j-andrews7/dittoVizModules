# Exporting plot source data

`collect_source_data()` assembles a record of what a plot is actually showing;
`create_source_download_handler()` turns that into a downloadable `.zip`. Use these
rather than hand-rolling `write.csv()` + `zip()` — the helpers pull the data straight
off the plotly object, so the export matches the plot (filtered rows only, not the whole
input frame).

```r
collect_source_data(plot_reactive, stats_reactive = NULL, inputs_reactive = NULL)
create_source_download_handler(data_list, filename_base = "source_data")
```

- `plot_reactive` — **required**, a reactive returning a plotly object (pass the reactive itself, not its value).
- `stats_reactive` — optional reactive returning a stats data frame.
- `inputs_reactive` — optional named list of UI inputs; `reactiveValuesToList(input)` is the usual value. Note this one takes the **value**, not the reactive.

```r
server <- function(input, output, session) {
    plot_reactive <- reactive({
        plot_ly(mtcars, x = ~wt, y = ~mpg, type = "scatter", mode = "markers")
    })
    output$plot <- renderPlotly(plot_reactive())

    AllInputs <- reactive(reactiveValuesToList(input))

    plot_summary <- reactive({
        collect_source_data(
            plot_reactive   = plot_reactive,
            stats_reactive  = reactive(my_stats_df()),
            inputs_reactive = AllInputs()
        )
    })

    output$download_summary <- create_source_download_handler(
        data_list     = plot_summary,
        filename_base = "my_plot_summary"
    )
}
```

`create_source_download_handler()` also accepts a **named list of summaries**, one per
plot, which is how the figure builder bundles every panel on its canvas into a single
download.

Each built-in module server already does this internally and **returns its own source
reactive**, so in an app you can often just capture the server's return value instead of
building your own:

```r
src <- plotthis_ViolinPlotServer("v", data = reactive(example_rnaseq))
output$dl <- create_source_download_handler(src, filename_base = "violin_source")
```

Every module already shows a **Source Download** button in its control block (the
"tack" that `module_tack_ui()` appends alongside Auto Update / Update / Reset), which
downloads the plot as self-contained HTML, as an SVG and a PNG, plus the source data and
stats as CSVs. A bespoke button is only needed when you want to bundle several plots,
rename the file, or put the control somewhere else in your layout.

The images are captured in the browser off the live graph, which needs the markup
`module_tack_ui()` puts on its button (the `viz-source-download` class and a
`data-viz-source-ns` attribute naming the module). A hand-rolled `downloadButton()`
without it still downloads everything else; to get images too, copy that markup. A module
whose output is not a plotly graph supplies its own instead, via `vector_svg` /
`raster_png` functions on its summary list -- see `draw_to_svg()` and `draw_to_png()`.
