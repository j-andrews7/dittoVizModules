# The Stats tab

Available on **`dittoViz_yPlot`, `plotthis_BoxPlot`, and `dittoViz_freqPlot` only** — the modules that put a numeric value against categorical
groups. No other module has one, and `hide.tabs = "Stats"` on a module without it is a
no-op, not an error.

Users switch it on with the **Enable Stats** toggle. To have an app open with it already
configured, pre-fill the keys through `defaults`.

## Keys and their defaults

| Key | Meaning | Default |
|---|---|---|
| `stats.enabled` | Master toggle | `FALSE` |
| `stat.test` | `"wilcox.test"`, `"t.test"`, `"kruskal.test"`, `"anova"` | `"wilcox.test"` |
| `stat.p.adjust` | `"holm"`, `"hochberg"`, `"hommel"`, `"bonferroni"`, `"BH"`, `"BY"`, `"fdr"`, `"none"` | `"holm"` |
| `stat.display` | `"p.adj"`, `"p.value"`, `"symbol"` | `"p.adj"` |
| `stat.sig.threshold` | P-values above this are labelled `ns` | `0.05` |
| `stat.hide.ns` | Drop non-significant brackets | `TRUE` |
| `stat.paired` | Paired test (signed-rank / paired t) | `FALSE` |
| `stat.pairs` | Specific `"A vs B"` comparisons; empty means all pairs | none selected |
| `stat.per.facet` | Test within each facet | `TRUE` |
| `stat.bracket.style` | Bracket shape | `"capped"` |
| `stat.bracket.inset`, `stat.step.increase`, `stat.text.bump` | Bracket geometry | numeric |
| `stat.line.color`, `stat.line.width` | Bracket styling | `"#000000"`, `1` |

Note the naming: the toggle is `stats.enabled` (plural), everything else is `stat.*`
(singular).

```r
plotthis_BoxPlotServer("b", data = reactive(example_rnaseq),
    defaults = list(
        x.data        = "cell_type",
        y.data        = "log2_cpm",
        group.by      = "condition",
        stats.enabled = TRUE,
        stat.test     = "wilcox.test",
        stat.p.adjust = "BH"
    ))
```

`wilcox.test` and `t.test` are pairwise; `kruskal.test` and `anova` are omnibus.

Paired tests require each group to have the same number of observations **in
corresponding row order** — the data must be sorted so paired samples align row-by-row
within each group.

## Limitations

On `dittoViz_yPlot` the Stats tab is hidden for the `"group"` and `"color"` multivar
aesthetics, and when a `split.by` facet is combined with several y variables — brackets
cannot be placed against those layouts.

On `dittoViz_freqPlot` testing is **always** per-facet: the module hides `stat.per.facet`
and passes `per.facet = TRUE` regardless. Each facet is a different level of the frequency
variable, so pooling across them would compare non-comparable quantities. A
`defaults = list(stat.per.facet = FALSE)` there is a no-op. Note also that the tests run
against the summarised frequency table (one point per sample), not the input rows, so the
group sizes are donor/sample counts — small enough that `wilcox.test` often cannot reach
significance.

## Stats in a hand-built figure

If you are not using a module, the helpers are exported and work against any
`ggplotly()` figure. `create_stat_annotations()` assumes ggplotly's 1-based categorical
axis convention (first factor level at `x = 1`), so build the figure with
`ggplot2` + `ggplotly()` the way the modules do.

```r
stats_df <- compute_pairwise_stats(df, x, y, pairs = NULL, test = "wilcox.test",
                                   p.adjust.method = "holm", paired = FALSE,
                                   group.by = NULL, facet.by = NULL, per.facet = TRUE,
                                   sig.threshold = 0.05)
ann <- create_stat_annotations(stats_df, fig, df, x, y, display = "p.adj", hide.ns = FALSE, ...)
fig <- apply_stat_annotations(fig, ann, y.min = NULL, y.max = NULL)
```

`generate_pair_strings(df, x, group.by)` builds the `"A vs B"` strings for a selector;
`parse_pair_strings()` converts them back to length-2 vectors.
`stat_bracket_y_max()` reports how high the brackets will reach, so an axis maximum can
be raised to clear them instead of clipping.
