#' Create a VizModules Figure Builder Application
#'
#' Build the multi-panel **Figure Builder** Shiny application as a returnable
#' object. The app lets users add any VizModules plot module to a free-form A4
#' canvas, drag and resize each plot, filter each plot's data independently,
#' label panels automatically, and export the whole figure as a single editable
#' SVG (or bundle every plot's source data, HTML plot, and statistics into one
#' `.zip`).
#'
#' Datasets are supplied via `data_list` and seed the "Add Plot" dialog; users
#' can also upload additional datasets (CSV, TSV, TXT, or RDS) at runtime. The
#' set of available plot modules is controlled by `module_registry`, so the app
#' can be extended with custom wrapper modules without editing the package.
#'
#' This is the recommended way to launch a standalone Figure Builder. Internally
#' it is a thin wrapper around the [figureBuilderUI()] / [figureBuilderServer()]
#' Shiny module, so the same builder can be embedded inside a larger app (and
#' instantiated more than once) by calling those two functions directly. The
#' bundled app at `system.file("apps/figure-builder", package = "VizModules")` is
#' itself a thin wrapper around this function.
#'
#' @param data_list An optional named list of data frames that seed the dataset
#'   registry. If `NULL` (the default), the bundled example datasets (plus a
#'   `sales_by_product` summary suited to the pie plot) are used. At least one
#'   element is required. An element is either a data frame, or a named list of
#'   data frames for a module that needs companion tables (the `ComplexHeatmap`
#'   module's `list(matrix = , column_annotations = )`); in the latter case only
#'   the primary table is filtered and shown in the panel's table pane.
#' @param module_registry An optional named list describing the plot modules to
#'   offer. If `NULL` (the default), all bundled VizModules modules are offered.
#'   Each entry is itself a list with components: `label` (character, shown in the
#'   picker), `dataset` (character, the dataset name its `defaults` were written
#'   for), `inputs_ui`, `output_ui`, and `server_fn` (the module's three
#'   functions), and `defaults` (a named list of input defaults applied only when
#'   `dataset` is the chosen dataset). An entry may also carry `primary.table`,
#'   naming which table of a multi-table dataset gets filtered (the first by
#'   default); its presence is also what marks the module as able to take a
#'   multi-table dataset at all, so modules without it are handed the primary
#'   table alone and any dataset stays usable with any module. See
#'   [figureBuilderServer()] for the `vector_svg` hook that lets a non-plotly
#'   module take part in the SVG figure export.
#' @param title A character string used as the page title and header
#'   (default: `"VizModules Figure Builder"`).
#' @param return_components Logical. When `FALSE` (the default) a
#'   [shiny::shinyApp()] object is returned. When `TRUE` a named list with `ui`
#'   and `server` elements is returned instead, which is convenient for
#'   deployment scripts that need an explicit `shinyApp(ui, server)` call.
#' @return Either a [shiny::shinyApp()] object, or (when
#'   `return_components = TRUE`) a list with elements `ui` and `server`.
#'
#' @import shiny
#'
#' @export
#' @author Jared Andrews
#' @seealso [figureBuilderUI()], [figureBuilderServer()]
#' @examples
#' library(VizModules)
#'
#' # Launch with the bundled example datasets and all modules:
#' app <- figureBuilderApp()
#' if (interactive()) runApp(app)
#'
#' # Launch with your own datasets:
#' app2 <- figureBuilderApp(data_list = list("iris" = iris, "mtcars" = mtcars))
#' if (interactive()) runApp(app2)
#'
#' # Return the UI and server separately (e.g. for a deployment app.R):
#' parts <- figureBuilderApp(return_components = TRUE)
#' if (interactive()) shinyApp(parts$ui, parts$server)
figureBuilderApp <- function(data_list = NULL,
                             module_registry = NULL,
                             title = "VizModules Figure Builder",
                             return_components = FALSE) {
    # The whole builder lives in the figureBuilder Shiny module; this wrapper just
    # drops one instance onto a full page so it can be launched standalone.
    fb_id <- "figure_builder"

    ui <- fluidPage(
        title = title,
        figureBuilderUI(fb_id, title = title)
    )

    server <- function(input, output, session) {
        figureBuilderServer(fb_id,
            data_list = data_list, module_registry = module_registry
        )
    }

    if (isTRUE(return_components)) {
        return(list(ui = ui, server = server))
    }

    shinyApp(ui, server)
}


# Build the default dataset catalogue: the bundled example datasets plus a
# derived `sales_by_product` summary that suits the pie plot.
.figure_builder_data <- function() {
    sales_by_product <- aggregate(revenue ~ product_line, example_sales, sum)
    list(
        "example_sales"           = example_sales,
        "example_bar"             = example_bar,
        "example_demographics"    = example_demographics,
        "example_markers"         = example_markers,
        "example_school_earnings" = example_school_earnings,
        "example_skills"          = example_skills,
        "example_rnaseq"          = example_rnaseq,
        "example_iris"            = example_iris,
        "example_mtcars"          = example_mtcars,
        "example_population"      = example_population,
        "example_composition"     = example_composition,
        "sales_by_product"        = sales_by_product,
        # A two-table entry: the ComplexHeatmap module needs per-sample metadata
        # alongside the matrix for its column annotations, splits and filters.
        # Only the matrix is filtered and shown in a panel's table pane.
        "example_heatmap"         = list(
            matrix = example_heatmap_matrix,
            column_annotations = example_heatmap_column_data
        )
    )
}

# Build the default module registry. Each entry wires up one VizModules module.
# `dataset` is the dataset that the supplied `defaults` were written for; it is
# used as the initial selection and the defaults are only applied when that
# dataset is chosen.
.figure_builder_registry <- function() {
    registry <- list(
        area = list(
            label = "Area Plot", dataset = "example_sales",
            inputs_ui = plotthis_AreaPlotInputsUI,
            output_ui = plotthis_AreaPlotOutputUI,
            server_fn = plotthis_AreaPlotServer,
            defaults = list(
                "x.data" = "year", "y.data" = "revenue",
                "group.by" = "product_line"
            )
        ),
        bar = list(
            label = "Bar Plot", dataset = "example_bar",
            inputs_ui = plotthis_BarPlotInputsUI,
            output_ui = plotthis_BarPlotOutputUI,
            server_fn = plotthis_BarPlotServer,
            defaults = list(
                "x.data" = "Group", "y.data" = "Values",
                "group.by" = "Type"
            )
        ),
        box = list(
            label = "Box Plot", dataset = "example_demographics",
            inputs_ui = plotthis_BoxPlotInputsUI,
            output_ui = plotthis_BoxPlotOutputUI,
            server_fn = plotthis_BoxPlotServer,
            defaults = list("x.data" = "department", "y.data" = "salary")
        ),
        density = list(
            label = "Density Plot", dataset = "example_demographics",
            inputs_ui = plotthis_DensityPlotInputsUI,
            output_ui = plotthis_DensityPlotOutputUI,
            server_fn = plotthis_DensityPlotServer,
            defaults = list("x.data" = "salary", "group.by" = "department")
        ),
        dotplot = list(
            label = "Dot Plot", dataset = "example_markers",
            inputs_ui = plotthis_DotPlotInputsUI,
            output_ui = plotthis_DotPlotOutputUI,
            server_fn = plotthis_DotPlotServer,
            defaults = list(
                "x.data" = "gene", "y.data" = "cell_type",
                "size.by" = "pct_expressed", "fill.by" = "avg_expression"
            )
        ),
        dumbbell = list(
            label = "Dumbbell Plot", dataset = "example_school_earnings",
            inputs_ui = dumbbellPlotInputsUI,
            output_ui = dumbbellPlotOutputUI,
            server_fn = dumbbellPlotServer,
            defaults = list()
        ),
        freq = list(
            label = "Frequency Plot", dataset = "example_composition",
            inputs_ui = dittoViz_freqPlotInputsUI,
            output_ui = dittoViz_freqPlotOutputUI,
            server_fn = dittoViz_freqPlotServer,
            defaults = list(
                "var" = "cell_type", "sample.by" = "sample",
                "group.by" = "condition"
            )
        ),
        histogram = list(
            label = "Histogram", dataset = "example_demographics",
            inputs_ui = plotthis_HistogramInputsUI,
            output_ui = plotthis_HistogramOutputUI,
            server_fn = plotthis_HistogramServer,
            defaults = list("x.data" = "salary")
        ),
        line = list(
            label = "Line Plot", dataset = "example_sales",
            inputs_ui = linePlotInputsUI,
            output_ui = linePlotOutputUI,
            server_fn = linePlotServer,
            defaults = list("x.value" = "product_line", "y.value" = "units")
        ),
        parallel = list(
            label = "Parallel Coordinates", dataset = "example_sales",
            inputs_ui = parallelCoordinatesPlotInputsUI,
            output_ui = parallelCoordinatesPlotOutputUI,
            server_fn = parallelCoordinatesPlotServer,
            defaults = list("color.by" = "product_line")
        ),
        pie = list(
            label = "Pie Plot", dataset = "sales_by_product",
            inputs_ui = piePlotInputsUI,
            output_ui = piePlotOutputUI,
            server_fn = piePlotServer,
            defaults = list("labels" = "product_line", "values" = "revenue")
        ),
        radar = list(
            label = "Radar Plot", dataset = "example_skills",
            inputs_ui = radarPlotInputsUI,
            output_ui = radarPlotOutputUI,
            server_fn = radarPlotServer,
            defaults = list("theta" = "category", "r" = "value", "group" = "player")
        ),
        scatter = list(
            label = "Scatter Plot", dataset = "example_sales",
            inputs_ui = dittoViz_scatterPlotInputsUI,
            output_ui = dittoViz_scatterPlotOutputUI,
            server_fn = dittoViz_scatterPlotServer,
            defaults = list(
                "x.by" = "revenue", "y.by" = "units",
                "color.by" = "product_line"
            )
        ),
        splitbar = list(
            label = "Split Bar Plot", dataset = "example_bar",
            inputs_ui = plotthis_SplitBarPlotInputsUI,
            output_ui = plotthis_SplitBarPlotOutputUI,
            server_fn = plotthis_SplitBarPlotServer,
            defaults = list("x.data" = "Score", "y.data" = "Group")
        ),
        yplot = list(
            label = "yPlot", dataset = "example_demographics",
            inputs_ui = dittoViz_yPlotInputsUI,
            output_ui = dittoViz_yPlotOutputUI,
            server_fn = dittoViz_yPlotServer,
            defaults = list("var" = "salary", "group.by" = "department")
        )
    )

    # The ComplexHeatmap module depends on Bioconductor packages that may not be
    # installed, and its server stops outright without them. Offer it only when
    # they are available, so the builder still runs without them -- the same
    # guard the module gallery applies to its Heatmap tab.
    heatmap_available <- all(vapply(
        c("ComplexHeatmap", "InteractiveComplexHeatmap", "circlize"),
        requireNamespace, logical(1),
        quietly = TRUE
    ))
    if (heatmap_available) {
        # Deliberately the *static* output: the InteractiveComplexHeatmap widget
        # carries a grey panel border, a control tab strip and a fixed pixel
        # width that no argument turns off, none of which belong on a figure
        # panel. See ComplexHeatmap_HeatmapStaticOutputUI().
        registry$heatmap <- list(
            label = "Heatmap", dataset = "example_heatmap",
            primary.table = "matrix",
            inputs_ui = ComplexHeatmap_HeatmapInputsUI,
            output_ui = ComplexHeatmap_HeatmapStaticOutputUI,
            server_fn = ComplexHeatmap_HeatmapServer,
            defaults = list(
                "rowname.col" = "gene",
                "matrix.cols" = setdiff(
                    names(example_heatmap_matrix),
                    c("gene", "pathway", "mean_expression")
                )
            )
        )
    }

    registry
}

.figure_builder_css <- function() {
    HTML("
.pb-canvas-scroll {
    overflow: auto;
    border: 1px solid #e0e0e0;
    background: #ececec;
    padding: 16px;
    border-radius: 6px;
}
.pb-canvas {
    position: relative;
    margin: 0 auto;
    background:
        #fff
        linear-gradient(90deg, #f3f3f3 1px, transparent 1px) 0 0 / 24px 24px,
        linear-gradient(0deg, #f3f3f3 1px, transparent 1px) 0 0 / 24px 24px;
    box-shadow: 0 1px 8px rgba(0,0,0,0.2);
    overflow: hidden;
}
/* A4 page sizes at 96dpi (210 x 297 mm). */
.pb-canvas.a4-portrait  { width: 794px;  height: 1123px; }
.pb-canvas.a4-landscape { width: 1123px; height: 794px; }
.viz-panel-card {
    position: absolute;
    width: 480px;
    height: 380px;
    background: #fff;
    border: 1px solid #ccc;
    border-radius: 6px;
    box-shadow: 0 2px 6px rgba(0,0,0,0.15);
    display: flex;
    flex-direction: column;
    overflow: hidden;
}
/* A small floating toolbar that only appears while hovering a card. It holds
   the drag handle and remove button so the static card (and the SVG export)
   stays free of any chrome. */
.viz-panel-toolbar {
    position: absolute;
    top: 4px;
    left: 4px;
    z-index: 10;
    display: flex;
    align-items: center;
    gap: 2px;
    padding: 2px 4px;
    background: rgba(44, 62, 80, 0.85);
    border-radius: 4px;
    opacity: 0;
    transition: opacity 0.15s ease-in-out;
    pointer-events: none;
}
.viz-panel-card:hover .viz-panel-toolbar { opacity: 1; pointer-events: auto; }
/* Live panel label (a, b, c, ...) shown in the card's top-left corner. It sits
   below the hover toolbar and is ignored by the SVG export (which draws its own
   labels), so it only provides on-screen feedback for the 'Panel labels' menu. */
.viz-panel-label {
    position: absolute;
    top: 4px;
    left: 8px;
    z-index: 5;
    font-family: Helvetica, Arial, sans-serif;
    font-weight: 700;
    font-size: 20px;
    color: #000;
    line-height: 1;
    pointer-events: none;
    user-select: none;
}
.viz-panel-drag {
    cursor: move;
    color: #fff;
    padding: 0 4px;
    font-size: 13px;
    line-height: 1;
    user-select: none;
}
.viz-panel-remove {
    background: transparent; border: none; color: #fff;
    padding: 0 4px; line-height: 1; cursor: pointer; font-size: 13px;
}
.viz-panel-remove:hover { color: #ff7675; }
.viz-panel-body { flex: 1 1 auto; padding: 6px; overflow: hidden; }
/* Let the plot fill the card so vertical resizing changes the plot height,
   not just the width. */
.viz-panel-body > .html-widget,
.viz-panel-body > .shiny-plot-output,
.viz-panel-body .js-plotly-plot,
.viz-panel-body .plotly,
.viz-panel-body .plot-container {
    width: 100% !important;
    height: 100% !important;
}
.pb-empty-hint { color: #999; text-align: center; padding-top: 30vh; }

/* --- Compact sidebar -------------------------------------------------------
   Tighten vertical rhythm so more controls fit without scrolling. Scoped to
   this builder's own layout: `.well` on its own is Bootstrap's class, which
   shiny::sidebarPanel() renders, so styling it bare reached every well on a
   host page that embedded the builder. */
.pb-app .well {
    padding: 10px 12px;
}
.pb-app .well h4 {
    margin-top: 8px;
    margin-bottom: 6px;
    font-size: 15px;
}
.pb-app .well hr {
    margin-top: 8px;
    margin-bottom: 8px;
    border-top: 1px solid #ccc;
}
.pb-app .well .help-block {
    margin-top: 2px;
    margin-bottom: 4px;
    font-size: 11px;
    line-height: 1.3;
}
.pb-app .well .form-group {
    margin-bottom: 8px;
}
.pb-app .well .control-label {
    margin-bottom: 2px;
}
.pb-app .well .btn {
    padding: 4px 10px;
}
/* Trim the gap shiny adds around fileInput's progress bar. */
.pb-app .well .form-group .progress {
    margin-bottom: 4px;
}
/* Make the two primary action buttons fill their half-row columns. */
.pb-app .well .btn-block {
    width: 100%;
}
/* Collapsible 'Load Data' disclosure: a clickable heading that hides its
   contents until opened, reclaiming vertical space in the sidebar. */
.pb-details {
    margin: 8px 0;
}
.pb-details > summary {
    cursor: pointer;
    font-size: 15px;
    font-weight: 700;
    list-style: none;
    padding: 2px 0;
    color: #2c3e50;
}
.pb-details > summary::-webkit-details-marker { display: none; }
.pb-details > summary::before {
    content: '\\25B8';
    display: inline-block;
    margin-right: 6px;
    transition: transform 0.15s ease-in-out;
}
.pb-details[open] > summary::before { transform: rotate(90deg); }
")
}

# Client-side export of the whole canvas to a single SVG file.
.figure_builder_js <- function() {
    HTML("
// The Figure Builder is a namespaced Shiny module, so its element ids are
// prefixed (e.g. '<ns>-pb_canvas'). To stay instance-safe we never hardcode an
// id: canvases are found by the 'pb-canvas' class and their sibling controls are
// derived from the canvas id's namespace prefix. All logic lives in one guarded
// IIFE so injecting the script for several instances never redefines globals or
// double-binds handlers.
(function() {
if (window.__vizFigureBuilderLoaded) { return; }
window.__vizFigureBuilderLoaded = true;

// Given a canvas element with id '<ns>pb_canvas', return the namespace prefix
// '<ns>' so sibling ids ('<ns>pb_label_case', ...) can be resolved.
function pbNamespace(canvas) {
    var id = canvas.id || '';
    var suffix = 'pb_canvas';
    return id.slice(0, id.length - suffix.length);
}
// Resolve the canvas that a given namespaced control belongs to.
function pbCanvasForControl(el, suffix) {
    var id = el.id || '';
    var prefix = id.slice(0, id.length - suffix.length);
    return document.getElementById(prefix + 'pb_canvas');
}
// Decode a Plotly SVG data URL ('data:image/svg+xml,...') into raw SVG markup
// and strip the XML prolog/doctype so it can be embedded as a nested element.
function pbDecodeSvgDataUrl(url) {
    if (!url) { return null; }
    var comma = url.indexOf(',');
    if (comma === -1) { return null; }
    var head = url.slice(0, comma);
    var data = url.slice(comma + 1);
    var svg = head.indexOf('base64') !== -1 ? atob(data) : decodeURIComponent(data);
    return svg.replace(/<\\?xml[^>]*\\?>/i, '').replace(/<!DOCTYPE[^>]*>/i, '').trim();
}
// Spreadsheet-style label for a 0-based index: 0->A, 25->Z, 26->AA, ...
function pbPanelLabel(index, mode) {
    if (mode !== 'upper' && mode !== 'lower') { return null; }
    var base = mode === 'lower' ? 97 : 65;
    var n = index, label = '';
    do {
        label = String.fromCharCode(base + (n % 26)) + label;
        n = Math.floor(n / 26) - 1;
    } while (n >= 0);
    return label;
}
// Escape text destined for an SVG <text> element.
function pbEscapeXml(s) {
    return String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;')
        .replace(/>/g, '&gt;');
}
// Order a canvas's cards the way a reader scans a figure: top-to-bottom by row,
// then left-to-right within a row. Cards whose tops differ by less than half the
// taller card's height are treated as the same row (the 'tol' tolerance below).
function pbOrderCards(canvas) {
    var canvasRect = canvas.getBoundingClientRect();
    var items = Array.prototype.map.call(
        canvas.querySelectorAll('.viz-panel-card'),
        function(card) {
            var r = card.getBoundingClientRect();
            return {
                card: card,
                x: r.left - canvasRect.left + canvas.scrollLeft,
                y: r.top - canvasRect.top + canvas.scrollTop,
                h: r.height
            };
        }
    );
    return items.sort(function(a, b) {
        var tol = Math.max(a.h, b.h) * 0.5;
        if (Math.abs(a.y - b.y) > tol) { return a.y - b.y; }
        return a.x - b.x;
    });
}
// Fill in each card's on-screen panel label from the 'Panel labels' menu, so
// selecting Upper/Lowercase gives immediate visual feedback (the labels also
// reorder as cards are dragged around). Mirrors the SVG export's labelling.
function pbAssignLabels(canvas) {
    if (!canvas) { return; }
    var ns = pbNamespace(canvas);
    var sel = document.getElementById(ns + 'pb_label_case');
    var mode = sel ? sel.value : 'none';
    pbOrderCards(canvas).forEach(function(it, i) {
        var el = it.card.querySelector('.viz-panel-label');
        if (!el) { return; }
        var label = pbPanelLabel(i, mode);
        el.textContent = label || '';
        el.style.display = label ? '' : 'none';
    });
}
function pbAssignLabelsAll() {
    document.querySelectorAll('.pb-canvas').forEach(pbAssignLabels);
}
// Recompute labels when the menu changes or after a drag settles the layout.
// The 'Panel labels' <select> is tagged with the 'pb-label-case' class so this
// delegated handler matches by class (like the SVG download button), then finds
// the sibling canvas from the select's own namespaced id.
document.addEventListener('change', function(e) {
    var t = e.target;
    if (!t || !t.closest || !t.closest('.pb-label-case')) { return; }
    pbAssignLabels(pbCanvasForControl(t, 'pb_label_case'));
});
document.addEventListener('mouseup', function() {
    setTimeout(pbAssignLabelsAll, 0);
});
// Cards whose module renders something other than a plotly graph have no SVG
// the browser can read out of them. The server can redraw those panels, so they
// are collected and asked for in one round trip; see figureBuilderServer().
var pbSvgRequests = {};
function pbRequestPanelSvg(ns, panels) {
    return new Promise(function(resolve) {
        if (!panels.length || typeof Shiny === 'undefined' || !Shiny.setInputValue) {
            resolve({});
            return;
        }
        var nonce = String(Date.now()) + '-' + Math.random().toString(36).slice(2);
        var settled = false;
        function finish(map) {
            if (settled) { return; }
            settled = true;
            delete pbSvgRequests[nonce];
            resolve(map);
        }
        pbSvgRequests[nonce] = finish;
        // A request that is never answered (a module error, a dropped socket)
        // must not leave the download hanging forever; the figure then goes out
        // without those panels rather than not at all.
        setTimeout(function() { finish({}); }, 20000);
        Shiny.setInputValue(ns + 'pb_svg_request',
            { nonce: nonce, panels: panels }, { priority: 'event' });
    });
}
function pbUnbox(x) { return Array.isArray(x) ? x[0] : x; }
if (typeof Shiny !== 'undefined' && Shiny.addCustomMessageHandler) {
    Shiny.addCustomMessageHandler('vizmodules-pb-svg', function(msg) {
        var finish = msg ? pbSvgRequests[pbUnbox(msg.nonce)] : null;
        if (!finish) { return; }
        var map = {};
        (msg.panels || []).forEach(function(p) {
            map[pbUnbox(p.pid)] = pbUnbox(p.svg);
        });
        finish(map);
    });
}
// A card's id is '<ns><pid>_card'; the server knows the panel by its bare pid.
function pbPanelIdFromCard(card, ns) {
    var id = card.id || '';
    if (ns && id.indexOf(ns) === 0) { id = id.slice(ns.length); }
    return id.replace(/_card$/, '');
}
function pbDownloadSVG(canvas) {
    if (!canvas) { return; }
    var cards = canvas.querySelectorAll('.viz-panel-card');
    if (!cards.length) {
        alert('Add at least one plot before downloading.');
        return;
    }
    var ns = pbNamespace(canvas);
    var labelSel = document.getElementById(ns + 'pb_label_case');
    var labelMode = labelSel ? labelSel.value : 'none';
    var W = canvas.clientWidth, H = canvas.clientHeight;
    var canvasRect = canvas.getBoundingClientRect();
    var metas = [];
    var tasks = [];
    var pending = [];
    cards.forEach(function(card) {
        var cardRect = card.getBoundingClientRect();
        var x = cardRect.left - canvasRect.left + canvas.scrollLeft;
        var y = cardRect.top - canvasRect.top + canvas.scrollTop;
        var w = cardRect.width, h = cardRect.height;
        var body = card.querySelector('.viz-panel-body');
        var gd = card.querySelector('.js-plotly-plot');
        var pw = body ? body.clientWidth : w;
        var ph = body ? body.clientHeight : h;
        var meta = { x: x, y: y, w: w, h: h, pw: pw, ph: ph, svg: null };
        metas.push(meta);
        if (gd && window.Plotly) {
            tasks.push(
                Plotly.toImage(gd, { format: 'svg', width: pw, height: ph })
                    .then(function(url) { meta.svg = pbDecodeSvgDataUrl(url); return meta; })
                    .catch(function() { return meta; })
            );
        } else {
            // Not a plotly graph, so ask the server to redraw this panel.
            meta.pid = pbPanelIdFromCard(card, ns);
            if (meta.pid) { pending.push({ pid: meta.pid, pw: pw, ph: ph }); }
            tasks.push(Promise.resolve(meta));
        }
    });
    Promise.all([pbRequestPanelSvg(ns, pending)].concat(tasks)).then(function(done) {
        var fromServer = done[0] || {};
        metas.forEach(function(it) {
            if (!it.svg && it.pid && fromServer[it.pid]) { it.svg = fromServer[it.pid]; }
        });
        // Order panels for labelling the way a reader scans a figure:
        // top-to-bottom by row, then left-to-right within a row. A row
        // tolerance groups panels whose tops are roughly aligned.
        var ordered = metas.slice().sort(function(a, b) {
            var tol = Math.max(a.h, b.h) * 0.5;
            if (Math.abs(a.y - b.y) > tol) { return a.y - b.y; }
            return a.x - b.x;
        });
        ordered.forEach(function(it, i) {
            it.label = pbPanelLabel(i, labelMode);
        });
        var parts = [];
        parts.push('<?xml version=\"1.0\" encoding=\"UTF-8\"?>');
        parts.push('<svg xmlns=\"http://www.w3.org/2000/svg\" ' +
            'xmlns:xlink=\"http://www.w3.org/1999/xlink\" ' +
            'width=\"' + W + '\" height=\"' + H + '\" ' +
            'viewBox=\"0 0 ' + W + ' ' + H + '\">');
        parts.push('<rect x=\"0\" y=\"0\" width=\"' + W + '\" height=\"' + H +
            '\" fill=\"#ffffff\"/>');
        metas.forEach(function(it) {
            if (it.svg) {
                parts.push('<g transform=\"translate(' + (it.x + 6) + ',' +
                    (it.y + 6) + ')\">');
                parts.push(it.svg);
                parts.push('</g>');
            }
            // Panel label sits in the top-left corner of the card, where the
            // hover toolbar lives in the live app.
            if (it.label) {
                parts.push('<text x=\"' + (it.x + 8) + '\" y=\"' + (it.y + 24) +
                    '\" font-family=\"Helvetica, Arial, sans-serif\" ' +
                    'font-size=\"20\" font-weight=\"bold\" fill=\"#000000\">' +
                    pbEscapeXml(it.label) + '</text>');
            }
        });
        parts.push('</svg>');
        var blob = new Blob([parts.join('\\n')], { type: 'image/svg+xml' });
        var a = document.createElement('a');
        a.href = URL.createObjectURL(blob);
        a.download = 'vizmodules-panel.svg';
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        URL.revokeObjectURL(a.href);
    });
}
// Delegated handler so every instance's download button works without an inline
// onclick (which would need a hardcoded, non-namespaced global).
document.addEventListener('click', function(e) {
    var btn = e.target.closest ? e.target.closest('.pb-download-svg') : null;
    if (!btn) { return; }
    e.preventDefault();
    pbDownloadSVG(pbCanvasForControl(btn, 'pb_download'));
});

// Keep each plot sized to its card so dragging the resize handle changes the
// plot's height as well as its width. jQuery UI resizable only resizes the
// card div, so we watch each card body and ask Plotly to relayout to fit.
var pbCardObservers = new WeakMap();
var pbResizeNudge = null;
// Shiny only recomputes an output's clientData size on a window resize, and
// jQuery UI resizing a card fires none. Debounced so dragging a handle does not
// ask the server for a redraw on every frame.
function pbNudgeWindowResize() {
    if (pbResizeNudge) { clearTimeout(pbResizeNudge); }
    pbResizeNudge = setTimeout(function() {
        pbResizeNudge = null;
        if (window.dispatchEvent) { window.dispatchEvent(new Event('resize')); }
    }, 150);
}
function pbResizePlot(card) {
    var gd = card.querySelector('.js-plotly-plot');
    if (gd && window.Plotly) { Plotly.Plots.resize(gd); return; }
    if (card.querySelector('.shiny-plot-output')) { pbNudgeWindowResize(); }
}
function pbObserveCard(card) {
    if (pbCardObservers.has(card) || !window.ResizeObserver) { return; }
    var body = card.querySelector('.viz-panel-body');
    if (!body) { return; }
    var ro = new ResizeObserver(function() { pbResizePlot(card); });
    ro.observe(body);
    pbCardObservers.set(card, ro);
}
function pbUnobserveCard(card) {
    var ro = pbCardObservers.get(card);
    if (ro) { ro.disconnect(); pbCardObservers.delete(card); }
}
function pbFindCard(node) {
    if (node.nodeType !== 1) { return null; }
    if (node.classList && node.classList.contains('viz-panel-card')) { return node; }
    if (node.querySelector) { return node.querySelector('.viz-panel-card'); }
    return null;
}
function pbContainsPlot(node) {
    if (node.nodeType !== 1) { return false; }
    var sel = '.js-plotly-plot, .shiny-plot-output';
    if (node.matches && node.matches(sel)) { return true; }
    return node.querySelector ? !!node.querySelector(sel) : false;
}
// Ask every plot on every canvas to relayout to its container. A card body is a
// fixed-size flex box, so its ResizeObserver never fires after Plotly finishes
// its (asynchronous) initial render. Without this nudge a freshly added plot can
// stay blank, which is why plots stopped appearing after the first few. We also
// dispatch a window resize so any plot that measured a zero-size container
// re-measures and draws.
function pbResizeAllPlots() {
    if (!window.Plotly) { return; }
    document.querySelectorAll('.pb-canvas .js-plotly-plot').forEach(function(gd) {
        try { Plotly.Plots.resize(gd); } catch (e) {}
    });
}
function pbScheduleResize() {
    [50, 200, 500].forEach(function(t) {
        setTimeout(function() {
            pbResizeAllPlots();
            if (window.dispatchEvent) { window.dispatchEvent(new Event('resize')); }
        }, t);
    });
}
// Watch the whole document so cards added to any canvas (including canvases that
// are themselves inserted at runtime) get a ResizeObserver, and torn-down cards
// release theirs. Nudge plots to resize whenever a card or plot is inserted so
// none are left blank.
$(function() {
    var mo = new MutationObserver(function(mutations) {
        var cardsChanged = false;
        mutations.forEach(function(m) {
            m.addedNodes.forEach(function(node) {
                var card = pbFindCard(node);
                if (card) { pbObserveCard(card); pbScheduleResize(); cardsChanged = true; }
                if (pbContainsPlot(node)) { pbScheduleResize(); }
            });
            m.removedNodes.forEach(function(node) {
                var card = pbFindCard(node);
                if (card) { pbUnobserveCard(card); cardsChanged = true; }
            });
        });
        // Renumber panel labels whenever the set of cards changes.
        if (cardsChanged) { pbAssignLabelsAll(); }
    });
    mo.observe(document.body, { childList: true, subtree: true });
    pbAssignLabelsAll();
});
})();
")
}
