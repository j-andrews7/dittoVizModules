library(VizModules)

# Derived summary dataset (pie plot)
sales_by_product <- aggregate(revenue ~ product_line, example_sales, sum)

pkg_desc <- utils::packageDescription("VizModules")
pkg_name <- pkg_desc[["Package"]]
if (is.null(pkg_name) || is.na(pkg_name) || !nzchar(pkg_name)) {
    pkg_name <- "VizModules"
}
pkg_authors <- NA_character_
pkg_authors_field <- pkg_desc[["Authors@R"]]
if (!is.null(pkg_authors_field) && !is.na(pkg_authors_field) &&
    nzchar(trimws(pkg_authors_field))) {
    pkg_authors <- tryCatch({
        authors <- eval(str2expression(pkg_authors_field))
        paste(
            vapply(seq_along(authors), function(i) {
                trimws(paste(c(authors[[i]]$given, authors[[i]]$family),
                             collapse = " "))
            }, character(1)),
            collapse = ", "
        )
    }, error = function(e) NA_character_)
}
if (is.na(pkg_authors) || !nzchar(trimws(pkg_authors))) {
    pkg_author_field <- pkg_desc[["Author"]]
    if (!is.null(pkg_author_field) && !is.na(pkg_author_field) &&
        nzchar(trimws(pkg_author_field))) {
        pkg_authors <- pkg_author_field
    }
}
if (is.na(pkg_authors) || !nzchar(trimws(pkg_authors))) {
    pkg_authors <- "Unavailable"
}
pkg_authors <- gsub("[[:space:]]+", " ", trimws(pkg_authors))
pkg_version <- as.character(utils::packageVersion("VizModules"))
pkg_url_field <- pkg_desc[["URL"]]
if (is.null(pkg_url_field) || is.na(pkg_url_field)) {
    pkg_url_field <- ""
}
pkg_urls <- trimws(strsplit(pkg_url_field, ",")[[1]])
docs_url <- pkg_urls[grepl("github\\.io|pkgdown", pkg_urls)][1]
repo_url <- pkg_urls[grepl("github\\.com", pkg_urls)][1]

if (is.na(docs_url) || !nzchar(docs_url)) {
    docs_url <- sprintf("https://j-andrews7.github.io/%s/", pkg_name)
}

if (is.na(repo_url) || !nzchar(repo_url)) {
    repo_url <- "https://github.com/j-andrews7/VizModules"
}

cran_url <- sprintf("https://cran.r-project.org/package=%s", pkg_name)


module_data <- list(
    area     = example_sales,
    bar      = example_bar,
    box      = example_demographics,
    density  = example_demographics,
    dotplot  = example_markers,
    dumbbell = example_school_earnings,
    freq     = example_composition,
    heatmap  = example_heatmap_matrix,
    histogram = example_demographics,
    line     = example_sales,
    parallel = example_sales,
    pie      = sales_by_product,
    radar    = example_skills,
    scatter  = example_sales,
    splitbar = example_bar,
    yplot    = example_demographics
)

# The ComplexHeatmap module depends on Bioconductor packages that may not be
# installed. Only register its tab when they are available so the gallery
# still runs without them.
heatmap_available <- all(vapply(
    c("ComplexHeatmap", "InteractiveComplexHeatmap", "circlize"),
    requireNamespace, logical(1), quietly = TRUE
))

# Module registry – each entry defines one plot module for the gallery.
module_registry <- list(
    list(
        label     = "Area",
        id        = "area",
        inputs_ui = plotthis_AreaPlotInputsUI,
        output_ui = plotthis_AreaPlotOutputUI,
        server_fn = plotthis_AreaPlotServer,
        defaults  = list("x.data" = "year", "y.data" = "revenue",
                         "group.by" = "product_line")
    ),
    list(
        label     = "Bar",
        id        = "bar",
        inputs_ui = plotthis_BarPlotInputsUI,
        output_ui = plotthis_BarPlotOutputUI,
        server_fn = plotthis_BarPlotServer,
        defaults  = list("x.data" = "Group", "y.data" = "Values",
                         "group.by" = "Type")
    ),
    list(
        label     = "Box",
        id        = "box",
        inputs_ui = plotthis_BoxPlotInputsUI,
        output_ui = plotthis_BoxPlotOutputUI,
        server_fn = plotthis_BoxPlotServer,
        defaults  = list("x.data" = "department", "y.data" = "salary")
    ),
    list(
        label     = "Density",
        id        = "density",
        inputs_ui = plotthis_DensityPlotInputsUI,
        output_ui = plotthis_DensityPlotOutputUI,
        server_fn = plotthis_DensityPlotServer,
        defaults  = list("x.data" = "salary", "group.by" = "department")
    ),
    list(
        label     = "Dumbbell",
        id        = "dumbbell",
        inputs_ui = dumbbellPlotInputsUI,
        output_ui = dumbbellPlotOutputUI,
        server_fn = dumbbellPlotServer,
        defaults  = list()
    ),
    list(
        label     = "Dot",
        id        = "dotplot",
        inputs_ui = plotthis_DotPlotInputsUI,
        output_ui = plotthis_DotPlotOutputUI,
        server_fn = plotthis_DotPlotServer,
        defaults  = list("x.data" = "gene", "y.data" = "cell_type",
                         "size.by" = "pct_expressed", "fill.by" = "avg_expression")
    ),
    list(
        label     = "Frequency",
        id        = "freq",
        inputs_ui = dittoViz_freqPlotInputsUI,
        output_ui = dittoViz_freqPlotOutputUI,
        server_fn = dittoViz_freqPlotServer,
        defaults  = list("var" = "cell_type", "sample.by" = "sample",
                         "group.by" = "condition")
    ),
    list(
        label     = "Histogram",
        id        = "histogram",
        inputs_ui = plotthis_HistogramInputsUI,
        output_ui = plotthis_HistogramOutputUI,
        server_fn = plotthis_HistogramServer,
        defaults  = list("x.data" = "salary")
    ),
    list(
        label     = "Line",
        id        = "line",
        inputs_ui = linePlotInputsUI,
        output_ui = linePlotOutputUI,
        server_fn = linePlotServer,
        defaults  = list("x.value" = "product_line", "y.value" = "units")
    ),
    list(
        label     = "Parallel Coordinates",
        id        = "parallel",
        inputs_ui = parallelCoordinatesPlotInputsUI,
        output_ui = parallelCoordinatesPlotOutputUI,
        server_fn = parallelCoordinatesPlotServer,
        defaults  = list("color.by" = "product_line")
    ),
    list(
        label     = "Pie",
        id        = "pie",
        inputs_ui = piePlotInputsUI,
        output_ui = piePlotOutputUI,
        server_fn = piePlotServer,
        defaults  = list("labels" = "product_line", "values" = "revenue")
    ),
    list(
        label     = "Radar",
        id        = "radar",
        inputs_ui = radarPlotInputsUI,
        output_ui = radarPlotOutputUI,
        server_fn = radarPlotServer,
        defaults  = list("theta" = "category", "r" = "value",
                         "group" = "player")
    ),
    list(
        label     = "Scatter",
        id        = "scatter",
        inputs_ui = dittoViz_scatterPlotInputsUI,
        output_ui = dittoViz_scatterPlotOutputUI,
        server_fn = dittoViz_scatterPlotServer,
        defaults  = list("x.by" = "revenue", "y.by" = "units",
                         "color.by" = "product_line")
    ),
    list(
        label     = "Split Bar",
        id        = "splitbar",
        inputs_ui = plotthis_SplitBarPlotInputsUI,
        output_ui = plotthis_SplitBarPlotOutputUI,
        server_fn = plotthis_SplitBarPlotServer,
        defaults  = list("x.data" = "Score", "y.data" = "Group")
    ),
    list(
        label     = "yPlot",
        id        = "yplot",
        inputs_ui = dittoViz_yPlotInputsUI,
        output_ui = dittoViz_yPlotOutputUI,
        server_fn = dittoViz_yPlotServer,
        defaults  = list("var" = "salary", "group.by" = "department")
    )
)

# Append the ComplexHeatmap module only when its Bioconductor dependencies are
# installed (see heatmap_available above).
if (heatmap_available) {
    module_registry <- c(module_registry, list(
        list(
            label     = "Heatmap",
            id        = "heatmap",
            inputs_ui = ComplexHeatmap_HeatmapInputsUI,
            output_ui = ComplexHeatmap_HeatmapOutputUI,
            server_fn = ComplexHeatmap_HeatmapServer,
            defaults  = list(
                "rowname.col" = "gene",
                "matrix.cols" = setdiff(names(example_heatmap_matrix), c("gene", "pathway", "mean_expression"))
            ),
            column_data = example_heatmap_column_data
        )
    ))
}


# Figure Builder tab – embeds the self-contained figureBuilder module, seeded
# with the same bundled datasets used elsewhere in the gallery.
figure_builder_tab <- tabPanel(
    "Figure Builder",
    value = "figure_builder",
    figureBuilderUI("figure_builder")
)

# Helper: build a tab panel for one module
build_tab <- function(mod) {
    tabPanel(
        mod$label,
        value = mod$id,
        sidebarLayout(
            sidebarPanel(
                width = 4,
                uiOutput(paste0(mod$id, "_inputs_ui"))
            ),
            mainPanel(
                width = 8,
                mod$output_ui(mod$id),
                hr(),
                h4("Data Table"),
                p("Filtering the data table will update the plot.",
                    style = "color: grey; font-size: 12px;"),
                dataFilterUI(paste0(mod$id, "_filter"))
            )
        )
    )
}

about_tab <- tabPanel(
    "About",
    value = "about",
    fluidPage(
        fluidRow(
            column(
                width = 9,
                h2("About VizModules"),
                p(pkg_desc$Title),
                p(pkg_desc$Description),
                tags$p(
                    tags$strong("Authors: "),
                    pkg_authors
                ),
                p(
                    "This gallery app showcases VizModules' interactive",
                    "Shiny modules using bundled example datasets so you can",
                    "preview each plot type and its configurable inputs. The",
                    tags$strong("Figure Builder"), "tab lets you compose",
                    "multiple modules into a free-form, multi-panel figure and",
                    "export it as a single editable SVG."
                ),
                tags$p(
                    tags$strong("Repository: "),
                    tags$a(
                        href = repo_url,
                        target = "_blank",
                        rel = "noopener noreferrer",
                        repo_url
                    )
                ),
                tags$p(
                    tags$strong("Documentation: "),
                    tags$a(
                        href = docs_url,
                        target = "_blank",
                        rel = "noopener noreferrer",
                        docs_url
                    )
                ),
                tags$p(
                    tags$strong("CRAN package page: "),
                    tags$a(
                        href = cran_url,
                        target = "_blank",
                        rel = "noopener noreferrer",
                        cran_url
                    )
                )
            )
        )
    )
)


ui <- do.call(navbarPage, c(
    list(
        title    = "VizModules Gallery",
        id       = "active_tab",
        position = "static-top",
        header   = tagList(
            shinyjs::useShinyjs(),
            tags$head(
                tags$style(HTML(
                    paste(
                        ".navbar { margin-bottom: 0; }",
                        # Keep all tabs on a single row by tightening link padding.
                        ".navbar-nav > li > a {",
                        "  padding-left: 9px;",
                        "  padding-right: 9px;",
                        "  font-size: 13px;",
                        "}",
                        ".navbar .navbar-collapse { flex-wrap: nowrap; }",
                        ".navbar-nav { white-space: nowrap; }",
                        ".navbar .navbar-right .navbar-version-label {",
                        "  color: #9d9d9d;",
                        "  display: block;",
                        "  padding: 15px 9px;",
                        "}",
                        ".navbar .navbar-right a.repo-link {",
                        "  display: flex;",
                        "  align-items: center;",
                        "  gap: 5px;",
                        "  padding-left: 9px;",
                        "  padding-right: 9px;",
                        "}",
                        sep = "\n"
                    )
                )),
                tags$script(HTML(
                    paste(
                        "(function() {",
                        "  function addNavbarItems() {",
                        "    var navContainer = document.querySelector('.navbar .navbar-collapse') ||",
                        "      document.querySelector('.navbar .container-fluid') ||",
                        "      document.querySelector('.navbar .container');",
                        "    var extras = document.getElementById('navbar-right-items');",
                        "    if (!navContainer || !extras || navContainer.querySelector('.navbar-right')) {",
                        "      return;",
                        "    }",
                        "    var extraNav = extras.querySelector('ul.navbar-right');",
                        "    if (extraNav) {",
                        "      navContainer.appendChild(extraNav.cloneNode(true));",
                        "    }",
                        "  }",
                        "  if (document.readyState === 'loading') {",
                        "    document.addEventListener('DOMContentLoaded', addNavbarItems);",
                        "  } else {",
                        "    addNavbarItems();",
                        "  }",
                        "})();",
                        sep = "\n"
                    )
                ))
            ),
            tags$div(
                id = "navbar-right-items",
                style = "display: none;",
                tags$ul(
                    class = "nav navbar-nav navbar-right",
                    tags$li(
                        tags$a(
                            class = "repo-link",
                            href = repo_url,
                            target = "_blank",
                            rel = "noopener noreferrer",
                            icon("github"),
                            "Repo"
                        )
                    ),
                    tags$li(
                        tags$a(
                            class = "repo-link",
                            href = docs_url,
                            target = "_blank",
                            rel = "noopener noreferrer",
                            "Docs"
                        )
                    ),
                    tags$li(
                        tags$span(
                            class = "navbar-version-label",
                            paste0("v", pkg_version)
                        )
                    )
                )
            )
        )
    ),
    list(about_tab),
    lapply(module_registry, build_tab),
    list(figure_builder_tab)
))


server <- function(input, output, session) {

    lapply(module_registry, function(m) {
        # Fixed dataset for this module (wrapped in reactive for dataFilterServer)
        active_data <- reactive(module_data[[m$id]])

        # Data filter feeds the plot module
        filtered_data <- dataFilterServer(
            paste0(m$id, "_filter"),
            active_data
        )

        # Modules that carry a `column_data` table (currently just the
        # ComplexHeatmap module) get their (still-filtered) data wrapped into
        # list(matrix = , column_annotations = ) so column annotations can be
        # demonstrated; every other module is unaffected.
        server_data <- if (!is.null(m$column_data)) {
            reactive(list(matrix = filtered_data(), column_annotations = m$column_data))
        } else {
            filtered_data
        }

        # Inputs UI rendered once on load with pre-selected data columns.
        # Only data-column defaults are passed; all other inputs use their
        # built-in defaults.
        output[[paste0(m$id, "_inputs_ui")]] <- renderUI({
            ui_data <- if (!is.null(m$column_data)) {
                list(matrix = active_data(), column_annotations = m$column_data)
            } else {
                active_data()
            }
            m$inputs_ui(
                m$id,
                ui_data,
                defaults = m$defaults,
                title    = h3(paste(m$label, "Settings"))
            )
        })

        m$server_fn(m$id, data = server_data)
    })

    # Figure Builder: uses its own bundled dataset catalogue and module
    # registry (including the pie-plot summary and per-dataset defaults),
    # so no data_list/module_registry override is needed here.
    figureBuilderServer("figure_builder")
}

shinyApp(ui, server)
