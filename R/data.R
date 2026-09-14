#' Bar dataset for bar and split bar plot examples
#'
#' A small dataset with five groups, two categorical variables, and three
#' numeric variables. Used as the default data for [plotthis_BarPlotApp()]
#' and [plotthis_SplitBarPlotApp()].
#'
#' @format A data frame with 5 rows and 5 columns:
#' \describe{
#'   \item{Group}{Group label (A through E)}
#'   \item{Type}{Category type (Alpha, Beta, or Gamma)}
#'   \item{Values}{Primary numeric values (positive)}
#'   \item{Numbers}{Secondary numeric values (can be negative)}
#'   \item{Score}{Tertiary numeric values (can be negative)}
#' }
#'
#' @source Generated in data-raw/generate_example_data.R.
#'
#' @author Jacob Martin
#' @keywords datasets
"example_bar"

#' Example school earnings dataset for dumbbell plots
#'
#' A small dataset of median annual earnings for men and women at six
#' universities, suitable for dumbbell plot examples.
#'
#' @format A data frame with 6 rows and 4 columns:
#' \describe{
#'   \item{School}{University name}
#'   \item{Women}{Median earnings for women (thousands of USD)}
#'   \item{Men}{Median earnings for men (thousands of USD)}
#'   \item{Group}{University type (STEM-heavy or Liberal Arts)}
#' }
#'
#' @source Generated in data-raw/generate_example_data.R.
#'
#' @author Jacob Martin
#' @keywords datasets
"example_school_earnings"

#' Example multi-player skills dataset for radar plots
#'
#' A dataset of skill ratings across five categories for three players,
#' suitable for radar/spider chart examples.
#'
#' @format A data frame with 15 rows and 3 columns:
#' \describe{
#'   \item{category}{Skill category (Speed, Strength, Defense, Stamina, Agility)}
#'   \item{value}{Skill rating (1-10)}
#'   \item{player}{Player identifier (Player A, B, or C)}
#' }
#'
#' @source Simulated in data-raw/generate_example_data.R.
#'
#' @author Jacob Martin
#' @keywords datasets
"example_skills"


#' Example sales dataset
#'
#' A simulated product-sales dataset (720 rows total).
#' Designed to showcase bar, box, area, line, scatter,
#' split-bar, density, and histogram plot modules.
#'
#' @format A data frame with 720 rows and 7 columns:
#' \describe{
#'   \item{region}{Region of the sale (factor: North, South, East, West, Central, International)}
#'   \item{revenue}{Revenue for month}
#'   \item{year}{The year}
#'   \item{month}{The month}
#'   \item{units}{Units sold}
#'   \item{sale_id}{Unique sale identifier}
#'   \item{product_line}{Product line (factor: Gadgets, Widgets, Doohickeys)}
#' }
#'
#' @source Generated in data-raw/generate_example_data.R.
#'
#' @author Jared Andrews
#' @keywords datasets
"example_sales"

#' Example demographics dataset
#'
#' A simulated employee survey dataset with 500 rows spanning six departments
#' and four job levels. Designed to showcase box, yPlot, density, and
#' histogram plot modules with realistic numeric distributions.
#'
#' @format A data frame with 500 rows and 9 columns:
#' \describe{
#'   \item{department}{Employee department (factor: Engineering, Marketing, Sales, HR, Finance, Operations)}
#'   \item{job_level}{Job seniority level (factor: Junior, Mid, Senior, Lead)}
#'   \item{gender}{Employee gender (factor: Male, Female)}
#'   \item{age}{Employee age in years}
#'   \item{salary}{Annual salary in USD}
#'   \item{satisfaction}{Job satisfaction score (1–10)}
#'   \item{performance}{Performance rating (1–10)}
#'   \item{tenure_years}{Years with the company}
#'   \item{weekly_hours}{Average weekly hours worked (35–65)}
#' }
#'
#' @source Simulated in data-raw/generate_example_data.R.
#'
#' @author Jared Andrews
#' @keywords datasets
"example_demographics"

#' Example grouped iris dataset
#'
#' The classic iris dataset with an added 'Group' column to facilitate multi-group plot examples.
#'
#' @format A data frame with 150 rows and 6 columns:
#' \describe{
#'   \item{Sepal.Length}{Sepal length in cm}
#'   \item{Sepal.Width}{Sepal width in cm}
#'   \item{Petal.Length}{Petal length in cm}
#'   \item{Petal.Width}{Petal width in cm}
#'   \item{Species}{Species of the iris (factor: setosa, versicolor, virginica)}
#'   \item{Group}{Group assignment (factor: A, B, C, D)}
#' }
#'
#' @source Generated from the classic iris dataset.
#'
#' @author Jared Andrews
#' @keywords datasets
"example_iris"

#' Example mtcars dataset with factors
#'
#' The classic mtcars dataset with key numeric columns converted to factors for categorical plotting examples.
#'
#' @format A data frame with 32 rows and 11 columns:
#' \describe{
#'   \item{mpg}{Miles per gallon}
#'   \item{cyl}{Number of cylinders (factor)}
#'   \item{disp}{Displacement (cubic inches)}
#'   \item{hp}{Gross horsepower}
#'   \item{drat}{Rear axle ratio}
#'   \item{wt}{Weight (1000 lbs)}
#'   \item{qsec}{1/4 mile time}
#'   \item{vs}{Engine (0 = V-shaped, 1 = straight) (factor)}
#'   \item{am}{Transmission (0 = automatic, 1 = manual) (factor)}
#'   \item{gear}{Number of forward gears (factor)}
#'   \item{carb}{Number of carburetors (factor)}
#' }
#' @source Generated from the classic mtcars dataset.
#'
#' @author Jared Andrews
#' @keywords datasets
"example_mtcars"

#' Example population dataset
#' A simulated population dataset with 400 rows covering 50 years and 8 age groups.
#' Designed for line, area, and stacked bar plot examples.
#' 
#' @format A data frame with 400 rows and 4 columns:
#' \describe{
#'   \item{year}{Year of the population record (factor: 1975–2024)}
#'   \item{age_group}{Age group category (factor: 0-9, 10-17, 18-34, 35-44, 45-54, 55-64, 65-74, 75+)}
#'   \item{count}{Population count for the given year and age group}
#'   \item{record_id}{Unique identifier for each population record}
#' }
#' 
#' @source Generated in data-raw/generate_example_data.R.
#' 
#' @author Jared Andrews
#' @keywords datasets
"example_population"

#' Example single-cell marker gene dataset for dot plots
#'
#' A simulated single-cell marker-gene expression dataset with 104 rows
#' covering eight immune cell types and thirteen canonical marker genes.
#' Each cell type strongly expresses its own marker genes (high average
#' expression and percent expressed) and weakly expresses the rest, making it
#' a realistic example for [plotthis_DotPlotApp()] where dot size encodes the
#' percent of cells expressing a gene and dot fill encodes average expression.
#'
#' @format A data frame with 104 rows and 4 columns:
#' \describe{
#'   \item{cell_type}{Immune cell type (factor: CD4 T, CD8 T, B, NK, Monocyte, Dendritic, Plasma, Platelet)}
#'   \item{gene}{Marker gene symbol (factor with 13 levels, e.g. CD3D, MS4A1, NKG7,
#'     LYZ, MZB1, PPBP)}
#'   \item{avg_expression}{Average expression of the gene in the cell type}
#'   \item{pct_expressed}{Percent of cells in the cell type expressing the gene}
#' }
#'
#' @source Simulated in data-raw/generate_example_data.R.
#'
#' @author Jacob Martin
#' @keywords datasets
"example_markers"

#' Example RNA-seq dataset for the RNA-seq showcase app
#'
#' A simulated pseudo-bulk RNA-seq dataset with 288 rows covering six immune
#' cell types, eight canonical marker genes, two conditions (Healthy / Disease),
#' and three biological replicates per condition. Marker genes are strongly
#' expressed in their canonical cell type; Disease replicates include a
#' simulated ~1.2 log2FC upregulation for marker genes, making biological
#' comparisons visually informative.
#'
#' The dataset is designed to simultaneously support three VizModules plot types:
#'
#' - DotPlot — summarised `avg_expression` and `pct_expressed`
#'   columns per cell type \eqn{\times} gene \eqn{\times} condition combination.
#' - yPlot — per-replicate `log2_cpm` values grouped by
#'   `cell_type` and coloured by `condition`.
#' - DensityPlot — per-replicate `log2_cpm` values grouped by
#'   `condition` and faceted by `cell_type`.
#'
#' @format A data frame with 288 rows and 7 columns:
#' \describe{
#'   \item{cell_type}{Immune cell type (factor: CD4 T, CD8 T, B Cell, NK Cell, Monocyte, pDC)}
#'   \item{gene}{Gene symbol (factor: CD3D, CD8A, MS4A1, NKG7, LYZ, LILRA4, CD14, GNLY)}
#'   \item{condition}{Experimental condition (factor: Healthy, Disease)}
#'   \item{replicate}{Biological replicate (factor: Rep1, Rep2, Rep3)}
#'   \item{log2_cpm}{Simulated log2 counts-per-million expression value}
#'   \item{avg_expression}{Mean log2_cpm across replicates for this cell_type \eqn{\times} gene \eqn{\times} condition}
#'   \item{neg_log10_pval}{Simulated \eqn{-\log_{10}(p)} value for differential expression summaries}
#' }
#'
#' @source Simulated in data-raw/generate_example_data.R.
#'
#' @author Jacob Martin
"example_rnaseq"

#' Example gene-expression-style matrix for the ComplexHeatmap module
#'
#' A tidy data frame shaped as observations (genes, rows) by samples
#' (columns) — the layout [ComplexHeatmap::Heatmap()] expects. 30 genes drawn
#' from three functional pathways (10 genes each), profiled across 12 samples
#' (6 "Healthy", 6 "Disease"). Values are simulated, *unscaled* log2-CPM-like
#' expression: Immune and Cell Cycle pathway genes are elevated in Disease
#' samples, Metabolic pathway genes are flat, so the module's row/column
#' scaling, clustering, splitting, and row-annotation controls all have real
#' signal to demonstrate on. Pairs with [example_heatmap_column_data] to
#' additionally demonstrate column annotations (see
#' [ComplexHeatmap_HeatmapApp()]'s `column_data` argument).
#'
#' @format A data frame with 30 rows and 15 columns:
#' \describe{
#'   \item{gene}{Gene symbol (character), used as row identifier}
#'   \item{pathway}{Functional pathway the gene belongs to (factor: Immune,
#'     Metabolic, Cell Cycle) — a categorical row-annotation column}
#'   \item{mean_expression}{Mean log2-CPM-like expression across the 12
#'     samples — a numeric row-annotation column}
#'   \item{Healthy_1, Healthy_2, Healthy_3, Healthy_4, Healthy_5,
#'     Healthy_6, Disease_1, Disease_2, Disease_3, Disease_4, Disease_5, 
#'     Disease_6}{Simulated
#'     log2-CPM-like expression values forming the heatmap matrix}
#' }
#'
#' @source Simulated in data-raw/generate_example_data.R.
#'
#' @author Jacob Martin
#' @keywords datasets
"example_heatmap_matrix"

#' Example sample-metadata table for the ComplexHeatmap module
#'
#' Per-sample metadata for the 12 samples in [example_heatmap_matrix],
#' keyed by `sample`. Supplying this alongside the matrix (as
#' `list(matrix = example_heatmap_matrix, column_annotations =
#' example_heatmap_column_data)`) enables column annotations in the
#' ComplexHeatmap module.
#'
#' @format A data frame with 12 rows and 4 columns:
#' \describe{
#'   \item{sample}{Sample identifier (factor), matching the sample column
#'     names in [example_heatmap_matrix]}
#'   \item{condition}{Experimental condition (factor: Healthy, Disease)}
#'   \item{batch}{Processing batch (factor: B1, B2), crossed with `condition`}
#'   \item{library_size}{Simulated sequencing library size (numeric)}
#' }
#'
#' @source Simulated in data-raw/generate_example_data.R.
#'
#' @author Jacob Martin
#' @keywords datasets
"example_heatmap_column_data"

#' Example single-cell-style composition data for the freqPlot module
#'
#' A per-cell record table from a simulated 12-donor immune profiling
#' experiment, shaped for [dittoViz::freqPlot()]. Each donor (`sample`)
#' contributes 150 cells and maps to exactly one `condition` and one `batch`,
#' which is the nesting `freqPlot()` requires to compare per-sample
#' cell-type frequencies across groups. `batch` is crossed with `condition`
#' (three donors each), so it works as a `color.by` without confounding the
#' comparison.
#'
#' Composition differs between the two conditions: the Disease donors show an
#' expanded monocyte compartment and depleted CD4 T cells relative to Healthy.
#'
#' @format A data frame with 1800 rows and 7 columns:
#' \describe{
#'   \item{cell_id}{Unique cell identifier (character)}
#'   \item{sample}{Donor identifier, `P01`-`P12` (factor); 150 cells each}
#'   \item{condition}{Disease state, `Healthy` or `Disease` (factor); six donors each}
#'   \item{batch}{Processing batch, `B1` or `B2` (factor); crossed with `condition`}
#'   \item{cell_type}{Annotated cell type (factor), the variable whose
#'     per-sample frequency `freqPlot()` tabulates}
#'   \item{n_genes}{Number of genes detected in the cell (integer)}
#'   \item{percent_mito}{Percentage of mitochondrial reads (numeric)}
#' }
#'
#' @source Simulated. Per-donor compositions are Dirichlet draws around
#'   condition-specific means, with cell counts drawn multinomially.
#'   See `data-raw/generate_example_data.R`.
#'
#' @author Jared Andrews
#' @keywords datasets
"example_composition"
