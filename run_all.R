# Run all manuscript sections ------------------------------------------------
# From the repository root: Rscript run_all.R path/to/extraction.xlsx
# In RStudio: source("run_all.R") (uses data/Supplementary Appendix 2.xlsx or file chooser).
args <- commandArgs(trailingOnly = TRUE)
input <- if (length(args)) args[1] else "data/Supplementary Appendix 2.xlsx"
if (!length(args) && !file.exists(input) && file.exists("data/Supplementary_Appendix_2.xlsx"))
  input <- "data/Supplementary_Appendix_2.xlsx"
if (!file.exists(input) && interactive()) input <- file.choose()
if (!file.exists(input)) stop("Provide the extraction workbook: Rscript run_all.R path/to/file.xlsx")
if (!file.exists("R/00_style.R")) stop("Run from the repository root.")
options(review.data_file = normalizePath(input, winslash = "/"),
        review.data_sheet = "Data Extraction")

required <- c("readxl", "dplyr", "tidyr", "stringr", "forcats", "ggplot2",
              "scales", "patchwork", "openxlsx", "countrycode", "sf",
              "rnaturalearth", "rnaturalearthdata")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Install required packages first: ", paste(missing, collapse = ", "))

section_envs <- list()
all_supplementary <- list()

scripts <- c("Section_3_1_Study_selection_overview.R", "Section_3_2_Biological_systems_experimental_design_and_Omics_Layers.R",
             "Section_3_3_Data_Characteristics_and_Preprocessing.R", "Sections_3_4_to_3_8_Integration_analyses.R",
             "Section_3_9_Software_and_Tools.R", "Section_3_10_Validation_Reproducibility_and_Quality_Indicators.R")
dir.create("results", showWarnings = FALSE)
for (script in scripts) {
  message("Running ", script)
  # Isolate similarly named objects and helper functions between sections.
  local_env <- new.env(parent = globalenv())
  if (script == "Section_3_3_Data_Characteristics_and_Preprocessing.R")
    local_env$figure3_section2 <- section_envs$section2
  sys.source(file.path("R", script), envir = local_env)
  if (exists("supplementary_tables", local_env, inherits = FALSE)) {
    incoming <- local_env$supplementary_tables
    if (any(names(incoming) %in% names(all_supplementary))) stop("Duplicate supplementary sheet name")
    all_supplementary <- c(all_supplementary, incoming)
  }
  if (script == "Section_3_2_Biological_systems_experimental_design_and_Omics_Layers.R") section_envs$section2 <- local_env
  if (script == "Section_3_3_Data_Characteristics_and_Preprocessing.R") {
    section_envs <- list()
  }
}
# One workbook only; numeric order S1–S16, without supporting HTML-only tables.
expected_sheets <- paste0("Supplementary Table S", 1:16)
if (!setequal(names(all_supplementary), expected_sheets))
  stop("Supplementary collection incomplete: ", paste(setdiff(expected_sheets, names(all_supplementary)), collapse = ", "))
workbook <- openxlsx::createWorkbook()
caption_style <- openxlsx::createStyle(textDecoration = "bold", wrapText = TRUE, valign = "center")
header_style <- openxlsx::createStyle(textDecoration = "bold", wrapText = TRUE, valign = "center")
for (sheet in expected_sheets) {
  entry <- all_supplementary[[sheet]]
  tab <- entry$table
  cols <- seq_along(tab)
  openxlsx::addWorksheet(workbook, sheet)
  openxlsx::writeData(workbook, sheet, entry$caption, startRow = 1, colNames = FALSE)
  if (length(cols) > 1) openxlsx::mergeCells(workbook, sheet, cols = cols, rows = 1)
  openxlsx::addStyle(workbook, sheet, caption_style, rows = 1, cols = 1)
  openxlsx::writeData(workbook, sheet, tab, startRow = 2, headerStyle = header_style)
  widths <- vapply(cols, function(i) {
    text <- c(names(tab)[i], as.character(tab[[i]]))
    max(15, min(65, max(nchar(text), na.rm = TRUE) + 2))
  }, numeric(1))
  openxlsx::setColWidths(workbook, sheet, cols, widths)
  openxlsx::setRowHeights(workbook, sheet, 1, max(32, 17 * ceiling(nchar(entry$caption) / sum(widths))))
}
openxlsx::saveWorkbook(workbook, "results/Supplementary_tables.xlsx", overwrite = TRUE)
capture.output(sessionInfo(), file = "results/sessionInfo.txt")
message("Completed: HTML and Supplementary_tables.xlsx in results/; PDF figures in figures/.")
