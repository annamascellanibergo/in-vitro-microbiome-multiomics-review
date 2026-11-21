# Manuscript section 3.2 Biological systems, experimental design, and Omics Layers
# Repository entry point: run from the repository root.
source("R/00_style.R", local = TRUE)
source("R/report_layout.R", local = TRUE)

# =============================================================================
# Scoping review: Section 2 — Biological systems, experimental design, and Omics Layers
# Plots are saved in figures; all tables are combined into ONE HTML report per section.
# =============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# ---- Settings ---------------------------------------------------------------
data_file  <- getOption("review.data_file", "data/Supplementary Appendix 2.xlsx")
data_sheet <- getOption("review.data_sheet", "Data Extraction")               # sheet with the extracted data
fig_dir    <- "figures"                       # folder for saved plots
report_dir <- "results"
expected_studies <- 102

dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(report_dir, showWarnings = FALSE, recursive = TRUE)
if (!file.exists(data_file) && interactive()) data_file <- file.choose()
if (!file.exists(data_file)) {
  stop("Extraction workbook not found. Set data_file to its full path.")
}

source("R/read_extraction.R", local = TRUE)
extraction <- read_extraction(data_file, data_sheet)
reviews <- extraction$data
col_info <- extraction$columns

# ---- Clean data -------------------------------------------------------------
# Same spelling for common entries in every text column
clean_text <- function(x) {
  x <- trimws(x)
  x[!is.na(x) & x == ""] <- NA_character_
  x[!is.na(x) & tolower(x) == "not reported"]   <- "Not reported"
  x[!is.na(x) & tolower(x) == "not applicable"] <- "Not applicable"
  x
}
# Preserve the coded treatment categories during whitespace and missing-value cleanup.
reviews <- reviews %>% mutate(across(where(is.character) &
                                       !any_of("Perturbations / Treatments macroclass"), clean_text))

# Entries that mean the same thing but were typed differently
reviews <- reviews %>%
  mutate(`Pooling of inoculum` =
           ifelse(grepl("^Not host-derived", `Pooling of inoculum`),
                  "Not host-derived", `Pooling of inoculum`))

n_studies <- nrow(reviews)
cat("Number of studies:", n_studies, "\n")
if (n_studies == 0) stop("No study rows found.")
if (n_studies != expected_studies) warning(sprintf(
  "Workbook contains %d studies; expected full corpus: %d. Percentages use %d, not %d.",
  n_studies, expected_studies, n_studies, expected_studies))
# col_info remains available in R for tracing retained columns to the original sheet.


# ---- Helper functions -------------------------------------------------------
# "n (%)" text
n_pct <- function(n, total) sprintf("%d (%.1f%%)", n, 100 * n / total)

# Table of n and % for a single-choice column, most frequent first.
# If levels are given, categories are shown in that order, including empty ones.
count_table <- function(data, column, label = column, levels = NULL) {
  values <- replace_na(as.character(data[[column]]), "Missing")
  if (is.null(levels)) {
    counts <- tibble(value = values) %>% count(value, sort = TRUE)
  } else {
    lv <- c(levels, setdiff(unique(values), levels))
    counts <- tibble(value = factor(values, levels = lv)) %>%
      count(value, .drop = FALSE) %>%
      mutate(value = as.character(value))
  }
  total <- sum(counts$n)
  out <- counts %>%
    bind_rows(tibble(value = "Total", n = total)) %>%
    mutate(`n (%)` = n_pct(n, total)) %>%
    select(value, `n (%)`)
  names(out)[1] <- label
  attr(out, "source_columns") <- column
  out
}

# Long-format comparisons: denominator is all studies WITHIN each row group.
# Missing/not-applicable categories are retained; no complete-case denominator.
within_group_table <- function(data, group, category) {
  out <- data %>% transmute(
    Group = replace_na(as.character(.data[[group]]), "Missing (empty cell)"),
    Category = replace_na(as.character(.data[[category]]), "Missing (empty cell)")) %>%
    count(Group, Category, name = "n") %>% group_by(Group) %>%
    complete(Category = sort(unique(replace_na(as.character(data[[category]]),
                                               "Missing (empty cell)"))), fill = list(n = 0L)) %>%
    mutate(`Group denominator` = sum(n), `Within-group %` = round(100 * n / sum(n), 1),
           `n (% within group)` = n_pct(n, sum(n))) %>% ungroup()
  names(out)[1:2] <- c(group, category)
  attr(out, "source_columns") <- c(group, category)
  out
}

# Console numbering is for navigation only; saved filenames identify manuscript sections.
table_no  <- 0
figure_no <- 0

# Each section has one HTML report; no individual CSV files are written.
# Reports open in any browser and can be copied into Word.
html_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x
}
current_report <- NULL
start_report <- function(filename, title) {
  current_report <<- file.path(report_dir, filename)
  table_no <<- 0
  cat(report_header(title, n_studies), file = current_report)
}
finish_report <- function() {
  cat(report_footer, file = current_report, append = TRUE)
}
# Source names refer to row 2 of the extraction workbook, not derived labels.
source_aliases <- c(Perturbation = "Experimental design", Time_series_label = "Experimental design")
source_registry <- list(
  S2_experimental_design = "Experimental design",
  S2_replication_strategy = "Replication Strategy",
  S2_perturbation_status = "Experimental design",
  S2_time_series_labels = "Experimental design"
)
excel_letter <- function(n) {
  out <- ""
  while (n > 0) { out <- paste0(LETTERS[(n - 1) %% 26 + 1], out); n <- (n - 1) %/% 26 }
  out
}
source_note <- function(sources) {
  if (!length(sources)) return("Source: user-supplied screening flow counts; not calculated from the extraction workbook.")
  descriptions <- vapply(unique(sources), function(column) {
    info <- col_info[col_info$col == column | col_info$name == column, ]
    if (!nrow(info)) return(column)
    paste(vapply(seq_len(nrow(info)), function(i) sprintf("%s: %s (Excel column %s)",
                                                          info$section[i], info$name[i], excel_letter(info$position[i])), character(1)), collapse = "; ")
  }, character(1))
  paste0("Source columns: ", paste(descriptions, collapse = "; "), ".")
}
render_table <- function(tab, title, filename = NULL) {
  sources <- attr(tab, "source_columns")
  if (!is.null(filename) && filename %in% names(source_registry)) sources <- source_registry[[filename]]
  if (title %in% names(source_registry)) sources <- source_registry[[title]]
  if (is.null(sources)) stop(paste("Source columns not defined for table:", title))
  sources <- vapply(sources, function(x) if (x %in% names(source_aliases)) source_aliases[[x]] else x, character(1))
  table_no <<- table_no + 1
  cat("\n===== Table ", table_no, ". ", title, " =====\n", sep = "")
  print(as.data.frame(tab), row.names = FALSE, right = FALSE)
  if (is.null(current_report)) stop("Start a section report before adding tables.")
  header <- paste0('<th>', html_escape(names(tab)), '</th>', collapse = '')
  rows <- if (nrow(tab)) vapply(seq_len(nrow(tab)), function(i) {
    cells <- vapply(tab, function(column) html_escape(column[i]), character(1))
    paste0('<tr>', paste0('<td>', cells, '</td>', collapse = ''), '</tr>')
  }, character(1)) else '<tr><td>No records</td></tr>'
  cat(paste0('<h2>', html_escape(title), '</h2><p class="source">',
             html_escape(source_note(sources)), '</p><div class="table"><table><thead><tr>',
             header, '</tr></thead><tbody>', paste(rows, collapse = ''),
             '</tbody></table></div>'), file = current_report, append = TRUE)
}

# Collect summary tables while calculating; render them in manuscript order below.
report_tables <- list()
show_table <- function(tab, title, filename = NULL) {
  if (title %in% names(report_tables)) stop("Duplicate report table: ", title)
  report_tables[[title]] <<- list(tab = tab, title = title, filename = filename)
}
render_named_table <- function(title) {
  item <- report_tables[[title]]
  if (is.null(item)) stop("Requested report table was not calculated: ", title)
  render_table(item$tab, item$title, item$filename)
}

# Base64 encoder uses base R; HTML reports need no companion image files.
encode_base64 <- function(bytes) {
  alphabet <- strsplit("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/", "", fixed = TRUE)[[1]]
  n <- length(bytes); padding <- (3L - n %% 3L) %% 3L
  v <- matrix(c(as.integer(bytes), rep(0L, padding)), nrow = 3L)
  indices <- rbind(bitwShiftR(v[1, ], 2L),
                   bitwOr(bitwShiftL(bitwAnd(v[1, ], 3L), 4L), bitwShiftR(v[2, ], 4L)),
                   bitwOr(bitwShiftL(bitwAnd(v[2, ], 15L), 2L), bitwShiftR(v[3, ], 6L)),
                   bitwAnd(v[3, ], 63L))
  result <- alphabet[as.vector(indices) + 1L]
  if (padding > 0L) result[(length(result) - padding + 1L):length(result)] <- "="
  paste0(result, collapse = "")
}
add_report_figure <- function(path, title, sources, placeholder = NULL) {
  bytes <- readBin(path, what = "raw", n = file.info(path)$size)
  fragment <- paste0('<h2>', html_escape(title), '</h2><p class="source">',
                     html_escape(source_note(sources)), '</p><img alt="', html_escape(title),
                     '" style="width:100%;height:auto" src="data:image/png;base64,',
                     encode_base64(bytes), '">')
  if (is.null(placeholder)) {
    cat(fragment, file = current_report, append = TRUE)
  } else {
    report <- paste(readLines(current_report, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    if (!grepl(placeholder, report, fixed = TRUE)) stop("Figure placeholder not found in report.")
    writeLines(sub(placeholder, fragment, report, fixed = TRUE), current_report, useBytes = TRUE)
  }
}

# Show a plot and save it with a white background.
# Only PDF files are exported; HTML previews are temporary.
save_plot <- function(plot, name, caption, width = 7, height = 4,
                      formats = "pdf") {
  figure_no <<- figure_no + 1
  cat("\n===== Figure ", figure_no, ". ", caption, " =====\n", sep = "")
  print(plot)
  if ("pdf" %in% formats)
    ggsave(file.path(fig_dir, paste0(name, ".pdf")),
           plot, width = width, height = height, bg = "white")
}

# Shared typography and restrained palette for publication.
theme_publication <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(text = element_text(color = pal("text")),
          axis.text = element_text(size = base_size, color = pal("text")),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = pal("grid"), linewidth = 0.25),
          panel.grid.major.y = element_blank(),
          strip.background = element_rect(fill = "#F3F6F7", color = NA),
          strip.text = element_text(face = "bold", size = base_size, hjust = 0),
          panel.spacing = grid::unit(9, "mm"),
          plot.title = element_text(face = "bold", size = base_size + 3, hjust = 0),
          plot.caption = element_text(size = base_size - 1, hjust = 0, color = pal("muted_text")),
          legend.title = element_text(size = base_size),
          legend.text = element_text(size = base_size - 1),
          plot.margin = margin(6, 9, 6, 6))
}
theme_set(theme_publication())


start_report("Section_3_2_Biological_systems_experimental_design_and_Omics_Layers.html",
             "3.2 Biological systems, experimental design, and Omics Layers")

# ---- Biological systems: single-study counts and within-model components -----
model_column <- "In vitro Model Type"
host_column <- "Host-derived Components"
biological_columns <- c(model_column, host_column, "Biological Source")
missing_biological_columns <- setdiff(biological_columns, names(reviews))
if (length(missing_biological_columns)) stop("Missing biological-system columns: ",
                                             paste(missing_biological_columns, collapse = ", "))
show_table(count_table(reviews, model_column, "Experimental model"), "Experimental models")
show_table(count_table(reviews, host_column, "Host-derived components"), "Host-derived components")
show_table(within_group_table(reviews, model_column, host_column),
           "Host-derived components within each experimental model")
show_table(count_table(reviews, "Biological Source", "Inoculum type"), "Inoculum types")

# Joint inoculum-type/site analysis is included here for manuscript Section 2.
# Other biological characteristics remain in Section 3; data structure in Section 4.
inoculum_site_columns <- c("Biological Source", "Sample Site")
missing_inoculum_columns <- setdiff(inoculum_site_columns, names(reviews))
if (length(missing_inoculum_columns)) stop("Missing inoculum/site columns: ",
                                           paste(missing_inoculum_columns, collapse = ", "))
inoculum_site_studies <- tibble(
  Study_row = seq_len(n_studies), Title = reviews$Title,
  `Inoculum type` = as.character(reviews[["Biological Source"]]),
  `Sampling site` = as.character(reviews[["Sample Site"]])) %>%
  mutate(across(c(`Inoculum type`, `Sampling site`),
                ~ ifelse(is.na(.x) | trimws(.x) == "", "Missing (empty cell)", trimws(.x))))
# Keep each charted category intact: splitting composite labels would invent
# source-site pairings not stated in the extraction workbook.
inoculum_totals <- inoculum_site_studies %>%
  count(`Inoculum type`, name = "Studies with inoculum type")
inoculum_site_table <- inoculum_site_studies %>%
  count(`Inoculum type`, `Sampling site`, name = "Studies") %>%
  left_join(inoculum_totals, by = "Inoculum type") %>%
  mutate(`All studies (%)` = round(100 * Studies / n_studies, 1),
         `Within inoculum type (%)` = round(100 * Studies / `Studies with inoculum type`, 1)) %>%
  arrange(desc(`Studies with inoculum type`), `Inoculum type`, desc(Studies), `Sampling site`)
stopifnot(sum(inoculum_site_table$Studies) == n_studies)
attr(inoculum_site_table, "source_columns") <- inoculum_site_columns
show_table(inoculum_site_table, "Sampling sites within each inoculum type")


# Treatments: cells are split on semicolons only; each resulting class keeps the
# coded wording and parenthetical details, with American English spelling
# and surrounding spaces trimmed.
# A class repeated within one study counts once for that study.
treatment_column <- "Perturbations / Treatments macroclass"
if (!treatment_column %in% names(reviews)) stop("Missing column: ", treatment_column)
show_treatment <- function(tab, title, sources) {
  attr(tab, "source_columns") <- sources
  show_table(tab, title)
}
treatment_entries <- reviews %>% transmute(study = row_number(),
                                           Entry = .data[[treatment_column]]) %>%
  mutate(Entry = ifelse(is.na(Entry) | trimws(Entry) == "", "Missing (empty cell)", Entry)) %>%
  separate_rows(Entry, sep = ";") %>% mutate(Entry = trimws(Entry)) %>%
  filter(Entry != "") %>% distinct(study, Entry)
treatment_counts <- treatment_entries %>% count(Entry, sort = TRUE) %>%
  mutate(`n (%)` = n_pct(n, n_studies))
show_treatment(treatment_counts %>% select(Entry, `n (%)`),
               "Perturbation and treatment macroclasses (multiple responses)", treatment_column)


show_table(count_table(reviews, "Experimental design", "Experimental design"),
           "Experimental designs", "S2_experimental_design")

# Infer ONLY perturbation status from recognized design categories.
# "Perturbation only" does not establish the number of sampling timepoints.
design <- reviews %>% mutate(
  Perturbation = case_when(
    grepl("perturbation", `Experimental design`, ignore.case = TRUE) ~ "Perturbation",
    tolower(`Experimental design`) == "time-series" ~ "No perturbation",
    TRUE ~ "Not established from design label"
  ),
  Time_series_label = case_when(
    grepl("time-series", `Experimental design`, ignore.case = TRUE) ~ "Time-series",
    is.na(`Experimental design`) ~ "Not reported",
    TRUE ~ "No time-series"
  )
)
show_table(count_table(design, "Perturbation", "Perturbation status",
                       c("Perturbation", "No perturbation", "Not established from design label")),
           "Perturbation status from design coding", "S2_perturbation_status")
show_table(count_table(design, "Time_series_label", "Time-series coding"),
           "Time-series design labels",
           "S2_time_series_labels")

# ---- Experimental design, donor handling, and replication --------------------
design_donor_columns <- c("Total Number of Donors / Subjects", "Pooling of inoculum", "Replication Strategy")
missing_design_donor_columns <- setdiff(design_donor_columns, names(reviews))
if (length(missing_design_donor_columns)) stop("Missing donor/design columns: ",
                                               paste(missing_design_donor_columns, collapse = ", "))
show_design_result <- function(tab, title, sources) {
  attr(tab, "source_columns") <- sources
  show_table(tab, title)
}
# Only exact non-negative integer entries are numeric donor counts.
# Explicit zero, unknown counts, and not-applicable entries remain distinct.
donor_column <- "Total Number of Donors / Subjects"
donor_raw <- reviews[[donor_column]]
donor_text <- trimws(donor_raw)
donor_integer <- !is.na(donor_text) & grepl("^[0-9]+([.]0+)?$", donor_text)
donor_value <- rep(NA_real_, n_studies)
donor_value[donor_integer] <- suppressWarnings(as.numeric(donor_text[donor_integer]))
donors <- design %>% mutate(
  study = row_number(), Donor_raw = donor_raw, Donors = donor_value,
  Donor_status = case_when(
    donor_integer & Donors > 0 ~ "Positive integer reported",
    donor_integer & Donors == 0 ~ "Zero explicitly recorded",
    is.na(Donor_raw) ~ "Missing (empty cell)",
    tolower(Donor_raw) == "not reported" ~ "Not reported",
    tolower(Donor_raw) == "not applicable" ~ "Not applicable",
    TRUE ~ "Non-integer entry: check coding"
  ),
  Donor_group = case_when(
    Donor_status == "Positive integer reported" & Donors == 1 ~ "1 donor",
    Donor_status == "Positive integer reported" & Donors <= 5 ~ "2–5 donors",
    Donor_status == "Positive integer reported" & Donors <= 10 ~ "6–10 donors",
    Donor_status == "Positive integer reported" ~ "More than 10 donors",
    TRUE ~ Donor_status
  )
)
donor_summary <- function(x, total) {
  values <- x[is.finite(x) & x > 0]
  nn <- length(values)
  tibble(`Studies with positive donor count` = nn,
         Median = if (nn) median(values) else NA_real_,
         Q1 = if (nn) as.numeric(quantile(values, 0.25, type = 7)) else NA_real_,
         Q3 = if (nn) as.numeric(quantile(values, 0.75, type = 7)) else NA_real_,
         Minimum = if (nn) min(values) else NA_real_,
         Maximum = if (nn) max(values) else NA_real_,
         `Studies with 1–5 donors` = sum(values <= 5),
         `1–5 donors (% of positive counts)` = if (nn) 100 * sum(values <= 5) / nn else NA_real_,
         `1–5 donors (% of group studies)` = if (total) 100 * sum(values <= 5) / total else NA_real_)
}

show_design_result(count_table(donors, "Donor_status", "Reporting status"),
                   "Donor-count reporting and applicability", donor_column)
show_design_result(count_table(donors, "Donor_group", "Donor category"),
                   "Donor-number categories", donor_column)
show_design_result(donor_summary(donors$Donors, n_studies),
                   "Donor numbers across included studies (positive integer counts only)", donor_column)
show_table(count_table(reviews, "Pooling of inoculum", "Inoculum handling"),
           "Inoculum handling as charted")
show_table(count_table(reviews, "Replication Strategy", "Replication strategy"),
           "Replication strategies as charted", "S2_replication_strategy")

# Preserve recorded categories and missingness; do not derive replication from pooling.
# Every row is an observed study-level combination; percentages within the first
# listed variable use ALL studies in that category, including missing entries.
design_links <- donors %>% mutate(across(
  all_of(c("Experimental design", "Pooling of inoculum", "Replication Strategy")),
  ~ ifelse(is.na(.x) | trimws(.x) == "", "Missing (empty cell)", trimws(.x))))
show_design_joint <- function(columns, title, sources) {
  tab <- design_links %>% group_by(across(all_of(columns))) %>%
    summarize(Studies = n(), .groups = "drop") %>%
    group_by(across(all_of(columns[1]))) %>%
    mutate(`Studies in first category` = sum(Studies),
           `Within first category (%)` = round(100 * Studies / sum(Studies), 1)) %>%
    ungroup() %>% mutate(`All studies (%)` = round(100 * Studies / n_studies, 1)) %>%
    arrange(desc(`Studies in first category`), .data[[columns[1]]], desc(Studies))
  stopifnot(sum(tab$Studies) == n_studies)
  show_design_result(tab, title, sources)
}
show_design_joint(c("Experimental design", "Donor_group"),
                  "Donor-number categories within each experimental design", c("Experimental design", donor_column))
show_design_joint(c("Experimental design", "Pooling of inoculum"),
                  "Inoculum handling within each experimental design", c("Experimental design", "Pooling of inoculum"))
show_design_joint(c("Time_series_label", "Pooling of inoculum"),
                  "Inoculum handling in time-series and no-time-series designs", c("Experimental design", "Pooling of inoculum"))
show_design_joint(c("Experimental design", "Replication Strategy"),
                  "Replication within each experimental design", c("Experimental design", "Replication Strategy"))
show_design_joint(c("Time_series_label", "Replication Strategy"),
                  "Replication in time-series and no-time-series designs", c("Experimental design", "Replication Strategy"))
show_design_joint(c("Pooling of inoculum", "Replication Strategy"),
                  "Replication within each inoculum-handling category", c("Pooling of inoculum", "Replication Strategy"))
show_design_joint(c("Experimental design", "Pooling of inoculum", "Replication Strategy"),
                  "Joint experimental design, inoculum handling, and replication", c("Experimental design", "Pooling of inoculum", "Replication Strategy"))

# Binary pooling summaries use only explicitly interpretable handling categories.
# Single-donor inocula are not pooled; their contribution is also shown separately.
design_links <- design_links %>% mutate(Pooling_comparison = case_when(
  grepl("not host-derived|non.host.derived|not applicable", `Pooling of inoculum`, ignore.case = TRUE) ~ "Not applicable / non-host-derived",
  grepl("not pooled|non.pooled|unpooled|individual donor|kept separate|single.donor", `Pooling of inoculum`, ignore.case = TRUE) ~ "Not pooled",
  grepl("pooled|pooling by group", `Pooling of inoculum`, ignore.case = TRUE) ~ "Pooled",
  TRUE ~ "Unknown / check handling category"
))
pooling_summary <- function(columns, title) {
  tab <- design_links %>% group_by(across(all_of(columns))) %>%
    group_modify(~ bind_cols(
      tibble(`All studies in group` = nrow(.x),
             `Single-donor inoculum studies` = sum(grepl("single.donor", .x$`Pooling of inoculum`, ignore.case = TRUE))),
      donor_summary(.x$Donors, nrow(.x)))) %>% ungroup()
  show_design_result(tab, title, c(donor_column, "Pooling of inoculum",
                                   if ("Time_series_label" %in% columns) "Experimental design" else character()))
}
pooling_summary("Pooling_comparison", "Donor medians for pooled and non-pooled inocula: overall")
pooling_summary(c("Time_series_label", "Pooling_comparison"),
                "Donor medians for pooled and non-pooled inocula within time-series and no-time-series designs")


# Donor distributions by the explicitly charted longitudinal/cross-sectional field.
longitudinal_column <- "Longitudinal vs Cross-sectional"
if (!longitudinal_column %in% names(design_links)) stop("Missing column: ", longitudinal_column)
show_table(count_table(reviews, longitudinal_column, "Longitudinal vs Cross-sectional"),
           "Longitudinal versus cross-sectional designs")
design_links <- design_links %>% mutate(
  Longitudinal_design = case_when(
    tolower(trimws(.data[[longitudinal_column]])) == "longitudinal" ~ "Longitudinal",
    tolower(trimws(.data[[longitudinal_column]])) %in% c("cross-sectional", "cross sectional") ~ "Cross-sectional",
    tolower(trimws(.data[[longitudinal_column]])) == "mixed" ~ "Mixed",
    TRUE ~ "Unknown / check design category"),
  Inoculum_plot_group = case_when(
    Pooling_comparison == "Pooled" ~ "Pooled",
    Pooling_comparison == "Not pooled" &
      grepl("single.donor", `Pooling of inoculum`, ignore.case = TRUE) & Donors > 1 ~ "Conflicting donor count / handling category",
    Pooling_comparison == "Not pooled" &
      (grepl("single.donor", `Pooling of inoculum`, ignore.case = TRUE) | Donors == 1) ~ "Single donor",
    Pooling_comparison == "Not pooled" & Donors > 1 ~ "Not pooled (multiple donors)",
    Pooling_comparison == "Not pooled" ~ "Not pooled: donor count unavailable",
    TRUE ~ Pooling_comparison))
show_design_result(design_links %>% count(.data[[longitudinal_column]], Longitudinal_design,
                                          `Pooling of inoculum`, Inoculum_plot_group, name = "Studies"),
                   "Charted design and inoculum categories used for the donor plot",
                   c(longitudinal_column, "Pooling of inoculum", donor_column))
plot_group_order <- c("Not pooled (multiple donors)", "Pooled", "Single donor")
plot_design_order <- c("Longitudinal", "Cross-sectional", "Mixed")
donor_plot_groups <- design_links %>% filter(
  Inoculum_plot_group %in% plot_group_order, Longitudinal_design %in% plot_design_order) %>%
  mutate(Inoculum_plot_group = factor(Inoculum_plot_group, levels = plot_group_order),
         Longitudinal_design = factor(Longitudinal_design, levels = plot_design_order))
longitudinal_donor_summary <- donor_plot_groups %>%
  group_by(Longitudinal_design, Inoculum_plot_group, .drop = FALSE) %>%
  group_modify(~ bind_cols(tibble(`All studies in group` = nrow(.x)),
                           donor_summary(.x$Donors, nrow(.x)))) %>% ungroup()
show_design_result(longitudinal_donor_summary,
                   "Donor medians by longitudinal/cross-sectional design and inoculum group (single donors separate)",
                   c(longitudinal_column, donor_column, "Pooling of inoculum"))

donor_plot_coverage <- donor_plot_groups %>%
  group_by(Longitudinal_design, Inoculum_plot_group, .drop = FALSE) %>%
  summarize(All_studies = n(), Usable_donor_counts = sum(Donor_status == "Positive integer reported"),
            Excluded_donor_counts = All_studies - Usable_donor_counts, .groups = "drop")
show_design_result(donor_plot_coverage, "Donor-distribution plot: usable and excluded counts by group",
                   c(donor_column, "Pooling of inoculum", longitudinal_column))
# Figure 3a: all studies share one horizontal coordinate across donor and metadata tracks.
# No study is dropped because its donor count, handling, or design is unavailable.
if (!requireNamespace("patchwork", quietly = TRUE))
  stop("Install patchwork for Figure 3a: install.packages('patchwork')")
handling_order <- c("Not pooled (multiple donors)", "Pooled", "Single donor",
                    "Not applicable / non-host-derived", "Not pooled: donor count unavailable",
                    "Unknown / check handling category", "Conflicting donor count / handling category")
handling_order <- c(handling_order, setdiff(unique(design_links$Inoculum_plot_group), handling_order))
design_order <- c("Longitudinal", "Cross-sectional", "Mixed", "Unknown / check design category")
design_order <- c(design_order, setdiff(unique(design_links$Longitudinal_design), design_order))
study_display <- design_links %>% mutate(
  Design_order = match(Longitudinal_design, design_order),
  Handling_order = match(Inoculum_plot_group, handling_order)) %>%
  arrange(Design_order, Handling_order, Donors, Title, study) %>%
  mutate(Display_order = row_number(),
         Design_block = match(Longitudinal_design, unique(Longitudinal_design)),
         Display_x = Display_order + 2 * (Design_block - 1L))
stopifnot(nrow(study_display) == n_studies, !anyDuplicated(study_display$study))
handling_colors <- c("Not pooled (multiple donors)" = pal("teal"), "Pooled" = pal("gold"),
                      "Single donor" = pal("purple"), "Not applicable / non-host-derived" = pal("not_reported"),
                      "Not pooled: donor count unavailable" = pal("light_teal"),
                      "Unknown / check handling category" = pal("not_reported"),
                      "Conflicting donor count / handling category" = pal("alert"))
handling_colors <- c(handling_colors,
                      setNames(rep(pal("not_reported"), length(setdiff(handling_order, names(handling_colors)))),
                               setdiff(handling_order, names(handling_colors))))
handling_labels <- c("Not pooled (multiple donors)" = "Separate donors", "Pooled" = "Pooled",
                     "Single donor" = "Single donor", "Not applicable / non-host-derived" = "Non-host-derived",
                     "Not pooled: donor count unavailable" = "Separate donors; count unavailable",
                     "Unknown / check handling category" = "Handling unreported / unclear",
                     "Conflicting donor count / handling category" = "Conflicting information")
replication_order <- c("Biological replicates only", "Both biological & technical",
                       "Technical replicates only", "Single sample, no replication", "Not reported", "Missing (empty cell)")
replication_order <- c(replication_order, setdiff(unique(study_display$`Replication Strategy`), replication_order))
replication_colors <- c("Biological replicates only" = pal("replication_blue"),
                         "Both biological & technical" = pal("replication_indigo"), "Technical replicates only" = pal("replication_cyan"),
                         "Single sample, no replication" = pal("replication_magenta"), "Not reported" = pal("not_reported"),
                         "Missing (empty cell)" = pal("not_reported"))
replication_colors <- c(replication_colors,
                         setNames(rep(pal("not_reported"), length(setdiff(replication_order, names(replication_colors)))),
                                  setdiff(replication_order, names(replication_colors))))
replication_labels <- c("Biological replicates only" = "Biological replicates only",
                        "Both biological & technical" = "Biological and technical replicates", "Technical replicates only" = "Technical replicates only",
                        "Single sample, no replication" = "No replication", "Not reported" = "Not reported",
                        "Missing (empty cell)" = "Empty cell")
label_from_key <- function(x, key) ifelse(x %in% names(key), unname(key[x]), x)
design_blocks <- study_display %>% group_by(Longitudinal_design, Design_block) %>%
  summarize(Left = min(Display_x) - 0.5, Right = max(Display_x) + 0.5,
            Centre = (Left + Right) / 2, Studies = n(), .groups = "drop") %>%
  mutate(Label = paste0(Longitudinal_design, "\n", sprintf("%.0f%%", 100 * Studies / n_studies)))
handling_blocks <- study_display %>% group_by(Longitudinal_design, Inoculum_plot_group) %>%
  summarize(Left = min(Display_x) - 0.5, Right = max(Display_x) + 0.5,
            Centre = (Left + Right) / 2, Studies = n(),
            Usable_counts = sum(Donor_status == "Positive integer reported"),
            Median = if (Usable_counts > 0) median(Donors[Donor_status == "Positive integer reported"]) else NA_real_,
            .groups = "drop")
show_design_result(handling_blocks %>% select(Longitudinal_design, Inoculum_plot_group,
                                              Studies, Usable_counts, Median), "Figure 3a: donor medians and study counts within each design/handling block",
                   c(longitudinal_column, "Pooling of inoculum", donor_column))
positive_display <- study_display %>% filter(Donor_status == "Positive integer reported")
missing_display <- study_display %>% filter(!Donor_status %in% c("Positive integer reported", "Zero explicitly recorded"))
zero_display <- study_display %>% filter(Donor_status == "Zero explicitly recorded")
donor_max <- if (nrow(positive_display)) max(positive_display$Donors) else 1
profile_span <- max(10, donor_max)
missing_y <- -0.14 * profile_span
shared_x_limits <- c(0.25, max(study_display$Display_x) + 0.75)
# Boundaries of each handling block remain aligned across all tracks.
block_boundaries <- handling_blocks$Right
# Dense panel: labels are at least 9 pt; data markers retain their original sizes.
p_study_donors <- ggplot() +
  geom_rect(data = design_blocks, aes(xmin = Left, xmax = Right,
                                      ymin = -0.21 * profile_span, ymax = 1.03 * profile_span), fill = "#F7F9FA", color = NA) +
  geom_vline(xintercept = block_boundaries, color = pal("grid"), linewidth = 0.2) +
  geom_segment(data = positive_display, aes(x = Display_x, xend = Display_x, y = 0, yend = Donors),
               color = "#BECBD1", linewidth = 0.25) +
  geom_point(data = positive_display, aes(Display_x, Donors, fill = Inoculum_plot_group),
             shape = 21, size = 1.1, color = "white", stroke = 0.1) +
  geom_point(data = missing_display, aes(Display_x, y = missing_y), shape = 4,
             size = 0.9, stroke = 0.35, color = pal("muted_text")) +
  geom_point(data = zero_display, aes(Display_x, Donors), shape = 0, size = 1) +
  geom_text(data = design_blocks, aes(Centre, y = 1.27 * profile_span, label = Label),
            fontface = "bold", size = 3.2, lineheight = 1.05) +
  geom_segment(data = handling_blocks %>% filter(!is.na(Median)),
               aes(x = Left + 0.15, xend = Right - 0.15, y = Median, yend = Median),
               color = pal("text"), linewidth = 0.45) +
  scale_fill_manual(values = handling_colors, guide = "none") +
  scale_x_continuous(limits = shared_x_limits, expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.22 * profile_span, 1.38 * profile_span),
                     breaks = unique(c(missing_y, 0, pretty(c(0, donor_max), n = 5))),
                     labels = function(x) ifelse(abs(x - missing_y) < 1e-8, "Unavailable", as.character(x)),
                     expand = c(0, 0)) +
  labs(x = NULL, y = "Number of donors") + theme_publication(9) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())
make_study_track <- function(column, title, colors, order, key) {
  observed <- order[order %in% unique(study_display[[column]])]
  ggplot(study_display, aes(Display_x, y = 1, fill = .data[[column]])) +
    geom_tile(width = 0.9, height = 0.34, color = "white", linewidth = 0.04) +
    geom_vline(xintercept = block_boundaries, color = pal("grid"), linewidth = 0.2) +
    scale_fill_manual(values = colors, breaks = observed,
                      labels = vapply(label_from_key(observed, key), function(x) paste(strwrap(x, 24), collapse = "\n"), character(1), USE.NAMES = FALSE), name = title) +
    scale_x_continuous(limits = shared_x_limits, expand = c(0, 0)) +
    scale_y_continuous(breaks = 1, labels = title, limits = c(0.5, 1.5), expand = c(0, 0)) +
    labs(x = NULL, y = NULL) + theme_publication(9) +
    guides(fill = guide_legend(ncol = 2, byrow = TRUE)) +
    theme(panel.grid = element_blank(), axis.text.x = element_blank(), axis.ticks = element_blank(),
          legend.position = "bottom", legend.title = element_blank(), legend.text = element_text(size = 9),
          legend.key.size = grid::unit(2.2, "mm"), plot.margin = margin(0, 4, 0, 4))
}
p_study_handling <- make_study_track("Inoculum_plot_group", "Inoculum handling",
                                     handling_colors, handling_order, handling_labels)
p_study_replication <- make_study_track("Replication Strategy", "Replication",
                                        replication_colors, replication_order, replication_labels)
figure_3a <- patchwork::wrap_plots(p_study_donors, p_study_handling, p_study_replication,
                                   ncol = 1, heights = c(5, 0.85, 0.85)) +
  patchwork::plot_annotation(title = "a",
                             subtitle = "Experimental design and donor structure",
                             theme = theme(plot.title = element_text(face = "bold", size = 11),
                                           plot.subtitle = element_text(face = "bold", size = 9, hjust = 0,
                                                                        margin = margin(b = 4)),
                                           plot.caption = element_text(size = 9, hjust = 0)))
# 102 mm panel width; paired panels plus 6 mm gutter form a 210 mm figure.
figure_3a_width  <- 102 / 25.4
figure_3a_height <- 160 / 25.4
save_plot(figure_3a, "Figure_3a_experimental_design_and_donors",
          "Figure 3a: aligned study-level design, donor counts, inoculum handling, and replication",
          width = figure_3a_width, height = figure_3a_height, formats = "pdf")
# The HTML report embeds a temporary PNG preview; it is not saved in the figures folder.
figure_3a_preview <- tempfile(fileext = ".png")
ggsave(figure_3a_preview, figure_3a, width = figure_3a_width, height = figure_3a_height,
       dpi = 300, bg = "white")


for (group_column in "Experimental design") {
  grouped_donors <- design_links %>% group_by(across(all_of(group_column))) %>%
    group_modify(~ bind_cols(tibble(`All studies in group` = nrow(.x)),
                             donor_summary(.x$Donors, nrow(.x)))) %>% ungroup()
  show_design_result(grouped_donors, paste("Donor-number summary by", group_column),
                     c(donor_column, if (group_column == "Time_series_label") "Experimental design" else group_column))
}


# ---- Assemble the HTML report in the requested order -------------------------
main_table_order <- c(
  "Experimental models",
  "Host-derived components",
  "Host-derived components within each experimental model",
  "Inoculum types",
  "Sampling sites within each inoculum type",
  "Perturbation status from design coding",
  "Perturbation and treatment macroclasses (multiple responses)",
  "Experimental designs",
  "Longitudinal versus cross-sectional designs",
  "Donor-number categories",
  "Donor-number categories within each experimental design",
  "Inoculum handling within each experimental design",
  "Inoculum handling in time-series and no-time-series designs",
  "Replication within each experimental design",
  "Replication in time-series and no-time-series designs",
  "Replication within each inoculum-handling category",
  "Donor medians for pooled and non-pooled inocula: overall",
  "Joint experimental design, inoculum handling, and replication")
figure_table_order <- c(
  "Figure 3a: donor medians and study counts within each design/handling block",
  "Donor medians by longitudinal/cross-sectional design and inoculum group (single donors separate)",
  "Donor-distribution plot: usable and excluded counts by group",
  "Charted design and inoculum categories used for the donor plot")
for (title in main_table_order) render_named_table(title)

add_report_figure(figure_3a_preview,
                  "Experimental design and donor structure (Figure 3a)",
                  c(donor_column, "Pooling of inoculum", longitudinal_column, "Replication Strategy"))
for (title in figure_table_order) render_named_table(title)

# Preserve all other summary tables after Figure 3a and its related tables.
remaining_tables <- setdiff(names(report_tables), c(main_table_order, figure_table_order))
if (length(remaining_tables)) {
  cat('<h2>Additional summary tables</h2>', file = current_report, append = TRUE)
  for (title in remaining_tables) render_named_table(title)
}
cat('<p>Treatment macroclass labels are retained exactly as recorded in the extraction file, including parenthetical details; no renaming or broad-category mapping is applied. Semicolons separate multiple responses. Macroclass percentages count each study once per class and may sum to more than 100%. Inoculum–site percentages use all studies or all studies within the recorded inoculum type, as labeled. Donor medians and quartiles use positive integer counts only (R quantile type 7). Not pooled includes separate-donor and single-donor inocula in the overall pooling comparison; single-donor counts are also shown separately. Missing, zero, not-reported and not-applicable counts remain visible in reporting tables. Replication uses the recorded classification. Cross-tabulations describe co-occurrence.</p>',
    file = current_report, append = TRUE)

# ---- Omics layers and analytical platforms ----------------------------------
# Continue the same report and shared data import. Omics tables render immediately.
show_table <- render_table
omics_source_columns <- c("Metagenomics", "Metatranscriptomics", "Metaproteomics",
                          "Metabolomics", "Other omics layer (specify)")
source_aliases <- c(source_aliases,
                    Combination = "Other omics layer (specify)",
                    Microbiome_approach = "Metagenomics",
                    SCFA_BCFA = "SCFA/BCFA panel (targeted adjunct)",
                    Exogenous_compound = "Targeted exogenous compound and its direct catabolites or other methods")
source_registry <- c(source_registry, list(
  S2_omics_presence = omics_source_columns,
  S2_omics_combinations = omics_source_columns,
  "Omics combinations: combination key" = omics_source_columns
))
cat('<h2>Omics Layers and analytical platforms</h2>', file = current_report, append = TRUE)

# Shared typography and restrained palette for publication.
theme_omics <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(text = element_text(color = pal("text")),
          axis.text = element_text(size = base_size, color = pal("text")),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = pal("grid"), linewidth = 0.25),
          panel.grid.major.y = element_blank(),
          strip.background = element_rect(fill = "#F3F6F7", color = NA),
          strip.text = element_text(face = "bold", size = base_size, hjust = 0),
          panel.spacing = grid::unit(9, "mm"),
          plot.title = element_text(face = "bold", size = base_size + 3, hjust = 0),
          plot.caption = element_text(size = base_size - 1, hjust = 0, color = pal("muted_text")),
          legend.title = element_text(size = base_size),
          legend.text = element_text(size = base_size - 1),
          plot.margin = margin(6, 9, 6, 6))
}
theme_set(theme_omics())


# Omics presence is derived conservatively: unknown entries stay unknown.
presence <- function(x, specified_layer = FALSE) {
  z <- tolower(trimws(x))
  case_when(
    is.na(z) | z == "" | z %in% c("not reported", "unknown", "unclear") ~ "Not reported",
    grepl("^no($|[ -])", z) | z %in% c("none", "not applicable", "absent") ~ "No",
    grepl("^yes", z) | z == "present" ~ "Yes",
    specified_layer ~ "Yes",
    TRUE ~ "Unclear / check coding"
  )
}
omics_columns <- c("Metagenomics", "Metatranscriptomics", "Metaproteomics", "Metabolomics")
required <- c(omics_columns, "Other omics layer (specify)",
              "SCFA/BCFA panel (targeted adjunct)",
              "Targeted exogenous compound and its direct catabolites or other methods")
missing_columns <- setdiff(required, names(reviews))
if (length(missing_columns)) stop(paste("Missing retained columns:", paste(missing_columns, collapse = ", ")))

layer_labels <- c("Metagenomics", "Metatranscriptomics", "Metaproteomics",
                  "Metabolomics", "Other omics layer (specify)")
omics_status <- bind_rows(lapply(seq_along(omics_columns), function(i) {
  tibble(study = seq_len(n_studies), Layer = layer_labels[i],
         Status = presence(reviews[[omics_columns[i]]]))
}), tibble(study = seq_len(n_studies), Layer = "Other omics layer (specify)",
           Status = presence(reviews[["Other omics layer (specify)"]], TRUE)))
layer_status_summary <- omics_status %>%
  filter(Status != "Unclear / check coding") %>%
  count(Layer, Status) %>%
  complete(Layer = layer_labels, Status = c("Yes", "No", "Not reported"),
           fill = list(n = 0)) %>%
  mutate(`n (%)` = n_pct(n, n_studies)) %>% select(Layer, Status, `n (%)`)
attr(layer_status_summary, "source_columns") <- omics_source_columns
show_table(layer_status_summary, "Summary of omics-layer presence")

# Report only observed original entries in each retained omics column.
layer_summary <- bind_rows(lapply(omics_source_columns, function(column) {
  tibble(Layer = column, Entry = replace_na(as.character(reviews[[column]]), "Missing (empty cell)")) %>%
    count(Layer, Entry, sort = TRUE)
})) %>% mutate(`n (%)` = n_pct(n, n_studies)) %>% select(Layer, Entry, `n (%)`)
show_table(layer_summary, "Omics layer presence and reporting", "S2_omics_presence")
# Omics-combination plot is generated below, then inserted here.
cat("<!--OMICS_COMBINATIONS-->", file = current_report, append = TRUE)
adjuncts <- reviews %>% transmute(
  SCFA_BCFA = presence(`SCFA/BCFA panel (targeted adjunct)`),
  Exogenous_compound = presence(`Targeted exogenous compound and its direct catabolites or other methods`, TRUE)
)
show_table(count_table(reviews, "SCFA/BCFA panel (targeted adjunct)", "SCFA/BCFA panel reported"),
           "SCFA/BCFA measurements", "S2_SCFA_BCFA_adjunct")
cat('<p>SCFA/BCFA panels are targeted adjunct measurements and are counted separately from the omics combinations.</p>',
    file = current_report, append = TRUE)
show_table(count_table(adjuncts, "Exogenous_compound", "Targeted exogenous compound measurements"),
           "Targeted exogenous compound measurements (separate from omics layers)",
           "S2_exogenous_compound_adjunct")
show_table(count_table(reviews, "Targeted exogenous compound and its direct catabolites or other methods"),
           "Targeted exogenous compound descriptions", "S2_exogenous_compound_descriptions")

# Profile approach within the column called Metagenomics.
classify_microbiome <- function(x) {
  z <- tolower(trimws(x))
  has_shotgun <- grepl("shotgun|whole.?genome|\\bwgs\\b", z)
  has_marker <- grepl("16s|18s|\\bits\\b|amplicon", z)
  case_when(
    presence(x) == "No" ~ "No metagenomics",
    presence(x) %in% c("Not reported", "Unclear / check coding") ~ "Not reported / unclear",
    has_shotgun & has_marker ~ "Shotgun + 16S",
    grepl("microarray", z) ~ "Marker-gene microarray",
    has_shotgun ~ "Shotgun",
    has_marker & grepl("16s", z) & grepl("its", z) ~ "16S + ITS",
    has_marker & grepl("16s", z) ~ "16S rRNA",
    has_marker ~ "Other marker-gene sequencing",
    TRUE ~ "Approach not specified"
  )
}
omics_wide <- omics_status %>% pivot_wider(names_from = Layer, values_from = Status) %>%
  arrange(study)
omics_wide$Microbiome_approach <- classify_microbiome(reviews$Metagenomics)
omics_wide$Combination_known <- apply(as.data.frame(omics_wide[layer_labels]), 1,
                                      function(x) all(x %in% c("Yes", "No")))
omics_wide$Combination <- apply(as.data.frame(omics_wide[layer_labels]), 1, function(x) {
  if (!all(x %in% c("Yes", "No"))) return("Unclear / incomplete layer reporting")
  included <- layer_labels[x == "Yes"]
  if (length(included) == 0) "No profiled layer recorded" else paste(included, collapse = " + ")
})
show_table(count_table(omics_wide, "Combination", "Omics combination"),
           "Omics combinations", "S2_omics_combinations")
show_table(count_table(omics_wide, "Microbiome_approach", "Microbiome profiling approach"),
           "Microbiome profiling approaches", "S2_microbiome_approaches")


# ---- Sequencing instruments in relation to sequencing platform ---------------
sequencing_sources <- c("Sequencing Platform", "Instrument Method")
sequencing_original <- reviews %>% transmute(
  study = seq_len(n_studies),
  Platform = replace_na(as.character(`Sequencing Platform`), "Not reported"),
  Instrument = replace_na(as.character(`Instrument Method`), "Not reported"))

split_recorded <- function(x, pattern = "[,;]") {
  parts <- trimws(unlist(strsplit(x, pattern, perl = TRUE)))
  parts <- parts[nzchar(parts)]
  if (!length(parts)) "Not reported" else parts
}
sequencing_pairs <- bind_rows(lapply(seq_len(nrow(sequencing_original)), function(i) {
  row <- sequencing_original[i, ]
  platforms <- split_recorded(row$Platform)
  instruments <- split_recorded(row$Instrument)
  if (length(unique(tolower(platforms))) == 1L) {
    instruments <- unique(unlist(lapply(instruments, split_recorded, pattern = "/")))
    return(tibble(study = row$study, Platform = platforms[1], Instrument = instruments,
                  Pairing = "Single recorded platform"))
  }
  if (length(platforms) == length(instruments)) {
    return(bind_rows(lapply(seq_along(platforms), function(j) {
      tibble(study = row$study, Platform = platforms[j],
             Instrument = split_recorded(instruments[j], pattern = "/"),
             Pairing = "Aligned entry order")
    })))
  }
  tibble(study = row$study, Platform = unique(platforms),
         Instrument = if (all(tolower(instruments) == "not reported")) "Not reported"
         else "Multiple entries: pairing unresolved",
         Pairing = "Unresolved; see original entries")
})) %>%
  mutate(platform_key = tolower(Platform), instrument_key = tolower(Instrument)) %>%
  group_by(platform_key) %>% mutate(Platform = first(Platform)) %>% ungroup() %>%
  group_by(instrument_key) %>% mutate(Instrument = first(Instrument)) %>% ungroup() %>%
  distinct(study, Platform, Instrument, .keep_all = TRUE)
platform_denominators <- sequencing_pairs %>% distinct(study, Platform) %>%
  count(Platform, name = "Platform studies")
sequencing_instruments <- sequencing_pairs %>% distinct(study, Platform, Instrument) %>%
  count(Platform, Instrument, name = "Studies") %>%
  left_join(platform_denominators, by = "Platform") %>%
  mutate(`All studies: n (%)` = n_pct(Studies, n_studies),
         `Within platform: n (%)` = n_pct(Studies, `Platform studies`)) %>%
  arrange(Platform, desc(Studies), Instrument)
attr(sequencing_instruments, "source_columns") <- sequencing_sources

sequencing_by_type <- sequencing_pairs %>% mutate(
  `Profiling approach` = omics_wide$Microbiome_approach[study],
  `Recorded profiling entry` = reviews$Metagenomics[study])
approach_denominators <- omics_wide %>% count(Microbiome_approach,
                                              name = "Approach studies") %>%
  rename(`Profiling approach` = Microbiome_approach)
platforms_by_type <- sequencing_by_type %>% distinct(study, `Profiling approach`, Platform) %>%
  count(`Profiling approach`, Platform, name = "Studies") %>%
  left_join(approach_denominators, by = "Profiling approach") %>%
  mutate(`All studies: n (%)` = n_pct(Studies, n_studies),
         `Within approach: n (%)` = n_pct(Studies, `Approach studies`)) %>%
  arrange(`Profiling approach`, desc(Studies), Platform)
attr(platforms_by_type, "source_columns") <- c("Metagenomics", "Sequencing Platform")
show_table(platforms_by_type, "Sequencing platforms within each microbiome profiling approach")

# Metabolomics targeting approach; untargeted is not matched as targeted.
metabolomics_mode <- reviews %>% mutate(Metabolomics_mode = case_when(
  presence(Metabolomics) == "No" ~ "No metabolomics",
  presence(Metabolomics) %in% c("Not reported", "Unclear / check coding") ~ "Not reported / unclear",
  grepl("both", Metabolomics, ignore.case = TRUE) ~ "Targeted and untargeted",
  grepl("untargeted", Metabolomics, ignore.case = TRUE) &
    grepl("(^|[^a-z])targeted([^a-z]|$)", Metabolomics, ignore.case = TRUE) ~ "Targeted and untargeted",
  grepl("untargeted", Metabolomics, ignore.case = TRUE) ~ "Untargeted",
  grepl("semi.?targeted", Metabolomics, ignore.case = TRUE) ~ "Semi-targeted",
  grepl("(^|[^a-z])targeted([^a-z]|$)", Metabolomics, ignore.case = TRUE) ~ "Targeted",
  TRUE ~ "Approach not specified"))
mode_table <- count_table(metabolomics_mode, "Metabolomics_mode", "Metabolomics approach")
attr(mode_table, "source_columns") <- "Metabolomics"
show_table(mode_table, "Targeted and untargeted metabolomics approaches")
cat('<p>Targeted metabolomics refers to the analysis of a predefined set of metabolites. Classification follows the recorded extraction entry. SCFA/BCFA panels and other restricted chemical measurements remain separate adjuncts.</p>',
    file = current_report, append = TRUE)

# Explicit method components, deduplicated within studies. These totals overlap.
component_patterns <- list(
  Metagenomics = c("16S rRNA gene sequencing / profiling" = "16s", "Shotgun metagenomics" = "shotgun|whole.?genome|\\bwgs\\b",
                   "ITS profiling" = "\\bits\\b", "Microarray profiling" = "microarray"),
  `Metabolomics analytical platform` = c("LC–MS" = "lc[ -–]?ms", "GC–MS" = "gc[ -–]?ms",
                                         "NMR" = "\\bnmr\\b", "LA-REIMS" = "la[ -–]?reims"),
  `Metaproteomics analytical platform` = c("LC–MS" = "lc[ -–]?ms", "GC–MS" = "gc[ -–]?ms",
                                           "NMR" = "\\bnmr\\b"))
component_counts <- bind_rows(lapply(names(component_patterns), function(field) {
  patterns <- component_patterns[[field]]
  bind_rows(lapply(names(patterns), function(method) {
    x <- as.character(reviews[[field]])
    detected <- !is.na(x) & grepl(patterns[[method]], x, ignore.case = TRUE, perl = TRUE)
    tibble(Field = field, Method = method, Studies = sum(detected),
           `Studies (%)` = round(100 * sum(detected) / n_studies, 1))
  }))
}))
attr(component_counts, "source_columns") <- names(component_patterns)
show_table(component_counts, "Explicit profiling-method and analytical-platform components (multiple responses)")
cat('<p>Component totals count explicitly named methods within combined entries, once per study per method. A study reporting LC–MS plus GC–MS contributes to both component totals. Microarray profiling can overlap sequencing approaches. These totals describe recorded components and do not replace the original-entry tables.</p>',
    file = current_report, append = TRUE)

# UpSet-style Omics combinations: intersections stacked by microbiome profiling approach.
known <- omics_wide %>% filter(Combination_known)
if (nrow(known) > 0) {
  combo_order <- known %>% count(Combination, sort = TRUE) %>%
    arrange(desc(n), Combination) %>% mutate(intersection = row_number())
  intersections <- known %>% count(Combination, Microbiome_approach) %>%
    left_join(combo_order %>% select(Combination, intersection), by = "Combination")
  totals <- combo_order %>% select(intersection, n)
  approach_colors <- c("16S rRNA" = pal("omics_orange"), "Shotgun" = pal("omics_peach"),
                        "Shotgun + 16S" = pal("omics_brown"), "16S + ITS" = pal("omics_rose"),
                        "No metagenomics" = pal("not_applicable"), "Marker-gene microarray" = pal("omics_dark"),
                        "Other marker-gene sequencing" = pal("omics_sand"), "Approach not specified" = pal("muted_text"),
                        "Not reported / unclear" = pal("not_reported"))
  approach_labels <- c(
    "16S rRNA" = "16S rRNA gene profiling",
    "Marker-gene microarray" = "16S rRNA marker-gene microarray",
    "Shotgun" = "Shotgun metagenomic sequencing",
    "Shotgun + 16S" = "Shotgun metagenomic sequencing and marker-gene profiling",
    "16S + ITS" = "16S rRNA gene and ITS profiling",
    "No metagenomics" = "No microbial community gene profiling")
  intersections$Microbiome_approach <- factor(intersections$Microbiome_approach,
                                              levels = names(approach_colors))
  plot_layers <- layer_labels
  if (!any(omics_status$Layer == "Other omics layer (specify)" & omics_status$Status == "Yes")) {
    plot_layers <- setdiff(plot_layers, "Other omics layer (specify)")
  }
  matrix_data <- known %>% distinct(Combination, across(all_of(layer_labels))) %>%
    left_join(combo_order %>% select(Combination, intersection), by = "Combination") %>%
    pivot_longer(all_of(plot_layers), names_to = "Layer", values_to = "Status") %>%
    mutate(Layer = factor(Layer, levels = rev(plot_layers)))
  segments <- matrix_data %>% filter(Status == "Yes") %>%
    mutate(y = as.integer(Layer)) %>% group_by(intersection) %>%
    summarize(ymin = min(y), ymax = max(y), .groups = "drop")
  set_totals <- omics_status %>% filter(Status == "Yes", Layer %in% plot_layers) %>% count(Layer) %>%
    complete(Layer = plot_layers, fill = list(n = 0)) %>%
    mutate(Layer = factor(Layer, levels = rev(plot_layers)))
  # Vertical layout sized for half an A4 page (105 mm wide):
  # rows = combinations (most frequent on top), dot matrix left, stacked bars right.
  n_int <- nrow(combo_order)
  fig_width_in  <- 102 / 25.4
  fig_height_in <- (69 + 9 * n_int + 5 * n_distinct(intersections$Microbiome_approach)) / 25.4
  set_n <- setNames(set_totals$n, as.character(set_totals$Layer))
  layer_axis <- setNames(paste0(sub(" \\(specify\\)$", "", plot_layers), " (", sprintf("%.0f%%", 100 * set_n[plot_layers] / n_studies), ")"),
                         plot_layers)
  matrix_v <- matrix_data %>%
    mutate(Layer = factor(as.character(Layer), levels = plot_layers),
           ypos = n_int + 1 - intersection)
  segments_v <- matrix_v %>% filter(Status == "Yes") %>%
    mutate(x = as.integer(Layer)) %>% group_by(intersection, ypos) %>%
    summarize(xmin = min(x), xmax = max(x), .groups = "drop")
  stripes_v <- tibble(Layer = factor(plot_layers[seq_along(plot_layers) %% 2 == 1],
                                     levels = plot_layers))
  p_matrix <- ggplot(matrix_v, aes(Layer, ypos)) +
    geom_tile(data = stripes_v, aes(x = Layer, y = (n_int + 1) / 2), inherit.aes = FALSE,
              width = 1, height = n_int, fill = "#F3F4F5") +
    geom_segment(data = segments_v, aes(x = xmin, xend = xmax, y = ypos, yend = ypos),
                 inherit.aes = FALSE, linewidth = 0.6, color = pal("omics_brown")) +
    geom_point(aes(color = Status), size = 2.2) +
    scale_color_manual(values = c(Yes = pal("omics_brown"), No = pal("not_applicable")), guide = "none") +
    scale_x_discrete(labels = layer_axis, position = "top", drop = FALSE) +
    scale_y_continuous(limits = c(0.5, n_int + 0.5), expand = c(0, 0)) +
    labs(x = NULL, y = NULL) + theme_void(base_size = 9) +
    theme(axis.text.x.top = element_text(angle = 90, hjust = 0, vjust = 0.5, size = 9,
                                         margin = margin(b = 2)),
          plot.margin = margin(0, 2, 0, 2))
  intersections_v <- intersections %>%
    mutate(ypos = n_int + 1 - intersection, pct = 100 * n / n_studies)
  totals_v <- totals %>%
    mutate(ypos = n_int + 1 - intersection, pct = 100 * n / n_studies,
           label = sprintf("%.0f%%", pct))
  p_bars <- ggplot(intersections_v, aes(pct, ypos, fill = Microbiome_approach)) +
    geom_col(orientation = "y", width = 0.56) +
    geom_text(data = totals_v, aes(pct, ypos, label = label), inherit.aes = FALSE,
              hjust = -0.15, size = 3.2, fontface = "bold") +
    scale_fill_manual(values = approach_colors, drop = TRUE,
                      labels = function(x) vapply(label_from_key(x, approach_labels),
                        function(label) paste(strwrap(label, width = 43), collapse = "\n"),
                        character(1), USE.NAMES = FALSE)) +
    scale_y_continuous(limits = c(0.5, n_int + 0.5), expand = c(0, 0)) +
    scale_x_continuous(position = "top", expand = expansion(mult = c(0, 0.28)),
                       labels = function(x) sprintf("%.0f%%", x)) +
    labs(x = paste0("Studies (% of ", n_studies, ")"), y = NULL, fill = "Microbiome profiling approach") +
    theme_classic(base_size = 9) +
    theme(axis.text.x = element_text(size = 9), axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.line.y = element_blank(), legend.position = "bottom",
          legend.title = element_text(size = 9), legend.text = element_text(size = 9),
          legend.key.size = grid::unit(0.28, "cm"),
          plot.margin = margin(0, 4, 0, 2)) +
    guides(fill = guide_legend(ncol = 1, title.position = "top"))
  g_with_legend <- ggplotGrob(p_bars)
  leg_idx <- grep("^guide-box", g_with_legend$layout$name)
  leg_idx <- leg_idx[!vapply(g_with_legend$grobs[leg_idx], inherits, logical(1), "zeroGrob")][1]
  legend_grob <- g_with_legend$grobs[[leg_idx]]
  g_matrix <- ggplotGrob(p_matrix)
  g_bars <- ggplotGrob(p_bars + theme(legend.position = "none"))
  shared_heights <- grid::unit.pmax(g_matrix$heights, g_bars$heights)
  g_matrix$heights <- shared_heights; g_bars$heights <- shared_heights
  draw_upset <- function(newpage = TRUE) {
    if (newpage) grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid::grid.layout(3, 2,
                                                                 widths = grid::unit(c(0.42, 0.58), "null"),
                                                                 heights = grid::unit.c(grid::unit(9, "mm"), grid::unit(1, "null"),
                                                                                        grid::grobHeight(legend_grob) + grid::unit(3, "mm")))))
    grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 1:2))
    grid::grid.text("b  Combinations of omics layers", x = 0.02, hjust = 0,
                    gp = grid::gpar(fontsize = 10, fontface = "bold"))
    grid::popViewport()
    grid::pushViewport(grid::viewport(layout.pos.row = 2, layout.pos.col = 1)); grid::grid.draw(g_matrix); grid::popViewport()
    grid::pushViewport(grid::viewport(layout.pos.row = 2, layout.pos.col = 2)); grid::grid.draw(g_bars); grid::popViewport()
    grid::pushViewport(grid::viewport(layout.pos.row = 3, layout.pos.col = 1:2)); grid::grid.draw(legend_grob); grid::popViewport()
    grid::popViewport()
  }
  # PNG is only a temporary file for embedding in the HTML report; only the PDF is saved.
  report_png <- tempfile(fileext = ".png")
  grDevices::png(report_png, width = fig_width_in,
                 height = fig_height_in, units = "in", res = 600, bg = "white")
  draw_upset(); grDevices::dev.off()
  grDevices::pdf(file.path(fig_dir, "Figure_3b_omics_combinations.pdf"), width = fig_width_in,
                 height = fig_height_in, bg = "white")
  draw_upset(); grDevices::dev.off()
  if (interactive()) draw_upset()
  add_report_figure(report_png,
                    "Figure 3b. Combinations of omics layers profiled per study", omics_source_columns,
                    placeholder = "<!--OMICS_COMBINATIONS-->")
  unlink(report_png)
  show_table(combo_order, "Omics combinations: combination key")
}

if (nrow(known) == 0) {
  report <- paste(readLines(current_report, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  writeLines(sub("<!--OMICS_COMBINATIONS-->", "<p>Omics-combination plot cannot be drawn: no complete omics combinations were recorded.</p>",
                 report, fixed = TRUE), current_report, useBytes = TRUE)
}

# ---- Publication supplementary tables S1-S5 ---------------------------------
# Each table has a separate caption and footnote within the existing HTML report.
supplementary_tables <- list()
show_supplementary_table <- function(tab, caption, note) {
  caption <- supplementary_caption(caption)
  sheet <- sub("\\..*$", "", caption)
  supplementary_tables[[sheet]] <<- list(table = as.data.frame(tab), caption = caption)
  stopifnot(!is.null(current_report), !is.null(attr(tab, "source_columns")))
  cat("\n", caption, "\n", sep = "")
  print(as.data.frame(tab), row.names = FALSE, right = FALSE)
  header <- paste0("<th>", html_escape(names(tab)), "</th>", collapse = "")
  rows <- if (nrow(tab)) vapply(seq_len(nrow(tab)), function(i) {
    cells <- vapply(tab, function(column) html_escape(column[i]), character(1))
    paste0("<tr>", paste0("<td>", cells, "</td>", collapse = ""), "</tr>")
  }, character(1)) else paste0('<tr><td colspan="', ncol(tab), '">No records</td></tr>')
  cat(paste0('<div class="table"><table><caption style="text-align:left;',
             'font-weight:bold;padding:12px 0;">', html_escape(caption),
             '</caption><thead><tr>', header, '</tr></thead><tbody>',
             paste(rows, collapse = ''), '</tbody></table></div><p>', html_escape(note),
             '</p><p class="source">', html_escape(source_note(attr(tab, "source_columns"))),
             '</p>'), file = current_report, append = TRUE)
}
cat('<h2>Supplementary tables for the manuscript</h2>',
    file = current_report, append = TRUE)

# S1: preserve recorded inoculum/site labels; use the full corpus denominator.
supp_table_S1 <- inoculum_site_table %>%
  transmute(`Inoculum type`, `Sampling site`, `Studies, n (%)` = n_pct(Studies, n_studies))
attr(supp_table_S1, "source_columns") <- inoculum_site_columns
show_supplementary_table(supp_table_S1,
                         "Supplementary Table S1. Inoculum sources and sampling sites across the included studies.",
                         paste0("Percentages use all ", n_studies, " included studies. Inoculum and sampling-site labels ",
                                "are retained as recorded, including composite categories; they have not been reclassified. ",
                                "GI, gastrointestinal. Not applicable denotes the absence of an applicable sampling site."))

# S2: show component presence using all studies in each experimental model as N.
# This adds a definition, not a recoding of the extraction workbook.
host_presence_summary <- reviews %>%
  transmute(`Experimental model` = replace_na(as.character(.data[[model_column]]), "Missing (empty cell)"),
            host_status = tolower(trimws(as.character(.data[[host_column]])))) %>%
  group_by(`Experimental model`) %>%
  summarize(N = n(), Present = sum(host_status %in% c("present", "yes")), .groups = "drop") %>%
  arrange(desc(N))
supp_table_S2 <- host_presence_summary %>%
  transmute(`Experimental model`, `Studies in model (N)` = N,
            `Host-derived components present, n (%)` = n_pct(Present, N))
attr(supp_table_S2, "source_columns") <- c(model_column, host_column)
show_supplementary_table(supp_table_S2,
                         "Supplementary Table S2. Presence of host-derived components by experimental model.",
                         paste0("Percentages use all studies within each experimental model (N). Presence follows the ",
                                "recorded Present/Yes classification; absent or unreported entries remain in the denominator. ",
                                "Host-derived components are host cells or materials representing the host intestinal ",
                                "environment, such as mucin and bile salts, excluding the microbial inoculum itself. ",
                                "The definition does not automatically change existing presence/absence coding."))

# S3: all original treatment labels, including no perturbation and missing entries.
supp_table_S3 <- treatment_counts %>%
  select(`Perturbation or treatment category` = Entry, `Studies, n (%)` = `n (%)`)
attr(supp_table_S3, "source_columns") <- treatment_column
show_supplementary_table(supp_table_S3,
                         "Supplementary Table S3. Perturbation and treatment categories across the included studies.",
                         paste0("Percentages use all ", n_studies, " included studies. Semicolons delimit multiple ",
                                "responses; each study is counted once per category. Categories may overlap, so percentages ",
                                "may sum to more than 100%. Category names are kept exactly as recorded in the extraction file."))

# S4: each cell uses the total studies in its inoculum-handling row.
replication_supp_long <- design_links %>%
  count(`Pooling of inoculum`, `Replication Strategy`, name = "Studies") %>%
  group_by(`Pooling of inoculum`) %>% mutate(N = sum(Studies)) %>% ungroup() %>%
  mutate(cell = n_pct(Studies, N))
stopifnot(sum(replication_supp_long$Studies) == n_studies)
supp_table_S4 <- replication_supp_long %>%
  select(`Inoculum handling` = `Pooling of inoculum`, `Studies in category (N)` = N,
         `Replication Strategy`, cell) %>%
  pivot_wider(names_from = `Replication Strategy`, values_from = cell,
              values_fill = "0 (0.0%)") %>%
  arrange(desc(`Studies in category (N)`))
attr(supp_table_S4, "source_columns") <- c("Pooling of inoculum", "Replication Strategy")
show_supplementary_table(supp_table_S4,
                         "Supplementary Table S4. Replication strategies by inoculum-handling category.",
                         paste0("Cells show n (% within inoculum-handling category); N includes all studies in that row, ",
                                "including those with unreported replication. Zeros denote no recorded study in that combination. ",
                                "Inoculum handling and replication retain the extraction classifications; replication is not ",
                                "inferred from donor counts or pooling. Cross-tabulations describe co-occurrence."))

# S5: separate single donors from multiple donors; retain usable-count coverage.
format_donor_number <- function(x) {
  vapply(x, function(value) if (is.na(value)) "Not available" else
    format(value, trim = TRUE, scientific = FALSE), character(1))
}
supp_table_S5 <- longitudinal_donor_summary %>%
  filter(`All studies in group` > 0) %>%
  transmute(`Sampling design` = as.character(Longitudinal_design),
            `Inoculum handling` = as.character(Inoculum_plot_group),
            `Donors, median (Q1-Q3)` = ifelse(`Studies with positive donor count` > 0,
                                              paste0(format_donor_number(Median), " (", format_donor_number(Q1), "-", format_donor_number(Q3), ")"),
                                              "Not available"),
            `Donors, range` = ifelse(`Studies with positive donor count` > 0,
                                     paste0(format_donor_number(Minimum), "-", format_donor_number(Maximum)), "Not available"))
attr(supp_table_S5, "source_columns") <- c(longitudinal_column, donor_column, "Pooling of inoculum")
show_supplementary_table(supp_table_S5,
                         "Supplementary Table S5. Donor-number distributions by sampling design and inoculum handling.",
                         paste0("Medians, quartiles and ranges use positive integer donor counts only; quartiles use R ",
                                "quantile type 7. Single-donor studies are shown separately from ",
                                "non-pooled multiple-donor studies. Non-host-derived and unknown handling groups and unknown ",
                                "sampling-design categories are excluded from this distribution table and remain in the ",
                                "full report. Donor counts do not necessarily equal the number of independent biological replicates."))

# ---- Publication supplementary tables S6-S8 ---------------------------------
supp_table_S6 <- sequencing_instruments %>%
  select(Platform, `Studies with platform (N)` = `Platform studies`, Instrument,
         `Studies, n (% of all studies)` = `All studies: n (%)`,
         `Studies, n (% within platform)` = `Within platform: n (%)`)
attr(supp_table_S6, "source_columns") <- sequencing_sources
show_supplementary_table(supp_table_S6,
                         "Supplementary Table S6. Sequencing instruments reported within each sequencing platform.",
                         paste0("Percentages use all ", n_studies, " included studies and the number of studies reporting ",
                                "each platform (N). Each study is counted once per platform-instrument pair; a study may report ",
                                "several platforms or instruments, so percentages may exceed 100%. Missing information is retained. ",
                                "Instruments were paired with the sole recorded platform, or by entry order for equally sized ",
                                "multi-platform lists; other pairings are shown as unresolved."))

supp_table_S7 <- component_counts %>%
  filter(Field %in% c("Metabolomics analytical platform", "Metaproteomics analytical platform"),
         Studies > 0) %>%
  transmute(`Omics layer` = sub(" analytical platform$", "", Field),
            `Analytical platform` = Method, `Studies, n (%)` = n_pct(Studies, n_studies))
attr(supp_table_S7, "source_columns") <- c("Metabolomics analytical platform",
                                           "Metaproteomics analytical platform")
show_supplementary_table(supp_table_S7,
                         "Supplementary Table S7. Analytical platforms used for metabolomics and metaproteomics.",
                         paste0("Percentages use all ", n_studies, " included studies. Each study is counted once per ",
                                "platform within each omics layer. A study reporting LC-MS and GC-MS contributes to both rows. ",
                                "Only platforms with non-zero counts are displayed. LC-MS, liquid chromatography-mass spectrometry; ",
                                "GC-MS, gas chromatography-mass spectrometry; NMR, nuclear magnetic resonance; ",
                                "LA-REIMS, laser-assisted rapid evaporative ionization mass spectrometry."))

adjunct_description_column <- "Targeted exogenous compound and its direct catabolites or other methods"
supp_table_S8 <- reviews %>%
  filter(presence(.data[[adjunct_description_column]], TRUE) == "Yes") %>%
  transmute(`Recorded measurement description` = .data[[adjunct_description_column]]) %>%
  count(`Recorded measurement description`, name = "Studies", sort = TRUE) %>%
  transmute(`Recorded measurement description`, `Studies, n (%)` = n_pct(Studies, n_studies))
attr(supp_table_S8, "source_columns") <- adjunct_description_column
show_supplementary_table(supp_table_S8,
                         "Supplementary Table S8. Additional targeted chemical measurements recorded separately from omics layers and SCFA/BCFA panels.",
                         paste0("Percentages use all ", n_studies, " included studies. Original descriptions are retained; ",
                                "each row counts studies with that recorded description. Entries indicating no measurement, ",
                                "missing information or non-applicability are omitted. These measurements are not counted as ",
                                "additional omics layers. SCFA/BCFA panels are reported separately. SCFA, short-chain fatty acid; ",
                                "BCFA, branched-chain fatty acid."))

finish_report()
cat("\nFinished. HTML report:", normalizePath(current_report), "\nFigures:", normalizePath(fig_dir), "\n")

# Temporary HTML preview is no longer needed after report assembly.
unlink(figure_3a_preview)
