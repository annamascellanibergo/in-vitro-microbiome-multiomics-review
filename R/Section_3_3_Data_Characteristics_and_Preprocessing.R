# Manuscript section 3.3 Data Characteristics and Preprocessing
# Repository entry point: run from the repository root.
source("R/00_style.R", local = TRUE)
source("R/report_layout.R", local = TRUE)

# =============================================================================
# Scoping review: Section 3 — Data Characteristics and Preprocessing
# Plots are saved in figures; all tables, including publication Tables S9-S11,
# are combined into ONE HTML report per section.
# Feature counts are described by category, without sample-count comparisons.
# =============================================================================

library(readxl)
library(dplyr)
library(tidyr)
supplementary_export <- new.env(parent = emptyenv())
supplementary_export$tables <- list()
supplementary_export$html <- list()
collect_supplementary <- function(tab, caption) {
  sheet <- sub("\\..*$", "", caption)
  if (!grepl("^Supplementary Table S[0-9]+$", sheet)) stop("Invalid supplementary caption: ", caption)
  if (sheet %in% names(supplementary_export$tables)) stop("Duplicate supplementary table: ", sheet)
  supplementary_export$tables[[sheet]] <- list(table = as.data.frame(tab), caption = caption)
}


# ---- Settings ---------------------------------------------------------------
data_file  <- getOption("review.data_file", "data/Supplementary Appendix 2.xlsx")
data_sheet <- getOption("review.data_sheet", "Data Extraction")               # sheet with the extracted data
fig_dir    <- "figures"                       # folder for saved plots
report_dir <- "results"                       # folder for the HTML report
fig_pdf    <- "Figure_3c_data_structure.pdf"
fig_png    <- tempfile(pattern = "Figure_3c_", fileext = ".png")
inside_min <- 12                              # threshold for external figure labels
expected_studies <- 102

# Separators used inside cells that list several methods (regular expressions)
sep_transform <- "\\s*[+;]\\s*"     # "Log + Centering"
sep_list      <- "\\s*[,;]\\s*"     # "Denoising, Prevalence filtering"; "mothur, VSEARCH"
sep_tools     <- "\\s*[,;+]\\s*"

# Values that mean "nothing reported" when deciding whether an item was reported
not_reported_values <- c("Not reported", "Not applicable", "None", "No", "Missing")

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
clean_text <- function(x) {
  x <- trimws(gsub("\\s+", " ", x))
  x[!is.na(x) & x == ""] <- NA_character_
  x[!is.na(x) & tolower(x) == "not reported"]   <- "Not reported"
  x[!is.na(x) & tolower(x) == "not applicable"] <- "Not applicable"
  x
}
reviews <- reviews %>% mutate(across(where(is.character), clean_text))

n_studies <- nrow(reviews)
cat("Number of studies:", n_studies, "\n")
if (n_studies == 0) stop("No study rows found.")
if (n_studies != expected_studies) warning(sprintf(
  "Workbook contains %d studies; expected full corpus: %d. Percentages use %d, not %d.",
  n_studies, expected_studies, n_studies, expected_studies))

# Find a column by exact name, or by a regular expression on the final name
find_col <- function(pattern, exact = TRUE) {
  hit <- if (exact) col_info$col[col_info$col == pattern]
  else col_info$col[grepl(pattern, col_info$col, ignore.case = TRUE)]
  if (length(hit) != 1) stop(sprintf("Column '%s' found %d times; check header row 2.",
                                     pattern, length(hit)))
  hit
}

# Short study label for study-level tables
study_no_col <- col_info$col[col_info$name == "Progressive Study No"][1]
first_author <- sub("\\s+and\\s.*$|[,;].*$", "", replace_na(reviews$Authors, "Unknown"))
first_author <- paste0(first_author, ifelse(grepl("\\sand\\s|[,;]", replace_na(reviews$Authors, "")), " et al.", ""))
drop_decimal <- function(x) sub("\\.0+$", "", trimws(as.character(x)))  # "4.0" -> "4"
study_label  <- paste0(if (!is.na(study_no_col)) paste0("#", drop_decimal(reviews[[study_no_col]]), " ") else "",
                       first_author, " (", replace_na(drop_decimal(reviews$`Year of Publication`), "n.d."), ")")


# ---- Helper functions -------------------------------------------------------
n_pct <- function(n, total) sprintf("%d (%.1f%%)", as.integer(n), 100 * n / total)
with_sources <- function(tab, sources) { attr(tab, "source_columns") <- sources; tab }

# Single-choice column: n (%) of all studies, most frequent first
count_table <- function(data, column, label = column, levels = NULL) {
  values <- replace_na(as.character(data[[column]]), "Missing (empty cell)")
  lv <- if (is.null(levels)) names(sort(table(values), decreasing = TRUE))
  else c(levels, setdiff(names(sort(table(values), decreasing = TRUE)), levels))
  counts <- tibble(value = factor(values, levels = lv)) %>%
    count(value, .drop = FALSE) %>% mutate(value = as.character(value))
  total <- sum(counts$n)
  out <- counts %>%
    bind_rows(tibble(value = "Total", n = total)) %>%
    mutate(`n (%)` = n_pct(n, total)) %>%
    select(value, `n (%)`)
  names(out)[1] <- label
  with_sources(out, column)
}

# Split a multi-value cell into one row per study and value. Spelling variants
# that differ only in capitalization are merged (most common spelling is shown).
split_values <- function(data, column, sep) {
  long <- tibble(study = seq_len(nrow(data)),
                 value = replace_na(as.character(data[[column]]), "Missing (empty cell)")) %>%
    separate_rows(value, sep = sep) %>%
    mutate(value = trimws(value)) %>%
    filter(value != "")
  spelling <- long %>% count(key = tolower(value), value, sort = TRUE) %>%
    distinct(key, .keep_all = TRUE) %>% select(key, label = value)
  long %>% mutate(key = tolower(value)) %>%
    left_join(spelling, by = "key") %>%
    transmute(study, value = label) %>%
    distinct()
}

# Multi-value column: studies per value, % of all studies (can exceed 100%)
count_multi <- function(data, column, label = column, sep = sep_list) {
  long <- split_values(data, column, sep)
  out <- long %>% count(value, sort = TRUE) %>%
    mutate(`Studies, n (%)` = n_pct(n, nrow(data))) %>%
    select(value, `Studies, n (%)`) %>%
    bind_rows(tibble(value = "Total studies", `Studies, n (%)` = n_pct(nrow(data), nrow(data))))
  names(out)[1] <- label
  with_sources(out, column)
}

# Assign each value to a broad group: first matching regular expression wins.
classify_values <- function(values, groups, other = "Other (see individual entries)") {
  out <- rep(other, length(values))
  done <- rep(FALSE, length(values))
  low <- tolower(values)
  is_nr <- low %in% tolower(c(not_reported_values, "Missing (empty cell)"))
  out[is_nr] <- values[is_nr]
  done <- out != other
  for (g in names(groups)) {
    hit <- !done & grepl(groups[[g]], values, ignore.case = TRUE, perl = TRUE)
    out[hit] <- g
    done <- done | hit
  }
  out
}

# Broad-group table: a study counts once per group; also lists the entries in each group
group_multi <- function(data, column, groups, label, sep) {
  long <- split_values(data, column, sep) %>%
    mutate(group = classify_values(value, groups))
  lv <- c(names(groups), "Other (see individual entries)")
  out <- long %>% group_by(group) %>%
    summarize(n = n_distinct(study),
              `Entries included` = paste(sort(unique(value)), collapse = "; "), .groups = "drop") %>%
    mutate(order = match(group, lv), order = ifelse(is.na(order), 999, order)) %>%
    arrange(order, desc(n)) %>%
    transmute(group, `Studies, n (%)` = n_pct(n, nrow(data)), `Entries included`)
  names(out)[1] <- label
  with_sources(out, column)
}

# Reported / not reported overview for any column
reporting_status <- function(x) {
  case_when(is.na(x) ~ "Missing (empty cell)",
            x == "Not applicable" ~ "Not applicable",
            tolower(x) %in% tolower(not_reported_values) ~ "Not reported",
            TRUE ~ "Reported")
}
status_table <- function(data, column, label = "Reporting status") {
  d <- tibble(s = reporting_status(data[[column]]))
  names(d) <- column
  count_table(d, column, label,
              levels = c("Reported", "Not reported", "Not applicable", "Missing (empty cell)")) %>%
    filter(!(grepl("^0 ", `n (%)`) & .data[[label]] %in% c("Not applicable", "Missing (empty cell)"))) %>%
    with_sources(column)
}

# Number of distinct procedures per study (0 = nothing reported)
n_procedures_table <- function(data, column, sep, label) {
  long <- split_values(data, column, sep) %>%
    filter(reporting_status(value) == "Reported")
  per_study <- tibble(study = seq_len(nrow(data))) %>%
    left_join(long %>% count(study, name = "k"), by = "study") %>%
    mutate(k = replace_na(k, 0L),
           k = ifelse(k >= 4, "4 or more", as.character(k)))
  names(per_study)[2] <- label
  count_table(per_study, label, label, levels = c("0", "1", "2", "3", "4 or more")) %>%
    with_sources(column)
}

# Heterogeneity: how many studies share exactly the same set of reported entries
heterogeneity_table <- function(data, column, sep) {
  combos <- split_values(data, column, sep) %>%
    filter(reporting_status(value) == "Reported") %>%
    group_by(study) %>%
    summarize(combo = paste(sort(tolower(value)), collapse = " | "), .groups = "drop")
  shared <- combos %>% count(combo) %>% filter(n > 1)
  tibble(
    Measure = c("Studies reporting at least one entry",
                "Distinct entries (individual criteria / methods)",
                "Distinct combinations across studies",
                "Combinations used by more than one study",
                "Studies using a combination shared with another study"),
    Value = c(nrow(combos),
              n_distinct(split_values(data, column, sep) %>%
                           filter(reporting_status(value) == "Reported") %>% pull(value) %>% tolower()),
              n_distinct(combos$combo), nrow(shared), sum(shared$n))
  ) %>% mutate(Value = as.character(Value)) %>% with_sources(column)
}

# Rows = groups, columns = categories, cells = n (% within group).
# pooled: named list of extra rows that combine several groups.
within_group_wide <- function(group, category, sources, group_label,
                              pooled = list(), category_levels = NULL, group_levels = NULL) {
  d <- tibble(g = group, c = category)
  for (p in names(pooled)) d <- bind_rows(d, tibble(g = p, c = category[group %in% pooled[[p]]]))
  lv <- if (is.null(category_levels)) names(sort(table(category), decreasing = TRUE))
  else c(category_levels, setdiff(unique(category), category_levels))
  lv <- lv[lv %in% category]
  d <- d %>% mutate(c = factor(c, levels = lv))
  g_order <- c(names(sort(table(group), decreasing = TRUE)), names(pooled))
  if (!is.null(group_levels)) g_order <- c(intersect(group_levels, g_order), setdiff(g_order, group_levels))
  out <- d %>% count(g, c, .drop = FALSE) %>% group_by(g) %>%
    mutate(total = sum(n), cell = n_pct(n, total)) %>% ungroup() %>%
    select(g, total, c, cell) %>%
    pivot_wider(names_from = c, values_from = cell) %>%
    mutate(g = factor(g, levels = g_order)) %>% arrange(g) %>%
    mutate(g = as.character(g)) %>%
    rename(`Studies in group (n)` = total)
  names(out)[1] <- group_label
  with_sources(out, sources)
}

# ---- Feature counts (n_features_coded) ----------------------------------------
# Codebook syntax: field1|level|count|basis; several records separated by "; ".
#   field1 = marker (microbiome) or approach (metabolome); count = integer only.
#   basis: total = all detected | filt = after filtering |
#          subset = only differential/selected features | panel = fixed targeted panel.
# Cell values "Not reported" (assay done, no usable count) and "Not applicable"
# (assay not done) are kept as their own categories.
feature_bases <- c("total", "filt", "subset", "panel")

parse_features <- function(data, column, codebook) {
  rec <- tibble(study = seq_len(nrow(data)), raw = as.character(data[[column]])) %>%
    filter(!is.na(raw), reporting_status(raw) == "Reported") %>%
    separate_rows(raw, sep = "\\s*;\\s*") %>%
    mutate(raw = trimws(raw)) %>%
    filter(raw != "")
  parts <- strsplit(rec$raw, "\\s*\\|\\s*")
  field <- function(i) vapply(parts, function(p) if (length(p) >= i) trimws(p[i]) else NA_character_, character(1))
  rec %>% mutate(
    Field1 = field(1), Level = field(2), Count_raw = field(3), Basis = field(4),
    n_fields = lengths(parts),
    Count = ifelse(grepl("^[0-9]+$", replace_na(Count_raw, "")), as.numeric(Count_raw), NA_real_),
    Problem = paste0(
      ifelse(n_fields != 4, "not 4 fields; ", ""),
      ifelse(!tolower(Field1) %in% codebook$field1, paste0(codebook$field1_name, " not in codebook; "), ""),
      ifelse(!tolower(Level) %in% codebook$level, "level not in codebook; ", ""),
      ifelse(is.na(Count), "count is not an integer; ", ""),
      ifelse(!tolower(Basis) %in% feature_bases, "basis not in codebook; ", "")),
    Problem = sub("; $", "", Problem),
    Valid = Problem == "",
    # Records that measure coverage of the profiled feature space
    Coverage = Valid & tolower(Basis) %in% c("total", "filt") &
      !tolower(Level) %in% codebook$effect_levels)
}
fmt_num <- function(x) ifelse(is.na(x), "", trimws(formatC(x, format = "fg", big.mark = ",", digits = 6)))
quantile_rows <- function(x) {
  q <- if (length(x)) quantile(x, c(0, .25, .5, .75, 1), names = FALSE) else rep(NA, 5)
  tibble(Measure = c("Minimum", "Lower quartile", "Median", "Upper quartile", "Maximum"),
         Value = fmt_num(q))
}
# ---- HTML report helpers ----------------------------------------------------
html_escape <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
current_report <- NULL
# All HTML is written as UTF-8 (keeps accented author names correct on Windows)
html_out <- function(text, append = TRUE) {
  con <- file(current_report, open = if (append) "a" else "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(enc2utf8(text), con, sep = "")
}
table_no <- 0
start_report <- function(filename, title) {
  current_report <<- file.path(report_dir, filename)
  table_no <<- 0
  cat(report_header(title, n_studies), file = current_report)
}
finish_report <- function() {
  cat(report_footer, file = current_report, append = TRUE)
}

add_heading <- function(text, level = 2) {
  html_out(sprintf('<h%d>%s</h%d>', level, html_escape(text), level))
}
add_note <- function(text) {
  html_out(paste0('<p>', html_escape(text), '</p>'))
}
excel_letter <- function(n) {
  out <- ""
  while (n > 0) { out <- paste0(LETTERS[(n - 1) %% 26 + 1], out); n <- (n - 1) %/% 26 }
  out
}
source_note <- function(sources) {
  descriptions <- vapply(unique(sources), function(column) {
    info <- col_info[col_info$col == column, ]
    if (!nrow(info)) return(column)
    sprintf("%s: %s (Excel column %s)", info$section[1], info$name[1], excel_letter(info$position[1]))
  }, character(1))
  paste0("Source columns: ", paste(descriptions, collapse = "; "), ".")
}
show_table <- function(tab, title) {
  sources <- attr(tab, "source_columns")
  if (is.null(sources)) stop(paste("Source columns not defined for table:", title))
  table_no <<- table_no + 1
  cat("\n===== Table ", table_no, ". ", title, " =====\n", sep = "")
  print(as.data.frame(tab), row.names = FALSE, right = FALSE)
  header <- paste0('<th>', html_escape(names(tab)), '</th>', collapse = '')
  rows <- if (nrow(tab)) vapply(seq_len(nrow(tab)), function(i) {
    cells <- vapply(tab, function(column) html_escape(column[i]), character(1))
    paste0('<tr>', paste0('<td>', cells, '</td>', collapse = ''), '</tr>')
  }, character(1)) else '<tr><td>No records</td></tr>'
  html_out(paste0('<h2>', html_escape(title), '</h2><p class="source">',
                  html_escape(source_note(sources)), '</p><div class="table"><table><thead><tr>',
                  header, '</tr></thead><tbody>', paste(rows, collapse = ''),
                  '</tbody></table></div>'))
}


# =============================================================================
# Section 3: data-characteristic columns and codebooks
# =============================================================================
sample_col      <- find_col("^Sample( size|\\(n\\))$", exact = FALSE)
design_col      <- "Longitudinal vs Cross-sectional"
overlap_col     <- "Sample Overlap Across Omics"
sparsity_col    <- "Sparsity"
comp_col        <- "Compositionality Addressed"
comp_how_col    <- find_col("^How is compositionality", exact = FALSE)
missingness_col <- "Missingness"
other_feat_col  <- find_col("^No\\. Features - Other Omics", exact = FALSE)

# Codebook levels (shown in this order, including empty categories)
design_levels   <- c("Longitudinal", "Cross-sectional", "Mixed", "Not reported", "Not applicable")
overlap_levels  <- c("All samples shared", "Partially overlapping", "Distinct sample sets",
                     "Not applicable", "Not reported")
sparsity_levels <- c("High (>70% zeros)", "Medium (30-70%)", "Low (<30%)", "Not reported")
comp_levels     <- c("Yes", "No", "Not reported")
missing_levels  <- c("Present - handled", "Present - not handled", "Absent", "Not reported")
sample_bins     <- c(0, 10, 20, 50, 100, 200, Inf)   # sample-size classes

# Feature-count codebooks (n_features_coded), used for parts G and H
codebooks <- list(
  Microbiome = list(column = find_col("^No\\. Features - Metagenomics", exact = FALSE),
                    field1_name = "Marker", field1 = c("16s", "its", "shotgun", "16s_microarray"),
                    level = c("otu", "asv", "phylotype", "taxon", "species", "genus", "family", "phylum",
                              "mag", "bin", "orf", "contig", "ko_gene", "mgs", "probe"),
                    effect_levels = character(0)),
  Metabolome = list(column = find_col("^No\\. Features - Metabolomics", exact = FALSE),
                    field1_name = "Approach", field1 = c("untargeted", "targeted"),
                    level = c("feature", "feature_pos", "feature_neg", "metabolite", "annotated",
                              "differential", "voc", "scfa"),
                    effect_levels = "differential"),
  "Other omics" = list(column = other_feat_col,
                       field1_name = "Assay", field1 = c("metatranscriptomics", "metaproteomics"),
                       level = c("transcript", "gene", "deg", "protein", "peptide", "dep"),
                       effect_levels = c("deg", "dep"))
)

# Broad groups for the free-text "How is compositionality addressed?" (first match wins)
comp_groups <- c(
  "Log-ratio transformation (CLR / ALR / ILR / PhILR)" = "\\bclr\\b|\\balr\\b|\\bilr\\b|philr|log[- ]?ratio|aitchison",
  "Compositional differential abundance (ANCOM / ANCOM-BC / ALDEx2)" = "ancom|aldex",
  "Proportionality / compositional correlation (SparCC, SPIEC-EASI)" = "sparcc|spiec|proportionality",
  "Dirichlet-multinomial model"            = "dirichlet",
  "Count-model normalization (DESeq2 / edgeR / TMM)" = "deseq|edger|\\btmm\\b",
  "Cumulative-sum scaling (CSS)"           = "\\bcss\\b|cumulative",
  "Arcsine square-root transformation"     = "arcsin",
  "Rarefaction"                            = "rarefi",
  "Log transformation"                     = "\\blog",
  "Scaling (unit variance, Pareto)"        = "scaling|unit[- ]variance|pareto")

needed <- c(design_col, overlap_col, sparsity_col, comp_col, missingness_col)
if (length(setdiff(needed, names(reviews))))
  stop("Columns not found: ", paste(setdiff(needed, names(reviews)), collapse = "; "))

design <- replace_na(reviews[[design_col]], "Missing (empty cell)")


# ---- Publication table and embedded figure helpers ---------------------------
# Manuscript captions are independent of console table numbering.
show_supplementary_table <- function(tab, caption, note) {
  caption <- supplementary_caption(caption)
  collect_supplementary(tab, caption)
  sheet <- sub("\\..*$", "", caption)
  stopifnot(!is.null(current_report), !is.null(attr(tab, "source_columns")))
  cat("\n", caption, "\n", sep = "")
  print(as.data.frame(tab), row.names = FALSE, right = FALSE)
  header <- paste0("<th>", html_escape(names(tab)), "</th>", collapse = "")
  rows <- if (nrow(tab)) vapply(seq_len(nrow(tab)), function(i) {
    cells <- vapply(tab, function(column) html_escape(column[i]), character(1))
    paste0("<tr>", paste0("<td>", cells, "</td>", collapse = ""), "</tr>")
  }, character(1)) else paste0('<tr><td colspan="', ncol(tab), '">No records</td></tr>')
  supplementary_export$html[[sheet]] <- paste0('<div class="table"><table><caption style="text-align:left;',
                  'font-weight:bold;padding:12px 0;">', html_escape(caption),
                  '</caption><thead><tr>', header, '</tr></thead><tbody>',
                  paste(rows, collapse = ''), '</tbody></table></div><p>', html_escape(note),
                  '</p><p class="source">', html_escape(source_note(attr(tab, "source_columns"))), '</p>')
}

# Base64 encoding keeps the HTML report independent of companion image files.
encode_base64 <- function(bytes) {
  alphabet <- strsplit("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/", "", fixed = TRUE)[[1]]
  padding <- (3L - length(bytes) %% 3L) %% 3L
  v <- matrix(c(as.integer(bytes), rep(0L, padding)), nrow = 3L)
  indices <- rbind(bitwShiftR(v[1, ], 2L),
                   bitwOr(bitwShiftL(bitwAnd(v[1, ], 3L), 4L), bitwShiftR(v[2, ], 4L)),
                   bitwOr(bitwShiftL(bitwAnd(v[2, ], 15L), 2L), bitwShiftR(v[3, ], 6L)),
                   bitwAnd(v[3, ], 63L))
  result <- alphabet[as.vector(indices) + 1L]
  if (padding > 0L) result[(length(result) - padding + 1L):length(result)] <- "="
  paste0(result, collapse = "")
}
add_report_figure <- function(path, title, sources) {
  bytes <- readBin(path, what = "raw", n = file.info(path)$size)
  html_out(paste0('<h2>', html_escape(title), '</h2><p class="source">',
                  html_escape(source_note(sources)), '</p><img alt="', html_escape(title),
                  '" style="width:100%;height:auto" src="data:image/png;base64,',
                  encode_base64(bytes), '">'))
}

# =============================================================================
# Build the report
# =============================================================================
start_report("Section_3_3_Data_Characteristics_and_Preprocessing.html",
             "3.3 Data Characteristics and Preprocessing")

# ---- A. Sample size ---------------------------------------------------------
n_raw <- trimws(as.character(reviews[[sample_col]]))
n_val <- ifelse(grepl("^[0-9]+(\\.[0-9]+)?$", replace_na(gsub(",", "", n_raw), "")),
                suppressWarnings(as.numeric(gsub(",", "", n_raw))), NA_real_)
n_status <- tibble(x = case_when(!is.na(n_val) ~ "Numeric value",
                                 is.na(n_raw) ~ "Missing (empty cell)",
                                 reporting_status(n_raw) == "Not reported" ~ "Not reported",
                                 TRUE ~ "Non-numeric entry (check)"))
names(n_status) <- sample_col
show_table(count_table(n_status, sample_col, "Sample size entry",
                       levels = c("Numeric value", "Not reported")), "Sample size: reporting")
add_note(paste0("As extracted: biological and technical replicates are counted together and n is the sum ",
                "of all treatments; where not stated explicitly, n was estimated from the study design."))

n_summary <- bind_rows(
  tibble(Measure = c("Studies with a numeric sample size", "Mean"),
         Value = c(n_pct(sum(!is.na(n_val)), n_studies), fmt_num(round(mean(n_val, na.rm = TRUE), 1)))),
  quantile_rows(n_val[!is.na(n_val)]))
show_table(with_sources(n_summary, sample_col), "Sample size: distribution")

bins <- tibble(x = ifelse(is.na(n_val), "No numeric value",
                          as.character(cut(n_val, sample_bins, right = TRUE,
                                           labels = c("1-10", "11-20", "21-50", "51-100", "101-200", ">200")))))
names(bins) <- sample_col
show_table(count_table(bins, sample_col, "Sample size class",
                       levels = c("1-10", "11-20", "21-50", "51-100", "101-200", ">200", "No numeric value")),
           "Sample size: classes")

n_by_design <- tibble(Design = design, n = n_val) %>%
  group_by(Design) %>%
  summarize(`Studies in group` = n(), `With numeric n` = sum(!is.na(n)),
            Median = fmt_num(median(n, na.rm = TRUE)),
            IQR = ifelse(sum(!is.na(n)) > 0, paste(fmt_num(quantile(n, .25, na.rm = TRUE)), "to",
                                                   fmt_num(quantile(n, .75, na.rm = TRUE))), ""),
            Range = ifelse(sum(!is.na(n)) > 0, paste(fmt_num(min(n, na.rm = TRUE)), "to",
                                                     fmt_num(max(n, na.rm = TRUE))), ""), .groups = "drop") %>%
  mutate(order = match(Design, design_levels)) %>% arrange(order) %>% select(-order)
show_table(with_sources(n_by_design, c(sample_col, design_col)), "Sample size by study design")

odd_n <- tibble(Study = study_label, Entry = n_raw, Status = n_status[[1]]) %>%
  filter(Status == "Non-numeric entry (check)") %>% select(-Status)
if (nrow(odd_n)) show_table(with_sources(odd_n, sample_col), "Sample size entries that are not numbers (check)")

# ---- B. Design --------------------------------------------------------------
show_table(count_table(reviews, design_col, "Design", levels = design_levels),
           "Longitudinal vs cross-sectional design")

# ---- C. Sample overlap ------------------------------------------------------
show_table(count_table(reviews, overlap_col, "Sample overlap", levels = overlap_levels),
           "Sample overlap across omics layers")
overlap_by_design <- within_group_wide(
  design, replace_na(reviews[[overlap_col]], "Missing (empty cell)"),
  c(design_col, overlap_col), "Design", category_levels = overlap_levels)
show_table(overlap_by_design, "Sample overlap by study design (% within group)")

# ---- D. Sparsity ------------------------------------------------------------
show_table(count_table(reviews, sparsity_col, "Sparsity", levels = sparsity_levels),
           "Sparsity of the feature matrix")

# ---- E. Compositionality ----------------------------------------------------
show_table(count_table(reviews, comp_col, "Compositionality addressed", levels = comp_levels),
           "Compositionality of microbiome data addressed")
comp_yes <- reviews %>% filter(.data[[comp_col]] %in% "Yes")
if (nrow(comp_yes)) {
  show_table(group_multi(comp_yes, comp_how_col, comp_groups, "Approach", sep = "\\s*;\\s*"),
             "How compositionality was addressed, broad groups (% of studies addressing it)")
  show_table(count_multi(comp_yes, comp_how_col, "Approach (as extracted)", sep = "\\s*;\\s*"),
             "How compositionality was addressed, entries as extracted (% of studies addressing it)")
}
# Consistency with Section 6: log-ratio transformation recorded there
tf_col <- "Transformation Method - Microbiome"
if (tf_col %in% names(reviews)) {
  logratio <- ifelse(grepl("\\bclr\\b|\\balr\\b|\\bilr\\b|log[- ]?ratio",
                           replace_na(reviews[[tf_col]], ""), ignore.case = TRUE),
                     "Log-ratio transformation in Section 6", "No log-ratio transformation in Section 6")
  show_table(within_group_wide(replace_na(reviews[[comp_col]], "Missing (empty cell)"), logratio,
                               c(comp_col, tf_col), "Compositionality addressed"),
             "Consistency check: compositionality vs log-ratio transformation (% within group)")
  lr_regex <- "\\bclr\\b|\\balr\\b|\\bilr\\b|philr|log[- ]?ratio"
  lr_mismatch <- tibble(Study = study_label,
                        `How addressed (Section 5)` = reviews[[comp_how_col]],
                        `Transformation (Section 6)` = reviews[[tf_col]]) %>%
    filter(xor(grepl(lr_regex, replace_na(`How addressed (Section 5)`, ""), ignore.case = TRUE),
               grepl(lr_regex, replace_na(`Transformation (Section 6)`, ""), ignore.case = TRUE)))
  if (nrow(lr_mismatch))
    show_table(with_sources(lr_mismatch, c(comp_how_col, tf_col)),
               "Log-ratio transformation recorded in only one of Sections 5 and 6 (check)")
}
how <- replace_na(reviews[[comp_how_col]], "")
comp_audit <- tibble(Study = study_label, `Compositionality addressed` = reviews[[comp_col]],
                     `How addressed` = how) %>%
  filter((`Compositionality addressed` %in% "Yes" & how %in% c("", "Not reported")) |
           (!`Compositionality addressed` %in% "Yes" & !how %in% c("", "Not reported", "Not applicable")))
if (nrow(comp_audit))
  show_table(with_sources(comp_audit, c(comp_col, comp_how_col)),
             "Compositionality entries that disagree with the free text (check)")

# ---- F. Missingness ---------------------------------------------------------
show_table(count_table(reviews, missingness_col, "Missingness", levels = missing_levels),
           "Missingness in the omics data")
mv_cols <- intersect(c("Handling of Missing Values - Microbiome", "Handling of Missing Values - Metabolome"),
                     names(reviews))
if (length(mv_cols)) {
  any_handling <- apply(sapply(mv_cols, function(cl) reporting_status(reviews[[cl]]) == "Reported"), 1, any)
  show_table(within_group_wide(replace_na(reviews[[missingness_col]], "Missing (empty cell)"),
                               ifelse(any_handling, "Handling reported in Section 6 or 7",
                                      "No handling reported in Section 6 or 7"),
                               c(missingness_col, mv_cols), "Missingness",
                               category_levels = "Handling reported in Section 6 or 7"),
             "Consistency check: missingness vs missing-value handling (% within group)")
}

# ---- G. Feature-count reporting ---------------------------------------------
feature_sources <- vapply(codebooks, function(x) x$column, character(1))
feature_records <- bind_rows(lapply(names(codebooks), function(layer) {
  parse_features(reviews, codebooks[[layer]]$column, codebooks[[layer]]) %>%
    mutate(Layer = layer)
})) %>% mutate(across(c(Field1, Level, Basis), tolower)) %>%
  mutate(Layer = case_when(Field1 == "metatranscriptomics" ~ "Metatranscriptomics",
                           Field1 == "metaproteomics" ~ "Metaproteomics", TRUE ~ Layer))

# Reporting percentages use all loaded studies; no sample counts are involved.
feature_reporting <- bind_rows(lapply(names(codebooks), function(layer) {
  cb <- codebooks[[layer]]
  raw <- as.character(reviews[[cb$column]])
  valid_studies <- unique(feature_records$study[feature_records$Valid &
                                                  feature_records$Layer %in% if (layer == "Other omics")
                                                    c("Metatranscriptomics", "Metaproteomics") else layer])
  status <- case_when(seq_len(n_studies) %in% valid_studies ~ "Valid count(s) recorded",
                      is.na(raw) | trimws(raw) == "" ~ "Empty cell",
                      reporting_status(raw) == "Not applicable" ~ "Not applicable",
                      reporting_status(raw) == "Not reported" ~ "Not reported",
                      TRUE ~ "No valid count: review coding")
  tibble(Layer = layer, Status = status) %>% count(Layer, Status, name = "Studies") %>%
    mutate(`% of all studies` = sprintf("%.1f", 100 * Studies / n_studies))
}))
show_table(with_sources(feature_reporting, feature_sources), "Feature-count reporting by omics layer")
add_note("Reporting percentages use all loaded studies. Not applicable, not reported, empty cells and invalid coding are kept separate. A valid count can describe a total, filtered dataset, selected subset or fixed panel; it does not necessarily describe the complete analytical matrix.")
bad_features <- feature_records %>% filter(!Valid) %>%
  transmute(Study = study_label[study], Layer, Record = raw, Problem)
if (nrow(bad_features)) {
  warning(nrow(bad_features), " feature records need review; see the HTML report.")
  show_table(with_sources(bad_features, feature_sources), "Feature records requiring coding review")
}

# ---- H. Feature counts by approach, type and processing stage ----------------
add_note("Counts are described directly, without comparison to study sample counts. No largest-count selection or summation across levels, assays or ionization modes is performed. Total counts, counts after filtering, selected subsets and fixed panels remain separate.")

# Identical coded records are counted once per study. Multiple distinct counts
# within the SAME category cannot be resolved from the four-field coding alone.
# Preserve these for review and exclude that study-category from numeric summaries.
feature_valid <- feature_records %>% filter(Valid) %>%
  distinct(study, Layer, Field1, Level, Basis, Count) %>%
  group_by(study, Layer, Field1, Level, Basis) %>%
  mutate(Counts_in_category = n()) %>% ungroup()
feature_single <- feature_valid %>% filter(Counts_in_category == 1)
feature_category_n <- feature_valid %>% group_by(Layer, Field1, Level, Basis) %>%
  summarize(`Studies reporting counts` = n_distinct(study),
            `Studies requiring review` = n_distinct(study[Counts_in_category > 1]), .groups = "drop")
feature_stats <- feature_single %>% group_by(Layer, Field1, Level, Basis) %>%
  summarize(`Studies in numeric summary` = n(),
            `Median (Q1-Q3) or single value` = if (n() == 1) fmt_num(Count[1]) else
              paste0(fmt_num(median(Count)), " (", fmt_num(quantile(Count, .25)), "-",
                     fmt_num(quantile(Count, .75)), ")"),
            Range = if (n() == 1) "" else paste(fmt_num(min(Count)), "to", fmt_num(max(Count))),
            .groups = "drop")
basis_labels <- c(total = "Total reported", filt = "After filtering",
                  subset = "Selected or differential subset", panel = "Fixed panel")
feature_summary_table <- feature_category_n %>%
  left_join(feature_stats, by = c("Layer", "Field1", "Level", "Basis")) %>%
  mutate(`Studies in numeric summary` = replace_na(`Studies in numeric summary`, 0L),
         `Median (Q1-Q3) or single value` = replace_na(`Median (Q1-Q3) or single value`, ""),
         Range = replace_na(Range, "")) %>%
  arrange(Layer, Field1, Level, match(Basis, c("total", "filt", "panel", "subset"))) %>%
  transmute(Layer, `Approach / feature type` = paste(Field1, Level, sep = " / "),
            `Count basis` = unname(basis_labels[Basis]), `Studies reporting counts`,
            `Studies in numeric summary`, `Studies requiring review`,
            `Median (Q1-Q3) or single value`, Range)
show_table(with_sources(feature_summary_table, feature_sources),
           "Reported feature counts by omics approach, feature type and processing stage")
# ---- I. Biological replication (donors) -------------------------------------
add_note(paste0("Donors are the independent biological units in most in vitro models; the sample size in part A ",
                "also counts replicates, treatments and time points."))
donor_col   <- "Total Number of Donors / Subjects"
pooling_col <- "Pooling of inoculum"
d_raw <- trimws(as.character(reviews[[donor_col]]))
d_val <- ifelse(grepl("^[0-9]+(\\.[0-9]+)?$", replace_na(d_raw, "")), suppressWarnings(as.numeric(d_raw)), NA_real_)
d_class <- case_when(!is.na(d_val) & d_val == 1 ~ "1 donor",
                     !is.na(d_val) & d_val <= 5 ~ "2-5 donors",
                     !is.na(d_val) & d_val <= 10 ~ "6-10 donors",
                     !is.na(d_val) ~ ">10 donors",
                     reporting_status(replace_na(d_raw, "Missing (empty cell)")) == "Not applicable" ~ "Not applicable (not host-derived)",
                     reporting_status(replace_na(d_raw, "Missing (empty cell)")) == "Not reported" ~ "Not reported",
                     is.na(d_raw) ~ "Missing (empty cell)",
                     TRUE ~ "Non-numeric entry (check)")
tmp <- tibble(x = d_class); names(tmp) <- donor_col
show_table(count_table(tmp, donor_col, "Number of donors / subjects",
                       levels = c("1 donor", "2-5 donors", "6-10 donors", ">10 donors",
                                  "Not applicable (not host-derived)", "Not reported")),
           "Number of donors or subjects")
show_table(with_sources(bind_rows(
  tibble(Measure = "Studies with a numeric donor count", Value = n_pct(sum(!is.na(d_val)), n_studies)),
  quantile_rows(d_val[!is.na(d_val)])), donor_col), "Number of donors: distribution")
if (pooling_col %in% names(reviews)) {
  show_table(count_table(reviews, pooling_col, "Pooling of inoculum"), "Pooling of the inoculum")
  show_table(within_group_wide(replace_na(reviews[[pooling_col]], "Missing (empty cell)"), d_class,
                               c(pooling_col, donor_col), "Pooling of inoculum",
                               category_levels = c("1 donor", "2-5 donors", "6-10 donors", ">10 donors")),
             "Number of donors by pooling of the inoculum (% within group)")
}
spd <- tibble(n = n_val, d = d_val) %>% filter(!is.na(n), !is.na(d), d > 0) %>% mutate(r = n / d)
show_table(with_sources(bind_rows(
  tibble(Measure = "Studies with numeric sample size and donor count", Value = as.character(nrow(spd))),
  quantile_rows(round(spd$r, 1))) %>% mutate(Measure = ifelse(Measure %in% c("Minimum", "Lower quartile", "Median",
                                                                             "Upper quartile", "Maximum"),
                                                              paste("Samples per donor:", tolower(Measure)), Measure)),
  c(sample_col, donor_col)), "Samples per donor")
few_n <- tibble(Study = study_label, `Sample size` = n_val, Donors = d_val,
                Pooling = if (pooling_col %in% names(reviews)) reviews[[pooling_col]] else NA) %>%
  filter(!is.na(`Sample size`), !is.na(Donors), `Sample size` < Donors)
if (nrow(few_n)) show_table(with_sources(few_n, c(sample_col, donor_col, pooling_col)),
                            "Studies with fewer samples than donors (check)")
odd_d <- tibble(Study = study_label, Entry = d_raw, Class = d_class) %>% filter(Class == "Non-numeric entry (check)") %>% select(-Class)
if (nrow(odd_d)) show_table(with_sources(odd_d, donor_col), "Donor entries that are not numbers (check)")

# ---- J. Fit between data structure and integration method --------------------
long_col  <- find_col("^Method Supports Longitudinal", exact = FALSE)
smalln_col <- find_col("^Method Tailored for Small N", exact = FALSE)
# Regularized / sparse methods are searched for in these Section 8 columns
method_cols <- intersect(c("Machine Learning Approaches", "Factorization Approaches", "All_Multivariate",
                           "Regression based methods", "Network-based Method specification"), names(reviews))
sparse_regex <- "lasso|elastic ?net|ridge|regulari[sz]|sparse|\\bspls|\\bsgcca|diablo|\\bsparcc|spiec|glmnet|penali[sz]"

yn_levels <- c("Yes", "No", "Not applicable", "Not reported")
show_table(within_group_wide(design, replace_na(reviews[[long_col]], "Missing (empty cell)"),
                             c(design_col, long_col), "Design", category_levels = yn_levels,
                             group_levels = design_levels),
           "Method supports longitudinal data, by study design (% within group)")

long_na <- tibble(Study = study_label, Design = reviews[[design_col]],
                  `Longitudinal support` = reviews[[long_col]],
                  `Integration type` = if ("Integration Type" %in% names(reviews)) reviews$`Integration Type` else NA) %>%
  filter(Design %in% c("Longitudinal", "Mixed"),
         `Longitudinal support` %in% c(NA, "Not applicable", "Not reported"))
if (nrow(long_na))
  show_table(with_sources(long_na, c(design_col, long_col, "Integration Type")),
             "Longitudinal or mixed designs without a Yes / No / Partly code for longitudinal support (check)")

n_class <- case_when(is.na(n_val) ~ "No numeric sample size", n_val <= 10 ~ "n <= 10",
                     n_val <= 20 ~ "n 11-20", n_val <= 50 ~ "n 21-50", TRUE ~ "n > 50")
show_table(within_group_wide(n_class, replace_na(reviews[[smalln_col]], "Missing (empty cell)"),
                             c(sample_col, smalln_col), "Sample size class", category_levels = yn_levels,
                             group_levels = c("n <= 10", "n 11-20", "n 21-50", "n > 50", "No numeric sample size")),
           "Method tailored for small n, by sample size (% within group)")

sparse_entries <- bind_rows(lapply(method_cols, function(cl)
  tibble(Study = study_label, Column = cl, Entry = reviews[[cl]]))) %>%
  filter(grepl(sparse_regex, replace_na(Entry, ""), ignore.case = TRUE))
show_table(with_sources(sparse_entries, method_cols),
           "Entries classified as regularized or sparse methods")
add_note(paste0("Regularized / sparse methods are detected by text matching in the Section 8 method columns (",
                paste(method_cols, collapse = "; "), ")."))

# ---- K. Reporting over time --------------------------------------------------
year <- suppressWarnings(as.numeric(reviews$`Year of Publication`))
period_breaks <- c(-Inf, 2020, 2022, 2024, Inf)            # edit to change the periods
period_labels <- c("Up to 2020", "2021-2022", "2023-2024", "2025 onwards")
period <- as.character(cut(year, period_breaks, labels = period_labels))
period[is.na(period)] <- "Year missing"
items <- list(
  "Sample size given or derivable"        = !is.na(n_val),
  "Donor number reported"                 = !is.na(d_val),
  "Design reported"                       = !reviews[[design_col]] %in% c(NA, "Not reported"),
  "Sample overlap reported"               = !reviews[[overlap_col]] %in% c(NA, "Not reported"),
  "Sparsity reported"                     = !reviews[[sparsity_col]] %in% c(NA, "Not reported"),
  "Missingness reported"                  = !reviews[[missingness_col]] %in% c(NA, "Not reported"),
  "Compositionality addressed"            = reviews[[comp_col]] %in% "Yes",
  "Microbiome feature count reported"     = reporting_status(reviews[[codebooks$Microbiome$column]]) == "Reported",
  "Metabolome feature count reported"     = reporting_status(reviews[[codebooks$Metabolome$column]]) == "Reported")
per_n <- table(factor(period, levels = c(period_labels, "Year missing")))
per_n <- per_n[per_n > 0]
trend_rows <- lapply(names(items), function(it) {
  x <- items[[it]]
  cells <- vapply(names(per_n), function(pr) n_pct(sum(x[period == pr]), sum(period == pr)), character(1))
  as_tibble(as.list(c(Item = it, setNames(cells, names(per_n)))))
})
show_table(with_sources(bind_rows(trend_rows),
                        c("Year of Publication", sample_col, donor_col, design_col, overlap_col, sparsity_col,
                          missingness_col, comp_col, codebooks$Microbiome$column, codebooks$Metabolome$column)),
           "Reporting by publication period, n (% of studies in the period)")
add_note(paste0("Studies per period: ", paste(names(per_n), per_n, sep = " = ", collapse = "; "), "."))

# =============================================================================
# L. Figure: data structure characteristics
# Uses base R graphics. Exports PDF; temporary previews are embedded in HTML.
# Labels are percentages of all loaded studies.
# Segments narrower than `inside_min` % get their label outside the bar,
# linked to the segment by a leader line with a dot on the segment.
# =============================================================================

fmt_pct    <- function(p) sprintf("%.0f%%", p)

# Colors (dark ones get white text)
# Blue shades for panel c; neutral grays reserved for reporting status.
col_dark1 <- pal("blue_dark"); col_mid <- pal("blue_mid"); col_light <- pal("blue_light")
col_gold  <- pal("blue_mid"); col_purple <- pal("blue_light"); col_gray <- pal("not_reported")
col_extra <- pal("blue_mid")

# Compositionality: one category per study
comp_how_txt <- replace_na(reviews[[comp_how_col]], "")
comp_yes_v   <- reviews[[comp_col]] %in% "Yes"
log_ratio <- grepl("\\bclr\\b|\\balr\\b|\\bilr\\b|philr|log[- ]?ratio|aitchison", comp_how_txt, ignore.case = TRUE)
clr_label <- "Centered log-ratio transformation (CLR)"
log_ratio_label <- if (all(grepl("\\bclr\\b|centered log[- ]ratio", comp_how_txt[comp_yes_v & log_ratio],
                                ignore.case = TRUE))) clr_label else "Log-ratio transformation"
fig_comp <- case_when(
  !comp_yes_v ~ "Not addressed",
  log_ratio ~ log_ratio_label,
  grepl("absolute|qpcr|flow ?cytometr|spike", comp_how_txt, ignore.case = TRUE) ~ "Absolute quantification",
  TRUE ~ "Other recorded approach")

nr_fill <- function(x) replace_na(as.character(x), "Not reported")

fig_rows <- list(
  list(label = "Study design", values = nr_fill(reviews[[design_col]]),
       levels = c("Longitudinal", "Mixed", "Cross-sectional", "Not reported"),
       colors = c("Longitudinal" = col_dark1, "Mixed" = col_mid,
                  "Cross-sectional" = pal("blue_light"), "Not reported" = col_gray)),
  list(label = "Sample overlap across omics", values = nr_fill(reviews[[overlap_col]]),
       levels = c("All samples shared", "Partially overlapping", "Distinct sample sets",
                  "Not applicable", "Not reported"),
       colors = c("All samples shared" = col_dark1, "Partially overlapping" = col_gold,
                  "Distinct sample sets" = col_purple, "Not applicable" = pal("not_applicable"),
                  "Not reported" = col_gray)),
  list(label = "Compositionality", values = fig_comp,
       levels = c(log_ratio_label, "Absolute quantification",
                  "Other recorded approach", "Not addressed"),
       colors = c(setNames(col_dark1, log_ratio_label), "Absolute quantification" = col_mid,
                  "Other recorded approach" = col_light, "Not addressed" = col_gray)),
  list(label = "Missingness", values = nr_fill(reviews[[missingness_col]]),
       levels = c("Present - handled", "Present - not handled", "Absent", "Not reported"),
       colors = c("Present - handled" = col_dark1, "Present - not handled" = col_gold,
                  "Absent" = col_mid, "Not reported" = col_gray)),
  list(label = "Sparsity", values = nr_fill(reviews[[sparsity_col]]),
       levels = c("High (>70% zeros)", "Medium (30-70%)", "Low (<30%)", "Not reported"),
       colors = c("High (>70% zeros)" = col_dark1, "Medium (30-70%)" = col_mid,
                  "Low (<30%)" = col_light, "Not reported" = col_gray))
)

# Percentages per row (also reported as a table)
fig_data <- bind_rows(lapply(fig_rows, function(r) {
  lv  <- c(r$levels, setdiff(sort(unique(r$values)), r$levels))
  cnt <- as.numeric(table(factor(r$values, levels = lv)))
  tibble(Characteristic = r$label, Category = lv, pct = 100 * cnt / n_studies) %>% filter(pct > 0)
}))

draw_fig <- function() {
  op <- par(mai = c(0.55, 1.55, 0.45, 2.65), mgp = c(2, 0.4, 0), tcl = -0.3, xpd = NA,
            family = "sans", col.axis = "black", fg = "black")
  on.exit(par(op))
  nr <- length(fig_rows); ys <- rev(seq_len(nr))
  plot(NA, xlim = c(0, 100), ylim = c(0.5, nr + 0.5), axes = FALSE, xlab = "", ylab = "")
  # Panel heading in the same style as "a" and "b"
  mtext("c  Data structure characteristics", side = 3, line = 0.8, adj = 0, at = par("usr")[1] - 
          diff(grconvertX(c(0, par("mai")[2]), "inches", "user")), font = 2, cex = 1.2)
  axis(1, at = seq(0, 100, 20), labels = paste0(seq(0, 100, 20), "%"), cex.axis = 1, lwd = 0.6)
  mtext(sprintf("Percentage of included studies (%d studies)", n_studies), side = 1, line = 1.9, cex = 1)
  
  for (i in seq_len(nr)) {
    r <- fig_rows[[i]]; y <- ys[i]
    d <- fig_data[fig_data$Characteristic == r$label, ]
    lv <- d$Category; pct <- d$pct
    cols <- unname(r$colors[lv]); cols[is.na(cols)] <- col_extra
    left <- c(0, head(cumsum(pct), -1)); right <- cumsum(pct); mid <- (left + right) / 2
    rect(left, y - 0.3, right, y + 0.3, col = cols, border = "white", lwd = 1)
    text(-2, y, r$label, adj = 1, cex = 1.1)
    
    # Labels inside wide segments
    force_out <- r$label == "Missingness" & lv == "Present - handled"
    ins <- which(pct >= inside_min & !force_out)
    if (length(ins))
      text(mid[ins], y, paste0(vapply(lv[ins], function(z) paste(strwrap(z, width = 19), collapse = "\n"), character(1)), "\n", fmt_pct(pct[ins])), cex = 1,
           col = ifelse(cols[ins] %in% c(pal("teal"), pal("purple"), pal("blue_dark")), "white", pal("text")))
    
    # Labels outside narrow segments, linked with elbow leader lines
    out <- which(pct < inside_min | force_out)
    k <- length(out)
    if (k) {
      h  <- y + 0.33 + 0.06 * (k - seq_len(k))                 # leftmost segment = highest elbow
      ty <- if (k == 1) y else y + 0.28 - (seq_len(k) - 1) * 0.56 / (k - 1)
      for (j in seq_len(k)) {
        s <- out[j]
        points(mid[s], y, pch = 16, cex = 0.5, col = "black")
        lines(c(mid[s], mid[s], 101.5, 104), c(y, h[j], h[j], ty[j]), col = "black", lwd = 0.8)
        text(105, ty[j], paste0(lv[s], ": ", fmt_pct(pct[s])), adj = 0, cex = 1, col = "black")
      }
    }
  }
}

pdf(file.path(fig_dir, fig_pdf), width = 210 / 25.4, height = 95 / 25.4,  # full A4 width x 72 mm
    pointsize = 9)                                                 # 9-point text at the exported size
draw_fig(); dev.off()

png(fig_png, width = 210 / 25.4, height = 95 / 25.4,
    units = "in", res = 300, pointsize = 9, bg = "white")
draw_fig(); dev.off()
add_report_figure(fig_png,
                  "Figure 3c. Data structure characteristics",
                  c(design_col, overlap_col, comp_col, comp_how_col, missingness_col, sparsity_col))
# Native grid version of panel c for reliable placement in the composite PDF.
# Uses the same rows, percentages, colors and label rules as the standalone panel.
draw_fig3c_grid <- function() {
  nr <- length(fig_rows)
  grid::pushViewport(grid::viewport(xscale = c(-55, 177), yscale = c(-0.4, nr + 1)))
  on.exit(grid::popViewport())
  txt <- function(label, x, y, just = "center", color = "black", bold = FALSE, size = 9) {
    grid::grid.text(label, x = grid::unit(x, "native"), y = grid::unit(y, "native"),
      just = just, gp = grid::gpar(fontsize = size, col = color,
                                  fontface = if (bold) "bold" else "plain"))
  }
  txt("c  Data structure characteristics", -52, nr + 0.72, "left", bold = TRUE, size = 11)
  for (i in seq_len(nr)) {
    r <- fig_rows[[i]]; y <- nr + 1 - i
    d <- fig_data[fig_data$Characteristic == r$label, ]
    lv <- d$Category; pct <- d$pct
    cols <- unname(r$colors[lv]); cols[is.na(cols)] <- col_extra
    right <- cumsum(pct); left <- c(0, head(right, -1)); mid <- (left + right) / 2
    grid::grid.rect(x = grid::unit(mid, "native"), y = grid::unit(rep(y, length(pct)), "native"),
      width = grid::unit(pct, "native"), height = grid::unit(0.6, "native"),
      gp = grid::gpar(fill = cols, col = "white", lwd = 0.7))
    txt(r$label, -3, y, "right")
    force_out <- r$label == "Missingness" & lv == "Present - handled"
    ins <- which(pct >= inside_min & !force_out)
    for (j in ins) {
      label <- paste0(paste(strwrap(lv[j], width = 19), collapse = "\n"), "\n", fmt_pct(pct[j]))
      txt(label, mid[j], y, color = if (cols[j] == pal("blue_dark")) "white" else "black")
    }
    out <- which(pct < inside_min | force_out); k <- length(out)
    if (k) {
      h <- y + 0.33 + 0.06 * (k - seq_len(k))
      ty <- if (k == 1) y else y + 0.28 - (seq_len(k) - 1) * 0.56 / (k - 1)
      for (j in seq_len(k)) {
        z <- out[j]
        grid::grid.lines(x = grid::unit(c(mid[z], mid[z], 101.5, 104), "native"),
          y = grid::unit(c(y, h[j], h[j], ty[j]), "native"), gp = grid::gpar(lwd = 0.7))
        grid::grid.points(x = grid::unit(mid[z], "native"), y = grid::unit(y, "native"),
          pch = 16, size = grid::unit(1, "mm"))
        txt(paste0(lv[z], ": ", fmt_pct(pct[z])), 105, ty[j], "left")
      }
    }
  }
  grid::grid.lines(x = grid::unit(c(0, 100), "native"), y = grid::unit(c(0.25, 0.25), "native"))
  for (x in seq(0, 100, 20)) {
    grid::grid.lines(x = grid::unit(c(x, x), "native"), y = grid::unit(c(0.25, 0.19), "native"))
    txt(paste0(x, "%"), x, 0.04)
  }
  txt(sprintf("Percentage of included studies (%d studies)", n_studies), 50, -0.28)
}

# ---- Compose Figure 3: a/b above c (reference layout) -----------------------
# run_all.R supplies the Section 2 environment before sourcing this section.
# Draw directly into fixed panel viewports; do not replay captured page layouts.
compose_figure3 <- function(section2, output = file.path(fig_dir, "Figure_3_composed.pdf")) {
  if (!exists("draw_upset", section2, inherits = FALSE))
    stop("Figure 3b is unavailable; check the omics-combination data in Section 2.")
  width <- 210 / 25.4
  top_height <- max(159 / 25.4, section2$fig_height_in)
  lower_height <- 95 / 25.4
  gap <- 5 / 25.4
  height <- top_height + gap + lower_height
  # Wider donor/design panel; 6 mm gutter within the 210 mm canvas.
  panel_a_width <- 124 / 25.4
  panel_b_width <- 80 / 25.4
  compact_a <- section2$figure_3a + patchwork::plot_annotation(
    title = "a  Experimental design and donor structure", subtitle = "", caption = "",
    theme = ggplot2::theme(plot.title = ggplot2::element_text(size = 10, face = "bold"),
                           plot.subtitle = ggplot2::element_blank(),
                           plot.caption = ggplot2::element_blank()))
  render <- function() {
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(x = 0, y = 1,
      width = grid::unit(panel_a_width, "in"), height = grid::unit(top_height, "in"),
      just = c("left", "top")))
    grid::grid.draw(patchwork::patchworkGrob(compact_a))
    grid::popViewport()
    grid::pushViewport(grid::viewport(x = 1, y = 1,
      width = grid::unit(panel_b_width, "in"), height = grid::unit(section2$fig_height_in, "in"),
      just = c("right", "top")))
    section2$draw_upset(newpage = FALSE)
    grid::popViewport()
    grid::pushViewport(grid::viewport(x = 0, y = 0, width = 1,
      height = grid::unit(lower_height, "in"), just = c("left", "bottom")))
    draw_fig3c_grid()
    grid::popViewport()

  }
  grDevices::pdf(output, width = width, height = height, useDingbats = FALSE)
  tryCatch(render(), finally = grDevices::dev.off())
  preview <- tempfile(fileext = ".png")
  tryCatch({
    grDevices::png(preview, width = width, height = height, units = "in", res = 300)
    tryCatch(render(), finally = grDevices::dev.off())
    add_report_figure(preview, "Figure 3. Experimental design, omics combinations and data structure",
      c(section2$donor_column, "Pooling of inoculum", section2$longitudinal_column,
        "Replication Strategy", section2$omics_source_columns, design_col, overlap_col,
        comp_col, comp_how_col, missingness_col, sparsity_col))
  }, finally = unlink(preview))
  message("Composed Figure 3: ", output)
  invisible(output)
}
if (exists("figure3_section2", inherits = FALSE)) {
  compose_figure3(figure3_section2)
} else {
  message("Figure 3c saved. To compose Figure 3, run run_all.R or supply figure3_section2 (see README).")
}

show_table(with_sources(fig_data %>% mutate(`% of studies` = sprintf("%.1f%%", pct)) %>% select(-pct),
                        c(design_col, overlap_col, comp_col, comp_how_col, missingness_col, sparsity_col)),
           "Values plotted in the data-structure figure (% of all loaded studies)")
add_note(paste0("Labels for segments under ", inside_min, "% are placed outside the bar and joined to their ",
                "segment by a leader line. Present - handled in Missingness is always labeled on the right. Empty cells are shown as 'Not reported'."))


# ---- Publication supplementary tables S9-S11 --------------------------------
# Standalone manuscript tables at the end of the same HTML report.
# Existing analytical tables and Figure 3c are retained above.

supp_table_S9 <- with_sources(n_by_design, c(sample_col, design_col))
show_supplementary_table(supp_table_S9,
                         "Supplementary Table S9. Distribution of total study sample counts by sampling design.",
                         paste0("Studies in group includes all studies in each design category; With numeric n is the ",
                                "number contributing to the descriptive statistics. IQR gives Q1-Q3. Total study sample ",
                                "counts were reported or derived from the experimental design and may include treatments, ",
                                "timepoints and biological or technical replicates. They do not represent independent biological units."))

# S10: compact descriptive ranges, independent of sample counts.
# Total and filtered counts are combined. Fixed panels remain separate.
# All eligible records contribute to ranges; studies are counted once per row.
approach_labels <- c("16s" = "16S", "its" = "ITS", "shotgun" = "Shotgun",
                     "16s_microarray" = "16S microarray", "untargeted" = "Untargeted",
                     "targeted" = "Targeted", "metatranscriptomics" = "Metatranscriptomics",
                     "metaproteomics" = "Metaproteomics")
feature_labels <- c(otu = "OTUs", asv = "ASVs", phylotype = "Phylotypes",
                    taxon = "Taxa (rank unspecified)", species = "Species", genus = "Genera",
                    family = "Families", phylum = "Phyla", mag = "MAGs", bin = "Bins",
                    orf = "ORFs", contig = "Contigs", ko_gene = "KO genes", mgs = "MGS",
                    probe = "Probes", feature = "Analytical features (mode unspecified)",
                    feature_pos = "Analytical features, positive mode",
                    feature_neg = "Analytical features, negative mode",
                    metabolite = "Reported metabolites", annotated = "Annotated compounds",
                    voc = "Volatile organic compounds", scfa = "Short-chain fatty acids",
                    transcript = "Transcripts", gene = "Genes", protein = "Proteins", peptide = "Peptides")
supp_table_S10 <- feature_valid %>%
  filter(Basis %in% c("total", "filt", "panel"),
         !Level %in% c("differential", "deg", "dep")) %>%
  mutate(Count_group = ifelse(Basis == "panel", "Fixed panel", "Total / filtered")) %>%
  group_by(Layer, Field1, Level, Count_group) %>%
  summarize(`Studies, n` = n_distinct(study),
            `Reported count range` = if (min(Count) == max(Count)) fmt_num(min(Count)) else
              paste(fmt_num(min(Count)), "to", fmt_num(max(Count))), .groups = "drop") %>%
  arrange(match(Layer, c("Microbiome", "Metabolome", "Metatranscriptomics", "Metaproteomics")),
          Field1, Level, Count_group) %>%
  mutate(Approach = coalesce(unname(approach_labels[Field1]), Field1),
         Feature = coalesce(unname(feature_labels[Level]), Level),
         Prefix = ifelse(Layer %in% c("Microbiome", "Metabolome"),
                         paste(ifelse(Layer == "Metabolome", "Metabolomics", Layer), Approach, sep = " / "),
                         Approach)) %>%
  transmute(`Feature category` = paste0(Prefix, " — ", Feature,
                                        ifelse(Count_group == "Fixed panel", " (fixed panel)", "")),
            `Studies, n`, `Reported count range`) %>%
  with_sources(feature_sources)
show_supplementary_table(supp_table_S10,
                         "Supplementary Table S10. Types and ranges of feature counts reported across omics approaches.",
                         paste0("Counts include totals and counts after filtering, as reported by the original studies, ",
                                "and therefore reflect different processing stages. Fixed panels are shown separately. ",
                                "Selected or differential subsets were excluded. Each study was counted once per category; ",
                                "all eligible counts contributed to the reported range, including multiple counts within a study. ",
                                "Categories are not mutually exclusive. Feature types and ionization modes were kept separate ",
                                "and were not summed. A single value indicates identical minimum and maximum counts. ",
                                "OTU, operational taxonomic unit; ASV, amplicon sequence variant; ITS, internal transcribed spacer; ",
                                "MAG, metagenome-assembled genome; ORF, open reading frame; KO, KEGG Orthology; ",
                                "MGS, metagenomic species."))

supp_table_S11 <- overlap_by_design
show_supplementary_table(supp_table_S11,
                         "Supplementary Table S11. Sample overlap across omics layers by sampling design.",
                         "Values are numbers of studies and percentages within each sampling-design category. Percentages describe studies, not individual samples; missing or unreported overlap categories are retained.")


# ---- Preprocessing analysis, continuing the same HTML report ----------------
# Headline rule: one number per study = the largest coverage count with basis
# "total"; if a study has no "total" record, the largest "filt" record is used.
# Ionization modes (feature_pos / feature_neg) are never summed.
coverage_per_study <- function(records) {
  records %>% filter(Coverage) %>%
    mutate(rank = ifelse(tolower(Basis) == "total", 1, 2)) %>%
    group_by(study) %>% filter(rank == min(rank)) %>%
    summarize(count = max(Count), basis_used = tolower(Basis[1]), .groups = "drop")
}
feature_summary <- function(records, column, per_study, total = n_studies) {
  bind_rows(
    tibble(Measure = c("Studies contributing a count", "Studies not contributing a count"),
           Value = c(n_pct(nrow(per_study), total), n_pct(total - nrow(per_study), total))),
    quantile_rows(per_study$count)) %>% with_sources(column)
}

# =============================================================================
# Omics-specific settings. Both parts are run by the SAME function below, so
# they produce the same set of tables. Broad groups are regular expressions
# applied to each entry (case-insensitive); the first matching group wins and
# unmatched entries go to "Other (see individual entries)". Edit as needed.
# =============================================================================
omics_specs <- list(
  Microbiome = list(
    label         = "Microbiome",
    layer_col     = "Metagenomics",
    feature_col   = find_col("^No\\. Features - Metagenomics", exact = FALSE),
    feature_codebook = list(
      field1_name = "Marker",
      field1 = c("16s", "its", "shotgun", "16s_microarray"),
      level = c("otu", "asv", "phylotype", "taxon", "species", "genus", "family", "phylum",
                "mag", "bin", "orf", "contig", "ko_gene", "mgs", "probe"),
      effect_levels = character(0)),
    transform_col = "Transformation Method - Microbiome",
    filter_col    = "Filtering Criteria - Microbiome",
    missing_col   = "Handling of Missing Values - Microbiome",
    batch_col     = "Batch Correction / Confounder Adjustment - Microbiome",
    qc_col        = "Quality Control Samples / Internal Standards - Microbiome",
    tool_col      = "Microbiome Profiling Tool",
    approach_pooled = list(),
    transform_groups = c(
      "Total-sum scaling (TSS, relative abundance)" = "\\btss\\b|total[- ]sum|relative abundance",
      "Log-ratio (CLR / ALR / ILR)"                 = "\\bclr\\b|\\balr\\b|\\bilr\\b|log[- ]ratio",
      "Cumulative-sum scaling (CSS)"                = "\\bcss\\b|cumulative",
      "Rarefaction"                                 = "rarefi|rarefaction",
      "RPKM / TPM / FPKM"                           = "rpkm|tpm|fpkm|cpm",
      "Log transformation"                          = "\\blog",
      "Arcsine square root"                         = "arcsin",
      "Library-size / median normalization"         = "library|median|deseq|tmm|upper quartile",
      "Scaling (unit variance, Pareto, z-score)"    = "unit variance|pareto|z-score|auto ?scal|scaling"),
    filter_groups = c(
      "OTU clustering / filtering"           = "\\botu",
      "ASV inference / denoising (DADA2)"    = "\\basv|denois|dada2",
      "Chimera removal"                      = "chimer",
      "Adapter / primer / read trimming"     = "adapter|primer|trimm",
      "Quality filtering"                    = "quality",
      "Length filtering"                     = "length",
      "Prevalence / abundance filtering"     = "prevalence|abundance|singleton|rare",
      "Read merging / demultiplexing"        = "merg|demultiplex",
      "Host / off-target / contaminant removal" = "host|off-target|contamin|mitochond|chloroplast",
      "Ambiguous base / homopolymer removal" = "ambiguous|homopolymer",
      "Taxonomic confidence threshold"       = "taxonom|confidence"),
    tool_groups = c(
      "QIIME / QIIME 2" = "qiime", "DADA2" = "dada2", "mothur" = "mothur",
      "VSEARCH / USEARCH / UPARSE" = "vsearch|usearch|uparse", "MetaPhlAn" = "metaphlan",
      "Kraken / Bracken" = "kraken|bracken", "HUMAnN" = "humann",
      "MicrobiomeAnalyst" = "microbiomeanalyst", "Vendor / array software" = "hugchip|agilent|illumina")
  ),
  Metabolome = list(
    label         = "Metabolome",
    layer_col     = "Metabolomics",
    feature_col   = find_col("^No\\. Features - Metabolomics", exact = FALSE),
    feature_codebook = list(
      field1_name = "Approach",
      field1 = c("untargeted", "targeted"),
      level = c("feature", "feature_pos", "feature_neg", "metabolite", "annotated",
                "differential", "voc", "scfa"),
      effect_levels = c("differential")),
    transform_col = "Transformation Method - Metabolome",
    filter_col    = "Filtering Criteria - Metabolome",
    missing_col   = "Handling of Missing Values - Metabolome",
    batch_col     = "Batch Correction / Confounder Adjustment - Metabolome",
    qc_col        = "Quality Control Samples / Internal Standards - Metabolome",
    tool_col      = "Profiling Tool",
    # Extra row in the by-approach tables: all studies with untargeted profiling
    approach_pooled = list("All untargeted (untargeted only + both)" = c("untargeted", "both")),
    transform_groups = c(
      "Log transformation"                          = "\\blog",
      "Scaling (unit variance, Pareto, z-score)"    = "unit variance|pareto|z-score|auto ?scal|range scal|scaling",
      "Internal standard normalization"             = "internal standard|\\bis\\b normal",
      "Centering"                                    = "centr|center",
      "Total-sum / total-ion-current normalization" = "total[- ]sum|total ion|\\btic\\b|sum normal",
      "Probabilistic quotient normalization (PQN)"  = "\\bpqn\\b|probabilistic",
      "Median / percentile normalization"           = "median|percentile",
      "Creatinine / weight / volume normalization"  = "creatinine|weight|volume|protein"),
    filter_groups = c(
      "Mass accuracy / retention-time tolerance"    = "mass accuracy|retention|\\brt\\b|ppm",
      "Missing-value imputation" = "imput",
      "Peak detection" = "peak detect",
      "Detection frequency / missing values" = "missing|detection|frequency|present in|replicate",
      "QC relative standard deviation"              = "rsd|\\bcv\\b|coefficient of variation",
      "Control / blank filtering"                   = "control|blank",
      "Statistical feature selection (VIP, p-value, fold change)" = "vip|model|p-value|q2|q²|pls|significan|fold",
      "Annotation / identification"                 = "annotat|identif|library|database",
      "Mass range / spectral region exclusion"      = "mass range|spectral|region|ppm range"),
    tool_groups = c(
      "XCMS" = "xcms", "MZmine" = "mzmine", "Chenomx" = "chenomx", "MS-DIAL" = "ms-dial|msdial",
      "Progenesis QI" = "progenesis", "Compound Discoverer" = "compound discoverer",
      "MassHunter / Mass Profiler (Agilent)" = "masshunter|mass profiler",
      "MetaboAnalyst" = "metaboanalyst", "Metabolon pipeline" = "metabolon",
      "TopSpin / MestReNova (NMR)" = "topspin|mestrenova|mnova")
  )
)


# =============================================================================
# One function produces the identical table structure for each omics layer
# =============================================================================
summarize_preprocessing <- function(spec) {
  L <- spec$label
  needed <- c(spec$layer_col, spec$transform_col,
              spec$filter_col, spec$missing_col, spec$batch_col, spec$qc_col, spec$tool_col)
  absent <- setdiff(needed, names(reviews))
  if (length(absent)) stop(L, ": columns not found: ", paste(absent, collapse = "; "))
  
  # Restrict every layer-specific numerator AND denominator before summarizing.
  eligible <- grepl("^yes", replace_na(reviews[[spec$layer_col]], ""), ignore.case = TRUE)
  study_label <- study_label[eligible]
  reviews <- reviews[eligible, , drop = FALSE]
  if (!nrow(reviews)) stop(L, ": no studies coded as profiling this layer.")
  add_note(paste0(L, " denominator: ", nrow(reviews), " studies profiling this layer."))
  
  # Profiling approach from the layer column, e.g. "Yes - untargeted" -> "untargeted"
  layer <- replace_na(reviews[[spec$layer_col]], "Missing (empty cell)")
  layer <- gsub("–", "-", gsub("—", "-", layer, fixed = TRUE), fixed = TRUE)  # en/em dash
  profiled <- grepl("^yes", layer, ignore.case = TRUE)
  approach <- ifelse(profiled,
                     trimws(sub("^yes\\s*-?\\s*", "", layer, ignore.case = TRUE)),
                     paste("Not profiled:", layer))
  approach[profiled & approach == ""] <- "Yes (approach not specified)"
  
  # ---- A. Feature dimensionality ----
  fc <- spec$feature_col
  cell_status <- tibble(x = reporting_status(replace_na(reviews[[fc]], "Missing (empty cell)")))
  cell_status$x[cell_status$x == "Reported"] <- "Count(s) coded"
  names(cell_status) <- fc
  show_table(count_table(cell_status, fc, "Feature-count cell",
                         levels = c("Count(s) coded", "Not reported", "Not applicable")),
             paste0(L, ": feature-count reporting"))
  
  records <- parse_features(reviews, fc, spec$feature_codebook)
  bad <- records %>% filter(!Valid) %>% transmute(Study = study_label[study], Record = raw, Problem)
  if (nrow(bad)) warning(L, ": ", nrow(bad), " feature-count records do not match the codebook; see the HTML report.")
  if (nrow(bad)) {
    show_table(with_sources(bad, fc), paste0(L, ": feature-count records not matching the codebook (fix in workbook)"))
  } else add_note(paste0("Codebook check: all ", nrow(records), " feature-count records match the codebook syntax."))
  
  coverage <- coverage_per_study(records)
  show_table(feature_summary(records, fc, coverage, nrow(reviews)),
             paste0(L, ": feature dimensionality per study (codebook rule; headline)"))
  basis_used <- coverage %>% count(basis_used) %>%
    transmute(`Basis used for the study's count` = basis_used, `Studies, n` = as.character(n))
  show_table(with_sources(basis_used, fc), paste0(L, ": basis of the headline count"))
  add_note(paste0("Headline rule: per study, the largest count with basis 'total'; if none, the largest with basis ",
                  "'filt'. Excluded: basis 'subset' and 'panel' (not discovery counts)",
                  if (length(spec$feature_codebook$effect_levels))
                    paste0(" and levels ", paste(spec$feature_codebook$effect_levels, collapse = ", "),
                           " (effect sizes, not coverage)") else "",
                  ". Ionization modes (feature_pos / feature_neg) are never summed; the larger one is used."))
  
  only_other <- setdiff(unique(records$study[records$Valid]), coverage$study)
  all_counts <- records %>% filter(!is.na(Count)) %>% group_by(study) %>%
    summarize(count = max(Count), .groups = "drop")
  show_table(feature_summary(records, fc, all_counts, nrow(reviews)),
             paste0(L, ": sensitivity, largest count of any kind per study (incl. panel, subset",
                    if (length(spec$feature_codebook$effect_levels)) ", differential" else "", ")"))
  add_note(paste0(length(only_other), " studies coded only panel, subset or differential counts and are ",
                  "therefore not in the headline range."))
  
  by_basis <- records %>% filter(Valid) %>%
    mutate(Basis = factor(tolower(Basis), levels = feature_bases)) %>%
    group_by(Basis, .drop = FALSE) %>%
    summarize(Studies = n_distinct(study), Records = n(),
              Median = fmt_num(median(Count)), Range = ifelse(n() > 0,
                                                              paste(fmt_num(min(Count)), "to", fmt_num(max(Count))), ""), .groups = "drop") %>%
    mutate(Basis = as.character(Basis))
  show_table(with_sources(by_basis, fc), paste0(L, ": feature counts by basis"))
  
  by_level <- records %>% filter(Valid) %>%
    mutate(Level = tolower(Level)) %>%
    group_by(Level) %>%
    summarize(Studies = n_distinct(study), Records = n(),
              Median = fmt_num(median(Count)),
              Range = paste(fmt_num(min(Count)), "to", fmt_num(max(Count))), .groups = "drop") %>%
    mutate(order = match(Level, spec$feature_codebook$level)) %>% arrange(order) %>% select(-order)
  show_table(with_sources(by_level, fc), paste0(L, ": feature counts by level"))
  
  # ---- B. Transformation ----
  show_table(status_table(reviews, spec$transform_col), paste0(L, ": transformation reported"))
  show_table(group_multi(reviews, spec$transform_col, spec$transform_groups,
                         "Transformation group", sep_transform),
             paste0(L, ": transformation methods, broad groups"))
  show_table(count_multi(reviews, spec$transform_col, "Transformation step", sep_transform),
             paste0(L, ": individual transformation steps"))
  show_table(count_table(reviews, spec$transform_col, "Transformation (full entry)"),
             paste0(L, ": transformation entries as extracted (sequence of steps)"))
  show_table(n_procedures_table(reviews, spec$transform_col, sep_transform, "Steps per study"),
             paste0(L, ": number of transformation steps per study"))
  
  # ---- C. Filtering ----
  show_table(status_table(reviews, spec$filter_col), paste0(L, ": filtering criteria reported"))
  show_table(group_multi(reviews, spec$filter_col, spec$filter_groups, "Filtering group", sep_list),
             paste0(L, ": filtering criteria, broad groups"))
  show_table(count_multi(reviews, spec$filter_col, "Filtering criterion", sep_list),
             paste0(L, ": individual filtering criteria"))
  show_table(heterogeneity_table(reviews, spec$filter_col, sep_list),
             paste0(L, ": heterogeneity of filtering criteria"))
  
  # ---- D. Missing values ----
  show_table(count_table(reviews, spec$missing_col, "Handling of missing values"),
             paste0(L, ": handling of missing values"))
  
  # ---- E. Batch correction ----
  show_table(count_table(reviews, spec$batch_col, "Batch correction / confounder adjustment"),
             paste0(L, ": batch correction / confounder adjustment"))
  batch_collapsed <- tibble(x = ifelse(tolower(replace_na(reviews[[spec$batch_col]], "")) == "applied",
                                       "Applied", "Not applied or not reported"))
  names(batch_collapsed) <- spec$batch_col
  show_table(count_table(batch_collapsed, spec$batch_col, "Batch correction / confounder adjustment (combined)",
                         levels = c("Applied", "Not applied or not reported")),
             paste0(L, ": batch correction / confounder adjustment, applied vs not applied or not reported"))
  
  # ---- F. QC ----
  show_table(count_table(reviews, spec$qc_col, "QC samples / internal standards"),
             paste0(L, ": QC samples / internal standards"))
  
  # ---- G. Tools ----
  show_table(status_table(reviews, spec$tool_col), paste0(L, ": profiling tool reported"))
  show_table(group_multi(reviews, spec$tool_col, spec$tool_groups, "Tool group", sep_tools),
             paste0(L, ": profiling tools, named groups"))
  show_table(count_multi(reviews, spec$tool_col, "Profiling tool", sep_tools),
             paste0(L, ": individual profiling tools"))
  show_table(heterogeneity_table(reviews, spec$tool_col, sep_tools),
             paste0(L, ": heterogeneity of tool combinations"))
  
  # ---- H. By profiling approach ----
  add_note("Denominator: studies within each profiling-approach group (row). Pooled rows combine groups.")
  pooled <- lapply(spec$approach_pooled, function(keys)
    unique(approach[tolower(approach) %in% tolower(keys)]))
  items <- list(
    "Transformation"   = list(col = spec$transform_col, f = reporting_status),
    "Filtering criteria" = list(col = spec$filter_col, f = reporting_status),
    "Handling of missing values" = list(col = spec$missing_col, f = identity),
    "Batch correction / confounder adjustment" = list(col = spec$batch_col, f = identity),
    "QC samples / internal standards" = list(col = spec$qc_col, f = identity),
    "Profiling tool"   = list(col = spec$tool_col, f = reporting_status))
  for (nm in names(items)) {
    it <- items[[nm]]
    category <- it$f(replace_na(reviews[[it$col]], "Missing (empty cell)"))
    show_table(within_group_wide(approach, category, c(spec$layer_col, it$col),
                                 "Profiling approach", pooled,
                                 category_levels = c("Reported", "Yes", "Applied")),
               paste0(L, ": ", nm, " by profiling approach (% within group)"))
  }
  
  # Values for the comparison table
  is_rep <- function(col) reporting_status(reviews[[col]]) == "Reported"
  tibble(
    Item = c("Feature count reported (codebook headline rule)", "Transformation reported",
             "Filtering criteria reported", "Missing-value handling reported",
             "Batch correction / confounder adjustment: applied or explicitly not applied",
             "Batch correction / confounder adjustment applied",
             "QC samples / internal standards used", "Profiling tool reported"),
    n = c(nrow(coverage),
          sum(is_rep(spec$transform_col)), sum(is_rep(spec$filter_col)),
          sum(is_rep(spec$missing_col)),
          sum(tolower(replace_na(reviews[[spec$batch_col]], "")) %in% c("applied", "not applied")),
          sum(tolower(replace_na(reviews[[spec$batch_col]], "")) == "applied"),
          sum(tolower(replace_na(reviews[[spec$qc_col]], "")) == "yes"),
          sum(is_rep(spec$tool_col))),
    profiled_n = sum(profiled),
    sources = list(c(spec$layer_col, spec$feature_col, spec$transform_col, spec$filter_col,
                     spec$missing_col, spec$batch_col, spec$qc_col, spec$tool_col))
  )
}


# =============================================================================
# Build the report
# =============================================================================
add_heading("Preprocessing: microbiome and metabolomics")

# Sparsity is kept because the metabolome text states that no study quantified it
show_table(count_table(reviews, "Sparsity", "Sparsity reported"), "Sparsity of the feature matrix")

results <- list()
for (s in names(omics_specs)) {
  results[[s]] <- summarize_preprocessing(omics_specs[[s]])
}

# ---- Side-by-side comparison of reporting completeness ----
comparison <- results$Microbiome %>% select(Item) %>%
  mutate(`Microbiome, n (% of profiled studies)` = n_pct(results$Microbiome$n, results$Microbiome$profiled_n),
         `Metabolomics, n (% of profiled studies)` = n_pct(results$Metabolome$n, results$Metabolome$profiled_n))
show_table(with_sources(comparison, unique(c(results$Microbiome$sources[[1]], results$Metabolome$sources[[1]]))),
           "Reporting completeness of preprocessing steps")
add_note(sprintf("Layer-specific denominators: microbiome %d; metabolomics %d. Studies not profiling a layer are excluded from both counts and percentages.",
                 results$Microbiome$profiled_n[1], results$Metabolome$profiled_n[1]))



# ---- Publication supplementary tables S12-S15 and Figure 4 --------------------
# ---- Figure 4 --------------------------------------------------------------
# a = microbiome and b = metabolomics transformations (top); c = reporting (bottom).
# Labels show % of studies; explicit negatives are shown as "Not reported".

# ---- Figure settings ---------------------------------------------------------
fig_dir <- "figures"
dir.create(fig_dir, showWarnings = FALSE, recursive = TRUE)
fig_width_mm  <- 210     # journal full-width limit (most journals: 170-183 mm)
fig_height_mm <- 240
fig_pointsize <- 10       # same as Figure 3
n_show        <- 8       # transformation methods shown individually in a and b
highlight_logratio <- FALSE  # TRUE = show CLR/ALR/ILR in gold

# Palette shared with Figure 3
col_dark1 <- pal("teal"); col_mid <- pal("light_teal"); col_gold <- pal("gold")
col_gray  <- pal("not_reported")  # "Not reported": darker than before so it survives print
col_na    <- pal("not_applicable")  # "Not applicable"
col_text  <- pal("text"); col_text2 <- pal("muted_text")

# ---- Data ---------------------------------------------------------------------
layer_data <- lapply(omics_specs, function(spec) {
  reviews[grepl("^yes", replace_na(reviews[[spec$layer_col]], ""), ignore.case = TRUE), , drop = FALSE]
})
layer_names <- c("Microbiome", "Metabolomics")
layer_N <- vapply(layer_data, nrow, integer(1))

# Status classification keeps explicit negatives separate from silence.
status_levels  <- c("Reported / applied", "Explicitly not applied", "Not reported", "Not applicable")
status_colors <- c(col_dark1, col_gold, col_gray, col_na)
figure_status <- function(x) {
  low <- tolower(trimws(replace_na(x, "")))
  # Explicit negatives (No, None, Not applied) are shown together with "Not reported".
  case_when(low %in% c("no", "not applied", "none") ~ status_levels[3],
            low == "not applicable" ~ status_levels[4],
            low %in% c("", "not reported", "missing", "nan") ~ status_levels[3],
            TRUE ~ status_levels[1])
}

# Pipeline order: filtering -> transformation -> missing values -> batch -> QC
figure_items  <- c("Filtering criteria", "Transformation / normalization", "Missing-value handling",
                   "Batch correction /\nconfounder adjustment", "Quality control samples / standards")
figure_fields <- c("filter_col", "transform_col", "missing_col", "batch_col", "qc_col")

plot_rows <- list()
for (k in seq_along(figure_items)) for (i in seq_along(omics_specs)) {
  dd <- layer_data[[i]]; spec <- omics_specs[[i]]
  status <- figure_status(dd[[spec[[figure_fields[k]]]]])
  plot_rows[[length(plot_rows) + 1L]] <- tibble(
    Item = gsub("\n", " ", figure_items[k]), Layer = layer_names[i], Status = status_levels,
    n = as.integer(table(factor(status, levels = status_levels))), N = nrow(dd)) %>%
    mutate(Percent = 100 * n / N)
}
figure_4c_data <- bind_rows(plot_rows)

# Synonyms merged before counting (a study is counted once per merged method).
# Extend this list if your extraction uses other spellings.
method_synonyms <- c(
  "tss"                       = "Total-sum scaling (TSS)",
  "total sum scaling"         = "Total-sum scaling (TSS)",
  "total-sum scaling"         = "Total-sum scaling (TSS)",
  "total sum normalization"   = "Total-sum scaling (TSS)",
  "total-sum normalization"   = "Total-sum scaling (TSS)",
  "relative abundance"        = "Total-sum scaling (TSS)",
  "relative abundances"       = "Total-sum scaling (TSS)",
  "clr"                       = clr_label,
  "absolute abundance"        = "Absolute quantification",
  "css"                       = "Cumulative-sum scaling (CSS)",
  "rpk"                       = "Reads per kilobase (RPK)",
  "log"                       = "Log transformation",
  "arcsine square root"       = "Arcsine square root",
  "centering"                 = "Mean centering")
harmonize_method <- function(x) {
  key <- tolower(trimws(x))
  out <- unname(method_synonyms[key])
  ifelse(is.na(out), x, out)
}

transformation_long <- lapply(seq_along(omics_specs), function(i) {
  split_values(layer_data[[i]], omics_specs[[i]]$transform_col, sep_transform) %>%
    mutate(value = ifelse(reporting_status(value) == "Reported", harmonize_method(value), "Not reported")) %>%
    distinct(study, value)
})
transformation_data <- lapply(seq_along(transformation_long), function(i)
  transformation_long[[i]] %>% count(value, sort = TRUE) %>%
    mutate(N = layer_N[i], Percent = 100 * n / N))

# Top n_show methods, then "Other methods" (studies counted once), then Not reported.
plot_transform_data <- lapply(seq_along(transformation_long), function(i) {
  d <- transformation_data[[i]]
  reported <- d %>% filter(value != "Not reported") %>% arrange(desc(n), value)
  top <- slice_head(reported, n = n_show)
  rest <- setdiff(reported$value, top$value)
  out <- top
  if (length(rest)) {
    k <- n_distinct(transformation_long[[i]]$study[transformation_long[[i]]$value %in% rest])
    out <- bind_rows(out, tibble(value = "Other methods",
                                 n = k, N = layer_N[i], Percent = 100 * k / layer_N[i]))
  }
  bind_rows(out, d %>% filter(value == "Not reported"))
})

# ---- Drawing helpers ----------------------------------------------------------
panel_title <- function(text) {
  mtext(text, side = 3, line = 0.7, adj = 0, at = par("usr")[1], font = 2, cex = 1.15, col = col_text)
}
pct_axis <- function(y) {
  axis(1, at = seq(0, 100, 20), labels = paste0(seq(0, 100, 20), "%"), pos = y,
       cex.axis = 1, lwd = 0.6, tcl = -0.3, mgp = c(2, 0.35, 0), col.axis = col_text)
}

# Panel c: reporting of preprocessing steps
draw_reporting <- function() {
  par(mar = c(0.6, 0.8, 3.0, 0.8), xaxs = "i", yaxs = "i", xpd = NA, family = "sans")
  n_items <- length(figure_items); step <- 3.5
  top <- n_items * step
  # Explicit space for the category column and a separate omics-layer column.
  plot.new(); plot.window(xlim = c(-80, 103), ylim = c(-2.3, top + 1.0))
  for (k in seq_len(n_items)) {
    yy <- top - (k - 1) * step
    text(-78, yy - 0.7, paste(strwrap(figure_items[k], width = 23), collapse = "\n"),
         adj = 0, cex = 0.95, col = col_text)
    for (i in 1:2) {
      y <- yy - (i - 1) * 1.4
      dd <- plot_rows[[(k - 1) * 2 + i]]
      right <- cumsum(dd$Percent); left <- c(0, head(right, -1)); mid <- (left + right) / 2
      rect(left, y - 0.35, right, y + 0.35, col = status_colors, border = "white", lwd = 0.5)
      text(-2, y, sprintf("%s (n = %d)", dd$Layer[1], dd$N[1]), adj = 1, cex = 0.9, col = col_text2)
      positive <- which(dd$n > 0)
      labels <- sprintf("%.0f%%", dd$Percent)
      fits <- dd$Percent >= strwidth(labels, cex = 0.9) + 1.5
      for (j in positive[fits[positive]])
        text(mid[j], y, labels[j], cex = 0.9, col = if (j == 1) "white" else col_text)
      outside <- positive[!fits[positive]]
      if (length(outside)) {
        label_x <- mid[outside]
        widths <- strwidth(labels[outside], cex = 0.9)
        if (length(outside) > 1) for (j in 2:length(outside))
          label_x[j] <- max(label_x[j], label_x[j-1] + (widths[j-1] + widths[j])/2 + 1)
        label_x <- label_x - max(0, max(label_x + widths/2) - 101)
        for (j in seq_along(outside)) {
          z <- outside[j]
          segments(mid[z], y + 0.35, label_x[j], y + 0.55, col = col_text, lwd = 0.6)
          text(label_x[j], y + 0.73, labels[z], cex = 0.9, col = col_text)
        }
      }
    }
  }
  pct_axis(1.2)
  text(50, -0.35, "Percentage of studies profiling each layer", col = col_text)
  used <- vapply(status_levels, function(z) any(figure_4c_data$n[figure_4c_data$Status == z] > 0), logical(1))
  legend(50, -1.5, xjust = 0.5, yjust = 0.5, ncol = 2, legend = status_levels[used],
         fill = status_colors[used], border = NA, bty = "n", cex = 0.95, x.intersp = 0.5)
  panel_title("c  Reporting of preprocessing steps, by omics layer")
}

# Panels a and b: individual transformation / normalization methods
draw_transform <- function(i, letter) {
  dd <- plot_transform_data[[i]]; m <- nrow(dd)
  labels <- dd$value
  labels[toupper(labels) == "RPKM"] <- "Reads per kilobase per million mapped reads (RPKM)"
  label_lines <- lapply(labels, strwrap, width = 23)
  row_heights <- pmax(1, lengths(label_lines) / 2)
  ys <- sum(row_heights) - cumsum(row_heights) + (row_heights + 1) / 2
  if (dd$value[m] == "Not reported") ys[m] <- ys[m] - 0.45   # set "Not reported" apart
  par(mar = c(4.0, 0.8, 4.4, 0.8), xaxs = "i", yaxs = "i", xpd = NA, family = "sans")
  plot.new(); plot.window(xlim = c(-66, 112), ylim = c(min(ys) - 0.55, max(ys) + 0.6))
  for (k in seq_len(m)) {
    is_nr <- dd$value[k] == "Not reported"
    fill <- if (is_nr) col_gray else
      if (highlight_logratio && grepl("clr|alr|ilr|log.ratio", dd$value[k], ignore.case = TRUE)) col_gold else col_mid
    rect(0, ys[k] - 0.3, dd$Percent[k], ys[k] + 0.3, col = fill, border = NA)
    text(-2, ys[k], paste(label_lines[[k]], collapse = "\n"), adj = 1,
         col = if (is_nr) col_text2 else col_text, font = if (is_nr) 3 else 1)
    text(dd$Percent[k] + 1.5, ys[k], sprintf("%.0f%%", dd$Percent[k]), adj = 0, cex = 0.9, col = col_text)
  }
  pct_axis(par("usr")[3])
  mtext(sprintf("Studies profiling %s (%%, n = %d)", tolower(layer_names[i]), layer_N[i]),
        side = 1, line = 1.9, cex = 1, col = col_text)
  panel_title(paste0(letter, "  ", layer_names[i], "\nTransformation / normalization"))
}

draw_figure_4 <- function() {
  layout(matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE), heights = c(1, 1.1))
  draw_transform(1, "a"); draw_transform(2, "b"); draw_reporting()
}

export_plot <- function(stem, draw, width_mm, height_mm, preview = FALSE) {
  w <- width_mm / 25.4; h <- height_mm / 25.4
  png_path <- tempfile(pattern = paste0(stem, "_"), fileext = ".png")
  if (preview) {
  grDevices::png(png_path, width = w, height = h, units = "in", res = 600,
                 pointsize = fig_pointsize, bg = "white")
  tryCatch(draw(), finally = grDevices::dev.off())
  }
  grDevices::pdf(file.path(fig_dir, paste0(stem, ".pdf")), width = w, height = h,
                 pointsize = fig_pointsize, bg = "white", useDingbats = FALSE)
  tryCatch(draw(), finally = grDevices::dev.off())
  png_path
}
figure_4_png <- export_plot("Figure_4_preprocessing", draw_figure_4, fig_width_mm, fig_height_mm, preview = TRUE)
export_plot("Figure_4a_microbiome_transformation", function() draw_transform(1, "a"), fig_width_mm / 2, 110)
export_plot("Figure_4b_metabolomics_transformation", function() draw_transform(2, "b"), fig_width_mm / 2, 110)
export_plot("Figure_4c_reporting", draw_reporting, fig_width_mm, 130)

# ---- Report: figure and plotted values ---------------------------------
add_heading("Figure 4. Preprocessing practices")
bytes <- readBin(figure_4_png, "raw", n = file.info(figure_4_png)$size)
figure4_sources <- unique(unlist(lapply(omics_specs, function(s) s[c("layer_col", figure_fields)])))
html_out(paste0('<p class="source">', html_escape(source_note(figure4_sources)), '</p>'))
html_out(paste0('<figure><img style="width:100%;height:auto" alt="Figure 4: preprocessing practices" src="data:image/png;base64,',
                encode_base64(bytes), '"></figure>'))
show_table(with_sources(figure_4c_data %>% mutate(Percent = sprintf("%.1f", Percent)),
                        figure4_sources),
           "Figure 4c: counts and layer-specific percentages")

for (i in 1:2) {
  shown <- plot_transform_data[[i]]$value
  tab <- transformation_data[[i]] %>%
    transmute(Method = value, Studies = n, Denominator = N, Percent = sprintf("%.1f", Percent),
              `Shown in panel` = ifelse(value %in% shown, "Yes, individually", "Pooled in 'Other methods'"))
  show_table(with_sources(tab, c(omics_specs[[i]]$layer_col, omics_specs[[i]]$transform_col)),
             paste0("Figure 4", c("a", "b")[i], ": all transformation-method counts (after merging synonyms)"))
}


# ---- Publication supplementary tables S12-S15 and HTML-only tool tables -------
captions <- c(
  "Reported combinations of microbiome normalization and transformation procedures.",
  "Sequence-processing and filtering procedures reported for microbiome data.",
  "Tools and pipelines reported for microbiome profiling and processing.",
  "Reported combinations of metabolomics normalization and transformation procedures.",
  "Filtering and feature-selection criteria reported for metabolomics data.",
  "Software reported for metabolomics processing and annotation.")
for (i in seq_along(omics_specs)) {
  spec <- omics_specs[[i]]; dd <- layer_data[[i]]
  tabs <- list(count_table(dd, spec$transform_col, "Transformation (full entry)"),
               count_multi(dd, spec$filter_col, "Filtering criterion", sep_list),
               count_multi(dd, spec$tool_col, "Profiling tool", sep_tools))
  if (i == 2L) {
    tab <- tabs[[2]]; v <- tab[[1]]
    tab$`Criterion category` <- case_when(
      grepl("^Total|^Not reported$|^Not applicable$|^Missing( \\(empty cell\\))?$|^None$|^No$", v, ignore.case = TRUE) ~ "Reporting status / denominator",
      grepl("vip|p-value|fdr|false discovery rate|fold|significan|q2|q²|pls", v, ignore.case = TRUE) ~ "Statistical feature selection",
      grepl("imput|peak detect|annotat|identif|library|database", v, ignore.case = TRUE) ~ "Other processing / annotation",
      grepl("mass accuracy|retention|ppm|missing|frequency|present in|replicate|rsd|cv|blank|control|mass range|spectral|region", v, ignore.case = TRUE) ~ "Data-quality filtering",
      TRUE ~ "Other processing / annotation")
    tabs[[2]] <- with_sources(tab, spec$filter_col)
  }
  for (j in 1:3) {
    k <- (i - 1L) * 3L + j
    note <- paste0("Denominator: ", nrow(dd), " studies profiling this layer. ",
                   if (j == 1L) "Full entries are retained as extracted; the written sequence does not establish the order of execution. " else
                     "Individual entries are retained; each study is counted once per entry and may contribute to several rows. Percentages may exceed 100% in total. ",
                   "Unreported procedures are not assumed to be absent.")
    if (j == 3L) {
      # Processing-tool tables remain in HTML only, without supplementary numbering.
      show_table(tabs[[j]], captions[k])
      add_note(note)
    } else {
      supplementary_number <- 11L + (i - 1L) * 2L + j
      show_supplementary_table(tabs[[j]],
                               paste0("Supplementary Table S", supplementary_number, ". ", captions[k]), note)
    }
  }
}


# ---- Export only manuscript supplementary tables S9-S15 ----------------------
expected_sheets <- paste("Supplementary Table", paste0("S", 9:15))
stopifnot(setequal(names(supplementary_export$tables), expected_sheets))
supplementary_tables <- supplementary_export$tables[expected_sheets]
add_heading("Supplementary tables for the manuscript")
html_out(unlist(supplementary_export$html[expected_sheets], use.names = FALSE))
finish_report()
cat("\nFinished. HTML report:", normalizePath(current_report),
    "\nFigures:", normalizePath(fig_dir), "\n")

# HTML previews have been embedded; no standalone PNG exports remain.
unlink(c(fig_png, figure_4_png))
