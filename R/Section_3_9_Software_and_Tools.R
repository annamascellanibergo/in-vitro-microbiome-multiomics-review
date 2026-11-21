# Manuscript section 3.9 Software & Tools
# Repository entry point: run from the repository root.
source("R/00_style.R", local = TRUE)
source("R/report_layout.R", local = TRUE)

# =============================================================================
# Scoping review: standalone section script
# Section 3.9 Software & Tools
# All tables are combined into ONE HTML report (results/Section_3_9_Software_and_Tools.html).
#
# Contents
#   A. Software packages and tools (number per study, individual tools)
#   B. Tools by function (broad groups)
#   C. Open-source vs proprietary software
#   D. Dedicated multi-omics integration software
#   E. Analysis environment (R, Python, MATLAB, command line, GUI)
#   F. Version information
#   G. Software reporting over time (descriptive, no statistical tests)
#
# Tool groups and the open-source / proprietary classification are made by text
# matching (settings below). Every tool is listed with its classification in the
# report so the coding can be checked and edited.
# Percentages use ALL loaded studies, unless a table says otherwise.
# =============================================================================

library(readxl)
library(dplyr)
library(tidyr)

# ---- Settings ---------------------------------------------------------------
data_file  <- getOption("review.data_file", "data/Supplementary Appendix 2.xlsx")
data_sheet <- getOption("review.data_sheet", "Data Extraction")               # sheet with the extracted data
report_dir <- "results"
expected_studies <- 102

# Separators used inside cells that list several methods (regular expressions)
sep_list      <- "\\s*[,;]\\s*"     # "Denoising, Prevalence filtering"; "mothur, VSEARCH"

# Values that mean "nothing reported" when deciding whether an item was reported
not_reported_values <- c("Not reported", "Not applicable", "None", "No", "Missing")

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

# Short study label for study-level tables
study_no_col <- col_info$col[col_info$name == "Progressive Study No"][1]
first_author <- sub("\\s+and\\s.*$|[,;].*$", "", replace_na(reviews$Authors, "Unknown"))
first_author <- paste0(first_author, ifelse(grepl("\\sand\\s|[,;]", replace_na(reviews$Authors, "")), " et al.", ""))
drop_decimal <- function(x) sub("\\.0+$", "", trimws(as.character(x)))  # "4.0" -> "4"
study_label  <- paste0(if (!is.na(study_no_col)) paste0("#", drop_decimal(reviews[[study_no_col]]), " ") else "",
                       first_author, " (", replace_na(drop_decimal(reviews$`Year of Publication`), "n.d."), ")")


# ---- Helper functions -------------------------------------------------------
fmt_num <- function(x) ifelse(is.na(x), "", trimws(formatC(x, format = "fg", big.mark = ",", digits = 6)))
quantile_rows <- function(x) {
  q <- if (length(x)) quantile(x, c(0, .25, .5, .75, 1), names = FALSE) else rep(NA, 5)
  tibble(Measure = c("Minimum", "Lower quartile", "Median", "Upper quartile", "Maximum"),
         Value = fmt_num(q))
}
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

# ---- HTML report (same layout as Section 1) ---------------------------------
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
start_report <- function(filename, title) {
  current_report <<- file.path(report_dir, filename)
  html_out(report_header(title, n_studies), append = FALSE)
}
finish_report <- function() html_out(report_footer)
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
  cat("\n===== ", title, " =====\n", sep = "")
  print(as.data.frame(tab), row.names = FALSE, right = FALSE)
  header <- paste0('<th>', html_escape(names(tab)), '</th>', collapse = '')
  rows <- if (nrow(tab)) vapply(seq_len(nrow(tab)), function(i) {
    cells <- vapply(tab, function(column) html_escape(column[i]), character(1))
    cls <- if (grepl("^Total", cells[1])) ' class="total"' else ''
    paste0('<tr', cls, '>', paste0('<td>', cells, '</td>', collapse = ''), '</tr>')
  }, character(1)) else '<tr><td>No records</td></tr>'
  body <- paste0('<thead><tr>', header, '</tr></thead><tbody>',
                 paste(rows, collapse = ''), '</tbody></table></div>')
  source <- paste0('<p class="source">', html_escape(source_note(sources)), '</p>')
  if (startsWith(title, "Supplementary Table S")) {
    html_out(paste0('<h2>Supplementary tables for the manuscript</h2>',
                    '<div class="table"><table><caption style="text-align:left;font-weight:bold;padding:12px 0;">',
                    html_escape(title), '</caption>', body, source))
  } else {
    html_out(paste0('<h2>', html_escape(title), '</h2>', source,
                    '<div class="table"><table>', body))
  }
}


# =============================================================================
# Section 9: software settings
# =============================================================================
tools_col   <- "Software Packages / Tools"
env_col     <- "Analysis Environment"
version_col <- "Version Information Reported"
version_levels <- c("Yes - all tools", "Yes - partial", "No", "Not reported")
sep_tools9  <- "\\s*[,;]\\s*"      # tools are separated by commas (or semicolons)
sep_env     <- "(?<![+])\\s*\\+\\s*(?![+])"  # "R + C++ + GUI": splits on +, keeps "C++" intact

# Tools by function, ordered along the analysis pipeline (first matching class wins;
# case-insensitive). Edit a class by adding names to its pattern.
tool_groups <- c(
  "Dedicated multi-omics integration" =
    "mixomics|diablo|\\bmofa|mmvec|xmwas|\\bm2ia|mimenet|omicsnet|mintea|rgcca|sgcca|mixkernel",
  "Sequencing read processing and quality control" =
    "fastp|fastqc|multiqc|trimmomatic|cutadapt|\\bflash\\b|\\bpear\\b|pandaseq|sickle|fastx|bbmap|bbduk|kneaddata|bowtie|\\bbwa\\b|samtools|bedtools|seqkit|nanofilt|nanoplot|pycoqc|guppy|minknow|bcl2fastq|torrent suite|ion reporter|smrt link|smrt portal|^lima$|sortmerna|minimap|soapaligner|porechop",
  "Amplicon processing and taxonomic classification" =
    "qiime|dada2|mothur|uparse|usearch|vsearch|uchime|uclust|chimeraslayer|\\brdp\\b|blast|lotus|\\bemu\\b|rescript|pynast|mafft|muscle|fasttree|\\bmega\\b|phangorn|^ape$|emperor|krona|ngless|metabarpipe|kraken|bracken|metaphlan|strainphlan|kaiju|megan\\b",
  "Metagenome / metatranscriptome assembly, binning and annotation" =
    "megahit|spades|soapdenovo|\\bcanu\\b|trinity|quast|bandage|prodigal|metagenemark|^metagene$|glimmer|prokka|barrnap|aragorn|trna-scan|cd-hit|mmseqs|diamond|hmmer|eggnog|dbcan|\\bdram\\b|humann|biobakery|metabat|maxbin|metabinner|checkm|gtdb|drep|refinem|comparem|busco|anvi|squeezemeta|sunbeam|salmon|rsem|transdecoder|minpath|gutsmash|microtrait|demic|gapseq|bacarena|sybil",
  "Functional prediction from marker genes" = "picrust|tax4fun|faprotax|bugbase",
  "Metabolomics and proteomics data processing" =
    "xcms|mzmine|progenesis|compound discoverer|masshunter|mass profiler|ms-dial|msdial|chenomx|topspin|amix|mestrenova|mnova|empower|xcalibur|chemstation|openlab|analyzer pro|dataanalysis|camera|metaboprofile|nist|peakview|sieve|skyline|tmbq|turbomass|tracefinder|masslynx|unifi|databridge|feature extraction|metabolon|gnps|sirius|csi:fingerid|canopus|proteowizard|msconvert|mscombine|analysis base file converter|metax|metdna|mycompoundid|peaks studio|metabominer|analyst tf|lipidsearch",
  "Web-based analysis platforms" =
    "microbiomeanalyst|metaboanalyst|calypso|omicstudio|wekemo|omicsmart|oecloud|majorbio|galaxy|bmkcloud|novomagic|gencloud|personalbio|chiplot|heatmapper|boxplotr|volcanoser|metorigin",
  "Microbiome and omics statistics (R / Python packages)" =
    "vegan|phyloseq|lefse|maaslin|deseq|edger|limma|linda|ancom|aldex|stamp|metastats|microeco|microviz|microbiotaprocess|treeclimbr|pairwiseadonis|ropls|wgcna|mfuzz|divcomanalyses|netcomi|mlr3|caret|randomforest|glmnet|scikit|scipy|xgboost|proc\\b|lme4|lmertest|emmeans|rstatix|pmcmrplus|^mass$|hmisc|rmcorr|permute|factoextra|adegenet|stats4|lattice|corrplot|sparcc|spiec",
  "Multivariate statistics software" = "simca|canoco|\\bpast\\b|primer|unscrambler",
  "General-purpose statistics and graphing" =
    "spss|graphpad|prism|\\borigin\\b|statistica|\\bsas\\b|excel|minitab|genstat|kaleidagraph|sigmaplot|design-expert|\\bjmp\\b|stata|xlstat",
  "Pathway and enrichment analysis" =
    "revigo|pathway tools|gsea|go::termfinder|chemrich|mb ?role|multiple list comparator|kegg mapper|ipath|metacyc",
  "Visualization and data handling" =
    "ggplot|pheatmap|complexheatmap|circlize|rcolorbrewer|venndiagram|ggvenn|ggpubr|^grid$|cytoscape|gephi|itol|gene-e|illustrator|inkscape|tidyverse|plyr|reshape",
  "Laboratory, imaging and structural software" =
    "imagej|image ?pro|flowjo|accuri|cflow|lightcycler|autodock|pymol|open babel|lucullus|nutritionist|smart nose|omnic|^astra$|omicsoft|c-bio|custom|in-house|delphi")
tool_groups <- tool_groups[!duplicated(names(tool_groups))]

# Proprietary (commercial / licensed) software; everything else counts as open / free
proprietary_regex <- paste0(
  "spss|graphpad|prism|\\borigin\\b|genstat|kaleidagraph|\\bsas\\b|stata|\\bjmp\\b|minitab|statistica|excel|sigmaplot|xlstat|",
  "design-expert|simca|unscrambler|canoco|primer|matlab|masshunter|mass profiler|progenesis|compound discoverer|chenomx|",
  "topspin|amix|mestrenova|mnova|empower|xcalibur|chemstation|openlab|analyzer pro|dataanalysis|metaboprofile|nist|peakview|",
  "sieve|tmbq|turbomass|tracefinder|masslynx|unifi|databridge|feature extraction|metabolon|analyst tf|peaks studio|omnic|",
  "smart nose|lipidsearch|majorbio|bmkcloud|novomagic|omicstudio|wekemo|omicsmart|oecloud|personalbio|gencloud|ion reporter|",
  "torrent suite|c-bio|illustrator|flowjo|accuri|cflow|image ?pro|lightcycler|lucullus|nutritionist|^astra$|omicsoft")

# Environment entries counted as scripted programming
scripted_regex <- "^r$|python|matlab|julia|c\\+\\+|delphi|perl|sas|stata|java|bash|command"

needed <- c(tools_col, env_col, version_col)
if (length(setdiff(needed, names(reviews))))
  stop("Columns not found: ", paste(setdiff(needed, names(reviews)), collapse = "; "))

start_report("Section_3_9_Software_and_Tools.html", "3.9 Software & Tools")

# ---- A. Tools ---------------------------------------------------------------
reviews[[tools_col]] <- gsub("\\", "", reviews[[tools_col]], fixed = TRUE)   # e.g. "VSEARC\H" -> "VSEARCH"
tools_long <- split_values(reviews, tools_col, sep_tools9) %>%
  filter(reporting_status(value) == "Reported")
show_table(status_table(reviews, tools_col, "Software reported"), "Software or tools reported")
per_study <- tibble(study = seq_len(n_studies)) %>%
  left_join(tools_long %>% count(study, name = "k"), by = "study") %>%
  mutate(k = replace_na(k, 0L))
show_table(with_sources(bind_rows(
  tibble(Measure = c("Studies naming at least one tool", "Distinct tools across studies", "Mean tools per study"),
         Value = c(n_pct(sum(per_study$k > 0), n_studies), as.character(n_distinct(tools_long$value)),
                   fmt_num(round(mean(per_study$k[per_study$k > 0]), 1)))),
  quantile_rows(per_study$k[per_study$k > 0]) %>% mutate(Measure = paste("Tools per study:", tolower(Measure)))),
  tools_col), "Number of tools per study (studies naming at least one tool)")
k_class <- tibble(x = cut(per_study$k, c(-Inf, 0, 2, 5, 10, Inf),
                          labels = c("None named", "1-2", "3-5", "6-10", ">10")) %>% as.character())
names(k_class) <- tools_col
show_table(count_table(k_class, tools_col, "Tools per study", levels = c("None named", "1-2", "3-5", "6-10", ">10")),
           "Tools per study, classes")
tool_freq <- tools_long %>% count(value, sort = TRUE) %>%
  mutate(Group = classify_values(value, tool_groups),
         License = ifelse(grepl(proprietary_regex, value, ignore.case = TRUE), "Proprietary", "Open / free"))
show_table(with_sources(tool_freq %>% transmute(Tool = value, `Studies, n (%)` = n_pct(n, n_studies),
                                                `Function group` = Group, License), tools_col),
           "Individual tools with their classification (check)")
# Publication supplementary table; exported centrally with S1–S15.
supp_table_S16 <- tool_freq %>%
  arrange(match(Group, c(names(tool_groups), "Other (see individual entries)")), desc(n), value) %>%
  transmute(`Function group` = Group, Tool = value, `Studies, n (%)` = n_pct(n, n_studies)) %>%
  with_sources(tools_col)
caption_S16 <- supplementary_caption("Supplementary Table S16. Software packages and tools reported across the included studies.")
supplementary_tables <- list("Supplementary Table S16" = list(table = as.data.frame(supp_table_S16), caption = caption_S16))

used_once <- sum(tool_freq$n == 1)
add_note(paste0(nrow(tool_freq), " distinct tools; ", used_once, " (", round(100 * used_once / max(1, nrow(tool_freq)), 1),
                "%) were used in a single study."))

# ---- B. Tools by function ---------------------------------------------------
show_table(group_multi(reviews %>% mutate(!!tools_col := ifelse(reporting_status(.data[[tools_col]]) == "Reported",
                                                                .data[[tools_col]], NA)),
                       tools_col, tool_groups, "Function group", sep_tools9) %>%
             filter(!grepl("^Missing", `Function group`)),
           "Studies using at least one tool in each function group")
groups_per_study <- tools_long %>% mutate(g = classify_values(value, tool_groups)) %>%
  group_by(study) %>% summarize(n_groups = n_distinct(g), .groups = "drop")
show_table(with_sources(tibble(Measure = "Median function groups per study (studies naming tools)",
                               Value = fmt_num(median(groups_per_study$n_groups))), tools_col),
           "Breadth of the software stack")

# ---- C. Open-source vs proprietary ------------------------------------------
lic <- tools_long %>% mutate(prop = grepl(proprietary_regex, value, ignore.case = TRUE)) %>%
  group_by(study) %>% summarize(any_prop = any(prop), all_prop = all(prop), .groups = "drop")
lic_class <- rep("No tool named", n_studies)
lic_class[lic$study] <- ifelse(lic$all_prop, "Proprietary only", ifelse(lic$any_prop, "Open and proprietary", "Open / free only"))
tmp <- tibble(x = lic_class); names(tmp) <- tools_col
show_table(count_table(tmp, tools_col, "Software license mix",
                       levels = c("Open / free only", "Open and proprietary", "Proprietary only", "No tool named")),
           "Open-source and proprietary software per study")
add_note("License is assigned by text matching (proprietary_regex in the script); tools not matched count as open / free.")

# ---- D. Dedicated integration software --------------------------------------
integ <- tools_long %>% filter(grepl(tool_groups[["Dedicated multi-omics integration"]], value, ignore.case = TRUE))
tmp <- tibble(x = ifelse(seq_len(n_studies) %in% integ$study, "Yes", "No")); names(tmp) <- tools_col
show_table(count_table(tmp, tools_col, "Dedicated integration software named", levels = c("Yes", "No")),
           "Studies naming dedicated multi-omics integration software")
if (nrow(integ)) show_table(with_sources(integ %>% transmute(Study = study_label[study], Tool = value), tools_col),
                            "Dedicated integration software by study")

# ---- E. Analysis environment ------------------------------------------------
show_table(count_multi(reviews, env_col, "Environment component", sep_env),
           "Analysis environment components (a study can use several)")
show_table(count_table(reviews, env_col, "Analysis environment (full entry)"),
           "Analysis environment combinations as extracted")
env_long <- split_values(reviews, env_col, sep_env) %>% filter(reporting_status(value) == "Reported")
env_flags <- env_long %>% group_by(study) %>%
  summarize(R = any(tolower(value) == "r"), Python = any(grepl("python", value, ignore.case = TRUE)),
            scripted = any(grepl(scripted_regex, value, ignore.case = TRUE, perl = TRUE)),
            gui = any(grepl("gui|web|online", value, ignore.case = TRUE)), .groups = "drop")
lang <- rep("Not reported", n_studies)
lang[env_flags$study] <- case_when(env_flags$R & env_flags$Python ~ "R and Python",
                                   env_flags$R ~ "R (no Python)",
                                   env_flags$Python ~ "Python (no R)",
                                   env_flags$scripted ~ "Command-line or other scripted only",
                                   TRUE ~ "GUI / web tools only")
tmp <- tibble(x = lang); names(tmp) <- env_col
show_table(count_table(tmp, env_col, "Programming environment",
                       levels = c("R (no Python)", "Python (no R)", "R and Python", "Command-line or other scripted only",
                                  "GUI / web tools only", "Not reported")),
           "Programming environment per study")
code_col <- intersect("Code Available", names(reviews))
if (length(code_col)) {
  scripted_group <- ifelse(lang %in% c("GUI / web tools only", "Not reported"), lang, "Scripted (R, Python or other)")
  show_table(within_group_wide(scripted_group, replace_na(reviews[[code_col]], "Missing (empty cell)"),
                               c(env_col, code_col), "Programming environment",
                               category_levels = c("Yes", "No", "Partial")),
             "Code availability by programming environment (% within group)")
}

# ---- F. Version information -------------------------------------------------
show_table(count_table(reviews, version_col, "Version information", levels = version_levels),
           "Software version information reported")
show_table(within_group_wide(cut(per_study$k, c(-Inf, 0, 5, 10, Inf), labels = c("None named", "1-5 tools", "6-10 tools", ">10 tools")) %>% as.character(),
                             replace_na(reviews[[version_col]], "Missing (empty cell)"),
                             c(tools_col, version_col), "Tools named", category_levels = version_levels,
                             group_levels = c("1-5 tools", "6-10 tools", ">10 tools", "None named")),
           "Version information by number of tools named (% within group)")

# ---- G. Over time (descriptive) ---------------------------------------------
year <- suppressWarnings(as.numeric(reviews$`Year of Publication`))
period_breaks <- c(-Inf, 2020, 2022, 2024, Inf)            # edit to change the periods
period_labels <- c("Up to 2020", "2021-2022", "2023-2024", "2025 onwards")
period <- as.character(cut(year, period_breaks, labels = period_labels))
period[is.na(period)] <- "Year missing"
items <- list(
  "At least one tool named"                 = per_study$k > 0,
  "Version information for all tools"       = reviews[[version_col]] %in% "Yes - all tools",
  "Version information for some or all tools" = reviews[[version_col]] %in% c("Yes - all tools", "Yes - partial"),
  "R used"                                  = lang %in% c("R (no Python)", "R and Python"),
  "Python used"                             = lang %in% c("Python (no R)", "R and Python"),
  "GUI / web tools only"                    = lang == "GUI / web tools only",
  "Dedicated integration software named"    = seq_len(n_studies) %in% integ$study,
  "Proprietary software only"               = lic_class == "Proprietary only")
per_n <- table(factor(period, levels = c(period_labels, "Year missing")))
per_n <- per_n[per_n > 0]
rows <- lapply(names(items), function(it) {
  x <- items[[it]]
  as_tibble(as.list(c(Item = it, vapply(names(per_n), function(pr)
    n_pct(sum(x[period == pr]), sum(period == pr)), character(1)))))
})
show_table(with_sources(bind_rows(rows), c("Year of Publication", tools_col, env_col, version_col)),
           "Software reporting by publication period, n (% of studies in the period)")
add_note(paste0("Studies per period: ", paste(names(per_n), per_n, sep = " = ", collapse = "; "), "."))

show_table(supp_table_S16, caption_S16)
finish_report()
cat("\nFinished. HTML report:", normalizePath(current_report), "\n")
