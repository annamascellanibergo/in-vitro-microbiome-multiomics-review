# Manuscript section 3.10 Validation, Reproducibility and Quality Indicators
# Repository entry point: run from the repository root.
source("R/00_style.R", local = TRUE)
source("R/report_layout.R", local = TRUE)

# =============================================================================
# 3.10 Validation, Reproducibility and Quality Indicators
#
# Run from the repository root.
# The HTML report lists each result table with the corresponding workbook fields.
# =============================================================================

data_file        <- getOption("review.data_file", "data/Supplementary Appendix 2.xlsx")
data_sheet       <- getOption("review.data_sheet", "Data Extraction")
output_dir       <- "results"
output_name      <- "Section_3_10_Validation_Reproducibility_and_Quality_Indicators.html"
expected_studies <- 102L   # comparison only; NEVER used as a denominator

# Header of the column that records whether any analysis was run in a scripted /
# command-line environment (needed for the "93% ... of these shared code"
# sentence). NULL = auto-detect a header containing "script" or "command line".
# If auto-detection fails, set the exact header text here.
scripted_field   <- NULL

# Optional manual adjudication, one row per DOI (leave empty to skip).
# partial_sequencing_only: "Yes" / "No", only for studies coded Partly raw data.
overrides <- data.frame(DOI = character(), partial_sequencing_only = character(),
                        stringsAsFactors = FALSE)

# ---- Setup ---------------------------------------------------------------------
if (!requireNamespace("readxl", quietly = TRUE))
  stop("Install readxl first: install.packages('readxl')")
if (!file.exists(data_file) && interactive()) data_file <- file.choose()
if (!file.exists(data_file)) stop("Workbook not found: ", data_file,
  "\nSet data_file to the workbook's full path, or set your working directory.")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

clean <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  trimws(gsub("[[:space:]\u00a0]+", " ", x))
}
normal_doi <- function(x) sub("^(https?://(dx\\.)?doi\\.org/|doi:[[:space:]]*)",
                              "", clean(x), ignore.case = TRUE)

# ---- Read two header rows; select fields by NAME, not Excel position ----------
source("R/read_extraction.R", local = TRUE)
extraction <- read_extraction(data_file, data_sheet)
headers <- extraction$columns$name
dat <- extraction$data
excel_rows <- extraction$excel_rows
column <- function(name) {
  # Accept the historical "on tool" typo as well as the corrected header.
  key <- function(x) gsub("at least on tool", "at least one tool", tolower(clean(x)), fixed = TRUE)
  i <- which(key(headers) == key(name))
  if (length(i) != 1L)
    stop("Expected exactly one column named '", name, "'; found ", length(i))
  as.character(dat[[i]])
}
N <- nrow(dat)
if (N == 0L) stop("No studies found in the selected sheet.")
if (N != expected_studies) warning("Loaded ", N, " studies, not ", expected_studies,
  ". Results describe only the loaded rows; all percentages use actual denominators.")

doi       <- normal_doi(column("DOI"))
valid_doi <- grepl("^10\\.[0-9]{4,9}/\\S+$", doi)
dup       <- duplicated(tolower(doi)) & valid_doi
if (any(dup)) stop("Duplicate DOI(s); resolve before counting: ",
                   paste(doi[dup], collapse = "; "))
study <- ifelse(valid_doi, doi, paste0("DOI missing/invalid [Excel row ", excel_rows, "]"))

# ---- Shared helpers ------------------------------------------------------------
issues <- data.frame(DOI = character(), Field = character(), Original = character(),
                     Issue = character(), stringsAsFactors = FALSE)
flag <- function(which_rows, field, original, reason) {
  i <- which(which_rows)
  if (length(i)) issues <<- rbind(issues, data.frame(DOI = study[i], Field = field,
    Original = as.character(original[i]), Issue = reason, stringsAsFactors = FALSE))
}
flag(!valid_doi, "DOI", column("DOI"), "Missing/invalid DOI; Excel row used as fallback")

# One coder for every Yes / Partly / No style field (validation and availability).
code_field <- function(x, field) {
  z <- tolower(clean(x)); out <- rep("Other / review", length(z))
  out[z == ""] <- "Missing"
  out[z %in% c("not reported", "unknown", "unclear")] <- "Not reported"
  out[z %in% c("not applicable", "n/a", "na")] <- "Not applicable"
  out[z %in% c("no", "none")] <- "No"
  out[grepl("^(partly|partial|partially)($|[[:space:]:;,(-])", z)] <- "Partly"
  out[grepl("^yes($|[[:space:]:;,(-])", z)] <- "Yes"
  flag(out == "Other / review", field, x, "Unrecognized entry; inspect source")
  out
}

pct <- function(n, d) {
  # Divide first so a shared denominator yields one percentage per numerator.
  p <- round(100 * n / d, 1); p[!is.finite(p)] <- NA_real_; p
}
metric <- function(label, n, d = N, population = "All studies") data.frame(
  Measure = label, Studies = as.integer(n), Denominator = as.integer(d),
  Percent = pct(n, d), Population = population, stringsAsFactors = FALSE)
freq <- function(x, d = N, population = "All studies") {
  tt <- sort(table(x), decreasing = TRUE)
  if (!length(tt)) return(data.frame(Measure = character(), Studies = integer(),
    Denominator = integer(), Percent = numeric(), Population = character()))
  metric(names(tt), as.integer(tt), d, population)
}
# whole-number percentage for manuscript sentences
pc <- function(n, d) if (d == 0) "[undefined: denominator 0]" else sprintf("%.0f%%", 100 * n / d)

# =============================================================================
# ---- PART A. VALIDATION (any analytical/modeling tool used within a study) ----
# =============================================================================
vfields <- c(
  Required    = "Study Employing at Least One Tool Requiring Validation",
  Internal    = "Internal Validation",
  External    = "External Validation",
  Sensitivity = "Sensitivity Analyses",
  Benchmark   = "Simulation / Benchmarking Datasets",
  Interpretation = "Biological Interpretation Level")
src <- lapply(vfields, column)

required    <- code_field(src$Required,    vfields[["Required"]])
eligible    <- required == "Yes"
E           <- sum(eligible)
external    <- code_field(src$External,    vfields[["External"]])
sensitivity <- code_field(src$Sensitivity, vfields[["Sensitivity"]])
benchmark   <- code_field(src$Benchmark,   vfields[["Benchmark"]])

# ---- Internal validation: keep original entries and multi-response methods ------
# Only explicit method terms are recognized. CV-ANOVA is kept separate from
# predictive cross-validation; out-of-bag evaluation is also separate.
# The observed typo 'permulation' is an alias for permutation, with a review note.
v <- tolower(clean(src$Internal))
v <- gsub("[\u2010-\u2015]", "-", v)
typo <- grepl("permulation", v)
v <- gsub("permulation", "permutation", v)
flag(typo, vfields[["Internal"]], src$Internal,
     "Observed spelling 'permulation' interpreted as permutation; original retained")
cv_anova      <- grepl("cv[ -]?anova", v)
without_anova <- gsub("cv[ -]?anova", "", v)
methods <- data.frame(
  `Cross-validation`    = grepl("cross[ -]?valid|\\bcv\\b|loo[ -]?cv|[0-9]+[ -]?fold", without_anova),
  `Permutation testing` = grepl("permutation", v),
  `Train/test split`    = grepl("train[ /-]*(test|validation)|hold[ -]?out", v),
  Bootstrap             = grepl("bootstrap", v),
  `Out-of-bag assessment` = grepl("out[ -]of[ -]bag|\\boob\\b", v),
  `CV-ANOVA`            = cv_anova, check.names = FALSE)
recognized     <- rowSums(methods) > 0L
none           <- v %in% c("none", "no", "no validation", "not performed", "not applied")
unknown        <- v %in% c("", "not reported", "unknown", "unclear")
not_applicable <- v %in% c("not applicable", "n/a", "na")
status <- ifelse(recognized, "Reported", ifelse(none, "None",
  ifelse(unknown, "Missing / not reported", ifelse(not_applicable,
    "Not applicable", "Other / review"))))
status[v == "yes"] <- "Reported (unspecified method)"
# Possible negation / qualified wording is reviewed, not silently counted.
ambiguous <- recognized & grepl("\\b(no|not|without|unclear)\\b", v)
status[ambiguous] <- "Other / review"
flag(status == "Other / review", vfields[["Internal"]], src$Internal,
     "Validation entry needs review; excluded from definitive reported/none counts")
reported <- status %in% c("Reported", "Reported (unspecified method)")
methods[!reported, ] <- FALSE
combination <- vapply(seq_len(N), function(i) {
  found <- names(methods)[as.logical(unlist(methods[i, ], use.names = FALSE))]
  if (length(found)) paste(found, collapse = " + ") else status[i]
}, character(1))
flag(!eligible & reported, vfields[["Required"]], src$Required,
     "Internal validation reported but validation requirement is not coded Yes")
flag(eligible & status == "Not applicable", vfields[["Internal"]], src$Internal,
     "Validation required but internal validation coded not applicable")

# Studies that required validation and report no internal validation
# (explicit None + missing/not reported): the "reported none" group in the text.
no_internal <- eligible & status %in% c("None", "Missing / not reported")
independent_cohort <- external == "Yes" &
  grepl("independent cohort", tolower(clean(src$External)))

# "Alone" = exactly one recognized method (plus the unspecified-method label).
single_methods <- names(methods)
alone <- eligible & reported & combination %in% single_methods

# =============================================================================
# ---- PART B. BIOLOGICAL INTERPRETATION (each study counted once per level) ----
# =============================================================================
interp_levels <- c("Taxa-metabolite links", "Pathway-level", "System-level", "Feature-level")
parse_levels <- function(x) {
  z <- tolower(clean(x)); z <- gsub("[\u2010-\u2015]", "-", z)
  if (!nzchar(z)) return("Missing")
  tokens <- clean(unlist(strsplit(z, "\\s*(?:\\+|;|,|\\||\\n)\\s*", perl = TRUE)))
  tokens <- tokens[nzchar(tokens)]
  canonical <- tolower(interp_levels)
  tokens <- ifelse(tokens %in% canonical, interp_levels[match(tokens, canonical)], tokens)
  sort(unique(tokens))
}
tokens <- lapply(src$Interpretation, parse_levels)
interp <- do.call(cbind, lapply(interp_levels, function(l)
  vapply(tokens, function(t) l %in% t, logical(1))))
colnames(interp) <- interp_levels
interp_combo <- vapply(tokens, paste, collapse = " + ", character(1))
unrecognized <- vapply(tokens, function(t)
  any(!t %in% c(interp_levels, "Missing", "not reported", "none", "not applicable")),
  logical(1))
flag(unrecognized, vfields[["Interpretation"]], src$Interpretation,
     "Unrecognized interpretation label; retained in combinations and not assigned to known levels")
taxa    <- interp[, "Taxa-metabolite links"]
pathway <- interp[, "Pathway-level"]
system_ <- interp[, "System-level"]
feature <- interp[, "Feature-level"]
taxa_only    <- vapply(tokens, function(t) identical(t, "Taxa-metabolite links"), logical(1))
pathway_only <- vapply(tokens, function(t) identical(t, "Pathway-level"), logical(1))

# =============================================================================
# ---- PART C. REPRODUCIBILITY (data, processed data, code, workflow systems) ----
# =============================================================================
raw_status       <- code_field(column("Raw Data Available"),       "Raw Data Available")
code_status      <- code_field(column("Code Available"),           "Code Available")
processed_status <- code_field(column("Processed Data Available"), "Processed Data Available")
raw_notes <- clean(column("Link to Raw Data"))
workflow  <- clean(column("Workflow Management System"))
any_raw   <- raw_status  %in% c("Yes", "Partly")
any_code  <- code_status %in% c("Yes", "Partly")
A <- sum(any_raw)

# Layer-specific evidence is separate from the study-wide availability code.
# Count each study once per layer. An accession is evidence of a deposit;
# confirmation of raw metabolomics files additionally requires a file listing.
seq_accession <- grepl("PRJ(NA|EB|DB)[0-9]+|[SED]RP[0-9]+|CRA[0-9]+", raw_notes, ignore.case = TRUE)
seq_record <- any_raw & (seq_accession | grepl("shotgun metagenome", raw_notes, ignore.case = TRUE))
met_profiled <- grepl("^Yes", clean(column("Metabolomics")), ignore.case = TRUE)
met_record <- any_raw & met_profiled & grepl(
  "MTBLS[0-9]+|metabolomicsworkbench[.]org|metabolome.{0,100}OMIX[0-9]+",
  raw_notes, ignore.case = TRUE)
# Repository records checked for the published corpus. Match study and deposit
# together so unrelated entries or subsequent accession changes require review.
raw_checks <- data.frame(
  DOI = c("10.3390/nu15010042", "10.3389/fmicb.2022.930392", "10.1039/d3fo02246c"),
  identifier = c("ST002338", "severinezirah_20211113_140942", "OMIX004477"),
  evidence = c("Workbench ST002338: download marked Contains raw data",
               "Workbench ST002014: download marked Contains raw data",
               "OMIX004477-01: raw.tar.gz, 11.5 GB"))
idx <- match(tolower(doi), tolower(raw_checks$DOI))
raw_met_confirmed <- rep(FALSE, N)
for (i in which(met_record & !is.na(idx))) {
  raw_met_confirmed[i] <- grepl(raw_checks$identifier[idx[i]], raw_notes[i], fixed = TRUE)
}
layer_summary <- rbind(
  metric("Identifiable sequencing deposit in raw-data field", sum(seq_record)),
  metric("Identifiable metabolomics deposit in raw-data field", sum(met_record)),
  metric("Repository explicitly identifies raw metabolomics files", sum(raw_met_confirmed)),
  metric("Identifiable metabolomics deposit among studies profiling metabolomics",
         sum(met_record), sum(met_profiled), "Studies profiling metabolomics"))
availability_table <- rbind(
  metric("Raw data reported as fully or partially available", sum(any_raw)),
  metric("Fully available", sum(raw_status == "Yes")),
  metric("Partially available", sum(raw_status == "Partly")),
  metric("No raw data available", sum(raw_status == "No")),
  layer_summary[1:3, ])
availability_table <- availability_table[c("Measure", "Studies", "Denominator", "Percent")]
confirmed_row <- nrow(availability_table)
availability_table$Measure[confirmed_row] <- "Confirmed raw metabolomics files (minimum)"
layer_audit <- data.frame(Study = column("Progressive Study No"), DOI = doi,
  Raw_status = raw_status, Sequencing_deposit = seq_record, Metabolomics_deposit = met_record,
  Extraction_entry = raw_notes, stringsAsFactors = FALSE)[any_raw, , drop = FALSE]

# Scripted / command-line environment (denominator for the code-sharing sentence)
if (is.null(scripted_field)) {
  cand <- unique(headers[grepl("script|command[ -]?line", headers, ignore.case = TRUE)])
  scripted_field <- if (length(cand) == 1L) cand else NA_character_
  if (is.na(scripted_field) && any(tolower(headers) == "analysis environment"))
    scripted_field <- headers[match("analysis environment", tolower(headers))]
}
have_scripted <- length(scripted_field) == 1L && !is.na(scripted_field) &&
  any(tolower(headers) == tolower(scripted_field))
if (have_scripted && tolower(scripted_field) == "analysis environment") {
  env <- tolower(clean(column(scripted_field)))
  env_parts <- strsplit(env, "(?<![+])\\s*\\+\\s*(?![+])|[;,]", perl = TRUE)
  scripted <- vapply(env_parts, function(x) any(grepl(
    "^r$|python|matlab|julia|c\\+\\+|delphi|perl|sas|stata|java|bash|command", trimws(x))), logical(1))
  scripted_status <- ifelse(scripted, "Yes", ifelse(env %in% c("gui", "gui / web tools only"), "No", "Not reported"))
} else if (have_scripted) {
  scripted_status <- code_field(column(scripted_field), scripted_field)
  scripted <- scripted_status %in% c("Yes", "Partly")
} else {
  warning("No scripted-analysis or Analysis Environment column found; code sharing uses all studies.")
  scripted_status <- rep("Column not found", N)
  scripted <- rep(FALSE, N)
}
S <- sum(scripted)
code_population <- if (have_scripted) scripted else rep(TRUE, N)

# Positive partial-sequencing-only classification needs explicit wording or an
# override, not merely a sequencing accession.
z <- tolower(raw_notes)
partial_seq <- rep("Not established from coded fields", N)
partial_seq[raw_status != "Partly"] <- "Not applicable"
partial_seq[raw_status == "Partly" &
  grepl("only.{0,20}(sequencing|16s|metagenom)|sequencing data only", z)] <- "Yes"
if (nrow(overrides)) {
  keys <- tolower(normal_doi(overrides$DOI))
  if (any(duplicated(keys))) stop("Duplicate DOI in overrides.")
  ix <- match(keys, tolower(doi))
  if (anyNA(ix)) stop("Override DOI not found in loaded studies: ",
                      paste(overrides$DOI[is.na(ix)], collapse = "; "))
  for (j in seq_len(nrow(overrides))) {
    i <- ix[j]
    if (!is.na(overrides$partial_sequencing_only[j])) {
      if (!overrides$partial_sequencing_only[j] %in% c("Yes", "No"))
        stop("Invalid sequencing-only override.")
      if (raw_status[i] != "Partly")
        stop("Sequencing-only override requires Partly raw-data coding.")
      partial_seq[i] <- overrides$partial_sequencing_only[j]
    }
  }
}

wf <- tolower(workflow)
workflow_status <- rep("Other named system / review", N)
workflow_status[wf %in% c("none", "no")]   <- "None"
workflow_status[wf == "not reported"]      <- "Not reported"
workflow_status[wf == ""]                  <- "Missing"
workflow_status[grepl("custom", wf)]       <- "Custom workflow"
workflow_status[grepl("snakemake|nextflow|galaxy|\\bcwl\\b|common workflow language|\\bwdl\\b|cromwell|toil|luigi|makeflow|\\bdrake\\b|\\btargets\\b", wf)] <-
  "Named workflow-management system"
n_workflow <- sum(workflow_status == "Named workflow-management system")

# =============================================================================
# TABLES (numerator AND denominator everywhere)
# =============================================================================
pop_all <- "All studies"; pop_req <- "Validation required"
validation_summary <- rbind(
  metric("Validation required (coded Yes)", E, N, pop_all),
  metric("Internal validation reported", sum(eligible & reported), E, pop_req),
  metric("No internal validation (explicit None + missing / not reported)", sum(no_internal), E, pop_req),
  metric("  of which explicitly None", sum(eligible & status == "None"), E, pop_req),
  metric("  of which missing / not reported", sum(eligible & status == "Missing / not reported"), E, pop_req),
  metric("Internal validation not applicable", sum(eligible & status == "Not applicable"), E, pop_req),
  metric("Internal validation needs review", sum(eligible & status == "Other / review"), E, pop_req),
  metric("External validation reported", sum(external == "Yes"), N, pop_all),
  metric("External validation: independent cohort", sum(independent_cohort), N, pop_all),
  metric("Sensitivity analyses reported", sum(sensitivity == "Yes"), N, pop_all),
  metric("Simulation / benchmarking reported", sum(benchmark == "Yes"), N, pop_all))

method_summary <- do.call(rbind, lapply(names(methods), function(m) rbind(
  metric(m, sum(methods[[m]]), N, pop_all),
  metric(m, sum(methods[[m]] & eligible), E, pop_req))))

alone_table <- metric(paste(single_methods, "alone"),
  vapply(single_methods, function(m) sum(eligible & combination == m), numeric(1)),
  E, pop_req)
alone_table <- alone_table[order(-alone_table$Studies), ]

overlap <- rbind(
  metric("Taxa-metabolite links only", sum(taxa_only), N, pop_all),
  metric("Taxa-metabolite links only", sum(taxa_only), sum(taxa), "Studies with taxa-metabolite links"),
  metric("Pathway-level only", sum(pathway_only), N, pop_all),
  metric("Taxa-metabolite links AND pathway-level", sum(taxa & pathway), N, pop_all),
  metric("Taxa-metabolite links AND pathway-level", sum(taxa & pathway), sum(pathway),
         "Studies with pathway-level interpretation"))

raw_categories <- do.call(rbind, lapply(names(src), function(nm) {
  x <- clean(src[[nm]]); x[x == ""] <- "[Missing]"
  cbind(Field = vfields[[nm]], freq(x, N, pop_all))
}))

summary_repro <- rbind(
  metric("Raw data available: Yes or Partly (coded)", sum(any_raw)),
  metric("Raw data fully available (coded Yes)", sum(raw_status == "Yes")),
  metric("Raw data partly available (coded Partly)", sum(raw_status == "Partly")),
  metric("Raw data unavailable (coded No)", sum(raw_status == "No")),
  metric("Studies with scripted / command-line analysis (Yes or Partly)", S, N, pop_all),
  metric("Code available (coded Yes)", sum(code_population & code_status == "Yes"), if (have_scripted) S else N,
         if (have_scripted) "Scripted / command-line studies" else pop_all),
  metric("Code partly available", sum(code_population & code_status == "Partly"), if (have_scripted) S else N,
         if (have_scripted) "Scripted / command-line studies" else pop_all),
  metric("Code unavailable (coded No)", sum(code_population & code_status == "No"), if (have_scripted) S else N,
         if (have_scripted) "Scripted / command-line studies" else pop_all),
  metric("Complete raw data and complete code (both coded Yes)", sum(raw_status == "Yes" & code_status == "Yes")),
  metric("Complete raw data and any code (Yes or Partly)", sum(raw_status == "Yes" & any_code)),
  metric("Processed data fully available (coded Yes)", sum(processed_status == "Yes")),
  metric("Processed data partly available (coded Partly)", sum(processed_status == "Partly")),
  metric("Processed data unavailable (coded No)", sum(processed_status == "No")),
  metric("Named workflow-management system", n_workflow))
if (!have_scripted) summary_repro <- summary_repro[
  summary_repro$Measure != "Studies with scripted / command-line analysis (Yes or Partly)", ]

raw_completeness <- rbind(
  metric("Raw data fully available (coded Yes)", sum(raw_status == "Yes"), A, "Raw data coded Yes or Partly"),
  metric("Raw data partly available (coded Partly)", sum(raw_status == "Partly"), A, "Raw data coded Yes or Partly"))

code_by_scripted <- if (have_scripted) freq(code_status[scripted], S, "Scripted / command-line studies") else
  freq(code_status, N, pop_all)

tables <- list(
  `Validation summary` = validation_summary,
  `Internal validation combinations - studies requiring validation` = freq(combination[eligible], E, pop_req),
  `Internal validation: single method alone - studies requiring validation` = alone_table,
  `Internal validation combinations - all studies` = freq(combination, N, pop_all),
  `Individual internal validation methods - multiple responses` = method_summary,
  `External validation entries` = freq(ifelse(nzchar(clean(src$External)), clean(src$External), "[Missing]"), N, pop_all),
  `Sensitivity analysis categories` = freq(sensitivity, N, pop_all),
  `Simulation and benchmarking categories` = freq(benchmark, N, pop_all),
  `Biological interpretation levels - multiple responses` = metric(interp_levels, colSums(interp), N, pop_all),
  `Biological interpretation combinations` = freq(interp_combo, N, pop_all),
  `Interpretation overlaps and exclusive categories` = overlap,
  `Reproducibility summary` = summary_repro,
  `Reported raw data availability and identifiable deposits across included studies` = availability_table,
  `Raw data availability` = freq(raw_status),
  `Layer-specific deposit evidence` = layer_summary,
  `Study-level audit of reported deposits` = layer_audit,
  `Completeness among studies with available raw data` = raw_completeness,
  `Partial deposits: sequencing-only evidence` = freq(partial_seq[raw_status == "Partly"],
      sum(raw_status == "Partly"), "Partly available raw data"),
  `Code availability - analysis population` = code_by_scripted,
  `Code availability - all studies` = freq(code_status),
  `Processed data availability` = freq(processed_status),
  `Workflow-management systems` = freq(ifelse(nzchar(workflow), workflow, "[Missing]")),
  `Workflow categories` = freq(workflow_status),
  `Raw coded entries by field (validation and interpretation)` = raw_categories)

# =============================================================================
# ONE self-contained HTML report
# =============================================================================
esc <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
html_table <- function(x) {
  if (!nrow(x)) return("<p>No entries.</p>")
  if (all(c("Studies", "Percent") %in% names(x))) {
    x[["Studies, n (%)"]] <- ifelse(is.na(x$Percent),
      paste0(x$Studies, " (not applicable)"), sprintf("%d (%.1f%%)", x$Studies, x$Percent))
    keep_names <- names(x)[!names(x) %in% c("Studies", "Percent", "Studies, n (%)")]
    pos <- match("Denominator", keep_names)
    if (is.na(pos)) pos <- length(keep_names) + 1L
    names_order <- append(keep_names, "Studies, n (%)", after = pos - 1L)
    x <- x[, names_order, drop = FALSE]
  }
  x <- as.data.frame(lapply(x, as.character), stringsAsFactors = FALSE, check.names = FALSE)
  rows <- vapply(seq_len(nrow(x)), function(i) paste0("<tr><td>",
    paste(esc(trimws(unlist(x[i, ], use.names = FALSE))), collapse = "</td><td>"), "</td></tr>"),
    character(1))
  paste0("<div class='table'><table><thead><tr><th>", paste(esc(names(x)), collapse = "</th><th>"),
         "</th></tr></thead><tbody>", paste(rows, collapse = "\n"), "</tbody></table></div>")
}
raw_field <- "Raw Data Available"
deposits <- c(raw_field, "Link to Raw Data", "Metabolomics")
env_source <- if (have_scripted) scripted_field else character()
code_field_name <- "Code Available"
processed_field <- "Processed Data Available"
workflow_field <- "Workflow Management System"
table_sources <- list(
  `Validation summary` = vfields[1:5],
  `Internal validation combinations - studies requiring validation` = vfields[c("Required", "Internal")],
  `Internal validation: single method alone - studies requiring validation` = vfields[c("Required", "Internal")],
  `Internal validation combinations - all studies` = vfields["Internal"],
  `Individual internal validation methods - multiple responses` = vfields[c("Required", "Internal")],
  `External validation entries` = vfields["External"],
  `Sensitivity analysis categories` = vfields["Sensitivity"],
  `Simulation and benchmarking categories` = vfields["Benchmark"],
  `Biological interpretation levels - multiple responses` = vfields["Interpretation"],
  `Biological interpretation combinations` = vfields["Interpretation"],
  `Interpretation overlaps and exclusive categories` = vfields["Interpretation"],
  `Reproducibility summary` = c(raw_field, env_source, code_field_name, processed_field, workflow_field),
  `Reported raw data availability and identifiable deposits across included studies` = c(deposits, "DOI"),
  `Raw data availability` = raw_field,
  `Layer-specific deposit evidence` = c(deposits, "DOI"),
  `Study-level audit of reported deposits` = c("Progressive Study No", "DOI", deposits),
  `Completeness among studies with available raw data` = raw_field,
  `Partial deposits: sequencing-only evidence` = c(raw_field, "Link to Raw Data", if (nrow(overrides)) "DOI"),
  `Code availability - analysis population` = c(env_source, code_field_name),
  `Code availability - all studies` = code_field_name,
  `Processed data availability` = processed_field,
  `Workflow-management systems` = workflow_field,
  `Workflow categories` = workflow_field,
  `Raw coded entries by field (validation and interpretation)` = vfields)
stopifnot(setequal(names(tables), names(table_sources)))
report <- report_header("3.10 Validation, Reproducibility and Quality Indicators", N)
for (nm in names(tables)) {
  source <- workbook_source_note(extraction$columns, table_sources[[nm]])
  report <- c(report, paste0("<h2>", esc(nm), "</h2><p class='source'>", esc(source), "</p>"),
              html_table(tables[[nm]]))
}
report <- c(report, report_footer)
html_file <- file.path(output_dir, output_name)
con <- file(html_file, open = "w", encoding = "UTF-8")
tryCatch(writeLines(enc2utf8(report), con), finally = close(con))

if (nrow(issues)) warning(nrow(issues), " extraction entries flagged during validation.")
cat("HTML report saved to:", normalizePath(html_file, winslash = "/"), "\n")
