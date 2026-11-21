# Manuscript section 3.4 Extent and level of integration
# Manuscript section 3.5 Analysis method families
# Manuscript section 3.6 Network analyses
# Manuscript section 3.7 Methods matched to the properties of the data
# Manuscript section 3.8 Integration depth against study design and time
# Repository entry point: run from the repository root.
source("R/00_style.R", local = TRUE)
source("R/report_layout.R", local = TRUE)

###############################################################################
## Scoping review of multi-omics analysis in in vitro microbiome models
## Sections 4–8 - Integration depth, analytical methods and methodological safeguards
##
## Input : data/Supplementary Appendix 2.xlsx  (sheet "Data Extraction", two header rows)
## Output: PDF figures and one HTML report containing all supporting tables.
##
## R 4.5.x; tidyverse. Run from the repository root.
###############################################################################

suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(stringr)
  library(forcats); library(ggplot2); library(scales); library(patchwork)
})

XLSX     <- getOption("review.data_file", "data/Supplementary Appendix 2.xlsx")
SHEET    <- getOption("review.data_sheet", "Data Extraction")
FIG_DIR  <- "figures"
TAB_DIR  <- "results"
dir.create(FIG_DIR, showWarnings = FALSE); dir.create(TAB_DIR, showWarnings = FALSE)
if (!file.exists(XLSX) && interactive()) XLSX <- file.choose()
if (!file.exists(XLSX)) stop("Extraction workbook not found. Set XLSX to its path.")
report_path <- file.path(TAB_DIR, "Sections_3_4_to_3_8_Integration_analyses.html")
html_escape <- function(x) {
  x <- as.character(x); x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}
html_out <- function(x, append = TRUE) {
  con <- file(report_path, if (append) "a" else "w", encoding = "UTF-8")
  on.exit(close(con)); writeLines(enc2utf8(x), con)
}
show_table <- function(tab, title, note = "") {
  head <- paste0("<th>", html_escape(names(tab)), "</th>", collapse = "")
  rows <- vapply(seq_len(nrow(tab)), function(i)
    paste0("<tr>", paste0("<td>", html_escape(unlist(tab[i, ], use.names = FALSE)), "</td>", collapse = ""), "</tr>"), character(1))
  html_out(paste0("<h2>", html_escape(title), "</h2>", source_html(title),
                  '<div class="table"><table><thead><tr>', head, "</tr></thead><tbody>",
                  paste(rows, collapse = ""), "</tbody></table></div>",
                  if (nzchar(note)) paste0("<p>", html_escape(note), "</p>")))
}
encode_base64 <- function(bytes) {
  abc <- strsplit("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/", "", fixed=TRUE)[[1]]
  pad <- (3L-length(bytes)%%3L)%%3L
  v <- matrix(c(as.integer(bytes),rep(0L,pad)),nrow=3L)
  ix <- rbind(bitwShiftR(v[1,],2L),bitwOr(bitwShiftL(bitwAnd(v[1,],3L),4L),bitwShiftR(v[2,],4L)),bitwOr(bitwShiftL(bitwAnd(v[2,],15L),2L),bitwShiftR(v[3,],6L)),bitwAnd(v[3,],63L))
  z <- abc[as.vector(ix)+1L]; if(pad>0L) z[(length(z)-pad+1L):length(z)] <- "="
  paste0(z,collapse="")
}


## ---------------------------------------------------------------- style ----
## Ordinal depth ramp (single hue, light -> dark) + one accent for emphasis.
## Deliberately avoids a red/green contrast (color-vision safety).
PAL_DEPTH <- c("No cross-layer statistic" = pal("blue_light"),
               "Cross-layer association"  = pal("blue_mid"),
               "Joint cross-layer model"  = pal("blue_dark"))
INK <- pal("teal"); GRAY <- pal("neutral"); ACCENT <- pal("gold"); META <- pal("muted_text")

## Figures are sized for a manuscript column, so every piece of text is BLACK and
## the point sizes are held while the canvas is narrow - that is what keeps them
## legible after the journal scales the figure down.
BASE <- 10; ANNOT <- 9; TICK <- 10    # match Figures 3–4 at exported size
FIGURE_WIDTH <- 210 / 25.4           # full A4 width, in inches
TXT  <- "black"
theme_pub <- function(base = BASE) {
  theme_minimal(base_size = base) +
    theme(
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "gray88", linewidth = .25),
      axis.line.x        = element_line(color = TXT, linewidth = .3),
      axis.line.y        = element_line(color = TXT, linewidth = .3),
      axis.ticks         = element_line(color = TXT, linewidth = .25),
      axis.text          = element_text(size = TICK, color = TXT),
      axis.title         = element_text(size = base, color = TXT),
      plot.title         = element_text(size = 11, hjust = 0, face = "bold",
                                        color = TXT, margin = margin(b = 3)),
      plot.subtitle      = element_text(size = ANNOT, color = TXT, margin = margin(b = 5)),
      plot.tag           = element_text(size = 11, face = "bold", color = TXT),
      plot.tag.position  = c(0, 1),
      legend.position    = "none",
      legend.title       = element_blank(),
      legend.text        = element_text(size = ANNOT, color = TXT),
      legend.key.size    = unit(7, "pt"),
      strip.text         = element_text(size = base, color = TXT, hjust = 0)
    )
}
## PDF-only standalone figures; temporary PNG previews are embedded then removed.
save_fig <- function(p, name, w, h) {
  # Match Figures 3–4: standard R PDF device, vector text and geometry.
  pdf_path <- file.path(FIG_DIR, paste0(name, ".pdf"))
  ggsave(pdf_path, p, width = w, height = h, device = grDevices::pdf,
         family = "Helvetica", useDingbats = FALSE, bg = "white")
  preview <- tempfile(fileext = ".png")
  tryCatch({
    # Match Figure 4's high-resolution HTML preview; no PNG output is retained.
    ggsave(preview, p, width = w, height = h, dpi = 600, bg = "white")
    bytes <- readBin(preview, "raw", n = file.info(preview)$size)
    html_out(paste0("<h2>", html_escape(gsub("_", " ", name)),
      '</h2>', source_html(name), '<img style="width:100%;height:auto" src="data:image/png;base64,',
      encode_base64(bytes), '">'))
  }, finally = unlink(preview))
  invisible(p)
}

## ----------------------------------------------------------------- data ----
## Row 1 = section banner, row 2 = field names, rows 3.. = studies.
source("R/read_extraction.R", local = TRUE)
extraction <- read_extraction(XLSX, SHEET)
raw <- extraction$data
fields <- extraction$columns
prefix <- ifelse(grepl("^6\\.", fields$section), "MIC_",
                 ifelse(grepl("^7\\.", fields$section), "MET_", ""))
names(raw) <- make.unique(paste0(prefix, fields$name), sep = "_")
resolve_header <- function(target) {
  hit <- which(tolower(trimws(names(raw))) == tolower(target))
  if(length(hit)!=1L) stop("Expected one column: ", target)
  names(raw)[hit] <<- target
}
for (header in c("Study employing at least one tool requiring validation",
                 "Network-based Method specification", "All_Multivariate",
                 "Regression based methods", "Pathway/Functional Enrichment Methods")) resolve_header(header)
nm <- names(raw)
d <- raw |>
  mutate(sid = as.integer(`Progressive Study No`),
         year = as.integer(`Year of Publication`),
         n_samples = suppressWarnings(as.numeric(`Sample size`)))
stopifnot(!anyNA(d$sid), !anyDuplicated(d$sid))

DEPTH_LEVELS <- c("No cross-layer statistic", "Cross-layer association", "Joint cross-layer model")
blank <- function(x) is.na(x) | trimws(x) %in% c("", "NA", "Not reported", "Not applicable", "None")

d <- d |>
  mutate(
    depth = factor(recode(trimws(`Integration Type`),
                          "No cross-layer statistic documented" = "No cross-layer statistic"),
                   levels = DEPTH_LEVELS),
    ## NB: four cells hold the literal string "None", not an empty cell.
    breadth = factor(recode(tidyr::replace_na(trimws(`Pairwise vs Multi-block`), "None"),
                            "Pairwise (2 omics)"      = "Pairwise (2 layers)",
                            "Multi-block (>=3 omics)" = "Multi-block (\u22653 layers)",
                            "None"                    = "None (no integration)"),
                     levels = c("Pairwise (2 layers)", "Multi-block (\u22653 layers)", "None (no integration)")),
    ## omics layers actually profiled
    has_mg  = grepl("^Yes", `Metagenomics`),
    has_mt  = trimws(`Metatranscriptomics`) == "Yes",
    has_mp  = trimws(`Metaproteomics`) == "Yes",
    has_mb  = grepl("^Yes", `Metabolomics`),
    has_sc  = trimws(`SCFA/BCFA panel (targeted adjunct)`) == "Yes",
    n_layers_core = has_mg + has_mt + has_mp + has_mb,
    model = fct_recode(factor(trimws(`In vitro Model Type`)),
                       "Continuous /\nsemi-continuous" = "Continuous / semi-continuous culture",
                       "Spontaneous\nfood-feed"        = "Spontaneous / in situ food-feed fermentation"),
    network_used  = trimws(`Network-based Methods`) %in% "Yes",
    factorization = !blank(`Factorization Approaches`),
    small_n_aware = trimws(`Method Tailored for Small N`) == "Yes",
    longit_ok     = trimws(`Method Supports Longitudinal Data`) %in% c("Yes", "Partly"),
    needs_valid   = trimws(.data[[grep("^Study employing", nm, value = TRUE)[1]]]) == "Yes",
    validated     = !blank(`Internal Validation`),
    code_shared   = trimws(`Code Available`) == "Yes",
    env_R         = grepl("\\bR\\b", `Analysis Environment`),
    versions_all  = trimws(`Version Information Reported`) == "Yes - all tools"
  )
N <- nrow(d)
html_out(report_header("3.4–3.8 Integration analyses", N), append = FALSE)
if (anyNA(d$depth) || anyNA(d$breadth)) stop("Unrecognized integration depth/breadth entries; check the extracted labels.")

## ------------------------------------------------- method harmonization ----
## Free-text method fields use different delimiters, so pool them and match on
## regular expressions rather than splitting. Families are NOT mutually exclusive.
METHOD_COLS <- c("Base Statistical Methods", "All_Multivariate", "Machine Learning Approaches",
                 "Regression based methods", "Pathway/Functional Enrichment Methods",
                 "Network-based Method specification", "Factorization Approaches")
depth_field <- "Integration Type"
breadth_field <- "Pairwise vs Multi-block"
network_field <- "Network-based Methods"
family_fields <- c(METHOD_COLS, network_field)
small_field <- "Method Tailored for Small N"
longitudinal_field <- "Method Supports Longitudinal Data"
design_field <- "Longitudinal vs Cross-sectional"
model_field <- "In vitro Model Type"
internal_field <- "Internal Validation"
code_field <- "Code Available"
year_field <- "Year of Publication"
layers <- c("Metagenomics", "Metatranscriptomics", "Metaproteomics", "Metabolomics")
ids <- c("Progressive Study No", "Title", "DOI")
validation_fields <- c(depth_field, "Study Employing at Least One Tool Requiring Validation", internal_field)
output_sources <- list(
  `Integration depth` = depth_field,
  `Integration breadth` = breadth_field,
  `Method families` = family_fields,
  `Integration depth by breadth` = c(depth_field, breadth_field),
  `Integration-depth profile` = c(depth_field, breadth_field, "Sample size", layers, network_field, small_field, longitudinal_field, internal_field, code_field),
  `Integration depth by year` = c(depth_field, year_field),
  `Method families by depth` = c(depth_field, family_fields),
  `Association tests: all studies` = c(depth_field, breadth_field, network_field, internal_field, code_field),
  `Frameworks named among joint-model studies` = c(depth_field, METHOD_COLS),
  `Explicit network-method mentions` = c(network_field, "Network-based Method specification"),
  `Study-level evidence for joint cross-layer models` = c(ids, depth_field, breadth_field, METHOD_COLS, small_field, internal_field),
  `Profiled layers among association-tier studies` = c(depth_field, layers, "SCFA/BCFA panel (targeted adjunct)", "Other omics layer (specify)"),
  `Named correlation coefficients by integration tier` = c(depth_field, METHOD_COLS),
  `Network-method inventory for classification` = c(ids, depth_field, network_field, METHOD_COLS),
  `Repeated-measures support against sampling design` = c(depth_field, design_field, longitudinal_field),
  `Integration depth by experimental model` = c(depth_field, model_field),
  `Integration depth by publication year` = c(depth_field, year_field),
  `Internal validation with both denominators` = validation_fields,
  `Internal-validation sensitivity tests` = validation_fields,
  `Code availability by integration tier` = c(depth_field, code_field),
  `Method-family coding audit` = c(ids[1:2], depth_field, METHOD_COLS),
  `Manuscript denominator checks` = c(design_field, longitudinal_field, year_field),
  `Interpretation and scope notes` = character(),
  `Figure_5_integration_landscape` = c(depth_field, breadth_field),
  `Figure_6_method_families_by_depth` = c(depth_field, family_fields),
  `Figure_7_depth_vs_design` = c(depth_field, "Sample size", model_field),
  `Supplementary_Figure_3` = c(depth_field, network_field, "Factorization Approaches", small_field, longitudinal_field, internal_field, "Version Information Reported", code_field))
source_html <- function(title) {
  if (!title %in% names(output_sources)) stop("Missing source mapping: ", title)
  paste0("<p class='source'>", html_escape(workbook_source_note(fields, output_sources[[title]])), "</p>")
}
pool <- apply(d[METHOD_COLS], 1, function(r) tolower(paste(na.omit(r), collapse = " ;; ")))

FAMILIES <- c(
  "Univariate tests (t, ANOVA, Kruskal\u2013Wallis)" = "\\bt-test|anova|kruskal|wilcoxon|mann|dunnett|duncan|tukey|student",
  "Pairwise correlation (Spearman, Pearson)"         = "spearman|pearson|correlat|mantel|kendall",
  "Ordination and distance (PCoA, PERMANOVA, RDA)"   = "pcoa|nmds|permanova|anosim|\\brda\\b|\\bcca\\b|procrustes|adonis|db-rda",
  "Unsupervised projection (PCA)"                    = "\\bpca\\b|pca-x|\\bhca\\b",
  "Supervised projection (PLS-DA, OPLS-DA, sPLS)"    = "\\bpls|opls|diablo",
  "Differential abundance / regression models"       = "deseq|maaslin|limma|edger|linda|lme|glmm|mixed model|linear mod|zero-inflated",
  "Effect-size ranking (LEfSe / LDA)"                = "lefse|\\blda\\b",
  "Clustering (hierarchical, k-means, WGCNA)"        = "hierarchical|k-means|upgma|wgcna|cluster",
  "Functional prediction / enrichment (PICRUSt, KEGG)" = "picrust|kegg|enrich|humann|tax4fun|metacyc|stamp|\\bgo\\b",
  "Supervised machine learning (RF, SVM)"            = "random forest|\\brf\\b|svm|xgboost|neural|lasso|elastic|boost",
  "Multi-block / latent-variable frameworks"         = "diablo|sgcca|\\brcca\\b|multi-?block|mofa|\\bjive\\b|o2pls|mmvec"
)
## Short forms for axis labels; the full names (with the worked examples) stay in
## the tables and belong in the figure caption, not on the axis.
FAM_SHORT <- c(
  "Univariate tests (t, ANOVA, Kruskal\u2013Wallis)"       = "Univariate tests",
  "Pairwise correlation (Spearman, Pearson)"             = "Pairwise correlation",
  "Ordination and distance (PCoA, PERMANOVA, RDA)"       = "Ordination / distance",
  "Unsupervised projection (PCA)"                        = "PCA",
  "Supervised projection (PLS-DA, OPLS-DA, sPLS)"        = "Supervised projection",
  "Differential abundance / regression models"           = "Regression models",
  "Effect-size ranking (LEfSe / LDA)"                    = "LEfSe / LDA",
  "Clustering (hierarchical, k-means, WGCNA)"            = "Clustering",
  "Network analysis"                                     = "Network analysis",
  "Functional prediction / enrichment (PICRUSt, KEGG)"   = "Functional enrichment",
  "Supervised machine learning (RF, SVM)"                = "Machine learning",
  "Multi-block / latent-variable frameworks"             = "Multi-block frameworks")
short_fam <- function(x) unname(FAM_SHORT[as.character(x)])

fam <- sapply(FAMILIES, function(p) grepl(p, pool))
# The manually reviewed study-level field determines network-analysis use in both panels.
fam <- cbind(fam, `Network analysis` = d$network_used)[, names(FAM_SHORT), drop = FALSE]
d <- bind_cols(d, as_tibble(fam))

## ---------------------------------------------------------------- tables ----
pct <- function(x, n = N) sprintf("%.1f", 100 * x / n)

t1 <- d |> count(depth, name = "n") |> mutate(`% of studies` = pct(n))
t2 <- d |> count(breadth, name = "n") |> mutate(`% of studies` = pct(n))
t3 <- tibble(Family = colnames(fam), n = colSums(fam)) |>
  mutate(`% of studies` = pct(n)) |> arrange(desc(n))
t4 <- as.data.frame.matrix(table(d$depth, d$breadth)) |> tibble::rownames_to_column("Integration depth")
t5 <- d |>
  group_by(depth) |>
  summarize(n = n(),
            `median samples` = median(n_samples, na.rm = TRUE),
            `IQR samples` = paste0(quantile(n_samples, .25, na.rm = TRUE), "\u2013",
                                   quantile(n_samples, .75, na.rm = TRUE)),
            `mean omics layers` = round(mean(n_layers_core), 2),
            `multi-block (%)` = pct(sum(breadth == "Multi-block (\u22653 layers)"), n()),
            `network (%)` = pct(sum(network_used), n()),
            `small-n aware (%)` = pct(sum(small_n_aware), n()),
            `longitudinal-capable (%)` = pct(sum(longit_ok), n()),
            `internal validation (%)` = pct(sum(validated), n()),
            `code shared (%)` = pct(sum(code_shared), n()), .groups = "drop")
t6 <- d |> count(year, depth) |> complete(year, depth, fill = list(n = 0L))
t7 <- as.data.frame.matrix(
  table(rep(d$depth, times = ncol(fam))[as.vector(fam)],
        rep(colnames(fam), each = N)[as.vector(fam)])) |>
  tibble::rownames_to_column("Integration depth")

## pre-specified association tests (counts are the result; p is supporting).
## Monte-Carlo p-values are seeded so the reported values are reproducible.
set.seed(20261102)
ft <- function(tab) tryCatch(fisher.test(tab, simulate.p.value = TRUE, B = 1e5)$p.value, error = function(e) NA_real_)
t8 <- tibble(
  Comparison = c("Integration depth x integration breadth",
                 "Integration depth x network analysis used",
                 "Integration depth x internal validation reported",
                 "Integration depth x analysis code shared"),
  `Fisher p` = c(ft(table(d$depth, d$breadth)),
                 ft(table(d$depth, d$network_used)),
                 ft(table(d$depth, d$validated)),
                 ft(table(d$depth, d$code_shared))))

table_titles <- c("Integration depth", "Integration breadth", "Method families",
                  "Integration depth by breadth", "Integration-depth profile", "Integration depth by year",
                  "Method families by depth", "Association tests: all studies")
for (i in 1:8) show_table(get(paste0("t",i)),table_titles[i],
                          "Original method-family definitions retained for comparison with the manuscript. Percentages use all studies unless labeled otherwise.")

###############################################################################
## Figure 5 - the integration landscape (panels a–c)
###############################################################################
f1a <- t1 |>
  mutate(lab = sprintf("%.0f%%", 100 * n / N), n = 100 * n / N) |>
  ggplot(aes(n, fct_rev(depth), fill = depth)) +
  geom_col(width = .62) +
  geom_text(aes(label = lab), hjust = -0.12, size = ANNOT / .pt, color = TXT) +
  scale_fill_manual(values = PAL_DEPTH) +
  scale_x_continuous(limits = c(0, 86), labels = function(x) sprintf("%.0f%%", x), expand = expansion(mult = c(0, .02))) +
  labs(title = "Depth of cross-layer integration",
       x = "Studies (% of all studies)", y = NULL, tag = "a") +
  theme_pub() + theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())

f1b <- t2 |>
  mutate(lab = sprintf("%.0f%%", 100 * n / N), n = 100 * n / N) |>
  ggplot(aes(n, fct_rev(breadth))) +
  geom_segment(aes(x = 0, xend = n, yend = fct_rev(breadth)), color = GRAY, linewidth = .5) +
  geom_point(size = 2.6, color = INK) +
  geom_text(aes(label = lab), nudge_x = 3.2, hjust = 0, size = ANNOT / .pt, color = TXT) +
  scale_x_continuous(limits = c(0, 104), labels = function(x) sprintf("%.0f%%", x), expand = expansion(mult = c(0, .04))) +
  labs(title = "Breadth of integration",
       x = "Studies (% of all studies)", y = NULL, tag = "b") +
  theme_pub() + theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())

f1c <- d |> count(breadth, depth) |>
  mutate(n = 100 * n / N) |>
  ggplot(aes(n, fct_rev(breadth), fill = depth)) +
  geom_col(width = .6, position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = PAL_DEPTH) +
  scale_x_continuous(labels = function(x) sprintf("%.0f%%", x),
                     expand = expansion(mult = c(0, .04))) +
  labs(title = "Integration depth by integration breadth",
       x = "Studies (% of all studies)", y = NULL, tag = "c") +
  theme_pub() + theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
                      legend.position = "bottom")

# Figure 5: depth, breadth and depth by breadth.
fig1 <- (f1a / f1b / f1c) + plot_layout(heights = c(1, 1, 1.5))
save_fig(fig1, "Figure_5_integration_landscape", FIGURE_WIDTH, 170 / 25.4)

###############################################################################
## Figure 6 - method families overall and by integration depth (panels a–b)
###############################################################################
f2a <- t3 |>
  mutate(n = 100 * n / N, Family = fct_reorder(Family, n),
         focal = Family %in% c("Pairwise correlation (Spearman, Pearson)",
                               "Multi-block / latent-variable frameworks")) |>
  ggplot(aes(n, Family, fill = focal)) +
  geom_col(width = .66) +
  geom_text(aes(label = sprintf("%.0f%%", n)), hjust = -0.25, size = ANNOT / .pt, color = TXT) +
  scale_fill_manual(values = c(`TRUE` = pal("blue_dark"), `FALSE` = pal("method_bar_gray"))) +
  scale_y_discrete(labels = short_fam) +
  scale_x_continuous(limits = c(0, 105), labels = function(x) sprintf("%.0f%%", x), expand = expansion(mult = c(0, .02))) +
  labs(title = "Analytical method families",
       x = "Studies using the family (%)", y = NULL, tag = "a") +
  theme_pub() + theme(axis.line.y = element_blank(), axis.ticks.y = element_blank())

fam_long <- as_tibble(fam) |>
  mutate(depth = d$depth) |>
  pivot_longer(-depth, names_to = "Family", values_to = "used") |>
  group_by(depth, Family) |>
  summarize(n = sum(used), share = sum(used) / n(), .groups = "drop") |>
  mutate(Family = factor(Family, levels = rev(t3$Family)))

f2b <- ggplot(fam_long, aes(depth, Family, fill = share)) +
  geom_tile(color = "white", linewidth = .9) +
  geom_text(aes(label = sprintf("%.0f%%", 100 * share), color = share > .55), size = ANNOT / .pt) +
  scale_fill_gradient(low = pal("blue_light"), high = pal("blue_dark"), labels = percent_format(accuracy = 1),
                      name = "Share of studies in tier",
                      guide = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  scale_color_manual(values = c(`TRUE` = "white", `FALSE` = TXT), guide = "none") +
  scale_x_discrete(labels = function(x) str_wrap(x, 12), position = "top") +
  scale_y_discrete(labels = short_fam) +
  labs(title = "Method families by integration depth", x = NULL, y = NULL, tag = "b") +
  theme_pub() +
  theme(panel.grid.major.x = element_blank(),
        axis.line = element_blank(), axis.ticks = element_blank(),
        legend.position = "bottom", legend.key.width = unit(26, "pt"),
        legend.title = element_text(size = ANNOT, color = TXT))
# Stack panels to preserve readable method-family labels.
fig2 <- (f2a / f2b) + plot_layout(heights = c(1, 1.15))
save_fig(fig2, "Figure_6_method_families_by_depth", FIGURE_WIDTH, 270 / 25.4)

###############################################################################
## Figure 7 - integration depth against sample size and model (panels a–b)
###############################################################################
f4a <- d |>
  ggplot(aes(depth, n_samples, color = depth)) +
  geom_jitter(width = .16, height = 0, size = 1.5, alpha = .75) +
  stat_summary(fun = median, geom = "crossbar", width = .42, linewidth = .4, color = TXT) +
  scale_color_manual(values = PAL_DEPTH) +
  scale_y_log10(breaks = c(3, 10, 30, 100, 300), labels = c("3", "10", "30", "100", "300")) +
  scale_x_discrete(labels = function(x) str_wrap(x, 12)) +
  labs(title = "Sample size by integration depth",
       x = NULL, y = "Samples per study", tag = "a") +
  theme_pub() + theme(panel.grid.major.x = element_blank(),
                      panel.grid.major.y = element_line(color = "gray92", linewidth = .3))

f4b <- d |> count(model, depth) |>
  mutate(n = 100 * n / N) |>
  ggplot(aes(n, fct_rev(model), fill = depth)) +
  geom_col(width = .62, position = position_stack(reverse = TRUE)) +
  scale_fill_manual(values = PAL_DEPTH) +
  scale_x_continuous(labels = function(x) sprintf("%.0f%%", x),
                     expand = expansion(mult = c(0, .04))) +
  labs(title = "Integration depth by in vitro model type",
       x = "Studies (% of all studies)", y = NULL, tag = "b") +
  theme_pub() + theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
                      legend.position = "bottom")


fig4 <- (f4a / f4b) + plot_layout(heights = c(1.5, 1.3))
save_fig(fig4, "Figure_7_depth_vs_design", FIGURE_WIDTH, 165 / 25.4)

###############################################################################
## Supplementary Figure 3 - methodological safeguards by integration depth
###############################################################################
safe <- d |>
  transmute(depth,
            `Network analysis`           = network_used,
            `Recorded latent-variable approach` = factorization,
            `Small-n method`             = small_n_aware,
            `Repeated-measures method`   = longit_ok,
            `Internal validation`        = validated,
            `All software versions`      = versions_all,
            `Code shared`                = code_shared) |>
  pivot_longer(-depth, names_to = "item", values_to = "yes") |>
  group_by(depth, item) |>
  summarize(n = sum(yes), tot = n(), share = n / tot, .groups = "drop")
item_order <- safe |> group_by(item) |> summarize(s = sum(n)) |> arrange(s) |> pull(item)

fig5 <- safe |>
  mutate(item = factor(item, levels = item_order)) |>
  ggplot(aes(share, item, fill = depth)) +
  geom_col(width = .66, position = position_dodge(width = .72)) +
  ## a zero-count bar would be invisible; draw a stub so the slot is not read as absent
  geom_point(data = ~ subset(.x, n == 0), aes(x = 0.004), shape = 22, size = 1.1,
             stroke = .3, color = TXT, position = position_dodge(width = .72)) +
  geom_text(aes(label = sprintf("%.0f%%", 100 * share)),
            position = position_dodge(width = .72), hjust = -0.35,
            size = TICK / .pt, color = TXT) +
  scale_fill_manual(values = PAL_DEPTH) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1.08),
                     expand = expansion(mult = c(0, .02))) +
  labs(title = "Methodological safeguards by integration depth",
       x = "Share of all studies within the tier", y = NULL) +
  theme_pub() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = "bottom")
save_fig(fig5, "Supplementary_Figure_3", FIGURE_WIDTH, 130 / 25.4)

###############################################################################
cat("\n-- Sections 4–8 summary -------------------------------------------------\n")
cat(sprintf("%-26s %3d (%s%%)\n", t1$depth, t1$n, t1$`% of studies`), sep = "")
cat(sprintf("%-26s %3d (%s%%)\n", t2$breadth, t2$n, t2$`% of studies`), sep = "")
cat("\nTop method families:\n")
cat(sprintf("  %-52s %3d\n", head(t3$Family, 6), head(t3$n, 6)), sep = "")
cat("\nFisher exact (depth x ...):",
    paste0(sub("Integration depth x ", "", t8$Comparison), " p=", signif(t8$`Fisher p`, 2), collapse = "; "), "\n")
cat("\n", length(list.files(FIG_DIR)), "files in", FIG_DIR, "|",
    length(list.files(TAB_DIR)), "files in", TAB_DIR, "\n")


## ---- Additional manuscript-supporting tables -------------------------------
framework_flags <- tibble(Framework=c("DIABLO", "sGCCA", "rCCA", "RDA mentioned"),
                          Studies=vapply(c("diablo", "sgcca", "\\brcca\\b", "\\brda\\b"),
                                         function(pattern) sum(grepl(pattern,pool) & d$depth=="Joint cross-layer model"), integer(1)))
show_table(framework_flags,"Frameworks named among joint-model studies",
           "Non-exclusive mentions. RDA being present does not verify the constraining matrix; inspect the study-level evidence table and articles.")
network_mentions <- tibble(Method=c("WGCNA", "DIABLO relevance network", "Multi-block PLS-DA network/circle", "GNPS or spectral-similarity network", "Curated pathway network"),
                           Studies=vapply(c("wgcna", "diablo.*relevance", "multi-block.*pls-da", "gnps|spectral similarity", "curated.*pathway"),
                                          function(pattern) sum(grepl(pattern,replace_na(d$`Network-based Method specification`,""),ignore.case=TRUE) & d$network_used),integer(1)))
show_table(network_mentions,"Explicit network-method mentions",
           "Non-exclusive text matches, not an exhaustive or reviewed classification. No 13/11 cross-layer/within-layer split is inferred.")

np <- function(n, denominator) ifelse(denominator > 0, sprintf("%d (%.1f%%)", as.integer(n),100*n/denominator), paste0(n," (NA)"))
add_html_table <- function(tab, title, note) {
  show_table(tab, title, note)
}
add_html_table( d |> filter(depth=="Joint cross-layer model") |>
                  select(sid,Title,DOI,`Integration Type`,`Pairwise vs Multi-block`,
                         all_of(METHOD_COLS),`Method Tailored for Small N`,`Internal Validation`),
                "Study-level evidence for joint cross-layer models",
                "Recorded fields are reproduced without inferring a framework from the integration tier. RDA constraints and shared model inputs require article-level confirmation.")
layer_combinations <- d |> filter(depth=="Cross-layer association") |>
  count(has_mg,has_mt,has_mp,has_mb,has_sc,`Other omics layer (specify)`,name="Studies") |>
  mutate(`Studies, n (% of association tier)`=np(Studies,sum(Studies))) |> select(-Studies)
add_html_table(layer_combinations,"Profiled layers among association-tier studies",
               "Denominator: association-tier studies. Profiled layers and SCFA adjuncts do not establish which variables actually entered a cross-layer analysis.")
correlation_flags <- tibble(sid=d$sid,depth=d$depth,
                            Spearman=grepl("spearman",pool),Pearson=grepl("pearson",pool))
# Count logical flags before formatting; do not overwrite them inside summarize().
correlation_summary <- correlation_flags |> group_by(depth) |>
  summarize(tier_n = n(),
            spearman_n = sum(Spearman),
            pearson_n = sum(Pearson),
            both_n = sum(Spearman & Pearson), .groups = "drop") |>
  transmute(depth, `Studies in tier` = tier_n,
            Spearman = np(spearman_n, tier_n),
            Pearson = np(pearson_n, tier_n),
            Both = np(both_n, tier_n))
add_html_table(correlation_summary,"Named correlation coefficients by integration tier",
               "Both is an overlap subset, not a third mutually exclusive category. Unspecified correlation methods remain outside these coefficient-specific counts.")
network_inventory <- d |> filter(network_used) |>
  select(sid,Title,DOI,depth,`Network-based Method specification`,all_of(METHOD_COLS[-6])) |>
  mutate(`Reviewed network type`="",`Review note`="")
add_html_table(network_inventory,"Network-method inventory for classification",
               "Review fields are intentionally blank. The extraction includes spectral-similarity, curated pathway, gene, taxon and cross-layer networks; the manuscript's 13/11 split is not imposed. Multiple network types may occur within one study.")
long_design <- trimws(d$`Longitudinal vs Cross-sectional`) %in% c("Longitudinal","Mixed")
matched <- d |> mutate(`Longitudinal or mixed design`=long_design,
                       `Repeated-measures support (recorded)`=replace_na(`Method Supports Longitudinal Data`,"Not reported")) |>
  count(depth,`Longitudinal or mixed design`,`Repeated-measures support (recorded)`,name="Studies") |>
  group_by(depth,`Longitudinal or mixed design`) |>
  mutate(`Studies, n (% within design/tier)`=np(Studies,sum(Studies))) |> ungroup() |> select(-Studies)
add_html_table(matched,"Repeated-measures support against sampling design",
               "Yes and Partly remain separate; capability recorded in the codebook does not by itself prove that dependence was modeled in every analysis.")
model_summary <- d |> count(model,depth,name="Studies") |> group_by(model) |>
  mutate(`Studies, n (% within model)`=np(Studies,sum(Studies))) |> ungroup() |> select(-Studies)
add_html_table(model_summary,"Integration depth by experimental model","Denominator: all studies within each recorded model category.")
time_summary <- d |> count(year,depth,name="Studies") |> group_by(year) |>
  mutate(`Studies, n (% within year)`=np(Studies,sum(Studies))) |> ungroup() |> select(-Studies)
add_html_table(time_summary,"Integration depth by publication year",
               "Descriptive counts only; no formal trend test. Publication years are used as recorded.")
validation <- d |> group_by(depth) |>
  summarize(`All studies`=n(),`Studies requiring validation`=sum(needs_valid,na.rm=TRUE),
            `Validated among all, n (%)`=np(sum(validated,na.rm=TRUE),n()),
            `Validated among requiring, n (%)`=np(sum(validated & needs_valid,na.rm=TRUE),sum(needs_valid,na.rm=TRUE)),.groups="drop")
validation_tests <- tibble(Population=c("All studies","Studies requiring validation"),
                           `Fisher p`=c(t8$`Fisher p`[3],ft(table(d$depth[d$needs_valid %in% TRUE],d$validated[d$needs_valid %in% TRUE]))))
add_html_table(validation,"Internal validation with both denominators",
               "The original all-study test and the test restricted to studies requiring validation answer different questions. Do not attach the all-study p-value to the restricted denominators.")
show_table(validation_tests,"Internal-validation sensitivity tests","Monte Carlo Fisher tests, B=100000. The random seed is set in the script; estimates may differ slightly across implementations.")
code_status <- d |> count(depth,`Code Available`,name="Studies") |> group_by(depth) |>
  mutate(`Studies, n (% within tier)`=np(Studies,sum(Studies))) |> ungroup() |> select(-Studies)
add_html_table(code_status,"Code availability by integration tier",
               "Yes, Partly and No are retained separately. The original binary test compares Yes with all other entries; it does not count Partly as Yes.")
audit_methods <- tibble(sid=d$sid,Title=d$Title,depth=d$depth,
                        `Original univariate flag`=fam[,1],
                        `ANOVA-like substring requiring review`=grepl("permanova|cv[- ]?anova|anosim",pool) & fam[,1],
                        `PCA explicitly named`=grepl("\\bpca\\b|pca-x",pool),
                        `HCA explicitly named`=grepl("\\bhca\\b",pool),
                        `Named factorization/framework text`=grepl("mofa|\\bjive\\b|\\bnmf\\b|non.?negative matrix|minttea",pool))
add_html_table(audit_methods,"Method-family coding audit",
               "Original family counts are retained, not validated by this audit. ANOVA substring matches can include PERMANOVA or CV-ANOVA; the original PCA family includes HCA. Framework absence means not identified in the charted fields, not independently verified absence from articles.")
show_table(tibble(Measure=c("Longitudinal or mixed studies","Yes/Partly repeated-measures support","Support within longitudinal/mixed studies","Published from 2023 onward"),
                  `n (%)`=c(np(sum(long_design),N),np(sum(d$longit_ok),N),np(sum(d$longit_ok & long_design),sum(long_design)),np(sum(d$year>=2023,na.rm=TRUE),N))),
           "Manuscript denominator checks")
show_table(tibble(Note=c(
  "A non-significant association test does not demonstrate independence; descriptive sample-size medians do not establish equivalence between tiers.",
  "The number of layers in the original depth profile counts four core layers only; it excludes other omics and SCFA panels.",
  "The network inventory requires review before using a mutually exclusive 13/11 classification or claiming all networks are correlation-derived.",
  "Original broad method-family rules are retained so results are not silently changed to match or contradict the manuscript. See the coding audit before interpreting exact family counts.",
  "Supplementary Figure 3 uses all studies within each tier. The conditional validation counts and test are provided separately.",
  "The colleague's manuscript text is not edited by this script.")),"Interpretation and scope notes")
html_out(report_footer)
cat("\nHTML report:", normalizePath(report_path), "\nPDF figures:", normalizePath(FIG_DIR), "\n")
