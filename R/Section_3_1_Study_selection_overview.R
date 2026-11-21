# Manuscript section 3.1 Study selection overview
# Repository entry point: run from the repository root.
source("R/00_style.R", local = TRUE)
source("R/report_layout.R", local = TRUE)

# =============================================================================
# Scoping review: standalone section script
# Plots are saved in figures; all tables are combined into ONE HTML report per section.
# For country tables/maps: countrycode, sf, rnaturalearth, rnaturalearthdata, patchwork.
# Use Not reported for unavailable information. Each study counts once per country.
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
first_year <- 2015                            # first year shown in the year plot
country_column <- "Countries"

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
reviews <- reviews %>% mutate(across(where(is.character), clean_text))

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
source_aliases <- c(Integration = "Integration Type", Year = "Year of Publication",
                    Perturbation = "Experimental design", Time_series_label = "Experimental design",
                    Combination = "Other omics layer (specify)", Microbiome_approach = "Metagenomics",
                    SCFA_BCFA = "SCFA/BCFA panel (targeted adjunct)",
                    Exogenous_compound = "Targeted exogenous compound and its direct catabolites or other methods")
omics_source_columns <- c("Metagenomics", "Metatranscriptomics", "Metaproteomics",
                          "Metabolomics", "Other omics layer (specify)")
source_registry <- list(
  S1_study_selection = character(0),
  S1_publication_year_counts = "Year of Publication",
  S2_omics_presence = omics_source_columns,
  S2_platforms_and_instruments = c("Sequencing Platform", "Instrument Method",
                                   "Metabolomics analytical platform", "Metaproteomics analytical platform"),
  "Missing or invalid publication years" = c("Title", "Year of Publication"),
  "Annual counts by integration category (including zero counts)" = c("Year of Publication", "Integration Type"),
  "Study-level omics classifications" = c("Title", omics_source_columns),
  "Study-level platform entries (deduplicated)" = c("Sequencing Platform", "Instrument Method",
                                                    "Metabolomics analytical platform", "Metaproteomics analytical platform"),
  "Figure 3b: combination key" = omics_source_columns,
  "Figure 3b: intersection counts by profiling approach" = omics_source_columns
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
# Format displayed tables only; numerical analysis and plotting data stay intact.
# Year and Study_row are identifiers, not counts, and are never formatted as percentages.
format_supplementary_table <- function(tab) {
  out <- as.data.frame(tab, stringsAsFactors = FALSE)
  count_columns <- intersect(c("n", "Studies", "All_studies_in_year",
                               "Studies_with_identified_country",
                               "Studies_without_identified_country"), names(out))
  year_denominator <- if ("All_studies_in_year" %in% names(out)) {
    out$All_studies_in_year
  } else NULL
  for (column in count_columns) {
    denominator <- if (!is.null(year_denominator) && column != "All_studies_in_year") {
      year_denominator
    } else rep(n_studies, nrow(out))
    values <- out[[column]]
    formatted <- rep(NA_character_, length(values))
    valid <- !is.na(values) & !is.na(denominator) & denominator > 0
    formatted[valid] <- sprintf("%d (%.1f%%)", as.integer(values[valid]),
                                100 * values[valid] / denominator[valid])
    undefined <- !is.na(values) & (is.na(denominator) | denominator <= 0)
    formatted[undefined] <- sprintf("%d (NA)", as.integer(values[undefined]))
    out[[column]] <- formatted
  }
  # Remove separate percentage columns now combined with their corresponding count.
  if (length(count_columns)) {
    out <- out[, setdiff(names(out), c("percent", "percent_of_loaded_studies",
                                       "Studies (%)", "Percentage_within_year")), drop = FALSE]
    for (column in count_columns) {
      label <- if (column == "n") "n (%)" else paste0(column, " n (%)")
      names(out)[names(out) == column] <- label
    }
  }
  out
}
show_table <- function(tab, title, filename = NULL) {
  sources <- attr(tab, "source_columns")
  if (!is.null(filename) && filename %in% names(source_registry)) sources <- source_registry[[filename]]
  if (title %in% names(source_registry)) sources <- source_registry[[title]]
  if (is.null(sources)) stop(paste("Source columns not defined for table:", title))
  if ("Combination" %in% sources) sources <- unique(c(setdiff(sources, "Combination"), omics_source_columns))
  sources <- vapply(sources, function(x) if (x %in% names(source_aliases)) source_aliases[[x]] else x, character(1))
  table_no <<- table_no + 1
  tab <- format_supplementary_table(tab)
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

# Show a plot and save only a white-background vector PDF.
save_plot <- function(plot, name, caption, width = 7, height = 4) {
  figure_no <<- figure_no + 1
  cat("\n===== Figure ", figure_no, ". ", caption, " =====\n", sep = "")
  print(plot)
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
          panel.spacing = grid::unit(6, "mm"),
          plot.title = element_text(face = "bold", size = base_size + 3, hjust = 0),
          plot.caption = element_text(size = base_size - 1, hjust = 0, color = pal("muted_text")),
          legend.title = element_text(size = base_size),
          legend.text = element_text(size = base_size - 1),
          plot.margin = margin(6, 9, 6, 6))
}
theme_set(theme_publication())


start_report("Section_3_1_Study_selection_overview.html",
             "3.1 Study selection overview")

# Keep original extraction labels in bars, legend and tables. Classify only
# for color assignment; never substitute the shortened color categories.
integration_levels <- c("No cross-layer statistic documented", "Cross-layer association",
                        "Joint cross-layer model", "Not reported")
# A restrained categorical palette with clear hue/lightness differences.
integration_colors <- c("No cross-layer statistic documented" = pal("not_reported"),
                         "Cross-layer association" = pal("blue_mid"),
                         "Joint cross-layer model" = pal("blue_dark"),
                         "Not reported" = pal("not_reported"))
classify_integration <- function(x) {
  z <- tolower(trimws(x))
  case_when(
    is.na(z) | z == "" | z == "not reported" ~ "Not reported",
    z == "no cross-layer statistic documented" ~ "No cross-layer statistic documented",
    z == "cross-layer association" ~ "Cross-layer association",
    z == "joint cross-layer model" ~ "Joint cross-layer model",
    TRUE ~ "Other / check coding"
  )
}
overview <- reviews %>% mutate(
  Year = suppressWarnings(as.numeric(`Year of Publication`)),
  Year = ifelse(!is.na(Year) & Year == floor(Year) & Year >= 1900 & Year <= 2100,
                as.integer(Year), NA_integer_),
  Integration = classify_integration(`Integration Type`),
  Integration_label = replace_na(as.character(`Integration Type`), "Missing (empty cell)")
)
if (anyNA(overview$Year)) warning("Some publication years are missing/invalid; these studies are omitted from the annual plot. Inspect overview in R.")
if (any(overview$Integration == "Other / check coding")) {
  warning("Unrecognized integration labels: retained as Other / check coding.")
  integration_levels <- c(integration_levels, "Other / check coding")
  integration_colors <- c(integration_colors, "Other / check coding" = pal("alert"))
}
integration_frequency <- overview %>% count(Integration_label, Integration)
integration_frequency <- integration_frequency %>%
  mutate(codebook_order = match(Integration, integration_levels)) %>%
  group_by(Integration) %>%
  mutate(group_total = sum(n)) %>%
  ungroup() %>%
  arrange(desc(group_total), codebook_order, desc(n), Integration_label)
integration_order <- integration_frequency$Integration_label
year_data <- overview %>% filter(!is.na(Year))
if (nrow(year_data) > 0) {
  # Journal-volume years are retained: 2026 entries may have appeared online in 2025.
  # The supplied column cannot establish first-online dates; do not recode or exclude them.
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop('Install the panel-layout package first: install.packages("patchwork")')
  }
  # Show the observed publication-year range, as in the reference layout.
  years_to_show <- seq(min(year_data$Year), max(year_data$Year))
  figure1_order <- c("No cross-layer statistic documented", "Cross-layer association",
                     "Joint cross-layer model")
  figure1_order <- c(figure1_order, setdiff(unique(year_data$Integration), figure1_order))
  figure1_colors <- c("No cross-layer statistic documented" = pal("blue_light"),
                       "Cross-layer association" = pal("blue_mid"),
                       "Joint cross-layer model" = pal("blue_dark"),
                       "Not reported" = pal("not_reported"),
                       "Other / check coding" = pal("alert"))
  figure1_labels <- setNames(figure1_order, figure1_order)
  figure1_labels["No cross-layer statistic documented"] <- "No cross-layer statistic"
  year_totals <- year_data %>% count(Year, name = "n") %>%
    complete(Year = years_to_show, fill = list(n = 0L)) %>%
    mutate(percent = 100 * n / n_studies)
  year_counts <- year_data %>%
    mutate(Integration = factor(Integration, levels = figure1_order)) %>%
    count(Year, Integration) %>%
    complete(Year = years_to_show, Integration, fill = list(n = 0L)) %>%
    left_join(year_totals %>% transmute(Year, All_studies_in_year = n), by = "Year") %>%
    mutate(percent = ifelse(All_studies_in_year > 0,
                            100 * n / All_studies_in_year, NA_real_))
  stopifnot(sum(year_totals$n) == nrow(year_data))
  # Within-year percentages also feed the annual integration table in the HTML report.
  figure1_theme <- theme_classic(base_size = 11, base_family = "sans") +
    theme(panel.grid.major.y = element_line(color = pal("grid"), linewidth = 0.3),
          axis.text = element_text(color = "black"),
          axis.text.x = element_text(angle = 0, hjust = 0.5),
          plot.title = element_text(face = "bold", size = 10, hjust = 0),
          plot.tag = element_text(face = "bold", size = 12),
          plot.margin = margin(6, 8, 6, 6))
  p_year_share <- ggplot(year_counts,
                         aes(factor(Year, levels = years_to_show), percent, fill = Integration)) +
    geom_col(width = 0.8, position = "stack", na.rm = TRUE) +
    geom_text(data = year_totals,
              aes(x = factor(Year, levels = years_to_show), y = 105,
                  label = paste0("n=", n)),
              inherit.aes = FALSE, size = 3.2) +
    scale_fill_manual(values = figure1_colors, breaks = figure1_order,
                      labels = figure1_labels, drop = FALSE) +
    scale_x_discrete(drop = FALSE, expand = expansion(add = 0.6)) +
    scale_y_continuous(limits = c(0, 112), breaks = seq(0, 100, 25),
                       labels = function(x) sprintf("%.0f%%", x), expand = c(0, 0)) +
    labs(x = "Year of publication", y = "Share of studies",
         title = "Integration depth by year of publication", fill = NULL) +
    figure1_theme +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
    theme(legend.position = "bottom", legend.text = element_text(size = 9),
          legend.key.width = grid::unit(3, "mm"),
          legend.key.height = grid::unit(3, "mm"))
  p_year_number <- ggplot(year_totals, aes(factor(Year, levels = years_to_show), n)) +
    geom_col(width = 0.8, fill = pal("neutral")) +
    geom_text(aes(label = n), vjust = -0.4, size = 3.2) +
    scale_x_discrete(drop = FALSE, expand = expansion(add = 0.6)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(x = "Year of publication", y = "Studies (n)", title = "Studies published per year") +
    figure1_theme
  p_year <- patchwork::wrap_plots(p_year_share, p_year_number,
                                  ncol = 1, heights = c(2, 1)) +
    patchwork::plot_annotation(tag_levels = "a")
  # Supplementary Figure 1: vertically stacked panels at A4 portrait width.
  supp1_width <- 210 / 25.4
  supp1_height <- 180 / 25.4
  save_plot(p_year, "Supplementary_Figure_1",
            "Supplementary Figure 1: within-year integration shares and annual study counts",
            width = supp1_width, height = supp1_height)
  # Generate a temporary PNG for the self-contained HTML preview, then delete
  # it. The PDF remains the only standalone figure in the figures folder.
  preview_path <- tempfile(pattern = "Supplementary_Figure_1_", fileext = ".png")
  tryCatch({
    ggsave(preview_path, p_year, width = supp1_width, height = supp1_height,
           dpi = 300, bg = "white")
    add_report_figure(preview_path, "Supplementary Figure 1: publication year by integration category",
                      c("Year of Publication", "Integration Type"))
  }, finally = unlink(preview_path))
}
# Original extraction labels only; no duplicate table of shortened categories.
integration_table_data <- overview %>% mutate(`Integration Type` = Integration_label)
show_table(count_table(integration_table_data, "Integration Type", "Integration category",
                       levels = integration_order),
           "Integration category, percentages of loaded studies", "S1_integration_descriptions")
if (nrow(year_data) > 0) {
  show_table(year_totals %>% mutate(percent_of_loaded_studies = 100 * n / n_studies),
             "Annual publication counts", "S1_publication_year_counts")
  show_table(year_counts, "Annual counts by integration category (including zero counts)")
}

# ---- Geographic distribution of contributing institutions -------------------
# Study rows, rather than individual authors or institutions, are the counting unit.
country_info <- col_info %>% filter(name == country_column)
if (nrow(country_info) > 1L) {
  country_info <- country_info %>% filter(grepl("Bibliographic Metadata", section, ignore.case = TRUE))
}
if (nrow(country_info) != 1L) {
  warning("Add one Countries column under 1. Bibliographic Metadata to produce the country table and world map.")
  cat('<h2>Geographic distribution</h2><p>Add a Countries column under 1. Bibliographic Metadata, with countries separated by semicolons.</p>',
      file = current_report, append = TRUE)
} else {
  required_country_packages <- c("countrycode", "sf", "rnaturalearth", "rnaturalearthdata", "patchwork")
  missing_country_packages <- required_country_packages[
    !vapply(required_country_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing_country_packages)) stop("Install the country-analysis packages first: install.packages(c(",
                                             paste(sprintf('"%s"', missing_country_packages), collapse = ", "), "))")
  country_source <- country_info$col[1]
  country_raw <- tibble(Study_row = seq_len(n_studies),
                        Countries_entered = as.character(reviews[[country_source]]))
  country_tokens <- country_raw %>%
    mutate(Countries_entered = replace_na(Countries_entered, "Not reported")) %>%
    separate_rows(Countries_entered, sep = ";") %>%
    mutate(Country_entered = trimws(Countries_entered),
           Missing = tolower(Country_entered) %in% c("", "not reported", "unknown", "na", "n/a"))
  country_aliases <- c("UK" = "GB", "U.K." = "GB", "England" = "GB", "Scotland" = "GB",
                       "Wales" = "GB", "Northern Ireland" = "GB", "USA" = "US", "U.S.A." = "US",
                       "United States of America" = "US", "Czech Republic" = "CZ", "Czechia" = "CZ",
                       "Peoples R China" = "CN", "People's Republic of China" = "CN", "South Korea" = "KR",
                       "Republic of Korea" = "KR", "Turkey" = "TR", "Turkiye" = "TR")
  country_tokens$ISO3 <- countrycode::countrycode(
    ifelse(country_tokens$Missing, NA_character_, country_tokens$Country_entered),
    origin = "country.name", destination = "iso3c", warn = FALSE)
  alias_index <- match(tolower(country_tokens$Country_entered), tolower(names(country_aliases)))
  alias_rows <- which(!is.na(alias_index))
  country_tokens$ISO3[alias_rows] <- countrycode::countrycode(
    unname(country_aliases[alias_index[alias_rows]]), "iso2c", "iso3c", warn = FALSE)
  country_pairs <- country_tokens %>% filter(!Missing, !is.na(ISO3)) %>%
    distinct(Study_row, ISO3) %>%
    mutate(Country = countrycode::countrycode(ISO3, "iso3c", "country.name", warn = FALSE))
  country_frequency <- country_pairs %>% count(Country, ISO3, name = "Studies") %>%
    mutate(`Studies (%)` = round(100 * Studies / n_studies, 1)) %>%
    arrange(desc(Studies), Country)
  attr(country_frequency, "source_columns") <- country_source
  show_table(country_frequency, "Country participation: included studies by affiliation country")
  
  country_unrecognized <- country_tokens %>% filter(!Missing, is.na(ISO3)) %>%
    select(Study_row, Country_entered)
  attr(country_unrecognized, "source_columns") <- country_source
  if (nrow(country_unrecognized)) {
    warning("Unrecognized country entries: check the country-coding audit in the HTML report.")
    show_table(country_unrecognized, "Country entries requiring correction")
  }
  country_coverage <- tibble(
    Status = c("At least one identified country", "No identified country", "Unrecognized country entries"),
    Studies = c(n_distinct(country_pairs$Study_row),
                n_studies - n_distinct(country_pairs$Study_row),
                n_distinct(country_unrecognized$Study_row))) %>%
    mutate(`Studies (%)` = round(100 * Studies / n_studies, 1))
  attr(country_coverage, "source_columns") <- country_source
  cat(paste0('<p>Each study is counted once for every affiliation country entered. Percentages use all ',
             n_studies, ' included studies and may sum above 100%. Blank and Not reported cells remain missing information. ',
             'Countries describe contributing author affiliations at publication, rather than nationality or experimental location.</p>'),
      file = current_report, append = TRUE)
  
  if (nrow(country_frequency)) {
    # Bundled medium-resolution country polygons: no online map download needed.
    world_countries <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
    world_countries$ISO3_join <- world_countries$iso_a3
    invalid_map_code <- is.na(world_countries$ISO3_join) | world_countries$ISO3_join == "-99"
    world_countries$ISO3_join[invalid_map_code] <- countrycode::countrycode(
      world_countries$admin[invalid_map_code], "country.name", "iso3c", warn = FALSE)
    world_countries <- world_countries %>% filter(admin != "Antarctica") %>%
      left_join(country_frequency %>% select(ISO3, Studies), by = c("ISO3_join" = "ISO3")) %>%
      mutate(Studies = replace_na(Studies, 0L))
    unmapped <- country_frequency %>% filter(!ISO3 %in% world_countries$ISO3_join)
    if (nrow(unmapped)) {
      attr(unmapped, "source_columns") <- country_source
      show_table(unmapped, "Countries counted in the table but absent from the map polygons")
    }
    p_country_map <- ggplot(world_countries) +
      geom_sf(aes(fill = Studies), color = "#62727D", linewidth = 0.2) +
      scale_fill_gradientn(colors = c("white", pal("blue_light"), pal("blue_mid")),
                           values = c(0, 1 / max(2, max(country_frequency$Studies)), 1),
                           limits = c(0, max(2, max(country_frequency$Studies))), name = "Studies (n)",
                           breaks = unique(round(pretty(c(0, max(country_frequency$Studies)), n = 5)))) +
      coord_sf(expand = FALSE, ylim = c(-60, 85)) +
      labs(caption = paste0("Each study counted once per affiliation country; ", n_studies,
                            " included studies.\nUnshaded countries have no identified contribution in the entered data. Source: Natural Earth.")) +
      theme_void(base_size = 11) +
      theme(legend.position = "bottom", legend.key.width = grid::unit(12, "mm"),
            plot.caption = element_text(size = 9, hjust = 0),
            plot.margin = margin(4, 4, 4, 4))
    print(p_country_map) # HTML preview only; country PDF is the combined supplementary figure.
    map_preview <- tempfile(pattern = "Country_map_preview_", fileext = ".png")
    tryCatch({
      ggsave(map_preview, p_country_map, width = 180 / 25.4, height = 105 / 25.4, dpi = 300, bg = "white")
      add_report_figure(map_preview, "Geographic distribution of contributing institutions", country_source)
    }, finally = unlink(map_preview))
  }
  
  # Country participation by publication year. Join using study-row identity so
  # repeated country entries never inflate either annual counts or denominators.
  study_years <- tibble(Study_row = seq_len(n_studies), Year = overview$Year)
  countries_with_year <- country_pairs %>% left_join(study_years, by = "Study_row") %>%
    filter(!is.na(Year))
  if (nrow(countries_with_year)) {
    years_country_plot <- seq(min(first_year, min(year_data$Year)), max(year_data$Year))
    # Count study rows directly, independently of the expanded country records.
    annual_denominators <- tibble(Year = years_country_plot,
                                  All_studies_in_year = vapply(years_country_plot,
                                                               function(y) sum(study_years$Year == y, na.rm = TRUE), integer(1)))
    stopifnot(sum(annual_denominators$All_studies_in_year) == nrow(year_data))
    country_year_counts <- countries_with_year %>% count(Country, Year, name = "Studies")
    country_year_table <- expand_grid(Country = country_frequency$Country, Year = years_country_plot) %>%
      left_join(country_year_counts, by = c("Country", "Year")) %>%
      left_join(annual_denominators, by = "Year") %>%
      mutate(Studies = replace_na(Studies, 0L),
             Percentage_within_year = ifelse(All_studies_in_year > 0L,
                                             100 * Studies / All_studies_in_year, NA_real_))
    stopifnot(all(country_year_table$Studies <= country_year_table$All_studies_in_year))
    attr(country_year_table, "source_columns") <- c(country_source, "Year of Publication")
    country_year_display <- country_year_table %>%
      mutate(Percentage_within_year = round(Percentage_within_year, 1))
    attr(country_year_display, "source_columns") <- c(country_source, "Year of Publication")
    show_table(country_year_display, "Country participation by year: counts and within-year percentages")
    excluded_year_n <- n_distinct(country_pairs$Study_row[is.na(
      study_years$Year[match(country_pairs$Study_row, study_years$Study_row)])])
    cat(paste0('<p>Within-year percentages use all included studies published in that year, including studies without country information. ',
               'A study contributes once to each affiliation country, so percentages can sum above 100%. ',
               excluded_year_n, ' studies with identified countries have no valid publication year and are omitted from these heatmaps. ',
               'Zero counts mean no identified contribution in the entered data; percentages are undefined for years with no studies.</p>'),
        file = current_report, append = TRUE)
    
    heatmap_data <- country_year_table %>% mutate(
      Country = factor(Country, levels = rev(country_frequency$Country)),
      Year_label = factor(Year, levels = years_country_plot),
      Count_label = ifelse(Studies > 0L, as.character(Studies), ""),
      Percent_label = ifelse(Studies > 0L & !is.na(Percentage_within_year),
                             sprintf("%.0f%%", Percentage_within_year), ""))
    heatmap_base <- ggplot(heatmap_data, aes(Year_label, Country)) +
      scale_x_discrete(labels = setNames(paste0(annual_denominators$Year,
                                                "\n(n=", annual_denominators$All_studies_in_year, ")"), annual_denominators$Year)) +
      labs(x = "Publication year", y = NULL) + theme_publication(10) +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            legend.position = "bottom", legend.key.width = grid::unit(12, "mm"),
            plot.caption = element_text(size = 9, hjust = 0))
    p_country_year_counts <- heatmap_base +
      geom_tile(aes(fill = Studies), color = "#AAB5BD", linewidth = 0.25) +
      geom_text(aes(label = Count_label,
                    color = Studies > max(Studies) * 0.55), size = 3.2, show.legend = FALSE) +
      scale_color_manual(values = c("FALSE" = pal("text"), "TRUE" = "white"), guide = "none") +
      scale_fill_gradientn(colors = c("white", pal("blue_mid"), pal("blue_mid")),
                           values = c(0, 1 / max(2, max(heatmap_data$Studies)), 1),
                           limits = c(0, max(2, max(heatmap_data$Studies))), name = "Studies (n)",
                           breaks = unique(round(pretty(c(0, max(heatmap_data$Studies)), n = 5)))) +
      labs(caption = "Each study counted once per affiliation country. Countries ordered by total participation.")
    p_country_year_percent <- heatmap_base +
      geom_tile(aes(fill = Percentage_within_year), color = "#AAB5BD", linewidth = 0.25) +
      geom_text(aes(label = Percent_label,
                    color = Percentage_within_year > 55), size = 3.2, show.legend = FALSE) +
      scale_color_manual(values = c("FALSE" = pal("text"), "TRUE" = "white"), guide = "none") +
      scale_fill_gradientn(colors = c("white", pal("blue_mid"), pal("blue_mid")),
                           values = c(0, min(50, heatmap_data$Percentage_within_year[
                             heatmap_data$Percentage_within_year > 0 & !is.na(heatmap_data$Percentage_within_year)]) / 100, 1),
                           limits = c(0, 100),
                           na.value = pal("not_reported"), labels = function(x) sprintf("%.0f%%", x),
                           name = "Studies within year (%)") +
      labs(caption = "Denominator: all included studies in each year. Gray: no studies that year.\nCountry percentages can sum above 100% because of international collaborations.")
    heatmap_height <- max(95, 50 + 4.5 * nrow(country_frequency)) / 25.4
    save_country_heatmap <- function(plot, filename, title) {
      print(plot) # Individual heatmaps are embedded in HTML only.
      preview <- tempfile(pattern = "Country_year_preview_", fileext = ".png")
      tryCatch({
        ggsave(preview, plot, width = 180 / 25.4, height = heatmap_height, dpi = 300, bg = "white")
        add_report_figure(preview, title, c(country_source, "Year of Publication"))
      }, finally = unlink(preview))
    }
    save_country_heatmap(p_country_year_percent, "Country_by_year_percentages",
                         "Country participation by publication year: within-year percentages")
    
    # Others is a UNION of study rows with at least one country outside the top ten.
    # Do not sum country counts: a study may list several countries in Others.
    top_countries <- head(country_frequency$Country, 10)
    others_by_year <- countries_with_year %>% filter(!Country %in% top_countries) %>%
      distinct(Study_row, Year) %>% count(Year, name = "Studies")
    others_table <- tibble(Year = years_country_plot, Country = "Others") %>%
      left_join(others_by_year, by = "Year") %>%
      mutate(Studies = replace_na(Studies, 0L))
    bubble_summary <- bind_rows(
      country_year_table %>% filter(Country %in% top_countries) %>% select(Country, Year, Studies),
      others_table)
    attr(bubble_summary, "source_columns") <- c(country_source, "Year of Publication")
    show_table(bubble_summary, "Top ten countries and Others by year (unique studies)")
    bubble_order <- c(top_countries, "Others")
    bubble_max <- max(bubble_summary$Studies)
    bubble_breaks <- unique(c(1, round(pretty(c(1, bubble_max), n = 4))))
    bubble_breaks <- bubble_breaks[bubble_breaks >= 1 & bubble_breaks <= bubble_max]
    p_bubble_others <- ggplot(bubble_summary %>% filter(Studies > 0L) %>%
                                mutate(Country = factor(Country, levels = rev(bubble_order)),
                                       Year_label = factor(Year, levels = years_country_plot)),
                              aes(Year_label, Country, size = Studies)) +
      geom_point(shape = 21, fill = pal("blue_mid"), color = "#24546E", stroke = 0.25) +
      scale_size_area(max_size = 5, limits = c(0, bubble_max),
                      breaks = bubble_breaks, name = "Studies (n)") +
      scale_x_discrete(drop = FALSE) +
      scale_y_discrete(limits = rev(bubble_order), drop = FALSE) +
      labs(x = "Publication year", y = NULL, title = "b") +
      theme_publication(10) +
      theme(panel.grid.major = element_line(color = pal("grid"), linewidth = 0.2),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            legend.position = "bottom", plot.title = element_text(face = "bold", hjust = 0))
    p_supplementary_map <- p_country_map +
      labs(title = "a", caption = NULL) +
      theme(plot.title = element_text(face = "bold", hjust = 0))
    # Supplementary Figure 2: horizontal panels, exactly the width of portrait A4 (210 mm).
    supplementary_width <- 210 / 25.4
    supplementary_height <- 105 / 25.4
    supplementary_figure_2 <- patchwork::wrap_plots(
      p_supplementary_map, p_bubble_others, ncol = 2, widths = c(1, 1)) +
      patchwork::plot_annotation(caption = paste0(
        "a: affiliation-country study counts. b: top ten countries plus Others; bubble area represents study count.\n",
        "Others counts each study once if it includes any country outside the top ten.\n",
        "Each study may contribute to multiple countries; zero counts are white in a and have no bubble in b. Map: Natural Earth."),
        theme = theme(plot.caption = element_text(size = 9, hjust = 0)))
    save_plot(supplementary_figure_2, "Supplementary_Figure_2",
              "Supplementary Figure 2: geographic distribution and country participation by year",
              width = supplementary_width, height = supplementary_height)
    supplementary_preview <- tempfile(pattern = "Supplementary_Figure_2_", fileext = ".png")
    tryCatch({
      ggsave(supplementary_preview, supplementary_figure_2,
             width = supplementary_width, height = supplementary_height, dpi = 300, bg = "white")
      add_report_figure(supplementary_preview, "Supplementary Figure 2", c(country_source, "Year of Publication"))
    }, finally = unlink(supplementary_preview))
  } else if (nrow(country_pairs)) {
    warning("Country-by-year heatmaps skipped: no country-coded studies have a valid publication year.")
  }
}

finish_report()
cat("\nFinished. HTML report:", normalizePath(current_report), "\nFigures:", normalizePath(fig_dir), "\n")
