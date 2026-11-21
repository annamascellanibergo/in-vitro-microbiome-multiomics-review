# Row 1 contains section headings, row 2 contains field names, and each
# subsequent row with a title represents a study. Keep Excel coordinates
# for source notes and distinguish fields repeated across omics layers.
read_extraction <- function(path, sheet) {
  cells <- readxl::read_excel(path, sheet = sheet, col_names = FALSE,
                              col_types = "text", .name_repair = "unique_quiet")
  if (nrow(cells) < 3L) stop("Expected two header rows and at least one study.")
  field <- trimws(gsub("[[:space:]]+", " ", unlist(cells[2, ], use.names = FALSE)))
  field[field %in% "QC Samples / Internal Standards"] <- "Quality Control Samples / Internal Standards"
  section <- unlist(cells[1, ], use.names = FALSE)
  section[section %in% "Go to pdf folder"] <- NA_character_
  info <- tibble::tibble(position = seq_along(field), name = field, section = section) |>
    tidyr::fill(section) |>
    dplyr::filter(!is.na(name), nzchar(name)) |>
    dplyr::mutate(section = dplyr::coalesce(section, "0. Study information")) |>
    dplyr::group_by(name) |>
    dplyr::mutate(col = if (dplyr::n() > 1L) {
      paste(name, "-", sub(".* - ", "", sub("^[0-9]+\\.\\s*", "", section)))
    } else name) |>
    dplyr::ungroup() |>
    dplyr::mutate(col = make.unique(col, sep = "_"))
  if (sum(info$name == "Title") != 1L) stop("Expected exactly one Title field.")
  studies <- cells[-c(1, 2), info$position, drop = FALSE]
  names(studies) <- info$col
  # Standardize spelling only in coded labels used in figures and tables.
  label_fields <- info$name %in% c("Biological Source", "Sample Site", "Microbial Source",
                                  "Perturbations / Treatments macroclass", "Transformation Method")
  us_spelling <- c(faecal = "fecal", faeces = "feces", fibre = "fiber", fibres = "fibers",
                   centring = "centering", centred = "centered", normalisation = "normalization")
  for (column in info$col[label_fields]) {
    values <- studies[[column]]
    for (word in names(us_spelling)) {
      pattern <- paste0("\\b", word, "\\b")
      values <- gsub(pattern, us_spelling[[word]], values, perl = TRUE)
      values <- gsub(paste0("\\b", tools::toTitleCase(word), "\\b"),
                     tools::toTitleCase(us_spelling[[word]]), values, perl = TRUE)
    }
    studies[[column]] <- values
  }
  keep <- !is.na(studies$Title) & nzchar(trimws(studies$Title))
  if (!any(keep)) stop("No study rows found.")
  list(data = studies[keep, , drop = FALSE], columns = info, excel_rows = which(keep) + 2L)
}

# Resolve source notes against the positions in the workbook used for this run.
workbook_source_note <- function(info, sources) {
  key <- function(x) sub("at least on tool", "at least one tool", tolower(x), fixed = TRUE)
  if (!length(sources)) return("Source: explanatory text from the analysis script.")
  descriptions <- vapply(unique(sources), function(field) {
    i <- which(key(info$col) == key(field))
    if (length(i) != 1L) stop("Cannot resolve source column: ", field)
    n <- info$position[i]
    letter <- ""
    while (n > 0L) {
      letter <- paste0(LETTERS[(n - 1L) %% 26L + 1L], letter)
      n <- (n - 1L) %/% 26L
    }
    sprintf("%s: %s (Excel column %s)", info$section[i], info$name[i], letter)
  }, character(1))
  paste0("Source columns: ", paste(descriptions, collapse = "; "), ".")
}
