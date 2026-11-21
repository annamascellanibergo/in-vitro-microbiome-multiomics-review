# Reproduce Figure 2 (PRISMA flow diagram) using PRISMA2020.
# Run this whole script in RStudio. Internet access is needed only to install
# missing packages. The vector PDF is written to PRISMA_output/Fig2.pdf.
# Package documentation:
# https://prisma-flowdiagram.github.io/PRISMA2020/reference/PRISMA_flowdiagram.html

packages <- c("PRISMA2020", "DiagrammeRsvg", "xml2", "rsvg")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")

output_dir <- file.path(getwd(), "PRISMA_output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Final inclusion count and database breakdown from the supplied figures.
included_studies <- 102L

counts <- c(
  database_results = 7702, register_results = NA_integer_,
  duplicates = 2294, excluded_automatic = 0, excluded_other = 0,
  records_screened = 5408, records_excluded = 5227,
  dbr_sought_reports = 181, dbr_notretrieved_reports = 0,
  dbr_assessed = 181,
  new_studies = included_studies, new_reports = NA_integer_
)

# Use the template bundled with the installed package, so all fields required
# by PRISMA_data() are present (including unused arms of the diagram).
template <- read.csv(
  system.file("extdata", "PRISMA.csv", package = "PRISMA2020"),
  stringsAsFactors = FALSE, check.names = FALSE
)
template$n <- as.character(template$n)
for (field in names(counts)) {
  row <- which(!is.na(template$data) & template$data == field)
  if (length(row) != 1L) stop("Template field missing or ambiguous: ", field)
  template$n[row] <- as.character(counts[[field]])
}
row <- which(!is.na(template$data) & template$data == "dbr_excluded")
template$n[row] <- "Not in vitro, 3; Not multi-omics, 76"

# Named databases: 5,138 + 1,516 + 1,048 = 7,702.
row <- which(!is.na(template$data) & template$data == "database_specific_results")
template$n[row] <- "Scopus, 5138; Web of Science, 1516; PubMed, 1048; Records identified through citation searching, 0"
row <- which(!is.na(template$data) & template$data == "new_studies")
template$boxtext[row] <- "Studies included in the review"

prisma_data <- PRISMA2020::PRISMA_data(template)
flow <- PRISMA2020::PRISMA_flowdiagram(
  data = prisma_data,
  interactive = FALSE,
  previous = FALSE,
  other = FALSE,
  detail_databases = TRUE,
  detail_registers = FALSE,
  fontsize = 14,
  font = "Helvetica",
  title_colour = "Goldenrod1",
  greybox_colour = "Gainsboro",
  main_colour = "Black",
  arrow_colour = "Black",
  arrow_head = "normal",
  arrow_tail = "none",
  side_boxes = TRUE
)

# Annotate the PubMed count before Graphviz sizes the identification box.
pubmed_count <- "1,?048"
if (!grepl(pubmed_count, flow$x$diagram)) stop("Cannot locate the PubMed count")
flow$x$diagram <- sub(pubmed_count, "1,048, including 3 preprints", flow$x$diagram)

# Export the package's diagram. Its vertical labels are normally inserted by
# browser JavaScript; insert them directly into SVG so they also appear in PDF/PNG.
doc <- xml2::read_xml(DiagrammeRsvg::export_svg(flow))
xml2::xml_ns_strip(doc)
root <- xml2::xml_root(doc)

number_values <- function(value) {
  match <- gregexpr("[-+]?[0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?", value, perl = TRUE)
  as.numeric(regmatches(value, match)[[1]])
}

# Use namespace-independent XPath. Tooltips can wrap shapes in <a>/<g>.
shape_xpath <- ".//*[local-name()='path' or local-name()='polygon' or local-name()='rect']"
shape_center <- function(shape) {
  kind <- xml2::xml_name(shape)
  if (kind == "rect") {
    return(c(
      as.numeric(xml2::xml_attr(shape, "x")) + as.numeric(xml2::xml_attr(shape, "width"))/2,
      as.numeric(xml2::xml_attr(shape, "y")) + as.numeric(xml2::xml_attr(shape, "height"))/2
    ))
  }
  values <- number_values(xml2::xml_attr(shape, if (kind == "path") "d" else "points"))
  if (!length(values) || length(values) %% 2L) stop("Unexpected SVG coordinates")
  xy <- matrix(values, ncol = 2, byrow = TRUE)
  c(mean(range(xy[, 1])), mean(range(xy[, 2])))
}

nodes <- xml2::xml_find_all(root,
                            ".//*[local-name()='g' and contains(concat(' ', normalize-space(@class), ' '), ' node ')]")

# Fallback: the three blue side bars, sorted from top to bottom.
blue_nodes <- nodes[vapply(nodes, function(node) {
  fills <- tolower(xml2::xml_attr(xml2::xml_find_all(node, shape_xpath), "fill"))
  any(fills %in% c("#bcd2ee", "lightsteelblue2", "rgb(188,210,238)"))
}, logical(1))]
if (length(blue_nodes) == 3L) {
  ys <- vapply(blue_nodes, function(node) {
    shape_center(xml2::xml_find_first(node, shape_xpath))[2]
  }, numeric(1))
  blue_nodes <- blue_nodes[order(ys)]
}

stages <- c("identification", "screening", "included")
for (i in seq_along(stages)) {
  stage <- stages[i]
  node <- xml2::xml_find_first(root, sprintf(
    ".//*[local-name()='g' and @class='node'][*[local-name()='title' and normalize-space(.)='%s']]",
    stage
  ))
  if (inherits(node, "xml_missing")) {
    if (length(blue_nodes) != 3L) stop(
      "Cannot locate the three blue stage bars. Try updating PRISMA2020."
    )
    node <- blue_nodes[[i]]
  }
  shape <- xml2::xml_find_first(node, shape_xpath)
  center <- shape_center(shape)
  cx <- center[1]; cy <- center[2]
  xml2::xml_remove(xml2::xml_find_all(node, ".//*[local-name()='text']"))
  label <- xml2::xml_add_child(node, "text", x = as.character(cx), y = as.character(cy),
                               transform = sprintf("rotate(-90 %s %s)", cx, cy),
                               `text-anchor` = "middle", `dominant-baseline` = "central",
                               `font-family` = "Helvetica,Arial,sans-serif", `font-size` = "14", fill = "black")
  xml2::xml_text(label) <- paste0(toupper(substr(stage, 1, 1)), substring(stage, 2))
}

# Restore the SVG namespace only after the XML edits.
xml2::xml_set_attr(root, "xmlns", "http://www.w3.org/2000/svg")

# Preserve the reference's nearly square aspect ratio.
# The package supplies all box positions and arrows; no screenshot is embedded.
xml2::xml_set_attr(root, "width", "210mm")
xml2::xml_set_attr(root, "height", "208.32mm")
xml2::xml_set_attr(root, "preserveAspectRatio", "none")
vb <- number_values(xml2::xml_attr(root, "viewBox"))

# White background for reliable export.
background <- xml2::xml_add_child(root, "rect", .where = 0,
                                  x = as.character(vb[1]), y = as.character(vb[2]),
                                  width = as.character(vb[3]), height = as.character(vb[4]), fill = "white")

# Render directly from vector paths and text at A4 page width.
pdf_file <- file.path(output_dir, "Fig2.pdf")
rsvg::rsvg_pdf(charToRaw(enc2utf8(as.character(doc))), pdf_file)
message("Saved Figure 2 in: ", normalizePath(pdf_file))
