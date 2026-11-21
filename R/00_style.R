# Shared publication colors ------------------------------------------------
# Use from the repository root. Category mappings remain explicit in each plot.
# Distinguish a documented negative result from missing reporting.
MANUSCRIPT_PALETTE <- c(
  method_bar_gray = "#B9C2CB",
  replication_blue = "#4477AA",
  replication_indigo = "#332288",
  replication_cyan = "#66CCEE",
  replication_magenta = "#AA3377",
  omics_orange = "#D66A28",
  omics_peach = "#F2B880",
  omics_brown = "#8C3B12",
  omics_rose = "#CC7A86",
  omics_dark = "#65352C",
  omics_sand = "#E4CEAE",
  teal = "#2E7B78",
  light_teal = "#6FB3B0",
  pale_teal = "#A9D3D1",
  gold = "#C99A44",
  purple = "#946C99",
  blue_light = "#C9D6E3",
  blue_mid = "#6B9AC4",
  blue_dark = "#1F4E79",
  not_reported = "#B8BEC4",
  not_applicable = "#ECEEF0",
  neutral = "#7E8FA6",
  text = "#222222",
  muted_text = "#4A4A4A",
  grid = "#E7EBEE",
  alert = "#D55E00")
pal <- function(name) {
  if (any(!name %in% names(MANUSCRIPT_PALETTE))) stop("Unknown palette key")
  unname(MANUSCRIPT_PALETTE[name])
}
# Text sizes are points; geom_text uses millimeters (9 pt is about 3.2 mm).
FIGURE_BASE_PT <- 11
FIGURE_LABEL_PT <- 9
