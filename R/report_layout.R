# Shared HTML layout for the manuscript section reports.
report_header <- function(title, loaded) {
  title <- gsub("&", "&amp;", title, fixed = TRUE)
  title <- gsub("<", "&lt;", title, fixed = TRUE)
  title <- gsub(">", "&gt;", title, fixed = TRUE)
  paste0('<!doctype html><html lang="en-US"><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    '<title>', title, '</title><style>',
    'body{font:16px/1.45 Arial,sans-serif;max-width:1200px;margin:32px auto;padding:0 24px;color:#222}',
    'h1{margin:0 0 6px}header p{margin:0}header{margin-bottom:28px}',
    'h2,h3{margin:28px 0 8px}p.source{font-size:0.8125rem;color:#666;line-height:1.4;margin:6px 0 12px}',
    'table{border-collapse:collapse;width:100%;margin:12px 0 24px}th,td{border:1px solid #ddd;padding:8px;text-align:left;vertical-align:top}',
    'th{background:#eaf1f6}.table,.scroll{overflow-x:auto}img{max-width:100%;height:auto}',
    '.note{background:#f6f8f9;padding:8px 12px}tr.total td{font-weight:bold}.toc{columns:2}',
    '</style></head><body><header><h1>', title, '</h1><p>Loaded studies: ', loaded,
    '</p></header><main>')
}
report_footer <- '</main></body></html>'

supplementary_caption <- function(title) {
  paste(title, "Abbreviations are defined in Supplementary Appendix 2.")
}
