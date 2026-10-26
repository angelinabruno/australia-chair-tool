# ---- One-line takeaways under each chart ------------------------------------
# Chart titles stay descriptive (decision of 12 Oct 2026); the finding goes in
# a one-line caption under the chart instead. Each chart script calls
#   takeaway("Trade/us_au_trade_balance.png", "A$ text", "US$ text")
# right after saving the chart, with text built from the same data, so the
# line updates with each quarterly refresh.
#
# The lines are kept in takeaways.tsv in the dashboard folder (chart, aud,
# usd; tab-separated). theme/takeaways.lua puts each one under its chart when
# '2. Australia Chair Tool.qmd' is rendered; the A$/US$ toggle swaps the two
# versions. A chart with no line simply has no caption.
#
# Sourced at the top of each chart script; needs DASHBOARD_DIR (section 1 of
# '1. Refresh all data.R').

TAKEAWAY_FILE <- file.path(Sys.getenv("DASHBOARD_DIR"), "takeaways.tsv")

# chart: path relative to the dashboard folder, as the .qmd writes it
# aud:   the line in A$ (or the only line, for charts not in dollars)
# usd:   the line in US$; defaults to the A$ line
takeaway <- function(chart, aud, usd = aud) {
  clean <- function(x) gsub("[\t\r\n]+", " ", trimws(x))
  old <- if (file.exists(TAKEAWAY_FILE))
    utils::read.delim(TAKEAWAY_FILE, colClasses = "character", quote = "",
                      encoding = "UTF-8") else
    data.frame(chart = character(), aud = character(), usd = character())
  new <- data.frame(chart = chart, aud = clean(aud), usd = clean(usd))
  out <- rbind(old[old$chart != chart, , drop = FALSE], new)
  out <- out[order(out$chart), ]
  con <- file(TAKEAWAY_FILE, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  utils::write.table(out, con, sep = "\t", quote = FALSE, row.names = FALSE)
  invisible(chart)
}

# Money for takeaways: A$1.4tn, A$60.5bn, A$850m
fmt_bn <- function(bn, sym = "A$", digits = 1) {
  ifelse(abs(bn) >= 1000, sprintf("%s%.2ftn", sym, bn / 1000),
         ifelse(abs(bn) >= 1, sprintf(paste0("%s%.", digits, "fbn"), sym, bn),
                sprintf("%s%.0fm", sym, bn * 1000)))
}
