# ==============================================================================
# Chart data: every chart saved with ggsave() also gets <name>_data.csv beside
# it, holding the data the chart plots. The dashboard links each chart to its
# CSV ("Download data") and reads the latest period from it ("Data to").
#
# Sourced near the top of the 1b-1j scripts (not Patents, which is refreshed
# by hand). It replaces ggsave() with a version that saves the chart as usual
# and then writes the CSV, so no chart code needs to change.
#   * US$ twins (<name>_usd.png) get no CSV: the A$ chart's CSV covers both.
#   * Charts made of several panels (patchwork) get one CSV with a "panel"
#     column naming each panel.
#   * The Trade in goods/services product charts (hundreds of them) are skipped.
# ==============================================================================

CHART_DATA_SKIP <- "^(goods_(exports|imports)|credits|debits)_[0-9]"

# Data frames behind one ggplot: the plot's own data plus any layer that brings
# its own (e.g. labels or reference lines). Columns are kept as text so that
# layers with different columns can be stacked.
chart_frames <- function(p) {
  as_text <- function(d) {
    d <- as.data.frame(d, stringsAsFactors = FALSE)
    d[] <- lapply(d, function(x) if (inherits(x, "Date")) format(x, "%Y-%m-%d") else as.character(x))
    d
  }
  out <- list()
  if (is.data.frame(p$data) && nrow(p$data) > 0) out[[1]] <- as_text(p$data)
  for (l in p$layers) {
    if (is.data.frame(l$data) && nrow(l$data) > 0 && !identical(l$data, p$data))
      out[[length(out) + 1]] <- cbind(as_text(l$data), layer = class(l$geom)[1])
  }
  if (!length(out)) return(NULL)
  dplyr::bind_rows(out)
}

chart_data <- function(plot) {
  if (inherits(plot, "patchwork")) {
    plots <- c(plot$patches$plots, list(plot))
    parts <- lapply(seq_along(plots), function(i) {
      d <- chart_frames(plots[[i]])
      if (is.null(d)) return(NULL)
      ttl <- plots[[i]]$labels$title
      cbind(panel = if (is.null(ttl)) paste("Panel", i) else ttl, d)
    })
    return(dplyr::bind_rows(parts))
  }
  if (inherits(plot, "ggplot")) return(chart_frames(plot))
  NULL
}

write_chart_data <- function(plot, filename) {
  file <- basename(filename)
  if (!grepl("\\.png$", file) || grepl("_usd\\.png$", file) || grepl(CHART_DATA_SKIP, file)) return(invisible(NULL))
  d <- tryCatch(chart_data(plot), error = function(e) NULL)
  if (is.null(d) || !nrow(d)) return(invisible(NULL))
  utils::write.csv(d, sub("\\.png$", "_data.csv", filename), row.names = FALSE, na = "")
  invisible(NULL)
}

ggsave <- function(filename, plot = ggplot2::last_plot(), ...) {
  res <- ggplot2::ggsave(filename, plot, ...)
  tryCatch(write_chart_data(plot, filename),
           error = function(e) message("Chart data not written for ", filename, ": ", conditionMessage(e)))
  invisible(res)
}
