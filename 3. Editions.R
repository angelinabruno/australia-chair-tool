# ---- Quarterly editions: keep each quarter's dashboard so people can go back --
#
# Each "edition" is the dashboard as it stood between two release dates. The
# release dates live in editions/release_dates.csv: one row per edition, set
# to the ABS national accounts (5206.0) release dates. To change when editions
# roll over, edit that file (add, move or delete dates; the label column is
# what readers see in the Edition menu). Nothing else needs changing.
#
# How it works
#   - '2. Australia Chair Tool.qmd' stamps the rendered dashboard with the
#     edition it belongs to (the latest release date on or before the day it
#     is rendered).
#   - '1. Refresh all data.R' calls archive_previous_edition() before it
#     refreshes anything. If the published dashboard belongs to an earlier
#     edition than today's, a copy of it is filed in editions/ (e.g.
#     editions/2026-09-02.html) and listed in editions/editions.json.
#   - The Edition menu in the dashboard's top bar reads editions.json, so
#     older editions (and the dashboard itself) always see the full list.
#
# Size: each edition is a full copy of the dashboard (about 80 MB). GitHub
# Pages sites are limited to 1 GB, so about ten editions fit alongside the
# live dashboard. Older ones can be moved elsewhere later (delete the file and
# its line in editions.json).

EDITIONS_DIR  <- file.path(Sys.getenv("DASHBOARD_DIR"), "editions")
RELEASE_FILE  <- file.path(EDITIONS_DIR, "release_dates.csv")
MANIFEST_FILE <- file.path(EDITIONS_DIR, "editions.json")
LIVE_HTML     <- file.path(Sys.getenv("DASHBOARD_DIR"), "2. Australia Chair Tool.html")

# The edition a given day falls in: the latest release date on or before it.
edition_for <- function(day = Sys.Date()) {
  rel <- utils::read.csv(RELEASE_FILE, stringsAsFactors = FALSE)
  rel$release_date <- as.Date(rel$release_date)
  rel <- rel[order(rel$release_date), ]
  i <- max(c(0, which(rel$release_date <= as.Date(day))))
  if (i == 0) {
    # Before the first date in the file: use the first row's label
    return(list(id = format(rel$release_date[1]), label = rel$label[1], data_to = rel$data_to[1]))
  }
  list(id = format(rel$release_date[i]), label = rel$label[i], data_to = rel$data_to[i])
}

# The stamp the .qmd writes into the dashboard's Overview header ("September
# 2026 edition"); its data- attributes are read by the Edition menu.
edition_stamp <- function(ed = edition_for()) {
  sprintf('<span id="csis-edition" data-id="%s" data-label="%s" data-data-to="%s" data-archived="false">%s edition</span>',
          ed$id, ed$label, ed$data_to, ed$label)
}

read_manifest <- function() {
  if (!file.exists(MANIFEST_FILE)) return(list())
  jsonlite::fromJSON(MANIFEST_FILE, simplifyDataFrame = FALSE)
}

write_manifest <- function(m) {
  m <- m[order(vapply(m, `[[`, "", "id"), decreasing = TRUE)]
  writeLines(jsonlite::toJSON(m, auto_unbox = TRUE, pretty = TRUE), MANIFEST_FILE)
}

# Files the published dashboard as an edition if a new release date has passed
# since it was rendered. Safe to run any number of times.
archive_previous_edition <- function(today = Sys.Date()) {
  dir.create(EDITIONS_DIR, showWarnings = FALSE)
  if (!file.exists(LIVE_HTML)) return(invisible(NULL))
  html <- readChar(LIVE_HTML, file.size(LIVE_HTML), useBytes = TRUE)
  stamp <- regmatches(html, regexpr('<span id="csis-edition"[^>]*>', html, useBytes = TRUE))
  if (!length(stamp)) {
    message("Editions: the dashboard has no edition stamp yet (rendered before editions were set up); nothing to file.")
    return(invisible(NULL))
  }
  live_id <- sub('.*data-id="([^"]+)".*', "\\1", stamp)
  now_id  <- edition_for(today)$id
  if (live_id >= now_id) {
    message("Editions: the dashboard is already the ", live_id, " edition; nothing to file.")
    return(invisible(NULL))
  }
  dest <- file.path(EDITIONS_DIR, paste0(live_id, ".html"))
  if (!file.exists(dest)) {
    archived <- sub('data-archived="false"', 'data-archived="true"', stamp, fixed = TRUE)
    html <- sub(stamp, archived, html, fixed = TRUE, useBytes = TRUE)
    writeChar(html, dest, eos = NULL, useBytes = TRUE)
    message("Editions: filed the ", live_id, " edition as editions/", basename(dest))
  }
  m <- read_manifest()
  if (!live_id %in% vapply(m, `[[`, "", "id")) {
    ed <- edition_for(as.Date(live_id))
    m[[length(m) + 1]] <- list(id = live_id, label = ed$label, data_to = ed$data_to,
                               file = basename(dest))
    write_manifest(m)
  }
  invisible(dest)
}
