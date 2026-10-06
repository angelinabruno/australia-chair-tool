rm(list = ls())
# The dashboard folder is set once, in section 1 of '1. Refresh all data.R'.
# To run this script on its own, run that section first.
if (Sys.getenv("DASHBOARD_DIR") == "")
  stop("Dashboard folder not set. Run section 1 of '1. Refresh all data.R' first.")
setwd(file.path(Sys.getenv("DASHBOARD_DIR"), "Investment"))
# ==============================================================================
# Foreign investment.R  -  two-way investment between Australia and its partners
#
# Source: ABS, International Investment Position, Australia: Supplementary
#         Statistics. Table 2 (foreign investment in Australia, level) and
#         Table 5 (Australian investment abroad, level), $ million.
#
# One click: downloads the latest Tables-all.zip from the ABS, extracts the two
# tables, finds the total level of investment for the latest year and draws
# Investment/Foreign investment/iip_levels_latest.png for the dashboard. No files are prepared by hand.
#
# The ABS publishes this once a year (early May, for the previous calendar
# year). The script looks for the newest release itself, so it does not need
# editing when the 2026 tables come out.
# ==============================================================================

library(httr2)
library(readxl)
library(dplyr)
library(ggplot2)
library(scales)

DASHBOARD_DIR <- Sys.getenv("DASHBOARD_DIR")
RAW_DIR <- file.path(DASHBOARD_DIR, "Investment", "Foreign investment", "raw")   # downloads (not for GitHub)
OUT_DIR <- file.path(DASHBOARD_DIR, "Investment", "Foreign investment")
dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

IIP_BASE <- paste0("https://www.abs.gov.au/statistics/economy/international-trade/",
                   "international-investment-position-australia-supplementary-statistics")
MAX_AGE_DAYS  <- 30       # re-check the ABS for a new release after this many days
FORCE_REFRESH <- FALSE    # TRUE = download again now

# Partners shown on the chart: display name = pattern matching the ABS row label
PARTNERS <- c(
  "United States"  = "^United States of America|^United States$",
  "United Kingdom" = "^United Kingdom",
  "Japan"          = "^Japan",
  "New Zealand"    = "^New Zealand",
  "Canada"         = "^Canada",
  "Hong Kong"      = "^Hong Kong",
  "China"          = "^China \\(excl|^China$"
)
TOTAL_PATTERN <- "^Total all countries"


# ------------------------------------------------------------------------------
# 1. Download the latest Tables-all.zip
# ------------------------------------------------------------------------------
# Release folders are named by reference year (.../2025/Tables-all.zip). The
# newest one that exists is used, checking this year and the three before it.

zip_file <- file.path(RAW_DIR, "iip_tables_all.zip")

is_fresh <- function(path) {
  !FORCE_REFRESH && file.exists(path) &&
    difftime(Sys.time(), file.mtime(path), units = "days") < MAX_AGE_DAYS
}
is_real_zip <- function(path) {
  file.exists(path) && file.size(path) > 1e5 &&
    identical(readBin(path, "raw", 2), charToRaw("PK"))
}

find_latest_zip <- function() {
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  for (y in this_year:(this_year - 3)) {
    url  <- sprintf("%s/%d/Tables-all.zip", IIP_BASE, y)
    resp <- request(url) |>
      req_method("HEAD") |>
      req_error(is_error = function(r) FALSE) |>
      req_perform()
    if (resp_status(resp) == 200) return(url)
  }
  stop("No Tables-all.zip found for the last four reference years under\n", IIP_BASE)
}

if (is_fresh(zip_file) && is_real_zip(zip_file)) {
  message("IIP: using copy from ", format(file.mtime(zip_file), "%d %b %Y"))
} else {
  ok <- tryCatch({
    url <- find_latest_zip()
    message("IIP: downloading ", url)
    tmp <- paste0(zip_file, ".part")
    request(url) |> req_timeout(600) |> req_retry(max_tries = 3) |> req_perform(path = tmp)
    if (!is_real_zip(tmp)) { unlink(tmp); stop("download was not a zip file") }
    unlink(zip_file)
    file.rename(tmp, zip_file)
  }, error = function(e) { message("IIP download failed: ", conditionMessage(e)); FALSE })
  
  if (isFALSE(ok)) {
    if (is_real_zip(zip_file)) {
      warning("Could not download from the ABS. Using the existing copy from ",
              format(file.mtime(zip_file), "%d %b %Y"), ".", call. = FALSE)
    } else {
      stop("Could not download from the ABS and there is no local copy.", call. = FALSE)
    }
  }
}

# Extract Tables 2 and 5 (files 5352002_YYYY.xlsx and 5352005_YYYY.xlsx)
inside   <- unzip(zip_file, list = TRUE)$Name
pick     <- function(prefix) {
  hit <- inside[grepl(paste0("^", prefix, "_.*\\.xlsx$"), basename(inside))]
  if (!length(hit)) stop(prefix, " not found in the zip. It contains: ",
                         paste(basename(inside), collapse = ", "))
  hit[1]
}
tab_files <- c(foreign = pick("5352002"), abroad = pick("5352005"))
unzip(zip_file, files = tab_files, exdir = RAW_DIR, junkpaths = TRUE)
tab_paths <- file.path(RAW_DIR, basename(tab_files))
names(tab_paths) <- names(tab_files)


# ------------------------------------------------------------------------------
# 2. Read the total level of investment by partner
# ------------------------------------------------------------------------------
# Rather than relying on fixed rows and columns, which the ABS changes from
# time to time, the reader:
#   * finds the row of years above the data and takes the latest year;
#   * among that year's columns, takes the one where "Total all countries" is
#     largest. Total investment is always at least as big as any one type
#     (direct, portfolio...), so this is the "Total" column;
#   * reads each partner's value from the same column and block of rows.
# The sheet, column and total it used are printed, so they can be checked
# against the ABS release page.

cell_num <- function(x) suppressWarnings(as.numeric(gsub("[,$ ]", "", x)))

cell_year <- function(x) {
  x  <- trimws(as.character(x)); x[is.na(x)] <- ""
  yr <- rep(NA_integer_, length(x))
  n  <- suppressWarnings(as.numeric(x))
  plain <- !is.na(n) & n >= 1900 & n <= 2100 & n == round(n)
  yr[plain] <- as.integer(n[plain])
  ser <- !is.na(n) & is.na(yr) & n > 20000 & n < 80000            # Excel date
  yr[ser] <- as.integer(format(as.Date(n[ser], origin = "1899-12-30"), "%Y"))
  txt <- is.na(n) & nchar(x) <= 14 & grepl("(19|20)\\d{2}", x)    # "Dec-2025", "2025 ($m)"
  yr[txt] <- as.integer(regmatches(x[txt], regexpr("(19|20)\\d{2}", x[txt])))
  yr
}

fill_forward <- function(v) {
  for (i in seq_along(v)[-1]) if (is.na(v[i])) v[i] <- v[i - 1]
  v
}

# First text (non-number) cell in each row = the row's label
row_labels <- function(g) {
  apply(g, 1, function(r) {
    r <- trimws(r)
    r <- r[!is.na(r) & r != "" & is.na(cell_num(r))]
    if (length(r)) r[1] else NA_character_
  })
}

extract_level <- function(path) {
  best <- NULL
  for (sh in excel_sheets(path)) {
    g <- as.matrix(suppressMessages(read_excel(path, sheet = sh, col_names = FALSE,
                                               col_types = "text", .name_repair = "minimal")))
    if (nrow(g) < 5 || ncol(g) < 2) next
    labs     <- row_labels(g)
    tot_rows <- which(grepl(TOTAL_PATTERN, labs, ignore.case = TRUE))
    if (!length(tot_rows)) next
    
    # Year header: the row above the data with the most year-like cells
    zone   <- g[seq_len(max(1, min(tot_rows) - 1)), , drop = FALSE]
    yr_mat <- matrix(cell_year(as.vector(zone)), nrow = nrow(zone))
    counts <- rowSums(!is.na(yr_mat))
    if (max(counts) < 2) next
    yrs    <- fill_forward(yr_mat[which.max(counts), ])   # handles merged year cells
    latest <- max(yrs, na.rm = TRUE)
    
    # Compared by size, ignoring sign: the ABS records investment abroad
    # (assets) as negative numbers
    for (tr in tot_rows) for (cc in which(yrs == latest)) {
      v <- abs(cell_num(g[tr, cc]))
      if (!is.na(v) && (is.null(best) || v > best$total))
        best <- list(sheet = sh, row = tr, col = cc, total = v, year = latest,
                     grid = g, labs = labs, tot_rows = tot_rows)
    }
  }
  if (is.null(best))
    stop(basename(path), ": could not find a 'Total all countries' row with a year header.")
  
  # Partner rows: the block of rows belonging to the chosen total. If total
  # rows sit above their countries, the block runs down to the next total;
  # if they sit below, it runs up to the previous one.
  n    <- nrow(best$grid)
  nxt  <- c(best$tot_rows[best$tot_rows > best$row], n + 1)[1]
  prv  <- c(rev(best$tot_rows[best$tot_rows < best$row]), 0)[1]
  find <- function(rows) vapply(PARTNERS, function(p) {
    hit <- rows[!is.na(best$labs[rows]) & grepl(p, best$labs[rows], perl = TRUE)]
    if (length(hit)) hit[1] else NA_integer_
  }, integer(1))
  
  is_partner    <- Reduce(`|`, lapply(PARTNERS, function(p)
    !is.na(best$labs) & grepl(p, best$labs, perl = TRUE)))
  totals_on_top <- min(best$tot_rows) < min(c(which(is_partner), n + 1))
  
  block <- if (totals_on_top) {
    if (nxt - 1 > best$row) (best$row + 1):(nxt - 1) else integer(0)
  } else {
    if (best$row - 1 > prv) (prv + 1):(best$row - 1) else integer(0)
  }
  hits <- find(block)
  if (anyNA(hits))
    warning(basename(path), ": partners not found: ",
            paste(names(PARTNERS)[is.na(hits)], collapse = ", "), call. = FALSE)
  
  message(sprintf("%s: sheet '%s', column %d, %d total = $%sm",
                  basename(path), best$sheet, best$col, best$year,
                  format(round(best$total), big.mark = ",")))
  
  data.frame(
    partner = names(PARTNERS),
    year    = best$year,
    value_m = vapply(hits, function(r) if (is.na(r)) NA_real_ else
      abs(cell_num(best$grid[r, best$col])), numeric(1)),
    total_m = best$total
  )
}

foreign <- extract_level(tab_paths[["foreign"]])
abroad  <- extract_level(tab_paths[["abroad"]])

stopifnot("Tables 2 and 5 report different latest years" =
            unique(foreign$year) == unique(abroad$year))
YEAR <- unique(foreign$year)

if (anyNA(c(foreign$value_m, abroad$value_m)))
  warning("Some partner values are missing; their bars will not be drawn.", call. = FALSE)

iip <- bind_rows(
  foreign |> mutate(direction = "Foreign investment in Australia"),
  abroad  |> mutate(direction = "Australian investment abroad")
) |>
  mutate(value_bn = value_m / 1000)

write.csv(iip, file.path(OUT_DIR, "iip_levels_latest.csv"), row.names = FALSE)


# ------------------------------------------------------------------------------
# 3. Chart
# ------------------------------------------------------------------------------
# Paired bars from zero for each partner, ordered by Australian investment
# abroad (largest at the top). Within each pair, investment abroad sits on top.

csis_navy <- "#00205B"; csis_teal <- "#0098A8"; csis_grey <- "#565A5C"

font_family <- tryCatch({
  sysfonts::font_add_google("Source Sans 3", "source_sans")
  showtext::showtext_auto()
  showtext::showtext_opts(dpi = 300)
  "source_sans"
}, error = function(e) "sans")          # offline: fall back to the default font

# ggplot draws the first factor level at the bottom, so sort ascending
partner_order <- iip |>
  filter(direction == "Australian investment abroad") |>
  arrange(value_bn) |>
  pull(partner)

plot_dat <- iip |>
  mutate(
    partner   = factor(partner, levels = partner_order),
    # second level is drawn on top within each pair
    direction = factor(direction, levels = c("Foreign investment in Australia",
                                             "Australian investment abroad")),
    label     = dollar(value_bn, accuracy = 1, suffix = "bn", big.mark = ",")
  )

x_max <- max(plot_dat$value_bn, na.rm = TRUE) * 1.12   # headroom for value labels

p_iip <- ggplot(plot_dat, aes(x = value_bn, y = partner, fill = direction)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75, na.rm = TRUE) +
  geom_text(aes(label = label), hjust = -0.1,
            position = position_dodge(width = 0.8),
            size = 2.9, family = font_family, colour = csis_grey, na.rm = TRUE) +
  scale_fill_manual(values = c("Australian investment abroad"    = csis_teal,
                               "Foreign investment in Australia" = csis_navy),
                    breaks = c("Australian investment abroad",
                               "Foreign investment in Australia")) +
  scale_x_continuous(labels = dollar_format(suffix = "bn", big.mark = ","),
                     limits = c(0, x_max),
                     expand = expansion(mult = c(0, 0))) +
  labs(
    title    = sprintf("Two-way investment between Australia and its partners, %d", YEAR),
    subtitle = sprintf("Level of investment at 31 December %d, A$ billion", YEAR),
    caption  = paste("Source: ABS, International Investment Position, Australia:",
                     "Supplementary Statistics, Tables 2 and 5."),
    x = NULL, y = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 12, base_family = font_family) +
  theme(
    plot.title         = element_text(face = "bold", colour = csis_navy, size = rel(1.2)),
    plot.subtitle      = element_text(colour = csis_grey, margin = margin(b = 8)),
    plot.caption       = element_text(colour = csis_grey, hjust = 0, size = rel(0.75)),
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position    = "top",
    legend.justification = "left",
    panel.grid.major.y = element_blank(),
    axis.line.y        = element_line(colour = csis_grey, linewidth = 0.4),
    panel.grid.minor   = element_blank(),
    axis.text          = element_text(colour = csis_grey),
    plot.margin        = margin(10, 25, 10, 10)    # room for the last axis label
  )

ggsave(file.path(OUT_DIR, "iip_levels_latest.png"), p_iip,
       width = 9, height = 5.5, dpi = 300, bg = "white")

message("Saved ", file.path(OUT_DIR, "iip_levels_latest.png"))