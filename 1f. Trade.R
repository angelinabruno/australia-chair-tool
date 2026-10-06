# ==============================================================================#
# source("trade_data.R") at the top of the trade chart script. It:
#   1. downloads the four ABS data cubes (merchandise and services by country)
#   2. downloads the latest DFAT calendar-year country and commodity pivot
#      table from its fixed address
#   3. reads the DFAT pivot table's hidden data (its "pivot cache"), so you no
#      longer have to filter it in Excel and save US / all-country copies
#   4. writes tidy CSVs that replace the ABS chart downloads
#   5. defines pivot_slice() for any country / direction cut
#   6. writes dfat_us_vs_all_by_sitc.csv, replacing the four hand-filtered
#      -us / -all Excel copies of the DFAT pivot table
#
# Files are re-downloaded only when they are older than MAX_AGE_DAYS.
# Set FORCE_REFRESH <- TRUE to fetch everything again.
#
# Packages: readabs, rvest, xml2, stringi, data.table, readxl, httr2
# ==============================================================================

library(readabs)
library(rvest)
library(xml2)
library(stringi)
library(data.table)
library(readxl)

# The dashboard folder is set once, in section 1 of '1. Refresh all data.R'.
# To run this script on its own, run that section first.
if (Sys.getenv("DASHBOARD_DIR") == "")
  stop("Dashboard folder not set. Run section 1 of '1. Refresh all data.R' first.")
TRADE_DIR     <- file.path(Sys.getenv("DASHBOARD_DIR"), "Trade")
setwd(TRADE_DIR)   # charts saved without a folder land in Trade/
RAW_DIR       <- file.path(TRADE_DIR, "raw")
MAX_AGE_DAYS  <- 30
FORCE_REFRESH <- FALSE

dir.create(RAW_DIR, showWarnings = FALSE, recursive = TRUE)

is_fresh <- function(path, days = MAX_AGE_DAYS) {
  !FORCE_REFRESH && length(path) == 1 && file.exists(path) &&
    difftime(Sys.time(), file.mtime(path), units = "days") < days
}


# ------------------------------------------------------------------------------
# 1. ABS data cubes
# ------------------------------------------------------------------------------
# International Trade: Supplementary Information, Calendar Year.
# readabs always fetches the latest release, so these update themselves.

ABS_CATALOGUE <- "international-trade-supplementary-information-calendar-year"
ABS_CUBES <- c(
  goods_exports    = "536805500401",   # Table 1, merchandise exports by country
  goods_imports    = "536805500402",   # Table 2, merchandise imports by country
  services_credits = "536805500405",   # Tables 5.x, services credits by country
  services_debits  = "536805500406"    # Tables 6.x, services debits by country
)

fetch_abs_cube <- function(cube) {
  dest <- file.path(TRADE_DIR, paste0(cube, ".xlsx"))   # name the scripts expect
  if (is_fresh(dest)) {
    message("ABS ", cube, ": using copy from ", format(file.mtime(dest), "%d %b %Y"))
    return(dest)
  }
  message("ABS ", cube, ": downloading")
  got <- download_abs_data_cube(ABS_CATALOGUE, cube, path = RAW_DIR)
  file.copy(got, dest, overwrite = TRUE)
  dest
}

abs_files <- vapply(ABS_CUBES, fetch_abs_cube, character(1))


# ------------------------------------------------------------------------------
# 2. DFAT country and commodity pivot table
# ------------------------------------------------------------------------------
# DFAT keeps the latest calendar-year file at a fixed address (the file name
# has no years in it), so it can be downloaded directly with no page scraping.
# DFAT's site blocks some scripted requests, so up to three methods are tried,
# each through a different network stack. If all fail, the existing local copy
# is used.

library(httr2)

DFAT_URL   <- "https://www.dfat.gov.au/sites/default/files/country-sitc-pivot-table-calendar-years.xlsx"
DFAT_DEST  <- file.path(TRADE_DIR, "country-sitc-pivot-table-calendar-years.xlsx")
BROWSER_UA <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
  "(KHTML, like Gecko) Chrome/124.0 Safari/537.36"
)

# A real .xlsx is a zip archive: starts with "PK" and is several MB.
# Anything else is usually a bot-check web page.
is_real_xlsx <- function(path) {
  file.exists(path) && file.size(path) > 1e6 &&
    identical(readBin(path, "raw", 2), charToRaw("PK"))
}

via_httr2 <- function(dest) {
  request(DFAT_URL) |>
    req_user_agent(BROWSER_UA) |>
    req_options(http_version = 2) |>      # 2 = HTTP/1.1 in curl's numbering
    req_timeout(600) |>
    req_perform(path = dest)
}

via_curl_exe <- function(dest) {          # Windows' built-in curl
  exe <- Sys.which("curl.exe")
  if (!nzchar(exe)) stop("curl.exe not found")
  status <- system2(exe, c("-sSL", "--http1.1", "-A", shQuote(BROWSER_UA),
                           "--max-time", "600", "-o", shQuote(dest),
                           shQuote(DFAT_URL)))
  if (status != 0) stop("curl.exe exit code ", status)
}

via_powershell <- function(dest) {        # the Windows .NET network stack
  if (.Platform$OS.type != "windows") stop("not on Windows")
  cmd <- sprintf(paste0(
    "$ProgressPreference='SilentlyContinue'; ",
    "Invoke-WebRequest -Uri '%s' -OutFile '%s' -UserAgent '%s' -UseBasicParsing"),
    DFAT_URL, normalizePath(dest, winslash = "\\", mustWork = FALSE), BROWSER_UA)
  status <- system2("powershell", c("-NoProfile", "-Command", shQuote(cmd)))
  if (status != 0) stop("PowerShell exit code ", status)
}

fetch_dfat <- function() {
  methods <- list(httr2 = via_httr2, `curl.exe` = via_curl_exe,
                  PowerShell = via_powershell)
  for (nm in names(methods)) {
    tmp <- tempfile(fileext = ".xlsx")
    res <- tryCatch({ methods[[nm]](tmp); "ok" },
                    error = function(e) conditionMessage(e))
    if (identical(res, "ok") && is_real_xlsx(tmp)) {
      file.copy(tmp, DFAT_DEST, overwrite = TRUE)
      message("DFAT pivot table: downloaded via ", nm,
              " (", round(file.size(DFAT_DEST) / 1024^2, 1), " MB)")
      return(TRUE)
    }
    message("DFAT via ", nm, ": failed (",
            if (identical(res, "ok")) "response was not a spreadsheet" else res, ")")
  }
  FALSE
}

if (is_fresh(DFAT_DEST) && is_real_xlsx(DFAT_DEST)) {
  message("DFAT pivot table: using copy from ",
          format(file.mtime(DFAT_DEST), "%d %b %Y"))
} else if (!fetch_dfat()) {
  if (is_real_xlsx(DFAT_DEST)) {
    warning("Could not download from DFAT. Using the existing copy from ",
            format(file.mtime(DFAT_DEST), "%d %b %Y"), ".", call. = FALSE)
  } else {
    stop("Could not download from DFAT and there is no usable local copy.\n",
         "Download ", DFAT_URL, "\nin your browser and save it as:\n",
         DFAT_DEST, call. = FALSE)
  }
}


# ------------------------------------------------------------------------------
# 3. Read the DFAT pivot cache
# ------------------------------------------------------------------------------
# An Excel pivot table keeps a full copy of its source data inside the .xlsx
# (xl/pivotCache/). Reading that directly gives every country, commodity, year
# and trade direction at once, with no Excel filtering. The first read takes a
# minute or two; the result is cached as an .rds next to the workbook.

decode_xml <- function(x) {
  if (!any(stri_detect_fixed(x, "&"), na.rm = TRUE)) return(x)
  x <- stri_replace_all_fixed(x, c("&lt;", "&gt;", "&quot;", "&apos;"),
                              c("<", ">", "\"", "'"), vectorize_all = FALSE)
  stri_replace_all_fixed(x, "&amp;", "&")
}

read_pivot_cache <- function(xlsx) {
  td <- tempfile("pivotcache_"); dir.create(td)
  on.exit(unlink(td, recursive = TRUE), add = TRUE)
  
  inside <- unzip(xlsx, list = TRUE)$Name
  unzip(xlsx, files = grep("^xl/pivotCache/", inside, value = TRUE), exdir = td)
  pc_dir <- file.path(td, "xl", "pivotCache")
  
  recs <- list.files(pc_dir, "^pivotCacheRecords\\d+\\.xml$", full.names = TRUE)
  if (!length(recs)) stop(basename(xlsx), " contains no pivot cache records.")
  rec  <- recs[which.max(file.size(recs))]                  # the main cache
  def  <- sub("Records", "Definition", rec)
  
  # Field names and their lookup lists
  d <- read_xml(def); xml_ns_strip(d)
  fields <- xml_find_all(d, "//cacheFields/cacheField")
  dbf    <- xml_attr(fields, "databaseField")
  fields <- fields[is.na(dbf) | dbf != "0"]                 # skip calculated fields
  fnames <- xml_attr(fields, "name")
  shared <- lapply(fields, function(f) {
    items <- xml_children(xml_find_first(f, "sharedItems"))
    if (length(items)) xml_attr(items, "v") else character(0)
  })
  
  # Records: one self-closing element per field, in field order
  txt <- readChar(rec, file.size(rec), useBytes = TRUE)
  m   <- stri_match_all_regex(txt, "<(x|n|s|m|b|d|e)\\b([^>]*)/>")[[1]]
  rm(txt); invisible(gc())
  tag <- m[, 2]
  val <- stri_match_first_regex(m[, 3], "\\bv=\"([^\"]*)\"")[, 2]
  rm(m)
  
  nf <- length(fnames)
  if (length(tag) %% nf != 0)
    stop("Pivot cache layout not recognised (", length(tag),
         " values for ", nf, " fields).")
  tag <- matrix(tag, ncol = nf, byrow = TRUE)
  val <- matrix(val, ncol = nf, byrow = TRUE)
  
  cols <- lapply(seq_len(nf), function(j) {
    v <- val[, j]
    is_x <- tag[, j] == "x"
    if (any(is_x)) v[is_x] <- shared[[j]][as.integer(v[is_x]) + 1L]
    v[tag[, j] == "m"] <- NA
    type.convert(decode_xml(v), as.is = TRUE)
  })
  out <- as.data.table(setNames(cols, make.unique(fnames)))
  message(sprintf("Pivot cache: %s records, fields: %s",
                  format(nrow(out), big.mark = ","), paste(fnames, collapse = " | ")))
  out
}

# Work out which column is which from the values, not the names, so a renamed
# field does not break the script. Override any guess in PIVOT_COLS.
PIVOT_COLS <- list(flow = NULL, country = NULL, sitc = NULL,
                   sector = NULL, year = NULL, value = NULL)

guess_cols <- function(dt) {
  share_matching <- function(col, pattern) {
    x <- dt[[col]]
    if (!is.character(x)) return(0)
    u <- unique(x[!is.na(x)])
    if (!length(u)) 0 else mean(stri_detect_regex(u, pattern))
  }
  chr <- names(dt)[vapply(dt, is.character, logical(1))]
  num <- names(dt)[vapply(dt, is.numeric,   logical(1))]
  
  pick <- function(cands, score, min_score) {
    if (!length(cands)) return(NA_character_)
    s <- vapply(cands, score, numeric(1))
    if (max(s) < min_score) NA_character_ else cands[which.max(s)]
  }
  
  g <- list()
  g$flow    <- pick(chr, function(c) share_matching(c, "(?i)export|import"), 0.99)
  g$sitc    <- pick(chr, function(c) share_matching(c, "^\\d{3}\\s"), 0.8)
  g$country <- pick(chr, function(col) {
    u <- unique(dt[[col]])
    sum(c("China", "Japan", "India", "Singapore") %in% u) +
      any(stri_detect_regex(u, "^United States"), na.rm = TRUE)
  }, 3)
  g$sector  <- pick(setdiff(chr, unlist(g)), function(c) {
    as.numeric(stri_detect_regex(c, "(?i)sector")) * 2 +
      as.numeric(uniqueN(dt[[c]]) %in% 2:8)
  }, 2)
  g$year    <- pick(num, function(c) {
    x <- dt[[c]]
    as.numeric(all(x[!is.na(x)] %% 1 == 0 & x[!is.na(x)] >= 1980 &
                     x[!is.na(x)] <= 2100))
  }, 1)
  # Year may also be text, e.g. "2025" or "CY2025"
  if (is.na(g$year))
    g$year <- pick(chr, function(c) share_matching(c, "^(CY ?)?(19|20)\\d{2}$"), 0.99)
  g$value   <- pick(setdiff(num, g$year), function(c)
    max(abs(dt[[c]]), na.rm = TRUE), 0)
  
  for (k in names(PIVOT_COLS)) if (!is.null(PIVOT_COLS[[k]])) g[[k]] <- PIVOT_COLS[[k]]
  g
}

PIVOT_RDS <- file.path(RAW_DIR, "dfat_pivot_tidy.rds")

load_dfat_pivot <- function() {
  if (file.exists(PIVOT_RDS) && file.mtime(PIVOT_RDS) > file.mtime(DFAT_DEST)) {
    message("DFAT pivot cache: using tidied copy")
    return(readRDS(PIVOT_RDS))
  }
  raw <- read_pivot_cache(DFAT_DEST)
  g   <- guess_cols(raw)
  message("Column mapping: ",
          paste(sprintf("%s = %s", names(g), unlist(g)), collapse = "; "))
  need <- c("flow", "country", "sitc", "year", "value")
  miss <- need[is.na(unlist(g[need]))]
  if (length(miss))
    stop("Could not identify: ", paste(miss, collapse = ", "),
         ". Set them in PIVOT_COLS using the field names printed above.")
  
  tidy <- data.table(
    flow    = fifelse(stri_detect_regex(raw[[g$flow]], "(?i)export"), "Exports",
                      fifelse(stri_detect_regex(raw[[g$flow]], "(?i)import"), "Imports", NA_character_)),
    country = raw[[g$country]],
    sitc    = stri_trim_both(raw[[g$sitc]]),
    sector  = if (!is.na(g$sector)) raw[[g$sector]] else NA_character_,
    year    = as.integer(stri_extract_first_regex(as.character(raw[[g$year]]), "\\d{4}")),
    value_k = as.numeric(raw[[g$value]])
  )[!is.na(flow) & !is.na(year)]
  
  saveRDS(tidy, PIVOT_RDS)
  tidy
}

dfat <- load_dfat_pivot()


# ------------------------------------------------------------------------------
# 4. Tidy outputs that replace manual downloads
# ------------------------------------------------------------------------------

# 4a. DFAT: country and sector totals (replace the files previously extracted
#     by hand: dfat_trade_by_country.csv, dfat_sector_by_year.csv,
#     dfat_exports_by_country.csv, sitc_sector_lookup.csv)

flow_label <- c(Exports = "Total Exports", Imports = "Total Imports")
totals <- dfat[, .(total_k = sum(value_k, na.rm = TRUE)), by = .(flow, year)]

by_country <- dfat[, .(value_k = sum(value_k, na.rm = TRUE)), by = .(flow, year, country)]
by_country <- merge(by_country, totals, by = c("flow", "year"))
fwrite(by_country[, .(trade_type = flow_label[flow], year, country, value_k, total_k)],
       file.path(TRADE_DIR, "dfat_trade_by_country.csv"))
fwrite(by_country[flow == "Exports",
                  .(year, country, exports_k = value_k, total_exports_k = total_k)],
       file.path(TRADE_DIR, "dfat_exports_by_country.csv"))

if (!all(is.na(dfat$sector))) {
  by_sector <- dfat[, .(value_k = sum(value_k, na.rm = TRUE)), by = .(flow, year, sector)]
  by_sector <- merge(by_sector, totals, by = c("flow", "year"))
  fwrite(by_sector[, .(trade_type = flow_label[flow], year, sector, value_k, total_k)],
         file.path(TRADE_DIR, "dfat_sector_by_year.csv"))
  fwrite(unique(dfat[, .(sitc_code = substr(sitc, 1, 3), sector)])[order(sitc_code)],
         file.path(TRADE_DIR, "sitc_sector_lookup.csv"))
} else {
  warning("No sector field found in the DFAT pivot cache; ",
          "dfat_sector_by_year.csv was not refreshed.")
}

# 4b. ABS services, from the data cubes instead of chart downloads.
#     Table x.13 is the all-services table, one row per country.

read_abs_country_table <- function(path, sheet, row_year = 7) {
  raw   <- as.data.table(read_excel(path, sheet = sheet, col_names = FALSE,
                                    col_types = "text", .name_repair = "minimal"))
  years <- suppressWarnings(as.integer(unlist(raw[row_year, -1])))
  keep  <- which(!is.na(years))
  body  <- raw[(row_year + 1):.N]
  lab   <- stri_trim_both(body[[1]])
  num   <- function(x) {
    x <- stri_trim_both(x); out <- suppressWarnings(as.numeric(x))
    out[x %in% c("-", "\u2013")] <- 0
    out
  }
  vals <- as.data.table(lapply(keep + 1L, function(j) abs(num(body[[j]]))))
  setnames(vals, as.character(years[keep]))
  vals[, country := lab]
  long <- melt(vals[!is.na(country) & country != ""], id.vars = "country",
               variable.name = "year", value.name = "value_m", variable.factor = FALSE)
  long[, year := as.integer(year)][]
}

last_table <- function(path) {
  s <- grep("^Table\\s*\\d+\\.\\d+$", excel_sheets(path), value = TRUE)
  s[which.max(as.integer(sub(".*\\.", "", s)))]
}

svc_cr <- read_abs_country_table(abs_files[["services_credits"]],
                                 last_table(abs_files[["services_credits"]]))
svc_db <- read_abs_country_table(abs_files[["services_debits"]],
                                 last_table(abs_files[["services_debits"]]))

total_row <- function(d) d[stri_detect_regex(country, "^Total all countries")]

# Replaces "Balance on trade in services (a).xlsx"
# Same columns as read_abs_chart(): year, series, value ($b)
balance <- merge(total_row(svc_cr)[, .(year, cr = value_m)],
                 total_row(svc_db)[, .(year, db = value_m)], by = "year")
fwrite(balance[, .(year, series = "Balance on trade in services",
                   value = (cr - db) / 1000)],
       file.path(TRADE_DIR, "abs_services_balance_tidy.csv"))

# Replaces "Services imports by country (a).xlsx"
# Same columns as before: country, value_m, share, change
yr  <- max(svc_db$year)
tot <- total_row(svc_db)[year == yr, value_m]
imp <- merge(svc_db[year == yr, .(country, value_m)],
             svc_db[year == yr - 1, .(country, prev = value_m)], by = "country")
imp <- imp[!stri_detect_regex(country, "(?i)^total|all other|^\\(")]
fwrite(imp[, .(country, value_m, share = round(100 * value_m / tot, 1),
               change = value_m - prev)][order(-value_m)],
       file.path(TRADE_DIR, "abs_services_imports_by_country.csv"))


# ------------------------------------------------------------------------------
# 5. pivot_slice(): any country / direction cut of the DFAT data
# ------------------------------------------------------------------------------
# Returns industry (SITC group), year and a value column in A$ million, plus a
# "Total goods" row. Country names are matched exactly, so "United States"
# does not also pick up "United States Minor Outlying Islands".
#   pivot_slice("Exports", US_LABELS, "us")   # US only
#   pivot_slice("Exports", NULL, "total")     # all countries

US_LABELS <- c("United States", "United States of America")

pivot_slice <- function(flow, country = NULL, value_name = "value") {
  want_flow <- flow; want_country <- country   # avoid clashing with column names
  d <- dfat[dfat$flow == want_flow]
  if (!is.null(want_country)) d <- d[d$country %in% want_country]
  out <- d[, .(v = sum(value_k, na.rm = TRUE) / 1000), by = .(industry = sitc, year)]
  tot <- out[, .(industry = "Total goods", v = sum(v)), by = year]
  out <- rbind(out, tot, use.names = TRUE)
  setnames(out, "v", value_name)
  out[order(industry, year)]
}


# ------------------------------------------------------------------------------
# 6. US vs all-country goods trade by SITC group
# ------------------------------------------------------------------------------
# Replaces the four Excel copies previously filtered by hand:
#   country-sitc-pivot-table-calendar-years-exports-us.xlsx
#   country-sitc-pivot-table-calendar-years-exports-all.xlsx
#   country-sitc-pivot-table-calendar-years-imports-us.xlsx
#   country-sitc-pivot-table-calendar-years-imports-all.xlsx
# One tidy file, A$ million: flow, industry, year, us_m, total_m

us_vs_all <- rbindlist(lapply(c("Exports", "Imports"), function(fl) {
  tot <- pivot_slice(fl, NULL,      "total_m")
  us  <- pivot_slice(fl, US_LABELS, "us_m")
  d   <- merge(tot, us, by = c("industry", "year"), all.x = TRUE)
  d[is.na(us_m), us_m := 0]
  d[, flow := fl][]
}))

if (!any(us_vs_all$us_m > 0))
  warning("No US trade found. Check how the United States is labelled: ",
          paste(grep("United", unique(dfat$country), value = TRUE), collapse = ", "))

fwrite(us_vs_all[, .(flow, industry, year, us_m, total_m)],
       file.path(TRADE_DIR, "dfat_us_vs_all_by_sitc.csv"))

message("Trade data ready. Latest DFAT year: ", max(dfat$year),
        "; latest ABS services year: ", max(svc_cr$year))

# ==================================================================
# ABS International Trade: Supplementary Information, CY2025
# Three charts recreated in CSIS house style
# ==================================================================
# Source workbooks (ABS chart downloads) all share one layout:
#   A1        title
#   row 2     header ("", "China ($b)", ...)
#   row 3+    data, YEAR STORED AS TEXT
#   bottom    footnote + source lines
#
# Two traps:
#   - Years are text, so a naive read gives a character column that
#     silently coerces to NA on the footnote rows.
#   - Imports/Japan 2021 is an EMPTY STRING, not a blank cell. That
#     turns the whole Japan column character. ABS excluded it for
#     confidentiality; the gap is real and should stay visible.
# Both are handled by reading everything as text, then casting.
# ==================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(ggrepel)
library(showtext)
library(sysfonts)


# ---------------------------------------------------------------
# 1. CSIS style
# ---------------------------------------------------------------
font_add_google("Source Sans 3", "source_sans")
showtext_auto()
showtext_opts(dpi = 150)   # match ggsave() dpi or text renders too small

csis_colours <- c(
  navy   = "#00205B",
  red    = "#D2232A",
  teal   = "#0098A8",
  gold   = "#F2A900",
  grey   = "#565A5C",
  ltgrey = "#B1B3B3"
)

theme_csis <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "source_sans") %+replace%
    theme(
      plot.title    = element_text(face = "bold", size = rel(1.25), hjust = 0,
                                   colour = csis_colours[["navy"]],
                                   margin = margin(b = 4)),
      plot.subtitle = element_text(hjust = 0, colour = csis_colours[["grey"]],
                                   margin = margin(b = 10)),
      plot.caption  = element_text(size = rel(0.75), hjust = 0,
                                   colour = csis_colours[["grey"]],
                                   margin = margin(t = 10)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(colour = "#E6E7E8", linewidth = 0.4),
      axis.line.x  = element_line(colour = csis_colours[["grey"]], linewidth = 0.4),
      axis.ticks.x = element_line(colour = csis_colours[["grey"]], linewidth = 0.4),
      axis.title   = element_text(colour = csis_colours[["grey"]], size = rel(0.85)),
      axis.text    = element_text(colour = csis_colours[["grey"]]),
      legend.position      = "top",
      legend.justification = "left",
      legend.title         = element_blank(),
      complete = TRUE
    )
}

scale_colour_csis <- function(...) {
  discrete_scale("colour", palette = function(n) unname(csis_colours)[seq_len(n)], ...)
}
scale_fill_csis <- function(...) {
  discrete_scale("fill", palette = function(n) unname(csis_colours)[seq_len(n)], ...)
}

ABS_SOURCE <- paste0(
  "Source: Australian Bureau of Statistics, International Trade: ",
  "Supplementary Information, Calendar Year 2025."
)


# ---------------------------------------------------------------
# 2. Data
# ---------------------------------------------------------------
# abs_services_balance_tidy.csv is written at the top of this script from
# the ABS data cubes it downloads (year, series, value in $b).

services <- readr::read_csv(file.path(TRADE_DIR, "abs_services_balance_tidy.csv"),
                            show_col_types = FALSE) |>
  filter(year >= 2005)


# ---------------------------------------------------------------
# 3. Chart — services balance
# ---------------------------------------------------------------
# Columns rather than a line: this is a signed balance, and the sign
# flip in 2020 is the whole story. Fill encodes direction, so no
# legend is needed once the axis label states the convention.

services_plot <- services |>
  mutate(direction = if_else(value >= 0, "Surplus", "Deficit"))

p_services <- ggplot(services_plot, aes(year, value, fill = direction)) +
  geom_col(width = 0.72) +
  geom_hline(yintercept = 0, colour = csis_colours[["grey"]], linewidth = 0.5) +
  annotate(
    "text",
    x = 2020, y = services_plot$value[services_plot$year == 2020] + 4,
    label = "Surplus in 2020",
    family = "source_sans", size = 3, lineheight = 0.95,
    colour = csis_colours[["teal"]], fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(Deficit = unname(csis_colours["red"]),
               Surplus = unname(csis_colours["teal"]))
  ) +
  scale_x_continuous(
    breaks = seq(2005, 2025, by = 5),
    minor_breaks = NULL,
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    labels = label_dollar(suffix = "b", accuracy = 1),
    expand = expansion(mult = c(0.10, 0.14))
  ) +
  labs(
    title    = "Australia's balance on trade in services",
    subtitle = "Exports (credits) less imports (debits), 2005-2025",
    caption  = paste0(
      ABS_SOURCE,
      "\nNote: a negative balance indicates imports exceed exports."
    ),
    x = NULL, y = "Balance ($ billion)"
  ) +
  theme_csis() +
  theme(
    legend.position    = "none",
    panel.grid.major.y = element_line(colour = "#E6E7E8", linewidth = 0.4)
  )


# ---------------------------------------------------------------
# 7. Export
# ---------------------------------------------------------------
# dpi MUST match showtext_opts() above or the text scales wrong.

save_csis <- function(plot, file, width = 8, height = 4.8) {
  ggsave(file, plot, width = width, height = height, dpi = 150, bg = "white")
}

save_csis(p_services, "abs_services_balance.png", height = 4.4)


# ---------------------------------------------------------------
# 4. Tidy output
# ---------------------------------------------------------------

readr::write_csv(services, "abs_services_tidy.csv")






# ==================================================================
# Australia's merchandise export shares by broad sector, 2006-2025
# Source: DFAT country-SITC pivot table (ABS cat. 5368.0), 'Pivot' tab
# ==================================================================
# Sheet layout:
#   rows 8-12   title block + collapsed pivot filters
#   row 15      header: "Row Labels", then years 2006 ... 2025
#   rows 16-278 the 264 SITC 3-digit groups (one is absent for exports)
#   row 279     Grand Total
#
# THE SECTOR MAPPING PROBLEM
# The 'Sector' field is a collapsed page filter, so the sheet shows no
# code-to-sector column. The mapping exists only inside the workbook's
# pivot cache. It has been extracted from that cache and shipped as
# sitc_sector_lookup.csv (264 rows, authoritative — not inferred from
# SITC section numbers, which would misclassify several groups).
#
# One known quirk: '988 Confidential items of trade' carries 8,736
# cache records tagged "Other goods" and a single stray record tagged
# "Minerals & fuels". The lookup resolves it by majority and flags it
# in the `note` column.
# ==================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(ggplot2)
library(scales)
library(ggrepel)
library(showtext)
library(sysfonts)

XLSX   <- "country-sitc-pivot-table-calendar-years.xlsx"
LOOKUP <- "sitc_sector_lookup.csv"


# ---------------------------------------------------------------
# 1. CSIS style
# ---------------------------------------------------------------
font_add_google("Source Sans 3", "source_sans")
showtext_auto()
showtext_opts(dpi = 150)   # match ggsave() dpi or text renders too small

csis_colours <- c(
  navy   = "#00205B",
  red    = "#D2232A",
  teal   = "#0098A8",
  gold   = "#F2A900",
  grey   = "#565A5C",
  ltgrey = "#B1B3B3"
)

theme_csis <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "source_sans") %+replace%
    theme(
      plot.title    = element_text(face = "bold", size = rel(1.25), hjust = 0,
                                   colour = csis_colours[["navy"]],
                                   margin = margin(b = 4)),
      plot.subtitle = element_text(hjust = 0, colour = csis_colours[["grey"]],
                                   margin = margin(b = 10)),
      plot.caption  = element_text(size = rel(0.75), hjust = 0,
                                   colour = csis_colours[["grey"]],
                                   margin = margin(t = 10)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(colour = "#E6E7E8", linewidth = 0.4),
      axis.line.x  = element_line(colour = csis_colours[["grey"]], linewidth = 0.4),
      axis.ticks.x = element_line(colour = csis_colours[["grey"]], linewidth = 0.4),
      axis.title   = element_text(colour = csis_colours[["grey"]], size = rel(0.85)),
      axis.text    = element_text(colour = csis_colours[["grey"]]),
      legend.position      = "top",
      legend.justification = "left",
      legend.title         = element_blank(),
      complete = TRUE
    )
}

scale_colour_csis <- function(...) {
  discrete_scale("colour", palette = function(n) unname(csis_colours)[seq_len(n)], ...)
}
scale_fill_csis <- function(...) {
  discrete_scale("fill", palette = function(n) unname(csis_colours)[seq_len(n)], ...)
}

DFAT_SOURCE <- paste0(
  "Source: DFAT, Australia's merchandise exports and imports country ",
  "and commodity pivot table (ABS cat. 5368.0), calendar years."
)


# ---------------------------------------------------------------
# 2. Import
# ---------------------------------------------------------------
# skip = 14 puts row 15 in as the header. Read as text throughout:
# the label column is character and forcing a common type up front
# avoids readxl guessing differently on different columns.

raw <- read_excel(XLSX, sheet = "Pivot", skip = 14, col_types = "text")

names(raw)[1] <- "sitc_label"

# Keep only the SITC detail rows. The label always begins with a
# 3-digit group code, which excludes "Grand Total", any stray blanks,
# and footer text — no reliance on hardcoded row numbers.
detail <- raw |>
  filter(str_detect(sitc_label, "^\\d{3}\\s")) |>
  mutate(sitc_code = str_sub(sitc_label, 1, 3))

# Grand total, captured separately from the same object (row 279)
grand_total <- raw |>
  filter(str_squish(sitc_label) == "Grand Total")

stopifnot(nrow(grand_total) == 1)
message("SITC detail rows read: ", nrow(detail))


# ---------------------------------------------------------------
# 3. Reshape to long, attach sectors
# ---------------------------------------------------------------

to_long <- function(df, value_name) {
  df |>
    select(-any_of("sitc_label")) |>
    pivot_longer(
      cols = matches("^\\d{4}$"),
      names_to = "year", values_to = "v"
    ) |>
    mutate(
      year = as.integer(year),
      !!value_name := suppressWarnings(as.numeric(na_if(str_trim(v), "")))
    ) |>
    select(-v)
}

lookup <- read_csv(LOOKUP, show_col_types = FALSE) |>
  select(sitc_code, sector)

detail_long <- detail |>
  to_long("value_k") |>
  left_join(lookup, by = "sitc_code")

# Any unmatched code means the lookup and the workbook have drifted
unmatched <- detail_long |> filter(is.na(sector)) |> distinct(sitc_code)
if (nrow(unmatched) > 0) {
  stop("SITC codes missing from lookup: ",
       paste(unmatched$sitc_code, collapse = ", "))
}

total_long <- grand_total |>
  to_long("total_k") |>
  select(year, total_k)


# ---------------------------------------------------------------
# 4. Aggregate to sectors and compute shares
# ---------------------------------------------------------------
# Shares are taken against the workbook's own Grand Total (row 279)
# rather than against the sum of the sector aggregates, as specified.
# The two should agree; section 5 checks that they do.

shares <- detail_long |>
  group_by(year, sector) |>
  summarise(sector_k = sum(value_k, na.rm = TRUE), .groups = "drop") |>
  left_join(total_long, by = "year") |>
  mutate(share = sector_k / total_k)


# ---------------------------------------------------------------
# 5. Validation
# ---------------------------------------------------------------
# If the sector mapping were wrong, sector sums would not reconcile to
# the printed Grand Total. This is the check that catches it.

recon <- shares |>
  group_by(year) |>
  summarise(
    sum_sectors = sum(sector_k),
    grand_total = first(total_k),
    diff_pct    = (sum_sectors - grand_total) / grand_total * 100,
    share_sum   = sum(share),
    .groups = "drop"
  )

print(recon, n = Inf)

if (any(abs(recon$diff_pct) > 0.01)) {
  warning("Sector aggregates do not reconcile to Grand Total. ",
          "Check sitc_sector_lookup.csv against the workbook's pivot cache.")
} else {
  message("Reconciliation OK: sector sums match Grand Total within 0.01%.")
}


# ---------------------------------------------------------------
# 6. Chart
# ---------------------------------------------------------------
# Series ordered by latest-year share so palette assignment tracks
# magnitude; direct end-labels instead of a legend.

lvls <- shares |>
  filter(year == max(year)) |>
  arrange(desc(share)) |>
  pull(sector)

plot_dat <- shares |>
  mutate(sector = factor(sector, levels = lvls))

ends <- plot_dat |> filter(year == max(year))

p_shares <- ggplot(plot_dat, aes(year, share, colour = sector, group = sector)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  geom_text_repel(
    data = ends,
    aes(label = str_wrap(sector, 18)),
    family = "source_sans", size = 3.2, fontface = "bold", lineheight = 0.9,
    hjust = 0, direction = "y", nudge_x = 0.4,
    segment.size = 0.3, segment.alpha = 0.5,
    min.segment.length = 0, seed = 42
  ) +
  scale_colour_csis() +
  scale_x_continuous(
    breaks = seq(2006, 2025, by = 2),
    expand  = expansion(mult = c(0.02, 0.20))
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.06))
  ) +
  labs(
    title    = "Share of total merchandise exports by broad sector, 2006-2025",
    #subtitle = "Share of total merchandise exports by broad sector, 2006-2025",
    caption  = paste0(
      DFAT_SOURCE,
      "\nSectors are DFAT's four-way aggregation of SITC Rev. 4 3-digit ",
      "groups. Shares are calculated against total exports.",
      "\n'Other goods' includes confidential items of trade and non-monetary gold."
    ),
    x = NULL, y = "Share of total merchandise exports"
  ) +
  theme_csis() +
  theme(legend.position = "none")

ggsave("abs_export_shares_by_sector.png", p_shares,
       width = 8.5, height = 5, dpi = 150, bg = "white")


# ---------------------------------------------------------------
# 7. Alternative view — stacked area
# ---------------------------------------------------------------
# Shares sum to 100%, so a stacked area is legitimate here and makes
# composition easier to read than four separate lines. Use whichever
# suits the argument: lines for individual trajectories, area for mix.

p_area <- ggplot(plot_dat, aes(year, share, fill = sector)) +
  geom_area(colour = "white", linewidth = 0.25) +
  scale_fill_csis() +
  scale_x_continuous(breaks = seq(2006, 2025, by = 2),
                     expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     expand = expansion(mult = c(0, 0))) +
  labs(
    title    = "Composition of Australia's merchandise exports",
    subtitle = "Share of total merchandise exports by broad sector, 2006-2025",
    caption  = DFAT_SOURCE,
    x = NULL, y = NULL
  ) +
  theme_csis() +
  theme(legend.position = "top")

ggsave("abs_export_shares_stacked.png", p_area,
       width = 8.5, height = 5, dpi = 150, bg = "white")


# ---------------------------------------------------------------
# 8. Tidy output
# ---------------------------------------------------------------

shares |>
  mutate(sector_bn = sector_k / 1e6, total_bn = total_k / 1e6) |>
  select(year, sector, sector_bn, total_bn, share) |>
  arrange(year, sector) |>
  write_csv("aus_export_shares_by_sector.csv")



# ==================================================================
# EXTRA BLOCK — long-run version of abs_exports_partners.png
# Append to abs_trade_csis.R (assumes theme_csis, csis_colours,
# scale_colour_csis and the showtext setup are already loaded).
# ==================================================================
#
# IMPORTANT — this is NOT the same measure as the ABS chart.
#
#   ABS "top partner countries for exports"  = goods AND services
#   DFAT country-SITC pivot table            = MERCHANDISE (goods) only
#
# The gap is large and uneven across partners. In 2025 the ABS series
# puts exports to the US at $59.9b; DFAT merchandise puts it at $40.6b,
# because US-bound exports are services-heavy. China moves far less
# ($195.6b vs $175.9b). So the two charts are not alternative views of
# one series — do not present them as a continuation of each other.
# Label this one "merchandise exports" and say so in the subtitle.
#
# Data: dfat_exports_by_country.csv, extracted from the workbook's
# pivot cache (Country is a collapsed field, so it is not readable off
# the Pivot tab). Columns: year, country, exports_k, total_exports_k.
# Values are A$'000. Country totals sum exactly to the grand total.
# ==================================================================

library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(ggrepel)


# ---------------------------------------------------------------
# 1. Load and harmonise country names
# ---------------------------------------------------------------
# DFAT's labels differ from the ABS chart's. Renaming keeps the two
# figures visually consistent even though the underlying measure
# differs.

dfat_countries <- read_csv("dfat_exports_by_country.csv",
                           show_col_types = FALSE) |>
  mutate(
    country = recode(
      country,
      "United States"     = "United States of America",
      "Republic of Korea" = "South Korea"
    )
  )

top5 <- c("China", "Japan", "United States of America",
          "South Korea", "India")

missing <- setdiff(top5, unique(dfat_countries$country))
if (length(missing) > 0) {
  stop("Countries not found after recode: ", paste(missing, collapse = ", "))
}

exports_long <- dfat_countries |>
  filter(country %in% top5) |>
  mutate(
    exports_bn = exports_k / 1e6,          # A$'000 -> A$ billion
    share      = exports_k / total_exports_k
  )


# ---------------------------------------------------------------
# 2. Chart — levels
# ---------------------------------------------------------------

lvls <- exports_long |>
  filter(year == max(year)) |>
  arrange(desc(exports_bn)) |>
  pull(country)

plot_dat <- exports_long |> mutate(country = factor(country, levels = lvls))
ends     <- plot_dat |> filter(year == max(year))

p_exports_long <- ggplot(plot_dat,
                         aes(year, exports_bn, colour = country, group = country)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.4) +
  geom_text_repel(
    data = ends,
    aes(label = str_wrap(country, 14)),
    family = "source_sans", size = 3.2, fontface = "bold", lineheight = 0.9,
    hjust = 0, direction = "y", nudge_x = 0.5,
    segment.size = 0.3, segment.alpha = 0.5,
    min.segment.length = 0, seed = 42
  ) +
  scale_colour_csis() +
  scale_x_continuous(
    breaks = seq(2006, 2025, by = 2),
    expand = expansion(mult = c(0.02, 0.18))
  ) +
  scale_y_continuous(
    labels = label_dollar(suffix = "b", accuracy = 1),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    #title    = "China's rise reshaped Australia's export base",
    title = "Merchandise exports",
    subtitle = "By destination, A$ billion, 2006-2025",
    caption  = paste0(
      "Source: DFAT, Australia's merchandise exports and imports country ",
      "and commodity pivot table (ABS cat. 5368.0), calendar years.",
      "\nNote: merchandise (goods) only. Not comparable in level with ABS ",
      "series covering goods and services."
    ),
    x = NULL, y = "Exports (A$ billion)"
  ) +
  theme_csis() +
  theme(legend.position = "none")

ggsave("dfat_exports_partners_long.png", p_exports_long,
       width = 8.5, height = 5, dpi = 150, bg = "white")


# ---------------------------------------------------------------
# 3. Chart — shares of total merchandise exports
# ---------------------------------------------------------------
# Arguably the more useful cut for a concentration argument: it strips
# out commodity-price swings, which drive much of the level series.

share_ends <- plot_dat |> filter(year == max(year))

p_shares_long <- ggplot(plot_dat,
                        aes(year, share, colour = country, group = country)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.4) +
  geom_text_repel(
    data = share_ends,
    aes(label = str_wrap(country, 14)),
    family = "source_sans", size = 3.2, fontface = "bold", lineheight = 0.9,
    hjust = 0, direction = "y", nudge_x = 0.5,
    segment.size = 0.3, segment.alpha = 0.5,
    min.segment.length = 0, seed = 42
  ) +
  scale_colour_csis() +
  scale_x_continuous(breaks = seq(2006, 2025, by = 2),
                     expand = expansion(mult = c(0.02, 0.18))) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(
    #title    = "A third of Australia's goods exports go to a single market",
    title = "Share of total merchandise exports",
    subtitle = "By destination, 2006-2025",
    caption  = paste0(
      "Source: DFAT, Australia's merchandise exports and imports country ",
      "and commodity pivot table (ABS cat. 5368.0), calendar years."
    ),
    x = NULL, y = "Share of total merchandise exports"
  ) +
  theme_csis() +
  theme(legend.position = "none")

ggsave("dfat_export_shares_partners_long.png", p_shares_long,
       width = 8.5, height = 5, dpi = 150, bg = "white")


# ---------------------------------------------------------------
# 4. Concentration summary
# ---------------------------------------------------------------

concentration_ts <- exports_long |>
  group_by(year) |>
  summarise(
    top5_share = sum(share),
    china_share = share[country == "China"],
    .groups = "drop"
  )

print(concentration_ts, n = Inf)

write_csv(exports_long, "dfat_top5_exports_tidy.csv")

# ==================================================================
# IMPORTS — partner countries and sector shares
# Append to abs_trade_csis.R (assumes theme_csis, csis_colours,
# scale_colour_csis / scale_fill_csis and showtext are already loaded).
# ==================================================================
#
# NO NEED TO RE-PIVOT THE WORKBOOK.
# The B10 "Trade type" button only changes which slice Excel displays.
# Both directions are stored in the workbook's pivot cache, so imports
# were extracted from the same file already supplied. The two CSVs
# below carry Total Imports and Total Exports side by side:
#
#   dfat_trade_by_country.csv  trade_type, year, country, value_k, total_k
#   dfat_sector_by_year.csv    trade_type, year, sector,  value_k, total_k
#
# Values are A$'000, merchandise (goods) only. Country and sector
# aggregates reconcile exactly to the direction's total in every year.
#
# MEASURE WARNING, again: the ABS chart you recreated earlier covers
# goods AND services. This is goods only. Levels will not match, and
# the divergence is largest for services-heavy partners such as the US.
# ==================================================================

library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(ggrepel)


# ---------------------------------------------------------------
# 1. Load, harmonise names, filter to imports
# ---------------------------------------------------------------

harmonise <- function(df) {
  df |> mutate(country = recode(
    country,
    "United States"     = "United States of America",
    "Republic of Korea" = "South Korea"
  ))
}

trade_country <- read_csv("dfat_trade_by_country.csv", show_col_types = FALSE) |>
  harmonise()

# The ABS chart's top five import sources were China, USA, Japan,
# Singapore and Thailand. On DFAT's MERCHANDISE ranking the 2025 top
# five is slightly different — South Korea ($22.0b) edges out Singapore
# ($20.0b) and Thailand ($19.2b). Keeping the ABS five preserves
# continuity with the earlier figure; switch to top5_dfat if you would
# rather the chart reflect this dataset's own ranking.

top5_abs  <- c("China", "United States of America", "Japan",
               "Singapore", "Thailand")

top5_dfat <- trade_country |>
  filter(trade_type == "Total Imports", year == max(year)) |>
  slice_max(value_k, n = 5) |>
  pull(country)

top5_imports <- top5_abs        # <- change to top5_dfat if preferred

message("ABS five:  ", paste(top5_abs,  collapse = ", "))
message("DFAT five: ", paste(top5_dfat, collapse = ", "))

imports_long <- trade_country |>
  filter(trade_type == "Total Imports", country %in% top5_imports) |>
  mutate(
    value_bn = value_k / 1e6,
    share    = value_k / total_k
  )

stopifnot(length(setdiff(top5_imports, unique(imports_long$country))) == 0)


# ---------------------------------------------------------------
# 2. Reusable partner-chart builder
# ---------------------------------------------------------------

plot_partner_series <- function(dat, yvar, ylab, y_scale,
                                title, subtitle, caption) {
  
  lvls <- dat |>
    filter(year == max(year)) |>
    arrange(desc({{ yvar }})) |>
    pull(country)
  
  dat  <- dat |> mutate(country = factor(country, levels = lvls))
  ends <- dat |> filter(year == max(year))
  
  ggplot(dat, aes(year, {{ yvar }}, colour = country, group = country)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.4) +
    geom_text_repel(
      data = ends,
      aes(label = str_wrap(country, 14)),
      family = "source_sans", size = 3.2, fontface = "bold", lineheight = 0.9,
      hjust = 0, direction = "y", nudge_x = 0.5,
      segment.size = 0.3, segment.alpha = 0.5,
      min.segment.length = 0, seed = 42
    ) +
    scale_colour_csis() +
    scale_x_continuous(breaks = seq(2006, 2025, by = 2),
                       expand = expansion(mult = c(0.02, 0.18))) +
    y_scale +
    labs(title = title, subtitle = subtitle, caption = caption,
         x = NULL, y = ylab) +
    theme_csis() +
    theme(legend.position = "none")
}

DFAT_SRC <- paste0(
  "Source: DFAT, Australia's merchandise exports and imports country ",
  "and commodity pivot table (ABS cat. 5368.0), calendar years."
)
GOODS_NOTE <- paste0(
  "\nNote: merchandise (goods) only. Not comparable in level with ABS ",
  "series covering goods and services."
)


# ---------------------------------------------------------------
# 3. Chart — import levels
# ---------------------------------------------------------------

p_imports_long <- plot_partner_series(
  imports_long, value_bn, "Imports (A$ billion)",
  scale_y_continuous(labels = label_dollar(suffix = "b", accuracy = 1),
                     limits = c(0, NA), expand = expansion(mult = c(0, 0.08))),
  #title    = "China supplies more than a quarter of Australia's goods imports",
   title = "Merchandise imports by origin",
  subtitle = "A$ billion, 2006-2025",
  caption  = paste0(DFAT_SRC, GOODS_NOTE)
)

ggsave("dfat_imports_partners_long.png", p_imports_long,
       width = 8.5, height = 5, dpi = 150, bg = "white")


# ---------------------------------------------------------------
# 4. Chart — import shares
# ---------------------------------------------------------------

p_imports_share <- plot_partner_series(
  imports_long, share, "Share of total merchandise imports",
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     limits = c(0, NA), expand = expansion(mult = c(0, 0.08))),
  #title    = "Import sourcing has concentrated on a single supplier",
   title = "Share of total merchandise imports",
  subtitle = "By origin, 2006-2025",
  caption  = DFAT_SRC
)

ggsave("dfat_import_shares_partners_long.png", p_imports_share,
       width = 8.5, height = 5, dpi = 150, bg = "white")


# ---------------------------------------------------------------
# 5. Chart — import sector shares
# ---------------------------------------------------------------
# Mirrors the export sector chart. Note the y-axis cannot be shared
# between the two: imports are ~76% manufactures against ~13% for
# exports, so a common scale would flatten the export series to
# illegibility. Keep them as separate panels, not a faceted pair.

sector_shares <- read_csv("dfat_sector_by_year.csv", show_col_types = FALSE) |>
  mutate(share = value_k / total_k)

imp_sectors <- sector_shares |> filter(trade_type == "Total Imports")

recon_imp <- imp_sectors |>
  group_by(year) |>
  summarise(share_sum = sum(share), .groups = "drop")
if (any(abs(recon_imp$share_sum - 1) > 1e-6)) {
  warning("Import sector shares do not sum to 1.")
} else {
  message("Import sector shares reconcile.")
}

lvls_s <- imp_sectors |>
  filter(year == max(year)) |>
  arrange(desc(share)) |>
  pull(sector)

imp_plot <- imp_sectors |> mutate(sector = factor(sector, levels = lvls_s))
ends_s   <- imp_plot |> filter(year == max(year))

p_imports_sector <- ggplot(imp_plot, aes(year, share, colour = sector, group = sector)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.4) +
  geom_text_repel(
    data = ends_s,
    aes(label = str_wrap(sector, 16)),
    family = "source_sans", size = 3.2, fontface = "bold", lineheight = 0.9,
    hjust = 0, direction = "y", nudge_x = 0.4,
    segment.size = 0.3, segment.alpha = 0.5,
    min.segment.length = 0, seed = 42
  ) +
  scale_colour_csis() +
  scale_x_continuous(breaks = seq(2006, 2025, by = 2),
                     expand = expansion(mult = c(0.02, 0.20))) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
  labs(
    #title    = "Australia imports mostly manufactured goods",
    title = "Share of total merchandise imports",
    subtitle = "By broad sector, 2006-2025",
    caption  = paste0(
      DFAT_SRC,
      "\nSectors are DFAT's four-way aggregation of SITC Rev. 4 3-digit groups."
    ),
    x = NULL, y = "Share of total merchandise imports"
  ) +
  theme_csis() +
  theme(legend.position = "none")

ggsave("dfat_import_shares_by_sector.png", p_imports_sector,
       width = 8.5, height = 5, dpi = 150, bg = "white")


# ---------------------------------------------------------------
# 6. Optional — exports vs imports sector composition
# ---------------------------------------------------------------
# A faceted stacked area works where the line version does not,
# because each panel is normalised to 100%.

p_both <- ggplot(sector_shares, aes(year, share, fill = sector)) +
  geom_area(colour = "white", linewidth = 0.25) +
  facet_wrap(~ trade_type) +
  scale_fill_csis() +
  scale_x_continuous(breaks = seq(2006, 2025, by = 4),
                     expand = expansion(mult = c(0, 0))) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     expand = expansion(mult = c(0, 0))) +
  labs(
    #title    = "Australia's trade composition is close to a mirror image",
    title = "Share of merchandise trade",
    subtitle = "By broad sector, 2006-2025",
    caption  = DFAT_SRC, x = NULL, y = NULL
  ) +
  theme_csis() +
  theme(legend.position = "top",
        strip.text = element_text(face = "bold",
                                  colour = csis_colours[["navy"]]))

ggsave("dfat_sector_composition_both.png", p_both,
       width = 9.5, height = 5, dpi = 150, bg = "white")


# ---------------------------------------------------------------
# 7. Tidy outputs
# ---------------------------------------------------------------

write_csv(imports_long,   "dfat_top5_imports_tidy.csv")
write_csv(sector_shares,  "dfat_sector_shares_tidy.csv")


# ---------------------------------------------------------------
# Services imports: importance of the United States
# Source: ABS International Trade: Supplementary Information, CY2025
# ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(scales)

# abs_services_imports_by_country.csv is written at the top of this script
# from the ABS data cube (country, value_m, share, change). Regional groups
# are dropped so the chart shows the top 10 individual sources.
services_imp <- readr::read_csv(
  file.path(TRADE_DIR, "abs_services_imports_by_country.csv"),
  show_col_types = FALSE
) %>%
  filter(
    !is.na(value_m),
    !country %in% c("APEC", "OECD", "ASEAN", "EU", "Unallocated")
  ) %>%
  slice_max(value_m, n = 10) %>%
  mutate(
    value_bn = value_m / 1000,
    is_us = country == "United States of America",
    country = if_else(is_us, "United States", country)
  ) %>%
  arrange(value_bn) %>%
  mutate(country = factor(country, levels = country))

# Useful headline numbers
us <- services_imp %>% filter(is_us)

cat(sprintf(
  "United States: $%.1fbn, %.1f%% of Australian services imports in 2025\n",
  us$value_bn, us$share
))

p_services_imp <- ggplot(services_imp, aes(value_bn, country, fill = is_us)) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = paste0("$", sprintf("%.1f", value_bn), "bn  (", share, "%)")),
    hjust = -0.1,
    size = 3,
    family = "source_sans",
    colour = csis_colours[["grey"]]
  ) +
  scale_fill_manual(
    values = c(`TRUE` = csis_colours[["red"]],
               `FALSE` = csis_colours[["ltgrey"]]),
    guide = "none"
  ) +
  scale_x_continuous(
    labels = label_dollar(suffix = "bn"),
    expand = expansion(mult = c(0, 0.22))
  ) +
  labs(
    title = "The United States dominates Australia's services imports",
    subtitle = "Services imports by source country, 2025",
    x = NULL, y = NULL,
    caption = paste(
      "Source: ABS, International Trade: Supplementary Information,",
      "Calendar Year 2025."
    )
  ) +
  theme_csis() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
      colour = "#E6E7E8", linewidth = 0.4
    ),
    axis.line.x = element_blank(),
    axis.ticks.x = element_blank()
  )

p_services_imp

ggsave(
  "services_imports_us.png",
  p_services_imp,
  width = 8,
  height = 5.5,
  dpi = 150,
  bg = "white"
)



# =============================================================================
# US share of Australia's services trade, by service category
#   Credits (Australian services exports)  -> 536805500405.xlsx (Tables 5.1-5.13)
#   Debits  (Australian services imports)  -> 536805500406.xlsx (Tables 6.1-6.13)
# Source: ABS, International Trade: Supplementary Information, Calendar Year
#
# Sheet layout (every "Table x.y" sheet):
#   row 6  = service name
#   row 7  = years
#   row 40 = United States of America
#   row 52 = Total all countries  (denominator)
# Cell codes: "-" = nil (treated as 0), "np" = not published (treated as NA)
# Debits are stored as negatives; absolute values are used so shares are positive.
# =============================================================================

library(readxl)
library(data.table)
library(ggplot2)
library(scales)
# rlang is used via rlang:: in the helper (not attached, to avoid masking data.table's :=)

# ---- Paths ------------------------------------------------------------------
in_dir  <- TRADE_DIR
out_dir <- TRADE_DIR

files <- c(
  Credits = file.path(in_dir, "536805500405.xlsx"),
  Debits  = file.path(in_dir, "536805500406.xlsx")
)

ROW_SERVICE <- 6
ROW_YEAR    <- 7
ROW_US      <- 40
ROW_TOTAL   <- 52

# Drop categories that are too small for a share to mean anything: if total
# trade with all countries never reaches this ($m in any year), the category is
# skipped. Manufacturing services, for example, peaks at $57m of credits, so
# its share sits near 100% on one or two rounded transactions. Set to 0 to keep
# every category.
MIN_TOTAL_AM <- 100

# ---- Helpers ----------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

export_chart_csv <- function(p, file,
                             y_format = c("number", "percent"),
                             chart_type = "Line with point markers",
                             notes = NULL) {
  y_format <- match.arg(y_format)
  
  xvar <- rlang::as_label(p$mapping$x)
  yvar <- rlang::as_label(p$mapping$y)
  
  dat <- as.data.table(p$data)[, c(xvar, yvar), with = FALSE]
  setorderv(dat, xvar)
  
  if (y_format == "percent") {
    dat[[yvar]] <- round(dat[[yvar]] * 100, 1)
    y_unit <- "Percent (values are 0-100); axis labels shown as whole-number %"
  } else {
    y_unit <- "Count; axis labels shown with thousands separator"
  }
  
  # Pull styling/scale info from the plot itself
  lyr_params <- p$layers[[1]]$aes_params
  y_scale    <- p$scales$get_scales("y")
  y_lims     <- if (!is.null(y_scale)) y_scale$limits else NULL
  fmt_lim    <- function(v) if (is.null(v) || is.na(v)) "auto" else as.character(v)
  
  meta <- data.table(
    field = c("title", "subtitle", "caption",
              "chart_type", "x_variable", "x_axis_label",
              "y_variable", "y_axis_label", "y_units_format",
              "y_axis_min", "y_axis_max",
              "line_colour", "line_width_mm", "point_size",
              "title_style", "notes"),
    value = c(p$labels$title    %||% "",
              p$labels$subtitle %||% "",
              p$labels$caption  %||% "",
              chart_type,
              xvar, p$labels$x %||% "(none)",
              yvar, p$labels$y %||% "(none)",
              y_unit,
              fmt_lim(y_lims[1]), fmt_lim(y_lims[2]),
              lyr_params$colour    %||% "",
              lyr_params$linewidth %||% "",
              as.character(p$layers[[2]]$aes_params$size %||% ""),
              "Bold",
              notes %||% "")
  )
  
  fwrite(meta, file, col.names = FALSE, bom = TRUE)   # BOM so Excel reads UTF-8 cleanly
  cat("\n", file = file, append = TRUE)
  fwrite(dat, file, append = TRUE, col.names = TRUE)
  invisible(file)
}

# Convert ABS cell text to numbers: "-" -> 0, "np" -> NA
parse_abs <- function(x) {
  x <- trimws(as.character(x))
  out <- suppressWarnings(as.numeric(x))
  out[x %in% c("-", "\u2013")] <- 0
  out[tolower(x) %in% c("np", "na", "n.a.", "..", "x")] <- NA_real_
  out
}

# File-name-safe slug
slugify <- function(x) {
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  gsub("^_|_$", "", x)
}

# Read one "Table x.y" sheet -> long data.table (year, us, total, share)
read_abs_sheet <- function(path, sheet, flow) {
  raw <- read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text",
                    range = cell_rows(c(1, ROW_TOTAL)), .name_repair = "minimal")
  raw <- as.data.table(raw)
  
  service   <- trimws(raw[[1]][ROW_SERVICE])
  us_lab    <- trimws(raw[[1]][ROW_US])
  total_lab <- trimws(raw[[1]][ROW_TOTAL])
  
  # Sanity-check that the fixed rows still point at the right lines
  if (!grepl("United States", us_lab, ignore.case = TRUE))
    warning(sprintf("%s [%s]: row %d is '%s', not USA", basename(path), sheet, ROW_US, us_lab))
  if (!grepl("^Total all countries", total_lab, ignore.case = TRUE))
    warning(sprintf("%s [%s]: row %d is '%s', not Total", basename(path), sheet, ROW_TOTAL, total_lab))
  
  years <- suppressWarnings(as.integer(unlist(raw[ROW_YEAR, -1])))
  keep  <- which(!is.na(years))                  # drop trailing blank columns
  
  us    <- abs(parse_abs(unlist(raw[ROW_US,    -1]))[keep])
  total <- abs(parse_abs(unlist(raw[ROW_TOTAL, -1]))[keep])
  
  data.table(
    flow    = flow,
    table   = sub("^Table\\s*", "", sheet),
    service = service,
    year    = years[keep],
    us      = us,
    total   = total,
    share   = fifelse(!is.na(total) & total > 0, us / total, NA_real_)
  )
}

# ---- Import -----------------------------------------------------------------
dt <- rbindlist(lapply(names(files), function(flow) {
  path   <- files[[flow]]
  sheets <- grep("^Table", excel_sheets(path), value = TRUE)
  rbindlist(lapply(sheets, read_abs_sheet, path = path, flow = flow))
}))

# Harmonise the aggregate rows so credits/debits share one label
dt[service %in% c("Services Credits", "Services Debits"), service := "Total services"]

# Order services as they appear in the ABS tables (x.1 ... x.13)
dt[, tab_no := as.integer(sub(".*\\.", "", table))]
dt[, service := factor(service, levels = unique(service[order(tab_no)]))]

# Tidy master file of all numbers behind the charts
fwrite(dt[order(flow, tab_no, year), .(flow, table, service, year,
                                       us_Am = us, total_Am = total,
                                       us_share_pct = round(share * 100, 2))],
       file.path(out_dir, "us_share_services_all.csv"), bom = TRUE)

# ---- Chart settings ---------------------------------------------------------
flow_meta <- list(
  Credits = list(colour = "#1F4E79",
                 title  = "US share of Australia's services exports",
                 short  = "exports (credits)"),
  Debits  = list(colour = "#C0504D",
                 title  = "US share of Australia's services imports",
                 short  = "imports (debits)")
)

theme_dash <- theme_minimal(base_size = 11) +
  theme(plot.title       = element_text(face = "bold"),
        plot.caption     = element_text(hjust = 0, colour = "grey40", size = 8),
        panel.grid.minor = element_blank())

src_caption <- function(tab) {
  sprintf("Source: ABS, International Trade: Supplementary Information, Calendar Year (Table %s). Gaps = not published (np).", tab)
}

# ---- One chart per flow x service -------------------------------------------
for (fl in names(flow_meta)) {
  fm <- flow_meta[[fl]]
  for (svc in levels(droplevels(dt[flow == fl]$service))) {
    
    d <- dt[flow == fl & service == svc, .(year, share, total)]
    tab <- dt[flow == fl & service == svc, table][1]
    
    if (all(is.na(d$share))) {
      message(sprintf("Skipping %s / %s: no published or non-zero data", fl, svc))
      next
    }
    
    peak <- suppressWarnings(max(d$total, na.rm = TRUE))
    if (!is.finite(peak) || peak < MIN_TOTAL_AM) {
      message(sprintf("Skipping %s / %s: peak total with all countries only $%.0fm",
                      fl, svc, if (is.finite(peak)) peak else 0))
      next
    }
    
    d[, total := NULL]
    
    np_years <- d[is.na(share), year]
    note <- if (length(np_years))
      sprintf("Share missing (np or zero total) for: %s", paste(np_years, collapse = ", "))
    else NULL
    
    p <- ggplot(d, aes(x = year, y = share)) +
      geom_line(colour = fm$colour, linewidth = 0.9, na.rm = TRUE) +
      geom_point(colour = fm$colour, size = 2, na.rm = TRUE) +
      scale_y_continuous(labels = percent_format(accuracy = 1),
                         limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
      scale_x_continuous(breaks = pretty_breaks(n = 8)) +
      labs(title    = paste0(fm$title, ": ", svc),
           subtitle = sprintf("United States as a share of total Australian services %s, calendar years", fm$short),
           caption  = src_caption(tab),
           x = NULL, y = "Share of total (%)") +
      theme_dash
    
    stem <- sprintf("%s_%02d_%s", tolower(fl), dt[flow == fl & service == svc, tab_no][1], slugify(svc))
    ggsave(file.path(out_dir, paste0(stem, ".png")), p, width = 8, height = 4.5, dpi = 150, bg = "white")
    export_chart_csv(p, file.path(out_dir, paste0(stem, ".csv")),
                     y_format = "percent", notes = note)
  }
}

# ---- Overview: small multiples, credits vs debits, per service ---------------
# Same threshold for the overview panel, so it matches the individual charts
big <- dt[, .(peak = suppressWarnings(max(total, na.rm = TRUE))),
          by = .(flow, service)][is.finite(peak) & peak >= MIN_TOTAL_AM]
ov <- merge(dt, big[, .(flow, service)], by = c("flow", "service"))
ov <- ov[, if (!all(is.na(share))) .SD, by = .(service)]
ov[, service := droplevels(service)]
p_ov <- ggplot(ov, aes(x = year, y = share, colour = flow)) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  geom_point(size = 1, na.rm = TRUE) +
  facet_wrap(~ service, ncol = 3, scales = "free_y",
             labeller = label_wrap_gen(width = 35)) +
  scale_colour_manual(values = c(Credits = flow_meta$Credits$colour,
                                 Debits  = flow_meta$Debits$colour),
                      labels = c(Credits = "Exports to US (credits)",
                                 Debits  = "Imports from US (debits)")) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA)) +
  labs(title    = "US share of Australia's services trade, by service",
       subtitle = "United States as a share of total services flows in each direction",
       caption  = "Source: ABS, International Trade: Supplementary Information, Calendar Year (Tables 5.x and 6.x). Gaps = not published (np).",
       x = NULL, y = NULL, colour = NULL) +
  theme_dash +
  theme(legend.position = "top", strip.text = element_text(face = "bold", size = 8))

ggsave(file.path(out_dir, "overview_us_share_by_service.png"), p_ov,
       width = 11, height = 10, dpi = 150, bg = "white")

# Wide CSV behind the overview (one column per service x flow, % values)
ov_wide <- dcast(ov[, .(year, key = paste(flow, service, sep = " | "),
                        share = round(share * 100, 1))],
                 year ~ key, value.var = "share")
fwrite(ov_wide, file.path(out_dir, "overview_us_share_by_service.csv"), bom = TRUE)

message("Done. Outputs written to: ", out_dir)



# =============================================================================
# APPENDIX: US share of Australia's MERCHANDISE trade
#   Table 1 - Merchandise exports, by selected countries, six month aggregates,
#             FOB value, $m           -> 536805500401.xlsx
#   Table 2 - Merchandise imports, by selected countries, six month aggregates,
#             customs value, $m       -> 536805500402.xlsx
# (Same ABS release as the services tables: International Trade: Supplementary
#  Information, Calendar Year.)
#
# Append this to us_australia_services_shares.R, or source that script first:
# it reuses in_dir, out_dir, export_chart_csv(), parse_abs(), slugify(),
# theme_dash and flow_meta from there.
#
# Tables 1 and 2 have no commodity breakdown, so this reproduces the
# "Total services" chart for goods: one chart per direction.
#
# NOTE ON LAYOUT: unlike Tables 5 and 6, the row/column positions here are not
# hard-coded. The script finds the header row and the USA / Total rows by
# matching their labels, then sums the six-month columns into calendar years.
# Check the console messages the first time you run it: it reports which rows
# and how many periods per year it found.
# =============================================================================

stopifnot(exists("export_chart_csv"))   # run the services script first

goods_files <- c(
  Exports = file.path(in_dir, "536805500401.xlsx"),   # Table 1
  Imports = file.path(in_dir, "536805500402.xlsx")    # Table 2
)

# ---- Helpers ----------------------------------------------------------------

# Pull a calendar year out of a six-month column header. Handles plain years,
# text like "Jan-Jun 2000" / "Jul-Dec 2000", and Excel date serials or
# date strings (the year of the period-end date is used).
period_year <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x)] <- ""
  yr <- rep(NA_integer_, length(x))
  n  <- suppressWarnings(as.numeric(x))
  isnum <- !is.na(n)
  
  # Plain year, e.g. 2000
  plain <- isnum & n >= 1900 & n <= 2100
  yr[plain] <- as.integer(n[plain])
  
  # Excel date serial (1900 system) - checked before any text matching, since
  # a serial like 42003 otherwise looks as if it contains the year 2003
  ser <- isnum & is.na(yr) & n > 20000 & n < 80000
  yr[ser] <- as.integer(format(as.Date(n[ser], origin = "1899-12-30"), "%Y"))
  
  # Date strings, e.g. "2000-06-30"
  d  <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  ds <- !isnum & is.na(yr) & !is.na(d)
  yr[ds] <- as.integer(format(d[ds], "%Y"))
  
  # Text labels containing a year, e.g. "Jan-Jun 2000"
  txt <- which(!isnum & is.na(yr))
  if (length(txt)) {
    pos <- regexpr("(19|20)\\d{2}", x[txt])
    ok  <- pos > 0
    if (any(ok)) yr[txt[ok]] <- as.integer(regmatches(x[txt], pos))
  }
  
  yr
}

# Read one merchandise table -> long data.table (year, us, total, n_periods)
read_merch_sheet <- function(path, sheet, flow) {
  raw <- as.data.table(
    read_excel(path, sheet = sheet, col_names = FALSE, col_types = "text",
               .name_repair = "minimal")
  )
  
  # Header row = the row in the first 15 with the most parseable periods
  n_hdr  <- vapply(seq_len(min(15, nrow(raw))),
                   function(i) sum(!is.na(period_year(unlist(raw[i, -1])))),
                   integer(1))
  hdr    <- which.max(n_hdr)
  if (n_hdr[hdr] == 0)
    stop(sprintf("%s [%s]: could not find a header row of periods", basename(path), sheet))
  
  labs     <- trimws(raw[[1]])
  row_us   <- which(grepl("^United States", labs, ignore.case = TRUE))[1]
  row_tot  <- which(grepl("^Total( all countries)?$", labs, ignore.case = TRUE))[1]
  if (is.na(row_tot))
    row_tot <- which(grepl("^Total", labs, ignore.case = TRUE))[1]
  if (is.na(row_us) || is.na(row_tot))
    stop(sprintf("%s [%s]: could not find the USA and/or Total row", basename(path), sheet))
  
  message(sprintf("%s [%s]: header row %d, USA row %d ('%s'), total row %d ('%s')",
                  basename(path), sheet, hdr, row_us, labs[row_us], row_tot, labs[row_tot]))
  
  years <- period_year(unlist(raw[hdr, -1]))
  keep  <- which(!is.na(years))
  
  long <- data.table(
    year  = years[keep],
    us    = abs(parse_abs(unlist(raw[row_us,  -1]))[keep]),
    total = abs(parse_abs(unlist(raw[row_tot, -1]))[keep])
  )
  
  # Sum the six-month columns into calendar years
  # A missing half-year makes the whole year missing rather than half-counted
  out <- long[, .(us = sum(us), total = sum(total), n_periods = .N), by = year]
  out[, `:=`(flow = flow, sheet = sheet)]
  out[]
}

# ---- Import -----------------------------------------------------------------
goods <- rbindlist(lapply(names(goods_files), function(flow) {
  path   <- goods_files[[flow]]
  sheets <- grep("^Table|^Data", excel_sheets(path), value = TRUE)
  if (!length(sheets)) sheets <- setdiff(excel_sheets(path), "Contents")
  rbindlist(lapply(sheets, read_merch_sheet, path = path, flow = flow))
}))

# Flag part-years (a calendar year should be two six-month periods)
np <- goods[, .N, by = n_periods]
message("Periods per year found: ", paste(sprintf("%d periods x %d years", np$n_periods, np$N),
                                          collapse = "; "))
goods[, full := max(n_periods), by = flow]
part_years <- goods[n_periods < full, sort(unique(year))]
if (length(part_years))
  message("Incomplete years dropped: ", paste(part_years, collapse = ", "))
goods <- goods[n_periods == full][, full := NULL]

goods[, share := fifelse(!is.na(total) & total > 0, us / total, NA_real_)]
setorder(goods, flow, year)

fwrite(goods[, .(flow, year, us_Am = us, total_Am = total,
                 us_share_pct = round(share * 100, 2))],
       file.path(out_dir, "us_share_goods_all.csv"), bom = TRUE)

# ---- Charts: one per direction ----------------------------------------------
goods_meta <- list(
  Exports = list(colour = flow_meta$Credits$colour,
                 title  = "US share of Australia's merchandise exports",
                 short  = "exports (FOB value)",
                 tab    = "1"),
  Imports = list(colour = flow_meta$Debits$colour,
                 title  = "US share of Australia's merchandise imports",
                 short  = "imports (customs value)",
                 tab    = "2")
)

for (fl in names(goods_meta)) {
  gm <- goods_meta[[fl]]
  d  <- goods[flow == fl, .(year, share)]
  if (all(is.na(d$share))) { message("Skipping goods / ", fl, ": no data"); next }
  
  p <- ggplot(d, aes(x = year, y = share)) +
    geom_line(colour = gm$colour, linewidth = 0.9, na.rm = TRUE) +
    geom_point(colour = gm$colour, size = 2, na.rm = TRUE) +
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
    scale_x_continuous(breaks = pretty_breaks(n = 8)) +
    labs(title    = gm$title,
         subtitle = sprintf("United States as a share of total Australian merchandise %s, calendar years", gm$short),
         caption  = sprintf("Source: ABS, International Trade: Supplementary Information, Calendar Year (Table %s). Merchandise trade basis.", gm$tab),
         x = NULL, y = "Share of total (%)") +
    theme_dash
  
  stem <- paste0("goods_", tolower(fl), "_us_share")
  ggsave(file.path(out_dir, paste0(stem, ".png")), p,
         width = 8, height = 4.5, dpi = 150, bg = "white")
  export_chart_csv(p, file.path(out_dir, paste0(stem, ".csv")),
                   y_format = "percent",
                   notes = paste("Six-month aggregates summed to calendar years.",
                                 "Merchandise trade basis, not balance of payments;",
                                 "not directly comparable with the services totals."))
}

# ---- Optional: goods vs services, same direction ----------------------------
# Runs only if the services script's `dt` is still in the session.
if (exists("dt")) {
  cmp <- rbind(
    goods[, .(year, share, series = paste0("Goods (", flow, ")"),
              direction = fifelse(flow == "Exports", "Exports / credits", "Imports / debits"))],
    dt[service == "Total services",
       .(year, share, series = paste0("Services (", flow, ")"),
         direction = fifelse(flow == "Credits", "Exports / credits", "Imports / debits"))]
  )
  
  p_cmp <- ggplot(cmp, aes(x = year, y = share, colour = series)) +
    geom_line(linewidth = 0.8, na.rm = TRUE) +
    geom_point(size = 1.4, na.rm = TRUE) +
    facet_wrap(~ direction) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA)) +
    scale_colour_manual(values = c("Goods (Exports)"    = flow_meta$Credits$colour,
                                   "Services (Credits)" = "#7FA8CC",
                                   "Goods (Imports)"    = flow_meta$Debits$colour,
                                   "Services (Debits)"  = "#E1A8A6")) +
    labs(title    = "US share of Australia's trade: goods vs services",
         subtitle = "United States as a share of total flows in each direction",
         caption  = paste("Source: ABS, International Trade: Supplementary Information, Calendar Year (Tables 1, 2, 5 and 6).",
                          "Goods on a merchandise trade basis, services on a balance of payments basis.", sep = "\n"),
         x = NULL, y = NULL, colour = NULL) +
    theme_dash + theme(legend.position = "top")
  
  ggsave(file.path(out_dir, "goods_vs_services_us_share.png"), p_cmp,
         width = 10, height = 5, dpi = 150, bg = "white")
  
  fwrite(dcast(cmp[, .(year, series, share = round(share * 100, 1))],
               year ~ series, value.var = "share"),
         file.path(out_dir, "goods_vs_services_us_share.csv"), bom = TRUE)
}

message("Goods appendix done.")
# =============================================================================
# US share of Australia's goods trade, by industry (SITC) category
# Data: dfat_us_vs_all_by_sitc.csv, written at the top of this script from the
#       DFAT pivot table it downloads (US and all-country values for every
#       3-digit SITC group). No hand-filtered Excel copies are needed.
# Source: DFAT country and SITC pivot tables (built from ABS International Trade
#         in Goods), calendar years.
#
# Share for each industry = US value / all-countries value for the SAME industry,
# i.e. the goods analogue of the services charts. Each chart is saved as
# goods_<exports|imports>_<NN>_<label>.png, which the dashboard's
# "Trade in goods" dropdown picks up automatically.
# =============================================================================

library(data.table)
library(ggplot2)
library(scales)
# rlang is used via rlang:: in the helper (not attached, to avoid masking data.table's :=)

# ---- Paths ------------------------------------------------------------------
out_dir <- TRADE_DIR

# ---- Helpers ----------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

export_chart_csv <- function(p, file,
                             y_format = c("number", "percent"),
                             chart_type = "Line with point markers",
                             notes = NULL) {
  y_format <- match.arg(y_format)
  
  xvar <- rlang::as_label(p$mapping$x)
  yvar <- rlang::as_label(p$mapping$y)
  
  dat <- as.data.table(p$data)[, c(xvar, yvar), with = FALSE]
  setorderv(dat, xvar)
  
  if (y_format == "percent") {
    dat[[yvar]] <- round(dat[[yvar]] * 100, 1)
    y_unit <- "Percent (values are 0-100); axis labels shown as whole-number %"
  } else {
    y_unit <- "Count; axis labels shown with thousands separator"
  }
  
  # Pull styling/scale info from the plot itself
  lyr_params <- p$layers[[1]]$aes_params
  y_scale    <- p$scales$get_scales("y")
  y_lims     <- if (!is.null(y_scale)) y_scale$limits else NULL
  fmt_lim    <- function(v) if (is.null(v) || is.na(v)) "auto" else as.character(v)
  
  meta <- data.table(
    field = c("title", "subtitle", "caption",
              "chart_type", "x_variable", "x_axis_label",
              "y_variable", "y_axis_label", "y_units_format",
              "y_axis_min", "y_axis_max",
              "line_colour", "line_width_mm", "point_size",
              "title_style", "notes"),
    value = c(p$labels$title    %||% "",
              p$labels$subtitle %||% "",
              p$labels$caption  %||% "",
              chart_type,
              xvar, p$labels$x %||% "(none)",
              yvar, p$labels$y %||% "(none)",
              y_unit,
              fmt_lim(y_lims[1]), fmt_lim(y_lims[2]),
              lyr_params$colour    %||% "",
              lyr_params$linewidth %||% "",
              as.character(p$layers[[2]]$aes_params$size %||% ""),
              "Bold",
              notes %||% "")
  )
  
  fwrite(meta, file, col.names = FALSE, bom = TRUE)   # BOM so Excel reads UTF-8 cleanly
  cat("\n", file = file, append = TRUE)
  fwrite(dat, file, append = TRUE, col.names = TRUE)
  invisible(file)
}

# File-name-safe slug
slugify <- function(x) {
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  gsub("^_|_$", "", x)
}

# ---- Import -----------------------------------------------------------------
goods <- fread(file.path(TRADE_DIR, "dfat_us_vs_all_by_sitc.csv"))
setnames(goods, c("us_m", "total_m"), c("us", "total"))
goods[, `:=`(us = abs(us), total = abs(total))]
setorder(goods, flow, industry, year)   # SITC order, "Total goods" last

# Keep the file's own row order, and mark the total row
ord <- unique(goods$industry)
goods[, industry := factor(industry, levels = ord)]
goods[, is_total := grepl("^total", as.character(industry), ignore.case = TRUE)]
goods[, share := fifelse(!is.na(total) & total > 0, us / total, NA_real_)]
setorder(goods, flow, industry, year)

# Sanity check: a US share above 100% means the two files aren't lined up
# (wrong industry labels matched, or the "all" file isn't all countries)
odd <- goods[!is.na(share) & share > 1.05]
if (nrow(odd))
  warning(sprintf("US value exceeds the all-countries value in %d cells, e.g. %s %s %d. Check the two files match.",
                  nrow(odd), odd$flow[1], as.character(odd$industry[1]), odd$year[1]))

# Tidy master file of all numbers behind the charts
fwrite(goods[, .(flow, industry, year, us_Am = round(us, 1), total_Am = round(total, 1),
                 us_share_pct = round(share * 100, 2))],
       file.path(out_dir, "us_share_goods_all.csv"), bom = TRUE)

# ---- Chart settings ---------------------------------------------------------
flow_meta <- list(
  Exports = list(colour = "#1F4E79",
                 title  = "US share of Australia's goods exports",
                 short  = "exports"),
  Imports = list(colour = "#C0504D",
                 title  = "US share of Australia's goods imports",
                 short  = "imports")
)

theme_dash <- theme_minimal(base_size = 11) +
  theme(plot.title       = element_text(face = "bold"),
        plot.caption     = element_text(hjust = 0, colour = "grey40", size = 8),
        panel.grid.minor = element_blank())

src_caption <- "Source: DFAT country and SITC pivot tables (ABS International Trade in Goods), calendar years. Merchandise trade basis."

# ---- One chart per flow x industry ------------------------------------------
# Clear last run's charts first: the number in each file name is the
# industry's position, so a new or dropped SITC group renames the rest and
# would otherwise leave duplicates in the dashboard dropdown.
invisible(file.remove(list.files(out_dir, "^goods_(exports|imports)_[0-9]+_.*\\.(png|csv)$",
                                 full.names = TRUE)))

for (fl in names(flow_meta)) {
  fm <- flow_meta[[fl]]
  for (ind in levels(droplevels(goods[flow == fl]$industry))) {
    
    d <- goods[flow == fl & industry == ind, .(year, share)]
    
    if (all(is.na(d$share))) {
      message(sprintf("Skipping %s / %s: no published or non-zero data", fl, ind))
      next
    }
    
    gap_years <- d[is.na(share), year]
    note <- if (length(gap_years))
      sprintf("Share missing (not published or zero total) for: %s",
              paste(gap_years, collapse = ", "))
    else NULL
    
    p <- ggplot(d, aes(x = year, y = share)) +
      geom_line(colour = fm$colour, linewidth = 0.9, na.rm = TRUE) +
      geom_point(colour = fm$colour, size = 2, na.rm = TRUE) +
      scale_y_continuous(labels = percent_format(accuracy = 1),
                         limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
      scale_x_continuous(breaks = pretty_breaks(n = 8)) +
      labs(title    = paste0(fm$title, ": ", ind),
           subtitle = sprintf("United States as a share of total Australian goods %s, calendar years", fm$short),
           caption  = src_caption,
           x = NULL, y = "Share of total (%)") +
      theme_dash
    
    idx  <- which(levels(goods$industry) == ind)
    file_stem <- sprintf("goods_%s_%02d_%s", tolower(fl), idx, slugify(ind))
    ggsave(file.path(out_dir, paste0(file_stem, ".png")), p,
           width = 8, height = 4.5, dpi = 150, bg = "white")
    export_chart_csv(p, file.path(out_dir, paste0(file_stem, ".csv")),
                     y_format = "percent", notes = note)
  }
}

# ---- Overview: small multiples, exports vs imports, per industry -------------
ov <- goods[, if (!all(is.na(share))) .SD, by = .(industry)]
p_ov <- ggplot(ov, aes(x = year, y = share, colour = flow)) +
  geom_line(linewidth = 0.7, na.rm = TRUE) +
  geom_point(size = 1, na.rm = TRUE) +
  facet_wrap(~ industry, ncol = 3, scales = "free_y",
             labeller = label_wrap_gen(width = 35)) +
  scale_colour_manual(values = c(Exports = flow_meta$Exports$colour,
                                 Imports = flow_meta$Imports$colour),
                      labels = c(Exports = "Exports to US", Imports = "Imports from US")) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA)) +
  labs(title    = "US share of Australia's goods trade, by industry",
       subtitle = "United States as a share of total goods flows in each direction",
       caption  = src_caption,
       x = NULL, y = NULL, colour = NULL) +
  theme_dash +
  theme(legend.position = "top", strip.text = element_text(face = "bold", size = 8))

n_panel <- uniqueN(ov$industry)
ggsave(file.path(out_dir, "goods_overview_us_share_by_industry.png"), p_ov,
       width = 11, height = max(4, 2.2 * ceiling(n_panel / 3)), dpi = 150, bg = "white",
       limitsize = FALSE)

# Wide CSV behind the overview (one column per industry x flow, % values)
ov_wide <- dcast(ov[, .(year, key = paste(flow, industry, sep = " | "),
                        share = round(share * 100, 1))],
                 year ~ key, value.var = "share")
fwrite(ov_wide, file.path(out_dir, "goods_overview_us_share_by_industry.csv"), bom = TRUE)

message("Done. Outputs written to: ", out_dir)

rm(list = ls())