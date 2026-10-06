rm(list = ls())
# The dashboard folder is set once, in section 1 of '1. Refresh all data.R'.
# To run this script on its own, run that section first.
if (Sys.getenv("DASHBOARD_DIR") == "")
  stop("Dashboard folder not set. Run section 1 of '1. Refresh all data.R' first.")
setwd(file.path(Sys.getenv("DASHBOARD_DIR"), "Patents"))

# ==============================================================================
# IP RAPID (IP Australia) — patent applications joined to party activity
# and application links
#
# Using IP RAPID Data Dictionary 2023
#
# Key facts from the dictionary that drive the code below:
#   * The primary key of every table is BOTH ip_right_type AND application_number.
#     Application numbers are only unique within a right type, so all joins are
#     on the composite key (dictionary, "Table joining", p.4).
#   * ip_right_type values are lowercase: trade_mark, design, patent, pbr.
#   * Patents have sub-types: standard_complete, provisional, innovation, petty.
#   * application_date is IP Australia's own basis for reported filing counts;
#     earliest_filed_date is the first filing anywhere (incl. PCT/Madrid).
# ==============================================================================

library(data.table)
library(httr2)

# ------------------------------------------------------------------------------
# 0. Settings and data download
# ------------------------------------------------------------------------------
# IP RAPID is refreshed weekly on data.gov.au. The zip is re-downloaded only
# when the local copy is older than MAX_AGE_DAYS; set FORCE_REFRESH <- TRUE to
# fetch it now. Only the three tables used below are extracted.

data_dir <- "iprapid"
out_dir  <- "output"
dir.create(data_dir, showWarnings = FALSE)
dir.create(out_dir,  showWarnings = FALSE)

IPRAPID_URL <- paste0(
  "https://data.gov.au/data/dataset/423000b8-5735-4447-bcb9-792644bcd7ea/",
  "resource/c79b3af6-3720-44ac-9e39-6a68f5635924/download/iprapid.zip"
)
# Fallback if the link above ever changes: ask data.gov.au's catalogue
IPRAPID_API <- "https://data.gov.au/data/api/3/action/package_show?id=iprapid"

MAX_AGE_DAYS  <- 30
FORCE_REFRESH <- FALSE

zip_file <- file.path(data_dir, "iprapid.zip")
needed   <- c("application.csv", "party_activity.csv", "application_links.csv",
              "application_classification.csv")

is_fresh <- function(path) {
  !FORCE_REFRESH && file.exists(path) &&
    difftime(Sys.time(), file.mtime(path), units = "days") < MAX_AGE_DAYS
}

# A real zip starts with the bytes "PK"; anything else is an error page
is_real_zip <- function(path) {
  file.exists(path) && file.size(path) > 1e6 &&
    identical(readBin(path, "raw", 2), charToRaw("PK"))
}

catalogue_zip_url <- function() {
  res  <- request(IPRAPID_API) |> req_perform() |> resp_body_json()
  urls <- vapply(res$result$resources, function(r) r$url %||% "", character(1))
  hit  <- grep("\\.zip$", urls, ignore.case = TRUE, value = TRUE)
  if (!length(hit)) stop("no zip file listed in the data.gov.au catalogue")
  hit[1]
}

download_zip <- function(url) {
  tmp <- paste0(zip_file, ".part")          # never overwrite a good copy mid-download
  request(url) |>
    req_timeout(3600) |>                    # large file: allow up to an hour
    req_retry(max_tries = 3) |>
    req_progress() |>                       # progress bar in the console
    req_perform(path = tmp)
  if (!is_real_zip(tmp)) { unlink(tmp); stop("download was not a zip file") }
  unlink(zip_file)
  file.rename(tmp, zip_file)
}

if (is_fresh(zip_file) && is_real_zip(zip_file)) {
  message("IP RAPID: using copy from ", format(file.mtime(zip_file), "%d %b %Y"))
} else {
  ok <- FALSE
  for (src in c("direct link", "catalogue")) {
    ok <- tryCatch({
      message("IP RAPID: downloading via ", src, " (large file, may take a while)")
      download_zip(if (src == "direct link") IPRAPID_URL else catalogue_zip_url())
      TRUE
    }, error = function(e) { message("  failed: ", conditionMessage(e)); FALSE })
    if (ok) break
  }
  if (!ok) {
    if (is_real_zip(zip_file)) {
      warning("IP RAPID download failed. Using the existing copy from ",
              format(file.mtime(zip_file), "%d %b %Y"), call. = FALSE)
    } else {
      stop("IP RAPID download failed and there is no local copy.\n",
           "Download ", IPRAPID_URL, "\nand save it as ", normalizePath(zip_file, mustWork = FALSE),
           call. = FALSE)
    }
  }
}

# Extract only the tables needed, and only when the zip is newer than them.
# Files are matched by name wherever they sit inside the zip.
inside <- unzip(zip_file, list = TRUE)$Name
in_zip <- vapply(needed, function(f) {
  m <- inside[tolower(basename(inside)) == f]
  if (!length(m))
    stop(f, " not found in the zip. It contains: ",
         paste(basename(inside), collapse = ", "))
  m[1]
}, character(1))

dest  <- file.path(data_dir, needed)
stale <- !file.exists(dest) | file.mtime(dest) < file.mtime(zip_file)
if (any(stale)) {
  message("IP RAPID: extracting ", paste(needed[stale], collapse = ", "))
  unzip(zip_file, files = in_zip[stale], exdir = data_dir, junkpaths = TRUE)
  # unzip keeps the archive's original dates; stamp them as extracted now
  Sys.setFileTime(dest[stale], Sys.time())
}

app_file   <- file.path(data_dir, "application.csv")
party_file <- file.path(data_dir, "party_activity.csv")
links_file <- file.path(data_dir, "application_links.csv")

app_file   <- file.path(data_dir, "application.csv")
party_file <- file.path(data_dir, "party_activity.csv")
links_file <- file.path(data_dir, "application_links.csv")

join_keys <- c("ip_right_type", "application_number")   # per the dictionary

# Which patent sub-types to keep. Options: standard_complete, provisional,
# innovation, petty. NULL = keep all.
#   - Keeping provisionals AND completes double counts inventions: a provisional
#     that matures into a complete appears as two applications.
#   - Innovation patents were phased out from 2021 and will break any trend.
# "standard_complete" is the usual choice for economic analysis.
sub_types <- "standard_complete"

# Which date to derive the year from:
#   "application_date"    — closest to IP Australia's published filing counts;
#                           for PCT cases this is the national-phase entry date
#   "earliest_filed_date" — first filing anywhere in the world
#   "priority_date"       — earliest priority claimed
date_basis <- "earliest_filed_date"

join_type  <- "left"        # "left" or "inner"
links_mode <- "aggregate"   # "aggregate" or "expand"

party_cols <- NULL
links_cols <- NULL

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

read_iprapid <- function(path, select = NULL) {
  dt <- fread(path, na.strings = c("", "NA", "NULL", "\\N"),
              showProgress = TRUE, integer64 = "character", select = select)
  setnames(dt, tolower(trimws(names(dt))))   # dictionary prints "Is_founding"
  dt
}

parse_iprapid_date <- function(x) {
  if (inherits(x, "Date") || inherits(x, "IDate")) return(as.IDate(x))
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NULL", "\\N")] <- NA_character_
  x <- sub("[T ].*$", "", x)
  out <- rep(as.Date(NA), length(x))
  for (f in c("%Y-%m-%d", "%d/%m/%Y", "%Y/%m/%d", "%d-%m-%Y", "%Y%m%d")) {
    todo <- is.na(out) & !is.na(x)
    if (!any(todo)) break
    out[todo] <- as.Date(x[todo], format = f)
  }
  as.IDate(out)
}

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


# ------------------------------------------------------------------------------
# 1-2. Applications -> patents
# ------------------------------------------------------------------------------

applications <- read_iprapid(app_file)

stopifnot(all(join_keys %in% names(applications)))

applications[, ip_right_type := tolower(trimws(ip_right_type))]
print(applications[, .N, by = ip_right_type][order(-N)])

patents <- applications[ip_right_type == "patent"]

cat("patents: ", format(nrow(patents), big.mark = ","), " of ",
    format(nrow(applications), big.mark = ","), " applications\n", sep = "")

print(patents[, .N, by = ip_right_sub_type][order(-N)])

if (!is.null(sub_types)) {
  patents <- patents[ip_right_sub_type %chin% sub_types]
  cat("after sub-type filter (", paste(sub_types, collapse = ", "), "): ",
      format(nrow(patents), big.mark = ","), " rows\n", sep = "")
}

rm(applications); invisible(gc())

patents[, application_number := trimws(as.character(application_number))]
patent_ids <- patents$application_number

# ------------------------------------------------------------------------------
# 2b. Filing year
# ------------------------------------------------------------------------------

stopifnot(date_basis %in% names(patents))

patents[, (date_basis) := parse_iprapid_date(get(date_basis))]
patents[, filing_year := year(get(date_basis))]
patents[filing_year < 1900 | filing_year > year(Sys.Date()) + 1,
        filing_year := NA_integer_]

cat("\nmissing/implausible ", date_basis, ": ",
    patents[is.na(filing_year), .N], "\n", sep = "")
print(tail(patents[!is.na(filing_year), .N, by = filing_year][order(filing_year)], 10))

# ------------------------------------------------------------------------------
# 3. Party activity
# ------------------------------------------------------------------------------

party_activity <- read_iprapid(party_file, select = party_cols)
stopifnot(all(join_keys %in% names(party_activity)))

party_activity[, ip_right_type := tolower(trimws(ip_right_type))]
party_activity[, application_number := trimws(as.character(application_number))]
party_activity <- party_activity[ip_right_type == "patent" &
                                   application_number %chin% patent_ids]

# Dictionary Table 3 / Table 11. For patents, party_role_category "applicant"
# covers party_role applicant and patentee. NOTE: patents have NO inventor role
# in IP RAPID — only designs have "designer". Any analysis you would frame as
# co-invention is really co-assignment here.
party_activity[, party_role_category := tolower(trimws(party_role_category))]
party_activity[, country_code := tolower(trimws(country_code))]
print(party_activity[, .N, by = .(party_role_category, party_role)][order(-N)])

cat("party_activity (patents): ", format(nrow(party_activity), big.mark = ","),
    " rows\n", sep = "")

# ------------------------------------------------------------------------------
# 4. Application links
# ------------------------------------------------------------------------------

application_links <- read_iprapid(links_file, select = links_cols)
stopifnot(all(join_keys %in% names(application_links)))

application_links[, ip_right_type := tolower(trimws(ip_right_type))]
application_links[, application_number := trimws(as.character(application_number))]
application_links <- application_links[ip_right_type == "patent" &
                                         application_number %chin% patent_ids]

application_links[, linked_application_country := tolower(trimws(linked_application_country))]
application_links[, link_type := tolower(trimws(link_type))]

# Dictionary Table 5: most patent link types are domestic conversions
# (provisional_to_a_complete, divisional_parent, innovation_from_a_complete...).
# International connections come through convention / convention_child, i.e.
# Paris priority claims. PCT national-phase entry is recorded in
# application-events (event_type pct_application_enters_national_phase),
# NOT in this table — so links alone understate international linkage.
print(application_links[, .N, by = .(link_type, linked_application_country)][order(-N)][1:20])

# ------------------------------------------------------------------------------
# 5. Joins — on the composite key
# ------------------------------------------------------------------------------

patents_parties <- merge(
  patents, party_activity,
  by = join_keys, all.x = (join_type == "left"),
  allow.cartesian = TRUE, suffixes = c("_app", "_party")
)

if (links_mode == "aggregate") {
  
  links_agg <- application_links[
    !is.na(linked_application_country),
    .(n_linked_applications = .N,
      linked_countries = paste(sort(unique(linked_application_country)), collapse = "|")),
    by = join_keys
  ]
  
  for (cc in c("us", "ep", "wo", "cn", "jp", "gb", "au")) {
    links_agg[, (paste0("link_", cc)) :=
                as.integer(grepl(paste0("(^|\\|)", cc, "($|\\|)"), linked_countries))]
  }
  
  # Convention (priority) links only — the cleaner "same invention filed
  # abroad first" signal.
  conv_agg <- application_links[
    grepl("^convention", link_type) & !is.na(linked_application_country),
    .(convention_countries = paste(sort(unique(linked_application_country)), collapse = "|")),
    by = join_keys
  ]
  conv_agg[, conv_us := as.integer(grepl("(^|\\|)us($|\\|)", convention_countries))]
  
  patents_full <- merge(patents_parties, links_agg, by = join_keys, all.x = TRUE)
  patents_full <- merge(patents_full,    conv_agg,  by = join_keys, all.x = TRUE)
  patents_full[is.na(n_linked_applications), n_linked_applications := 0L]
  patents_full[is.na(conv_us), conv_us := 0L]
  
} else if (links_mode == "expand") {
  
  patents_full <- merge(patents_parties, application_links, by = join_keys,
                        all.x = TRUE, allow.cartesian = TRUE,
                        suffixes = c("", "_link"))
  
} else stop("links_mode must be 'aggregate' or 'expand'.")

# ------------------------------------------------------------------------------
# 6. Diagnostics & save
# ------------------------------------------------------------------------------

n_patents <- uniqueN(patent_ids)
cat("\n--- join summary ---\n")
cat("unique patents:              ", format(n_patents, big.mark = ","), "\n")
cat("with >=1 party record:       ", format(uniqueN(party_activity$application_number), big.mark = ","), "\n")
cat("with >=1 link:               ", format(uniqueN(application_links$application_number), big.mark = ","), "\n")
cat("rows after joins:            ", format(nrow(patents_full), big.mark = ","), "\n")
cat("rows per patent (mean):      ", round(nrow(patents_full) / n_patents, 2), "\n\n")

annual_patents <- patents_full[!is.na(filing_year),
                               .(n_patents = uniqueN(application_number)),
                               by = filing_year][order(filing_year)]
print(tail(annual_patents, 20))

saveRDS(patents_full, file.path(out_dir, "patents_parties_links.rds"))
fwrite(annual_patents, file.path(out_dir, "annual_patents.csv"))
cat("Written to ", normalizePath(out_dir), "\n", sep = "")


# ------------------------------------------------------------------------------
# 7. Australian patent applications with at least one US applicant
# ------------------------------------------------------------------------------

library(ggplot2)

# Guard against the merge having renamed these (only happens if the
# applications table also carries a column of the same name).
stopifnot(all(c("party_role_category", "country_code") %in% names(patents_full)))

# Drop the most recent year(s): with date_basis = "earliest_filed_date" an AU
# filing can enter the data up to 12 months (Paris) or 30+ months (PCT national
# phase) after its earliest filing, so the tail is heavily truncated.
year_min <- 1990L
year_max <- year(Sys.Date()) - 3L

# One row per patent: was any applicant/patentee party recorded in the US?
# Dictionary Table 3/11: party_role_category "applicant" covers party_role
# applicant and patentee. Patents have no inventor role, so this is the
# ownership/assignee signal, not inventor residence.
patent_flags <- patents_full[
  !is.na(filing_year),
  .(us_applicant = any(party_role_category == "applicant" &
                         country_code == "us", na.rm = TRUE),
    any_applicant = any(party_role_category == "applicant", na.rm = TRUE)),
  by = .(application_number, filing_year)
]

annual_us <- patent_flags[
  filing_year %between% c(year_min, year_max),
  .(n_us       = sum(us_applicant),
    n_total    = .N,
    n_known    = sum(any_applicant)),
  by = filing_year
][order(filing_year)]

annual_us[, share_us := n_us / n_known]   # denominator excludes patents with
# no applicant party record at all

cat("\n--- US-applicant patents ---\n")
print(tail(annual_us, 15))

p_count <- ggplot(annual_us, aes(filing_year, n_us)) +
  geom_line(linewidth = 0.8, colour = "#1f4e79") +
  geom_point(size = 1.4, colour = "#1f4e79") +
  scale_x_continuous(breaks = scales::pretty_breaks(8)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Australian patent applications with a US applicant",
    subtitle = paste0("Standard complete applications, by earliest filing date"),
    x = NULL, y = "Applications",
    caption  = "Source: IP Australia, IP RAPID"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

p_share <- ggplot(annual_us, aes(filing_year, share_us)) +
  geom_line(linewidth = 0.8, colour = "#a33") +
  geom_point(size = 1.4, colour = "#a33") +
  scale_x_continuous(breaks = scales::pretty_breaks(8)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0.3, NA)) +
  labs(title = "US share of Australian patent applications",
       subtitle = "Share of applications with a known applicant party",
       x = NULL, y = NULL,
       caption = "Source: IP Australia, IP RAPID") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

print(p_count)

ggsave("us_applicants_count.png", p_count, width = 9, height = 5, dpi = 300)
ggsave("us_applicants_share.png", p_share, width = 9, height = 5, dpi = 300)
export_chart_csv(p_count, "us_applicants_count.csv", y_format = "number")
export_chart_csv(p_share, "us_applicants_share.csv", y_format = "percent",
                 notes = "Denominator is applications with a known applicant party")

# ==============================================================================
# 8. TRADE MARKS — applications joined to party activity and application links
#
# Paste after the patent script. Reuses from section 0: app_file, party_file,
# links_file, join_keys, party_cols, links_cols, out_dir, read_iprapid(),
# parse_iprapid_date(), `%||%`, export_chart_csv().
# All objects here are prefixed tm_ so the patent objects are left untouched.
#
# Trade-mark specifics from the IP RAPID Data Dictionary (2023-08-01):
#   * ip_right_type == "trade_mark". Sub-types (Table 1): trade_mark,
#     collective_trade_mark, certification_trade_mark, defensive_trade_mark.
#   * party_role_category "applicant" covers party_role applicant AND owner
#     (Table 11): applicants become owners once the mark is registered.
#   * International routes are in application-links (Table 5): madrid_import
#     (an international registration designating Australia), madrid_export,
#     ir_full_transform, partial_transformation, and convention_priority
#     (Paris priority from an earlier filing abroad).
#   * application_date for a Madrid import is the date the IR sought
#     protection in Australia; for direct filings it is the filing date.
#     It is IP Australia's own basis for published filing counts.
# ==============================================================================

library(data.table)
library(ggplot2)

# ------------------------------------------------------------------------------
# 8.0 Settings
# ------------------------------------------------------------------------------

# Sub-types to keep. NULL = keep all.
#   Defensive marks exist to block others rather than for trade; collective and
#   certification marks are niche. "trade_mark" alone is the usual choice.
tm_sub_types <- "trade_mark"

# Statuses to drop (Table 2). NULL = keep all.
#   not_filed: fees unpaid / deficient, never a valid application
#   voided:    record created in error
tm_drop_status <- c("not_filed", "voided")

# Year basis:
#   "application_date"    — matches IP Australia's published filing counts
#   "earliest_filed_date" — for Madrid imports, the international filing
#   "priority_date"       — earliest convention priority claimed
tm_date_basis <- "application_date"

# TRUE = count only the earliest applicant on record (is_founding), i.e. who
# originally filed, excluding later assignees/owners.
tm_founding_only <- FALSE

# Trade marks have a much shorter reporting lag than patents (no 30-month PCT
# route), so only the current, incomplete year is dropped.
tm_year_min <- 1990L
tm_year_max <- year(Sys.Date()) - 1L

to_bool <- function(x) {
  if (is.logical(x)) return(x %in% TRUE)
  tolower(trimws(as.character(x))) %chin% c("true", "t", "1", "yes", "y")
}

# ------------------------------------------------------------------------------
# 8.1 Applications -> trade marks
# ------------------------------------------------------------------------------

tm_apps <- read_iprapid(app_file)
tm_apps[, ip_right_type := tolower(trimws(ip_right_type))]
trade_marks <- tm_apps[ip_right_type == "trade_mark"]
rm(tm_apps); invisible(gc())

trade_marks[, ip_right_sub_type := tolower(trimws(ip_right_sub_type))]
trade_marks[, status := tolower(trimws(status))]

cat("trade marks: ", format(nrow(trade_marks), big.mark = ","), "\n", sep = "")
print(trade_marks[, .N, by = ip_right_sub_type][order(-N)])
print(trade_marks[, .N, by = status][order(-N)])

if (!is.null(tm_sub_types)) {
  trade_marks <- trade_marks[ip_right_sub_type %chin% tm_sub_types]
}
if (!is.null(tm_drop_status)) {
  trade_marks <- trade_marks[!status %chin% tm_drop_status]
}
cat("after sub-type/status filters: ",
    format(nrow(trade_marks), big.mark = ","), " rows\n", sep = "")

trade_marks[, application_number := trimws(as.character(application_number))]
tm_ids <- trade_marks$application_number

# ------------------------------------------------------------------------------
# 8.2 Filing year
# ------------------------------------------------------------------------------

stopifnot(tm_date_basis %in% names(trade_marks))

trade_marks[, (tm_date_basis) := parse_iprapid_date(get(tm_date_basis))]
trade_marks[, filing_year := year(get(tm_date_basis))]
trade_marks[filing_year < 1900 | filing_year > year(Sys.Date()) + 1,
            filing_year := NA_integer_]

cat("\nmissing/implausible ", tm_date_basis, ": ",
    trade_marks[is.na(filing_year), .N], "\n", sep = "")
print(tail(trade_marks[!is.na(filing_year), .N, by = filing_year][order(filing_year)], 10))

# ------------------------------------------------------------------------------
# 8.3 Party activity
# ------------------------------------------------------------------------------

tm_party <- read_iprapid(party_file, select = party_cols)
stopifnot(all(join_keys %in% names(tm_party)))

tm_party[, ip_right_type := tolower(trimws(ip_right_type))]
tm_party <- tm_party[ip_right_type == "trade_mark"]
tm_party[, application_number := trimws(as.character(application_number))]
tm_party <- tm_party[application_number %chin% tm_ids]

tm_party[, party_role          := tolower(trimws(party_role))]
tm_party[, party_role_category := tolower(trimws(party_role_category))]
tm_party[, country_code        := tolower(trimws(country_code))]
if ("is_founding" %in% names(tm_party)) tm_party[, is_founding := to_bool(is_founding)]
if ("is_current"  %in% names(tm_party)) tm_party[, is_current  := to_bool(is_current)]

print(tm_party[, .N, by = .(party_role_category, party_role)][order(-N)])

# Applicant flag used for the US test (Table 11: applicant + owner).
if (tm_founding_only) {
  stopifnot("is_founding" %in% names(tm_party))
  tm_party[, is_applicant := party_role_category == "applicant" & is_founding]
} else {
  tm_party[, is_applicant := party_role_category == "applicant"]
}

cat("party_activity (trade marks): ", format(nrow(tm_party), big.mark = ","),
    " rows\n", sep = "")

# ------------------------------------------------------------------------------
# 8.4 Application links -> one row per trade mark
# ------------------------------------------------------------------------------

tm_links <- read_iprapid(links_file, select = links_cols)
stopifnot(all(join_keys %in% names(tm_links)))

tm_links[, ip_right_type := tolower(trimws(ip_right_type))]
tm_links <- tm_links[ip_right_type == "trade_mark"]
tm_links[, application_number := trimws(as.character(application_number))]
tm_links <- tm_links[application_number %chin% tm_ids]
tm_links[, link_type := tolower(trimws(link_type))]
tm_links[, linked_application_country := tolower(trimws(linked_application_country))]

print(tm_links[, .N, by = .(link_type, linked_application_country)][order(-N)][1:20])

# Route flags are taken from link_type, not country: a Madrid import's linked
# number is an international registration, whose country field may be a WIPO
# office code rather than the holder's home country.
tm_links_agg <- tm_links[, .(
  is_madrid_import = as.integer(any(link_type == "madrid_import")),
  is_madrid_export = as.integer(any(link_type == "madrid_export")),
  has_convention   = as.integer(any(link_type == "convention_priority")),
  convention_countries = paste(sort(unique(
    linked_application_country[link_type == "convention_priority" &
                                 !is.na(linked_application_country)])),
    collapse = "|")
), by = join_keys]
tm_links_agg[, conv_us := as.integer(grepl("(^|\\|)us($|\\|)", convention_countries))]

# ------------------------------------------------------------------------------
# 8.5 Joins — composite key
# ------------------------------------------------------------------------------

tm_full <- merge(trade_marks, tm_party, by = join_keys, all.x = TRUE,
                 allow.cartesian = TRUE, suffixes = c("_app", "_party"))
tm_full <- merge(tm_full, tm_links_agg, by = join_keys, all.x = TRUE)

for (v in c("is_madrid_import", "is_madrid_export", "has_convention", "conv_us")) {
  set(tm_full, which(is.na(tm_full[[v]])), v, 0L)
}

n_tm <- uniqueN(tm_ids)
cat("\n--- trade mark join summary ---\n")
cat("unique trade marks:     ", format(n_tm, big.mark = ","), "\n")
cat("with >=1 party record:  ", format(uniqueN(tm_party$application_number), big.mark = ","), "\n")
cat("with >=1 link:          ", format(uniqueN(tm_links$application_number), big.mark = ","), "\n")
cat("Madrid imports:         ", format(sum(tm_links_agg$is_madrid_import), big.mark = ","), "\n")
cat("rows after joins:       ", format(nrow(tm_full), big.mark = ","), "\n")

rm(tm_links); invisible(gc())

saveRDS(tm_full, file.path(out_dir, "trade_marks_parties_links.rds"))

# ------------------------------------------------------------------------------
# 8.6 Australian trade mark applications with at least one US applicant
# ------------------------------------------------------------------------------

stopifnot(all(c("is_applicant", "country_code") %in% names(tm_full)))

tm_flags <- tm_full[
  !is.na(filing_year),
  .(us_applicant  = any(is_applicant & country_code == "us", na.rm = TRUE),
    any_applicant = any(is_applicant, na.rm = TRUE),
    madrid_import = is_madrid_import[1] == 1L),
  by = .(application_number, filing_year)
]

tm_annual_us <- tm_flags[
  filing_year %between% c(tm_year_min, tm_year_max),
  .(n_us        = sum(us_applicant),
    n_us_madrid = sum(us_applicant & madrid_import),
    n_total     = .N,
    n_known     = sum(any_applicant)),
  by = filing_year
][order(filing_year)]

tm_annual_us[, share_us := n_us / n_known]            # excludes marks with no applicant record
tm_annual_us[, share_us_via_madrid := n_us_madrid / n_us]

cat("\n--- US-applicant trade marks ---\n")
print(tail(tm_annual_us, 15))
fwrite(tm_annual_us, file.path(out_dir, "annual_us_trade_marks_full.csv"))

tm_subtitle_basis <- gsub("_", " ", tm_date_basis)

p_tm_count <- ggplot(tm_annual_us, aes(filing_year, n_us)) +
  geom_line(linewidth = 0.8, colour = "#1f4e79") +
  geom_point(size = 1.4, colour = "#1f4e79") +
  scale_x_continuous(breaks = scales::pretty_breaks(8)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Australian trade mark applications with a US applicant",
    subtitle = paste0("Standard trade mark applications, by ", tm_subtitle_basis),
    x = NULL, y = "Applications",
    caption  = "Source: IP Australia, IP RAPID"
  ) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

p_tm_share <- ggplot(tm_annual_us, aes(filing_year, share_us)) +
  geom_line(linewidth = 0.8, colour = "#a33") +
  geom_point(size = 1.4, colour = "#a33") +
  scale_x_continuous(breaks = scales::pretty_breaks(8)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, NA)) +
  labs(title = "US share of Australian trade mark applications",
       subtitle = "Share of applications with a known applicant party",
       x = NULL, y = NULL,
       caption = "Source: IP Australia, IP RAPID") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

print(p_tm_count)
print(p_tm_share)

ggsave("us_applicants_tm_count.png", p_tm_count, width = 9, height = 5, dpi = 300)
ggsave("us_applicants_tm_share.png", p_tm_share, width = 9, height = 5, dpi = 300)
export_chart_csv(p_tm_count, "us_applicants_tm_count.csv", y_format = "number")
export_chart_csv(p_tm_share, "us_applicants_tm_share.csv", y_format = "percent",
                 notes = "Denominator is applications with a known applicant party")


# ==============================================================================
# 9. PATENTS — technology fields and US–Australian co-applicants
#
# Run after the patent script (sections 0–7). Uses: data_dir, out_dir,
# join_keys, patents, patents_full, patent_ids, year_min, year_max,
# read_iprapid(), `%||%`.
#
# Dictionary notes (Table 8, application-classification):
#   * classification_area   = WIPO technology concordance, 35 fields (patents)
#   * coarse_classification_area = 5 WIPO sectors
#   * is_current            = keep only current classifications
#   * classification_importance = primary / secondary (patents only)
# IP RAPID has no industry code for parties, so "industry" here means the
# technology field of the co-applied patents. The ABN column in the partner
# tables is the hook for linking to industry data elsewhere.
# ==============================================================================

library(data.table)
library(ggplot2)

stopifnot(exists("patents_full"), exists("patents"), exists("year_max"))

# ------------------------------------------------------------------------------
# 9.0 Settings and helpers
# ------------------------------------------------------------------------------

class_file <- file.path(data_dir, "application_classification.csv")

# How to assign a patent to fields:
#   "fractional" — split each patent equally across all its current fields
#                  (standard WIPO/OECD practice; totals still add up)
#   "primary"    — only fields from primary classifications
field_method <- "fractional"

recent_years <- (year_max - 4L):year_max    # pooled window for field/state charts
early_years  <- recent_years - 10L          # comparison window, ten years earlier
window_lab   <- paste0(min(recent_years), "\u2013", max(recent_years))
min_field_n  <- 30                          # min applications in a field to chart its share

if (!exists("to_bool")) {
  to_bool <- function(x) {
    if (is.logical(x)) return(x %in% TRUE)
    tolower(trimws(as.character(x))) %chin% c("true", "t", "1", "yes", "y")
  }
}

pretty_label <- function(x) {
  x <- gsub("_", " ", trimws(as.character(x)))
  sub("^(.)", "\\U\\1", x, perl = TRUE)
}

theme_chart <- function(horizontal_bars = FALSE) {
  t <- theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          plot.title = element_text(face = "bold"),
          plot.title.position = "plot")
  if (horizontal_bars) t <- t + theme(panel.grid.major.y = element_blank())
  t
}

# Australian state from state_code, falling back to a 4-digit postcode.
clean_au_state <- function(state_code, postcode) {
  s <- toupper(trimws(as.character(state_code)))
  s[!s %chin% c("NSW", "VIC", "QLD", "WA", "SA", "TAS", "ACT", "NT")] <- NA_character_
  pc_chr <- gsub("\\D", "", as.character(postcode))
  pc <- suppressWarnings(as.integer(ifelse(nchar(pc_chr) == 4, pc_chr, NA)))
  from_pc <- fcase(
    pc %between% c(200, 299) | pc %between% c(2600, 2619) | pc %between% c(2900, 2920), "ACT",
    pc %between% c(800, 999),   "NT",
    pc %between% c(1000, 2999), "NSW",
    pc %between% c(3000, 3999) | pc %between% c(8000, 8999), "VIC",
    pc %between% c(4000, 4999) | pc %between% c(9000, 9999), "QLD",
    pc %between% c(5000, 5999), "SA",
    pc %between% c(6000, 6999), "WA",
    pc %between% c(7000, 7999), "TAS",
    default = NA_character_
  )
  fifelse(is.na(s), from_pc, s)
}

# ------------------------------------------------------------------------------
# 9.0b Generalised chart CSV export
# Replaces the earlier export_chart_csv(); existing calls still work.
# Handles line charts, horizontal/vertical bar charts, and multi-series charts
# (written wide: one column per series). Reference lines are recorded too.
# ------------------------------------------------------------------------------

export_chart_csv <- function(p, file,
                             y_format = c("number", "percent"),
                             chart_type = "Line with point markers",
                             series_colours = NULL,
                             digits = NULL,
                             notes = NULL) {
  y_format <- match.arg(y_format)
  pct <- y_format == "percent"
  
  map_var <- function(a) if (is.null(p$mapping[[a]])) NULL else rlang::as_label(p$mapping[[a]])
  xvar <- map_var("x"); yvar <- map_var("y")
  series_var <- map_var("colour") %||% map_var("fill")
  
  dat <- as.data.table(p$data)
  value_axis <- if (is.numeric(dat[[xvar]]) && !is.numeric(dat[[yvar]])) "x" else "y"
  vvar <- if (value_axis == "y") yvar else xvar
  cvar <- if (value_axis == "y") xvar else yvar
  dat  <- dat[, unique(c(cvar, series_var, vvar)), with = FALSE]
  
  if (pct) set(dat, j = vvar, value = dat[[vvar]] * 100)
  if (pct || !is.null(digits)) set(dat, j = vvar, value = round(dat[[vvar]], digits %||% 1))
  
  if (is.numeric(dat[[cvar]])) {
    setorderv(dat, c(series_var, cvar))
  } else if (is.null(series_var)) {
    setorderv(dat, vvar, order = -1L)          # bar charts: largest first
    set(dat, j = cvar, value = as.character(dat[[cvar]]))
  }
  if (!is.null(series_var)) {
    dat <- dcast(dat, as.formula(paste0("`", cvar, "` ~ `", series_var, "`")),
                 value.var = vvar)
  }
  
  sc   <- p$scales$get_scales(value_axis)
  lims <- if (!is.null(sc) && is.numeric(sc$limits)) sc$limits else c(NA, NA)
  if (pct) lims <- lims * 100
  fmt_lim <- function(v) if (is.null(v) || is.na(v)) "auto" else as.character(v)
  
  layer_desc <- vapply(p$layers, function(l) {
    prm <- l$aes_params
    ld  <- l$data %||% list()
    ref <- intersect(names(ld), c("xintercept", "yintercept"))
    if (length(ref)) {
      v <- ld[[ref[1]]]
      prm[["reference_value"]] <- if (pct) round(v * 100, 1) else v
    }
    body <- if (length(prm)) {
      paste(names(prm), vapply(prm, function(z) paste(as.character(z), collapse = "/"),
                               character(1)), sep = "=", collapse = ", ")
    } else "default"
    paste0(sub("^Geom", "", class(l$geom)[1]), " (", body, ")")
  }, character(1))
  
  lab <- function(nm) {
    v <- p$labels[[nm]]
    if (is.null(v) || identical(v, "")) "(none)" else as.character(v)
  }
  cat_axis <- if (value_axis == "y") "x" else "y"
  
  meta <- data.table(
    field = c("title", "subtitle", "caption", "chart_type",
              "category_variable", "category_axis", "category_axis_label",
              "value_variable", "value_axis", "value_axis_label", "value_units",
              "value_axis_min", "value_axis_max",
              "series_variable", "series_colours",
              "layers", "title_style", "notes"),
    value = c(lab("title"), lab("subtitle"), lab("caption"), chart_type,
              cvar, if (cat_axis == "x") "horizontal" else "vertical", lab(cat_axis),
              vvar, if (value_axis == "x") "horizontal" else "vertical", lab(value_axis),
              if (pct) "Percent (values are 0-100)" else "Number",
              fmt_lim(lims[1]), fmt_lim(lims[2]),
              series_var %||% "(none)",
              if (is.null(series_colours)) "(none)" else
                paste(names(series_colours), series_colours, sep = "=", collapse = "; "),
              paste(layer_desc, collapse = " | "),
              "Bold", notes %||% "")
  )
  
  fwrite(meta, file, col.names = FALSE, bom = TRUE)
  cat("\n", file = file, append = TRUE)
  fwrite(dat, file, append = TRUE, col.names = TRUE)
  invisible(file)
}

# ------------------------------------------------------------------------------
# 9.1 Classification -> field weights per patent
# ------------------------------------------------------------------------------

wanted <- c(join_keys, "is_current", "classification_importance",
            "classification_area", "coarse_classification_area")
hdr <- names(fread(class_file, nrows = 0))
pat_class <- read_iprapid(class_file, select = hdr[tolower(trimws(hdr)) %in% wanted])

pat_class[, ip_right_type := tolower(trimws(ip_right_type))]
pat_class <- pat_class[ip_right_type == "patent"]
pat_class[, application_number := trimws(as.character(application_number))]
pat_class <- pat_class[application_number %chin% patent_ids & to_bool(is_current)]
pat_class <- pat_class[!is.na(classification_area) & trimws(classification_area) != ""]
pat_class[, classification_importance := tolower(trimws(classification_importance))]

print(pat_class[, .N, by = classification_importance][order(-N)])

if (field_method == "primary") {
  pat_class <- pat_class[classification_importance %chin% c("primary", "first")]
}

field_w <- unique(
  pat_class[, .(application_number,
                field  = pretty_label(classification_area),
                sector = pretty_label(coarse_classification_area))],
  by = c("application_number", "field")
)
field_w[, w := 1 / .N, by = application_number]

cat("patents with a current field: ",
    format(uniqueN(field_w$application_number), big.mark = ","), " of ",
    format(length(patent_ids), big.mark = ","), "\n", sep = "")
print(field_w[, .(n = round(sum(w))), by = sector][order(-n)])

rm(pat_class); invisible(gc())

# ------------------------------------------------------------------------------
# 9.2 Applicant parties -> US / AU flags
# ------------------------------------------------------------------------------

party_vars <- intersect(c("application_number", "filing_year", "party_id", "party_name",
                          "party_type", "abn", "country_code", "state_code", "postcode"),
                        names(patents_full))

pat_party <- patents_full[party_role_category %chin% "applicant", ..party_vars]
pat_party[, pkey := fifelse(is.na(party_id), paste0("n:", party_name), as.character(party_id))]
pat_party <- unique(pat_party, by = c("application_number", "pkey"))

pat_party[, state := NA_character_]
pat_party[country_code %chin% "au" | is.na(country_code),
          state := clean_au_state(state_code, if ("postcode" %in% names(.SD)) postcode else NA)]
pat_party[, is_us := country_code %chin% "us"]
pat_party[, is_au := country_code %chin% "au" | (is.na(country_code) & !is.na(state))]

pat_level <- unique(patents[!is.na(filing_year), .(application_number, filing_year)])
pat_level <- merge(pat_level,
                   pat_party[, .(us = any(is_us), au = any(is_au)), by = application_number],
                   by = "application_number", all.x = TRUE)
pat_level[, known := !is.na(us)]
pat_level[is.na(us), `:=`(us = FALSE, au = FALSE)]
pat_level[, us_au := us & au]
pat_level <- pat_level[filing_year %between% c(year_min, year_max)]

cat("\nUS-AU co-applied patents, all years: ", pat_level[us_au == TRUE, .N], "\n", sep = "")

# ------------------------------------------------------------------------------
# 9.3 Technology fields
# ------------------------------------------------------------------------------

field_table <- function(years) {
  fw <- merge(field_w, pat_level[filing_year %in% years & known == TRUE],
              by = "application_number")
  fw[, .(n_all = sum(w), n_us = sum(w * us), n_us_au = sum(w * us_au)),
     by = .(field, sector)]
}

ft <- field_table(recent_years)
tot_share_us <- ft[, sum(n_us) / sum(n_all)]

ft[, `:=`(share_us     = n_us / n_all,
          rta_us       = (n_us / n_all) / tot_share_us,   # >1 = US over-represented
          pct_of_us    = n_us / sum(n_us),
          pct_of_us_au = n_us_au / sum(n_us_au))]
ft[, collab_specialisation := pct_of_us_au / pct_of_us]   # >1 = co-application concentrated here

ft_early <- field_table(early_years)
ft <- merge(ft, ft_early[, .(field, n_all_early = n_all, share_us_early = n_us / n_all)],
            by = "field", all.x = TRUE)
ft[, change_share_pp := round((share_us - share_us_early) * 100, 1)]
setorder(ft, -share_us)

print(ft[, .(field, n_all = round(n_all), share_us = round(share_us, 3),
             rta_us = round(rta_us, 2), change_share_pp, n_us_au = round(n_us_au, 1))])
fwrite(ft, file.path(out_dir, "tech_fields_us_share_full.csv"))

# Chart 1: US share by field (recent window)
plot_fs <- ft[n_all >= min_field_n][order(share_us)]
plot_fs[, field := factor(field, levels = field)]

p_field_share <- ggplot(plot_fs, aes(share_us, field)) +
  geom_col(fill = "#1f4e79", width = 0.7) +
  geom_vline(xintercept = tot_share_us, linetype = "dashed", colour = "grey40") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title    = "US share of Australian patent applications by technology field",
       subtitle = paste0("Standard complete applications, ", window_lab,
                         ". Dashed line = US share across all fields"),
       x = NULL, y = NULL,
       caption  = "Source: IP Australia, IP RAPID. Fields: WIPO technology concordance.") +
  theme_chart(horizontal_bars = TRUE)

# Chart 2: US applications by sector over time (fractional counts)
sector_trend <- merge(field_w, pat_level[known == TRUE & us == TRUE],
                      by = "application_number")[
                        , .(n_us = round(sum(w), 1)), by = .(filing_year, sector)][order(sector, filing_year)]

sectors <- sort(unique(sector_trend$sector))
sector_pal <- setNames(rep_len(c("#1f4e79", "#a33", "#2a9d8f", "#e9a23b", "#6c757d"),
                               length(sectors)), sectors)

p_sector_trend <- ggplot(sector_trend, aes(filing_year, n_us, colour = sector)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.2) +
  scale_colour_manual(values = sector_pal) +
  scale_x_continuous(breaks = scales::pretty_breaks(8)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title    = "Australian patent applications with a US applicant, by technology sector",
       subtitle = "Standard complete applications; each application split across its sectors",
       x = NULL, y = "Applications", colour = NULL,
       caption  = "Source: IP Australia, IP RAPID. Sectors: WIPO technology concordance.") +
  theme_chart() +
  theme(legend.position = "top")

# ------------------------------------------------------------------------------
# 9.4 US–Australian co-applicants
# ------------------------------------------------------------------------------

collab_annual <- pat_level[known == TRUE,
                           .(n_us_au = sum(us_au), n_us = sum(us), n_au = sum(au)),
                           by = filing_year][order(filing_year)]
collab_annual[, share_of_us_with_au := n_us_au / n_us]
fwrite(collab_annual, file.path(out_dir, "us_au_coapplied_annual_full.csv"))

# Chart 3: co-applied patents per year
p_collab_trend <- ggplot(collab_annual, aes(filing_year, n_us_au)) +
  geom_line(linewidth = 0.8, colour = "#2a9d8f") +
  geom_point(size = 1.4, colour = "#2a9d8f") +
  scale_x_continuous(breaks = scales::pretty_breaks(8)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(title    = "Patent applications with both US and Australian applicants",
       subtitle = "Standard complete applications filed in Australia",
       x = NULL, y = "Applications",
       caption  = "Source: IP Australia, IP RAPID") +
  theme_chart()

# Chart 4: co-applied patents by technology field (recent window, top 15)
plot_cf <- head(ft[n_us_au > 0][order(-n_us_au)], 15)[order(n_us_au)]
plot_cf[, `:=`(field = factor(field, levels = field), n_us_au = round(n_us_au, 1))]

p_collab_fields <- ggplot(plot_cf, aes(n_us_au, field)) +
  geom_col(fill = "#2a9d8f", width = 0.7) +
  scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.05))) +
  labs(title    = "Where US and Australian applicants file together",
       subtitle = paste0("Co-applied patent applications by technology field, ", window_lab),
       x = "Applications", y = NULL,
       caption  = "Source: IP Australia, IP RAPID. Applications split across fields, so counts can be fractional.") +
  theme_chart(horizontal_bars = TRUE)

# Chart 5: Australian partner state (recent window)
recent_known <- pat_level[filing_year %in% recent_years & known == TRUE, application_number]
recent_us_au <- pat_level[filing_year %in% recent_years & us_au == TRUE, application_number]

state_tab <- merge(
  pat_party[is_au & application_number %chin% recent_us_au,
            .(n_us_au = uniqueN(application_number)), by = .(state = fcoalesce(state, "Unknown"))],
  pat_party[is_au & application_number %chin% recent_known,
            .(n_au_all = uniqueN(application_number)), by = .(state = fcoalesce(state, "Unknown"))],
  by = "state", all = TRUE
)
state_tab[is.na(n_us_au), n_us_au := 0L]
state_tab[, share_with_us_partner := n_us_au / n_au_all]   # collaboration intensity
setorder(state_tab, -n_us_au)
print(state_tab)
fwrite(state_tab, file.path(out_dir, "us_au_partner_states_full.csv"))

plot_st <- state_tab[state != "Unknown"][order(n_us_au)]
plot_st[, state := factor(state, levels = state)]

p_collab_states <- ggplot(plot_st, aes(n_us_au, state)) +
  geom_col(fill = "#2a9d8f", width = 0.7) +
  scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.05))) +
  labs(title    = "Location of Australian co-applicants on US–Australian patents",
       subtitle = paste0("Patent applications by state of the Australian applicant, ", window_lab),
       x = "Applications", y = NULL,
       caption  = "Source: IP Australia, IP RAPID. An application with partners in two states counts in both.") +
  theme_chart(horizontal_bars = TRUE)

# Partner tables (recent window) --------------------------------------------

first_word <- function(x) {
  w <- sub("^THE\\s+", "", toupper(trimws(x)))
  sub("[^A-Z0-9&].*$", "", w)
}
generic_words <- c("UNIVERSITY", "NATIONAL", "INTERNATIONAL", "AUSTRALIAN", "AMERICAN",
                   "GLOBAL", "COMMONWEALTH", "UNITED", "GENERAL", "ADVANCED", "NEW")

has_type <- "party_type" %in% names(pat_party)

us_side <- pat_party[is_us & application_number %chin% recent_us_au,
                     .(application_number, us_name = party_name,
                       us_type = if (has_type) party_type else NA_character_)]
au_side <- pat_party[is_au & application_number %chin% recent_us_au,
                     .(application_number, au_name = party_name,
                       au_type = if (has_type) party_type else NA_character_,
                       au_abn = abn, au_state = state)]

pairs <- merge(us_side, au_side, by = "application_number", allow.cartesian = TRUE)
pair_tab <- pairs[, .(n_applications = uniqueN(application_number),
                      au_abn = au_abn[!is.na(au_abn)][1], au_state = au_state[!is.na(au_state)][1]),
                  by = .(us_name, us_type, au_name, au_type)][order(-n_applications)]
pair_tab[, possible_same_group := first_word(us_name) == first_word(au_name) &
           !first_word(us_name) %chin% generic_words &
           nchar(first_word(us_name)) >= 3]
fwrite(pair_tab, file.path(out_dir, "us_au_partner_pairs.csv"))

au_partners <- au_side[, .(n_applications = uniqueN(application_number),
                           au_abn = au_abn[!is.na(au_abn)][1],
                           au_state = au_state[!is.na(au_state)][1]),
                       by = .(au_name, au_type)][order(-n_applications)]
fwrite(au_partners, file.path(out_dir, "us_au_australian_partners.csv"))

cat("\nTop US-AU pairs,", window_lab, "\n")
print(head(pair_tab, 20))
cat("\nShare of pairs flagged as possible same corporate group: ",
    round(pair_tab[, weighted.mean(possible_same_group, n_applications)], 3), "\n", sep = "")

# ------------------------------------------------------------------------------
# 9.5 Save charts + designer CSVs
# ------------------------------------------------------------------------------

print(p_field_share); print(p_sector_trend); print(p_collab_trend)
print(p_collab_fields); print(p_collab_states)

ggsave("us_share_by_field.png",       p_field_share,   width = 9, height = 8, dpi = 300)
ggsave("us_by_sector_trend.png",      p_sector_trend,  width = 9, height = 5, dpi = 300)
ggsave("us_au_coapplied_trend.png",   p_collab_trend,  width = 9, height = 5, dpi = 300)
ggsave("us_au_coapplied_fields.png",  p_collab_fields, width = 9, height = 6, dpi = 300)
ggsave("us_au_coapplied_states.png",  p_collab_states, width = 9, height = 5, dpi = 300)

export_chart_csv(p_field_share, "us_share_by_field.csv", y_format = "percent",
                 chart_type = "Horizontal bar with reference line",
                 notes = paste0("Fields with fewer than ", min_field_n,
                                " applications in the window are omitted. Reference line = US share across all fields."))
export_chart_csv(p_sector_trend, "us_by_sector_trend.csv", y_format = "number",
                 chart_type = "Multi-line with point markers",
                 series_colours = sector_pal,
                 notes = "Fractional counts: each application split equally across its sectors.")
export_chart_csv(p_collab_trend, "us_au_coapplied_trend.csv", y_format = "number",
                 notes = "Applications with at least one US and one Australian applicant/patentee.")
export_chart_csv(p_collab_fields, "us_au_coapplied_fields.csv", y_format = "number",
                 chart_type = "Horizontal bar", digits = 1,
                 notes = "Top 15 fields. Fractional counts.")
export_chart_csv(p_collab_states, "us_au_coapplied_states.csv", y_format = "number",
                 chart_type = "Horizontal bar",
                 notes = "State of the Australian co-applicant. Applications with partners in two states count in both.")

# ==============================================================================
# 10. TRADE MARKS — Nice classes and US–Australian co-applicants
#
# Run after the trade mark block (section 8) and section 9.0 (helpers).
# Uses from section 8: trade_marks, tm_full, tm_ids, tm_year_min, tm_year_max,
#   tm_date_basis (applicant flag is_applicant respects tm_founding_only).
# Uses from section 9.0: to_bool(), pretty_label(), theme_chart(),
#   clean_au_state(), export_chart_csv(), first_word(), generic_words.
#   (first_word / generic_words are defined in 9.4 — run that too, or copy them.)
#
# Dictionary notes (Table 8, application-classification) for trade marks:
#   * classification_system = Nice; classification_area replicates the Nice class
#   * coarse_classification_area = whether the whole mark covers goods only,
#     services only, or both (a mark-level attribute repeated on each row)
#   * classification_importance is not populated for trade marks, so there is
#     no "primary" class: a multi-class mark is split fractionally or counted
#     once per class (tm_class_method below).
# ==============================================================================

library(data.table)
library(ggplot2)

stopifnot(exists("tm_full"), exists("trade_marks"), exists("tm_year_max"),
          exists("export_chart_csv"), exists("clean_au_state"), exists("first_word"))

# ------------------------------------------------------------------------------
# 10.0 Settings
# ------------------------------------------------------------------------------

class_file <- file.path(data_dir, "application_classification.csv")

# "fractional" — each mark split equally across its classes (totals = marks)
# "full"       — each mark counted once in every class it covers (class counts,
#                closer to how IP Australia sometimes reports class activity)
tm_class_method <- "fractional"

tm_recent_years <- (tm_year_max - 4L):tm_year_max
tm_early_years  <- tm_recent_years - 10L
tm_window_lab   <- paste0(min(tm_recent_years), "\u2013", max(tm_recent_years))
tm_min_class_n  <- 100

# Short Nice class labels (paraphrased headings, Nice 12th edition numbering)
nice_labels <- c(
  "Chemicals", "Paints & coatings", "Cosmetics & cleaning", "Industrial oils & fuels",
  "Pharmaceuticals", "Metal goods", "Machinery", "Hand tools",
  "Computers, software & electronics", "Medical devices", "Lighting, heating & appliances",
  "Vehicles", "Firearms & explosives", "Jewellery & watches", "Musical instruments",
  "Paper & printed matter", "Rubber & plastics", "Leather goods & bags",
  "Non-metal building materials", "Furniture", "Housewares", "Ropes & raw fibres",
  "Yarns & threads", "Textiles", "Clothing & footwear", "Haberdashery", "Floor coverings",
  "Toys & sporting goods", "Meat, dairy & processed food", "Staple foods & confectionery",
  "Agricultural produce", "Beer & soft drinks", "Alcoholic beverages", "Tobacco",
  "Advertising & business services", "Financial & insurance services",
  "Construction & repair", "Telecommunications", "Transport & storage",
  "Treatment of materials", "Education & entertainment",
  "Scientific & technology services", "Food & accommodation services",
  "Medical & beauty services", "Legal & security services"
)

# Broad groups for the trend chart. This grouping is our own, not an official one.
nice_groups <- list(
  "Technology & telecoms"          = c(9, 38, 42),
  "Health, pharma & beauty"        = c(3, 5, 10, 44),
  "Chemicals & materials"          = c(1, 2, 4, 6, 17, 19),
  "Machinery, vehicles & tools"    = c(7, 8, 11, 12, 13),
  "Fashion & household"            = c(14, 18, 20:27),
  "Leisure, media & education"     = c(15, 16, 28, 41),
  "Food, drink & hospitality"      = c(29:34, 43),
  "Business & financial services"  = c(35, 36, 37, 39, 40, 45)
)
nice_group_lookup <- data.table(
  class_no = unlist(nice_groups),
  class_group = rep(names(nice_groups), lengths(nice_groups))
)
stopifnot(setequal(nice_group_lookup$class_no, 1:45))

# ------------------------------------------------------------------------------
# 10.1 Classification -> class weights per trade mark
# ------------------------------------------------------------------------------

wanted <- c(join_keys, "is_current", "classification_system",
            "classification_area", "coarse_classification_area")
hdr <- names(fread(class_file, nrows = 0))
tm_class <- read_iprapid(class_file, select = hdr[tolower(trimws(hdr)) %in% wanted])

tm_class[, ip_right_type := tolower(trimws(ip_right_type))]
tm_class <- tm_class[ip_right_type == "trade_mark"]
tm_class[, application_number := trimws(as.character(application_number))]
tm_class <- tm_class[application_number %chin% tm_ids & to_bool(is_current)]

print(tm_class[, .N, by = classification_system][order(-N)])

tm_class[, class_no := suppressWarnings(as.integer(gsub("\\D", "", classification_area)))]
tm_class <- tm_class[class_no %between% c(1L, 45L)]

# Mark-level goods/services/both
tm_gs <- tm_class[!is.na(coarse_classification_area),
                  .(goods_services = pretty_label(coarse_classification_area[1])),
                  by = application_number]

tm_class_w <- unique(tm_class[, .(application_number, class_no)])
tm_class_w[, w := if (tm_class_method == "fractional") 1 / .N else 1, by = application_number]
tm_class_w <- merge(tm_class_w, nice_group_lookup, by = "class_no")
tm_class_w[, class_label := sprintf("Class %d: %s", class_no, nice_labels[class_no])]

cat("trade marks with a current Nice class: ",
    format(uniqueN(tm_class_w$application_number), big.mark = ","), " of ",
    format(length(tm_ids), big.mark = ","), "\n", sep = "")
cat("mean classes per mark: ",
    round(nrow(tm_class_w) / uniqueN(tm_class_w$application_number), 2), "\n", sep = "")

rm(tm_class); invisible(gc())

# ------------------------------------------------------------------------------
# 10.2 Applicant parties -> US / AU flags
# ------------------------------------------------------------------------------

tm_party_vars <- intersect(c("application_number", "party_id", "party_name", "party_type",
                             "abn", "country_code", "state_code", "postcode"),
                           names(tm_full))

tm_pp <- tm_full[is_applicant %in% TRUE, ..tm_party_vars]
tm_pp[, pkey := fifelse(is.na(party_id), paste0("n:", party_name), as.character(party_id))]
tm_pp <- unique(tm_pp, by = c("application_number", "pkey"))

tm_pp[, state := NA_character_]
tm_pp[country_code %chin% "au" | is.na(country_code),
      state := clean_au_state(state_code, if ("postcode" %in% names(.SD)) postcode else NA)]
tm_pp[, is_us := country_code %chin% "us"]
tm_pp[, is_au := country_code %chin% "au" | (is.na(country_code) & !is.na(state))]

tm_level <- unique(tm_full[!is.na(filing_year),
                           .(application_number, filing_year,
                             madrid = is_madrid_import == 1L)],
                   by = "application_number")
tm_level <- merge(tm_level,
                  tm_pp[, .(us = any(is_us), au = any(is_au)), by = application_number],
                  by = "application_number", all.x = TRUE)
tm_level[, known := !is.na(us)]
tm_level[is.na(us), `:=`(us = FALSE, au = FALSE)]
tm_level[, us_au := us & au]
tm_level <- tm_level[filing_year %between% c(tm_year_min, tm_year_max)]

cat("\nUS-AU co-applied trade marks, all years: ", tm_level[us_au == TRUE, .N], "\n", sep = "")

# ------------------------------------------------------------------------------
# 10.3 Nice classes
# ------------------------------------------------------------------------------

tm_class_table <- function(years) {
  cw <- merge(tm_class_w, tm_level[filing_year %in% years & known == TRUE],
              by = "application_number")
  cw[, .(n_all       = sum(w),
         n_us        = sum(w * us),
         n_us_madrid = sum(w * (us & madrid)),
         n_us_au     = sum(w * us_au)),
     by = .(class_no, class_label, class_group)]
}

tct <- tm_class_table(tm_recent_years)
tm_tot_share_us <- tct[, sum(n_us) / sum(n_all)]

tct[, `:=`(share_us            = n_us / n_all,
           rta_us              = (n_us / n_all) / tm_tot_share_us,
           share_us_via_madrid = n_us_madrid / n_us,
           pct_of_us           = n_us / sum(n_us),
           pct_of_us_au        = n_us_au / sum(n_us_au))]
tct[, collab_specialisation := pct_of_us_au / pct_of_us]

tct_early <- tm_class_table(tm_early_years)
tct <- merge(tct, tct_early[, .(class_no, n_all_early = n_all, share_us_early = n_us / n_all)],
             by = "class_no", all.x = TRUE)
tct[, change_share_pp := round((share_us - share_us_early) * 100, 1)]
setorder(tct, -share_us)

print(tct[, .(class_label, n_all = round(n_all), share_us = round(share_us, 3),
              rta_us = round(rta_us, 2), change_share_pp,
              via_madrid = round(share_us_via_madrid, 2), n_us_au = round(n_us_au, 1))])
fwrite(tct, file.path(out_dir, "tm_classes_us_share_full.csv"))

# Chart 1: US share by Nice class (recent window)
plot_tc <- tct[n_all >= tm_min_class_n][order(share_us)]
plot_tc[, class_label := factor(class_label, levels = class_label)]

p_tm_class_share <- ggplot(plot_tc, aes(share_us, class_label)) +
  geom_col(fill = "#1f4e79", width = 0.7) +
  geom_vline(xintercept = tm_tot_share_us, linetype = "dashed", colour = "grey40") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title    = "US share of Australian trade mark applications by Nice class",
       subtitle = paste0("Standard trade mark applications, ", tm_window_lab,
                         ". Dashed line = US share across all classes"),
       x = NULL, y = NULL,
       caption  = "Source: IP Australia, IP RAPID. Nice classification; class names shortened.") +
  theme_chart(horizontal_bars = TRUE)

# Chart 2: US trade marks by class group over time
tm_group_trend <- merge(tm_class_w, tm_level[known == TRUE & us == TRUE],
                        by = "application_number")[
                          , .(n_us = round(sum(w), 1)), by = .(filing_year, class_group)][order(class_group, filing_year)]

tm_group_pal <- setNames(
  c("#1f4e79", "#a33", "#2a9d8f", "#e9a23b", "#6c757d", "#7b4ea3", "#8c6d31", "#4ba3c3"),
  names(nice_groups)
)

p_tm_group_trend <- ggplot(tm_group_trend, aes(filing_year, n_us, colour = class_group)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.1) +
  scale_colour_manual(values = tm_group_pal) +
  scale_x_continuous(breaks = scales::pretty_breaks(8)) +
  scale_y_continuous(labels = scales::comma) +
  labs(title    = "Australian trade mark applications with a US applicant, by sector",
       subtitle = "Standard trade mark applications grouped by Nice class",
       x = NULL, y = "Applications", colour = NULL,
       caption  = "Source: IP Australia, IP RAPID. Multi-class applications split across sectors.") +
  theme_chart() +
  theme(legend.position = "top") +
  guides(colour = guide_legend(nrow = 2))

# ------------------------------------------------------------------------------
# 10.4 US–Australian co-applicants
# ------------------------------------------------------------------------------

tm_collab_annual <- tm_level[known == TRUE,
                             .(n_us_au = sum(us_au), n_us = sum(us), n_au = sum(au)),
                             by = filing_year][order(filing_year)]
tm_collab_annual[, share_of_us_with_au := n_us_au / n_us]
fwrite(tm_collab_annual, file.path(out_dir, "tm_us_au_coapplied_annual_full.csv"))

# Chart 3: co-applied trade marks per year
p_tm_collab_trend <- ggplot(tm_collab_annual, aes(filing_year, n_us_au)) +
  geom_line(linewidth = 0.8, colour = "#2a9d8f") +
  geom_point(size = 1.4, colour = "#2a9d8f") +
  scale_x_continuous(breaks = scales::pretty_breaks(8)) +
  scale_y_continuous(labels = scales::comma, limits = c(0, NA)) +
  labs(title    = "Trade mark applications with both US and Australian applicants",
       subtitle = "Standard trade mark applications filed in Australia",
       x = NULL, y = "Applications",
       caption  = "Source: IP Australia, IP RAPID") +
  theme_chart()

# Chart 4: co-applied trade marks by Nice class (recent window, top 15)
plot_tcf <- head(tct[n_us_au > 0][order(-n_us_au)], 15)[order(n_us_au)]
plot_tcf[, `:=`(class_label = factor(class_label, levels = class_label),
                n_us_au = round(n_us_au, 1))]

p_tm_collab_classes <- ggplot(plot_tcf, aes(n_us_au, class_label)) +
  geom_col(fill = "#2a9d8f", width = 0.7) +
  scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.05))) +
  labs(title    = "Where US and Australian applicants register brands together",
       subtitle = paste0("Co-applied trade mark applications by Nice class, ", tm_window_lab),
       x = "Applications", y = NULL,
       caption  = "Source: IP Australia, IP RAPID. Multi-class applications split across classes.") +
  theme_chart(horizontal_bars = TRUE)

# Chart 5: Australian partner state (recent window)
tm_recent_known <- tm_level[filing_year %in% tm_recent_years & known == TRUE, application_number]
tm_recent_us_au <- tm_level[filing_year %in% tm_recent_years & us_au == TRUE, application_number]

tm_state_tab <- merge(
  tm_pp[is_au & application_number %chin% tm_recent_us_au,
        .(n_us_au = uniqueN(application_number)), by = .(state = fcoalesce(state, "Unknown"))],
  tm_pp[is_au & application_number %chin% tm_recent_known,
        .(n_au_all = uniqueN(application_number)), by = .(state = fcoalesce(state, "Unknown"))],
  by = "state", all = TRUE
)
tm_state_tab[is.na(n_us_au), n_us_au := 0L]
tm_state_tab[, share_with_us_partner := n_us_au / n_au_all]
setorder(tm_state_tab, -n_us_au)
print(tm_state_tab)
fwrite(tm_state_tab, file.path(out_dir, "tm_us_au_partner_states_full.csv"))

plot_tst <- tm_state_tab[state != "Unknown"][order(n_us_au)]
plot_tst[, state := factor(state, levels = state)]

p_tm_collab_states <- ggplot(plot_tst, aes(n_us_au, state)) +
  geom_col(fill = "#2a9d8f", width = 0.7) +
  scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.05))) +
  labs(title    = "Location of Australian co-applicants on US–Australian trade marks",
       subtitle = paste0("Trade mark applications by state of the Australian applicant, ", tm_window_lab),
       x = "Applications", y = NULL,
       caption  = "Source: IP Australia, IP RAPID. An application with partners in two states counts in both.") +
  theme_chart(horizontal_bars = TRUE)

# Partner tables (recent window) --------------------------------------------

tm_has_type <- "party_type" %in% names(tm_pp)

tm_us_side <- tm_pp[is_us & application_number %chin% tm_recent_us_au,
                    .(application_number, us_name = party_name,
                      us_type = if (tm_has_type) party_type else NA_character_)]
tm_au_side <- tm_pp[is_au & application_number %chin% tm_recent_us_au,
                    .(application_number, au_name = party_name,
                      au_type = if (tm_has_type) party_type else NA_character_,
                      au_abn = abn, au_state = state)]

tm_pairs <- merge(tm_us_side, tm_au_side, by = "application_number", allow.cartesian = TRUE)
tm_pair_tab <- tm_pairs[, .(n_applications = uniqueN(application_number),
                            au_abn   = au_abn[!is.na(au_abn)][1],
                            au_state = au_state[!is.na(au_state)][1]),
                        by = .(us_name, us_type, au_name, au_type)][order(-n_applications)]
tm_pair_tab[, possible_same_group := first_word(us_name) == first_word(au_name) &
              !first_word(us_name) %chin% generic_words &
              nchar(first_word(us_name)) >= 3]
fwrite(tm_pair_tab, file.path(out_dir, "tm_us_au_partner_pairs.csv"))

tm_au_partners <- tm_au_side[, .(n_applications = uniqueN(application_number),
                                 au_abn   = au_abn[!is.na(au_abn)][1],
                                 au_state = au_state[!is.na(au_state)][1]),
                             by = .(au_name, au_type)][order(-n_applications)]
fwrite(tm_au_partners, file.path(out_dir, "tm_us_au_australian_partners.csv"))

cat("\nTop US-AU trade mark pairs,", tm_window_lab, "\n")
print(head(tm_pair_tab, 20))
cat("\nShare of pairs flagged as possible same corporate group: ",
    round(tm_pair_tab[, weighted.mean(possible_same_group, n_applications)], 3), "\n", sep = "")

# Goods vs services mix of US marks (mark-level; table only)
tm_gs_mix <- merge(tm_level[known == TRUE & us == TRUE], tm_gs, by = "application_number")[
  , .N, by = .(filing_year, goods_services)][order(filing_year, goods_services)]
fwrite(dcast(tm_gs_mix, filing_year ~ goods_services, value.var = "N", fill = 0L),
       file.path(out_dir, "tm_us_goods_services_mix.csv"))

# ------------------------------------------------------------------------------
# 10.5 Save charts + designer CSVs
# ------------------------------------------------------------------------------

print(p_tm_class_share); print(p_tm_group_trend); print(p_tm_collab_trend)
print(p_tm_collab_classes); print(p_tm_collab_states)

ggsave("tm_us_share_by_class.png",       p_tm_class_share,    width = 9, height = 11, dpi = 300)
ggsave("tm_us_by_sector_trend.png",      p_tm_group_trend,    width = 9, height = 5.5, dpi = 300)
ggsave("tm_us_au_coapplied_trend.png",   p_tm_collab_trend,   width = 9, height = 5, dpi = 300)
ggsave("tm_us_au_coapplied_classes.png", p_tm_collab_classes, width = 9, height = 6, dpi = 300)
ggsave("tm_us_au_coapplied_states.png",  p_tm_collab_states,  width = 9, height = 5, dpi = 300)

class_note <- if (tm_class_method == "fractional") {
  "Fractional counts: each application split equally across its Nice classes."
} else "Class counts: each application counted once in every class it covers."

export_chart_csv(p_tm_class_share, "tm_us_share_by_class.csv", y_format = "percent",
                 chart_type = "Horizontal bar with reference line",
                 notes = paste0("Classes with fewer than ", tm_min_class_n,
                                " applications in the window are omitted. ", class_note))
export_chart_csv(p_tm_group_trend, "tm_us_by_sector_trend.csv", y_format = "number",
                 chart_type = "Multi-line with point markers",
                 series_colours = tm_group_pal,
                 notes = paste("Sectors are groups of Nice classes defined for this analysis.", class_note))
export_chart_csv(p_tm_collab_trend, "tm_us_au_coapplied_trend.csv", y_format = "number",
                 notes = "Applications with at least one US and one Australian applicant/owner.")
export_chart_csv(p_tm_collab_classes, "tm_us_au_coapplied_classes.csv", y_format = "number",
                 chart_type = "Horizontal bar", digits = 1,
                 notes = paste("Top 15 classes.", class_note))
export_chart_csv(p_tm_collab_states, "tm_us_au_coapplied_states.csv", y_format = "number",
                 chart_type = "Horizontal bar",
                 notes = "State of the Australian co-applicant. Applications with partners in two states count in both.")
