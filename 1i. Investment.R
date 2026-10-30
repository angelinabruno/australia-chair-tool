rm(list = ls())
# The dashboard folder is set once, in section 1 of '1. Refresh all data.R'.
# To run this script on its own, run that section first.
if (Sys.getenv("DASHBOARD_DIR") == "")
  stop("Dashboard folder not set. Run section 1 of '1. Refresh all data.R' first.")
setwd(file.path(Sys.getenv("DASHBOARD_DIR"), "Investment"))
source(file.path(Sys.getenv("DASHBOARD_DIR"), "0. Exchange rates.R"))   # A$/US$ toggle
source(file.path(Sys.getenv("DASHBOARD_DIR"), "0. Takeaways.R"))       # line under each chart
# ==============================================================================
# Foreign investment.R  -  two-way investment between Australia and its partners
#
# Source: ABS, International Investment Position, Australia: Supplementary
#         Statistics. Table 2 (foreign investment in Australia, level) and
#         Table 5 (Australian investment abroad, level), $ million.
#
# One click: downloads the latest Tables-all.zip from the ABS, extracts the two
# tables, finds the total level of investment for the latest year and draws
# Investment/iip_levels_latest.png for the dashboard. No files are prepared by hand.
#
# The ABS publishes this once a year (early May, for the previous calendar
# year). The script looks for the newest release itself, so it does not need
# editing when the 2026 tables come out.
# ==============================================================================

library(httr2)
library(readxl)
library(dplyr)
library(ggplot2)
source(file.path(Sys.getenv("DASHBOARD_DIR"), "0. Chart data.R"))   # <chart>_data.csv beside each chart
library(scales)

DASHBOARD_DIR <- Sys.getenv("DASHBOARD_DIR")
RAW_DIR <- file.path(DASHBOARD_DIR, "Investment", "raw")   # downloads (not for GitHub)
OUT_DIR <- file.path(DASHBOARD_DIR, "Investment")
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
tab_files <- c(foreign = pick("5352002"), abroad = pick("5352005"),
               ind_foreign = pick("53520015"), ind_abroad = pick("53520017"))
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
  mutate(value_bn = value_m / 1000,
         # Positions are stocks at 31 December, so US$ uses the rate on that date.
         usd_per_aud  = fx_on(as.Date(paste0(year, "-12-31"))),
         value_bn_usd = value_bn * usd_per_aud)

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

# Drawn twice, in A$ and US$ (at the exchange rate on 31 December).
make_iip <- function(cur) {
plot_dat <- iip |>
  mutate(
    value_bn  = if (cur == "USD") value_bn_usd else value_bn,
    partner   = factor(partner, levels = partner_order),
    # second level is drawn on top within each pair
    direction = factor(direction, levels = c("Foreign investment in Australia",
                                             "Australian investment abroad")),
    label     = dollar(value_bn, accuracy = 1, prefix = cur_sym[[cur]], suffix = "bn", big.mark = ",")
  )

x_max <- max(plot_dat$value_bn, na.rm = TRUE) * 1.12   # headroom for value labels

ggplot(plot_dat, aes(x = value_bn, y = partner, fill = direction)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75, na.rm = TRUE) +
  geom_text(aes(label = label), hjust = -0.1,
            position = position_dodge(width = 0.8),
            size = 2.9, family = font_family, colour = csis_grey, na.rm = TRUE) +
  scale_fill_manual(values = c("Australian investment abroad"    = csis_teal,
                               "Foreign investment in Australia" = csis_navy),
                    breaks = c("Australian investment abroad",
                               "Foreign investment in Australia")) +
  scale_x_continuous(labels = dollar_format(prefix = cur_sym[[cur]], suffix = "bn", big.mark = ","),
                     limits = c(0, x_max),
                     expand = expansion(mult = c(0, 0))) +
  labs(
    title    = sprintf("Two-way investment between Australia and its partners, %d", YEAR),
    subtitle = sprintf("Level of investment at 31 December %d, %s billion", YEAR, cur_sym[[cur]]),
    caption  = paste("Source: ABS, International Investment Position, Australia:",
                     "Supplementary Statistics, Tables 2 and 5.",
                     if (cur == "USD") "
Converted to US$ at the RBA exchange rate on 31 December." else ""),
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
}

save_both(make_iip, file.path(OUT_DIR, "iip_levels_latest.png"),
          function(p, f) ggsave(f, p, width = 9, height = 5.5, dpi = 300, bg = "white"))



# ------------------------------------------------------------------------------
# 4. By type of investment: direct, portfolio and other
# ------------------------------------------------------------------------------
# Direct investment (a lasting interest, 10% or more of voting power) behaves
# very differently from portfolio holdings of shares and bonds, so the total
# above is split by type. Same Tables 2 and 5: country names in the first
# column, types of investment in the second, one column per year.
# Other = financial derivatives, other investment (loans, deposits, trade
# credit) and, for assets, reserve assets.

read_by_type <- function(path) {
  g <- as.matrix(suppressMessages(read_excel(path, sheet = 2, col_names = FALSE,
                                             col_types = "text", .name_repair = "minimal")))
  yr_row <- which.max(rowSums(!is.na(matrix(cell_year(as.vector(g)), nrow = nrow(g)))))
  yrs    <- cell_year(g[yr_row, ])
  col    <- which(yrs == max(yrs, na.rm = TRUE))[1]
  country <- fill_forward(ifelse(is.na(g[, 1]) | trimws(g[, 1]) == "", NA, trimws(g[, 1])))
  data.frame(country = country, type = trimws(g[, 2]), value_m = abs(cell_num(g[, col])),
             year = max(yrs, na.rm = TRUE)) |>
    filter(!is.na(type))
}

type_group <- function(type) dplyr::case_when(
  grepl("^Direct investment( in Australia| abroad)?$", type)         ~ "Direct",
  grepl("^Portfolio investment (liabilities|assets)$", type)         ~ "Portfolio",
  grepl("^(Financial derivative|Other investment) (liabilities|assets)$|^Reserve assets$", type) ~ "Other",
  TRUE ~ NA_character_)

by_type <- bind_rows(
  read_by_type(tab_paths[["foreign"]]) |> mutate(direction = "Foreign investment in Australia"),
  read_by_type(tab_paths[["abroad"]])  |> mutate(direction = "Australian investment abroad")
) |>
  mutate(group = type_group(type)) |>
  filter(!is.na(group))
by_type$partner <- NA_character_
for (p in names(PARTNERS)) by_type$partner[grepl(PARTNERS[[p]], by_type$country, perl = TRUE)] <- p
by_type <- by_type |>
  filter(!is.na(partner)) |>
  group_by(direction, partner, group, year) |>
  summarise(value_bn = sum(value_m, na.rm = TRUE) / 1000,
            np = any(is.na(value_m)), .groups = "drop") |>
  mutate(usd_per_aud = fx_on(as.Date(paste0(year, "-12-31"))))
stopifnot("Types of investment not found in Tables 2 and 5" = nrow(by_type) > 0)
write.csv(by_type, file.path(OUT_DIR, "iip_by_type_latest.csv"), row.names = FALSE)

make_iip_type <- function(cur) {
  d <- by_type |>
    mutate(value_bn = if (cur == "USD") value_bn * usd_per_aud else value_bn,
           partner  = factor(partner, levels = partner_order),
           group    = factor(group, levels = c("Other", "Portfolio", "Direct")),
           direction = factor(direction, levels = c("Foreign investment in Australia",
                                                    "Australian investment abroad")))
  ggplot(d, aes(value_bn, partner, fill = group)) +
    geom_col(width = 0.7) +
    facet_wrap(~direction, ncol = 2) +
    scale_fill_manual(values = c(Direct = csis_navy, Portfolio = csis_teal, Other = "#B1B3B3"),
                      breaks = c("Direct", "Portfolio", "Other")) +
    scale_x_continuous(labels = dollar_format(prefix = cur_sym[[cur]], suffix = "bn", big.mark = ","),
                       breaks = scales::breaks_pretty(n = 3),
                       expand = expansion(mult = c(0, 0.05))) +
    labs(
      title    = sprintf("Two-way investment by type, %d", YEAR),
      subtitle = sprintf("Level of investment at 31 December %d, %s billion", YEAR, cur_sym[[cur]]),
      caption  = paste0("Source: ABS, International Investment Position, Australia: Supplementary Statistics, Tables 2 and 5.",
                        "\nDirect = a lasting interest (10% or more of voting power). Portfolio = shares and bonds.",
                        " Other = loans, deposits, trade credit, derivatives and reserves.",
                        if (any(by_type$np)) "\nSome partner-type values are not published by the ABS and are left out." else "",
                        if (cur == "USD") "\nConverted to US$ at the RBA exchange rate on 31 December." else ""),
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
      strip.text         = element_text(face = "bold", colour = csis_navy, hjust = 0),
      panel.spacing.x    = unit(2.5, "lines"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text          = element_text(colour = csis_grey),
      plot.margin        = margin(10, 25, 10, 10)
    )
}

save_both(make_iip_type, file.path(OUT_DIR, "iip_by_type_latest.png"),
          function(p, f) ggsave(f, p, width = 10, height = 5.5, dpi = 300, bg = "white"))

# ------------------------------------------------------------------------------
# 4b. Direct investment by broad industry
# ------------------------------------------------------------------------------
# Tables 15a and 17a: the level of direct investment at 31 December by ANZSIC
# division, for all partner countries together (the ABS does not publish
# industry by partner). Divisions are grouped into broad sectors; values the
# ABS does not publish (np) are counted in "Unallocated or not published",
# which is the published total less the published sectors.
SECTOR_OF <- c(
  "Mining" = "Mining",
  "Manufacturing" = "Manufacturing",
  "Financial and Insurance Services" = "Finance and insurance",
  "Rental, Hiring and Real Estate Services" = "Real estate",
  "Wholesale and Retail Trade" = "Wholesale and retail trade",
  "Electricity, Gas, Water and Waste Services" = "Utilities, transport and construction",
  "Transport, Postal and Warehousing" = "Utilities, transport and construction",
  "Construction" = "Utilities, transport and construction",
  "Information Media and Telecommunications" = "Information, professional and other services",
  "Professional, Scientific and Technical Services" = "Information, professional and other services",
  "Administrative and Support Services" = "Information, professional and other services",
  "Accommodation and Food Services" = "Information, professional and other services",
  "Health Care and Social Assistance" = "Information, professional and other services",
  "Education and Training" = "Information, professional and other services",
  "Arts and Recreation Services" = "Information, professional and other services",
  "Other Services" = "Information, professional and other services",
  "Public Administration and Safety" = "Information, professional and other services",
  "Agriculture, Forestry and Fishing" = "Agriculture, forestry and fishing"
)
UNALLOC <- "Unallocated or not published"

read_by_industry <- function(path) {
  g <- as.matrix(suppressMessages(read_excel(path, sheet = 2, col_names = FALSE,
                                             col_types = "text", .name_repair = "minimal")))
  # Years are in the row headed "Industry" (some values look like Excel dates)
  yr_row <- which(trimws(g[, 1]) == "Industry")[1]
  yrs    <- suppressWarnings(as.integer(g[yr_row, ]))
  col    <- which(yrs == max(yrs, na.rm = TRUE))[1]
  d <- data.frame(industry = trimws(g[, 1]), value_m = abs(cell_num(g[, col])),
                  year = max(yrs, na.rm = TRUE))
  total <- d$value_m[which(d$industry == "Total")[1]]
  stopifnot("Industry total not found" = length(total) == 1 && !is.na(total))
  s <- d |>
    filter(industry %in% names(SECTOR_OF), !is.na(value_m)) |>
    mutate(sector = unname(SECTOR_OF[industry])) |>
    group_by(sector, year) |>
    summarise(value_m = sum(value_m), .groups = "drop")
  bind_rows(s, data.frame(sector = UNALLOC, year = s$year[1], value_m = total - sum(s$value_m)))
}

by_ind <- bind_rows(
  read_by_industry(tab_paths[["ind_foreign"]]) |> mutate(direction = "Direct investment in Australia"),
  read_by_industry(tab_paths[["ind_abroad"]])  |> mutate(direction = "Australian direct investment abroad")
) |>
  mutate(value_bn = value_m / 1000,
         usd_per_aud = fx_on(as.Date(paste0(year, "-12-31"))))
stopifnot("Industry tables 15 and 17 not read" = n_distinct(by_ind$direction) == 2)
write.csv(by_ind, file.path(OUT_DIR, "iip_direct_by_industry_latest.csv"), row.names = FALSE)

# Sectors in order of their total across both directions, the remainder last
sector_order <- by_ind |>
  filter(sector != UNALLOC) |>
  group_by(sector) |> summarise(v = sum(value_bn), .groups = "drop") |>
  arrange(v) |> pull(sector)
sector_order <- c(UNALLOC, sector_order)

make_iip_industry <- function(cur) {
  d <- by_ind |>
    mutate(value_bn  = if (cur == "USD") value_bn * usd_per_aud else value_bn,
           sector    = factor(sector, levels = sector_order),
           shade     = sector == UNALLOC,
           direction = factor(direction, levels = c("Direct investment in Australia",
                                                    "Australian direct investment abroad")))
  ggplot(d, aes(value_bn, sector, fill = shade)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = sprintf("%s%.0fbn", cur_sym[[cur]], value_bn)), hjust = -0.12,
              size = 3, family = font_family, colour = csis_grey) +
    facet_wrap(~direction, ncol = 2) +
    scale_fill_manual(values = c(`FALSE` = csis_navy, `TRUE` = "#B1B3B3"), guide = "none") +
    scale_x_continuous(labels = dollar_format(prefix = cur_sym[[cur]], suffix = "bn", big.mark = ","),
                       breaks = scales::breaks_pretty(n = 3),
                       expand = expansion(mult = c(0, 0.22))) +
    labs(
      title    = sprintf("Direct investment by industry, %d", unique(by_ind$year)[1]),
      subtitle = sprintf("Level of direct investment at 31 December %d, all partner countries, %s billion",
                         unique(by_ind$year)[1], cur_sym[[cur]]),
      caption  = paste0("Source: ABS, International Investment Position, Australia: Supplementary Statistics, Tables 15a and 17a",
                        " (industry of the Australian enterprise, ANZSIC divisions).",
                        "\nThe ABS does not publish industry by partner country, so this covers all countries, not just the US.",
                        " Direct investment only.",
                        "\nUnallocated or not published = the total less the industries the ABS publishes.",
                        if (cur == "USD") "\nConverted to US$ at the RBA exchange rate on 31 December." else ""),
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 12, base_family = font_family) +
    theme(
      plot.title         = element_text(face = "bold", colour = csis_navy, size = rel(1.2)),
      plot.subtitle      = element_text(colour = csis_grey, margin = margin(b = 8)),
      plot.caption       = element_text(colour = csis_grey, hjust = 0, size = rel(0.75)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      strip.text         = element_text(face = "bold", colour = csis_navy, hjust = 0),
      panel.spacing.x    = unit(2.5, "lines"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text          = element_text(colour = csis_grey),
      plot.margin        = margin(10, 25, 10, 10)
    )
}

save_both(make_iip_industry, file.path(OUT_DIR, "iip_direct_by_industry_latest.png"),
          function(p, f) ggsave(f, p, width = 10, height = 5.5, dpi = 300, bg = "white"))

# ------------------------------------------------------------------------------
# 5. Takeaways under each chart
# ------------------------------------------------------------------------------
local({
  inv <- iip |> filter(direction == "Foreign investment in Australia") |> arrange(desc(value_bn))
  out <- iip |> filter(direction == "Australian investment abroad") |> arrange(desc(value_bn))
  line <- function(cur) {
    k <- if (cur == "USD") inv$usd_per_aud[1] else 1
    us_in  <- inv$value_bn[inv$partner == "United States"] * k
    us_out <- out$value_bn[out$partner == "United States"] * k
    sprintf("US investors held %s in Australia at the end of %d, %s; Australians held %s in the US.",
            fmt_bn(us_in, cur_sym[[cur]]), YEAR,
            if (inv$partner[1] == "United States") "more than any other country" else
              paste0("second to ", inv$partner[1]),
            fmt_bn(us_out, cur_sym[[cur]]))
  }
  takeaway("Investment/iip_levels_latest.png", line("AUD"), line("USD"))
  us_t <- by_type |> filter(partner == "United States")
  sh <- function(dir) { x <- us_t |> filter(direction == dir); 100 * sum(x$value_bn[x$group == "Direct"]) / sum(x$value_bn) }
  takeaway("Investment/iip_by_type_latest.png", sprintf(
    "Direct investment is %.0f%% of US investment in Australia and %.0f%% of Australian investment in the US; the rest is mostly portfolio holdings.",
    sh("Foreign investment in Australia"), sh("Australian investment abroad")))
})

message("Saved ", file.path(OUT_DIR, "iip_levels_latest.png"))