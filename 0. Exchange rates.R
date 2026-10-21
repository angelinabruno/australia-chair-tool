# ---- Exchange rates for the AUD / USD toggle ---------------------------------
# Every chart in A$ (or US$) is saved twice: <name>.png in A$ and
# <name>_usd.png in US$. The toggle in the dashboard's top bar swaps them.
#
# Conversion rule: each value is converted at the average of the RBA's daily
# A$/US$ rates over its own period (calendar year, financial year, quarter or
# month). Stocks measured at a date (e.g. investment positions at 31 December)
# use the rate on that date. Real (constant-price) series use the average for
# their reference year, so growth rates are unchanged.
#
# Source: RBA, Historical daily exchange rates (F11.1 and historical files),
# A$1 = US$ (series FXRUSD), daily from 1983.
#
# Sourced at the top of each chart script; needs DASHBOARD_DIR (section 1 of
# '1. Refresh all data.R').

# Uses readxl without attaching it, so loading this file never changes which
# package functions a chart script sees.

FX_DIR       <- file.path(Sys.getenv("DASHBOARD_DIR"), "Overview", "fx-raw")
FX_CACHE     <- file.path(Sys.getenv("DASHBOARD_DIR"), "Overview", "fx_aud_usd_daily.csv")
FX_MAX_AGE   <- 7   # days before the current-year file is downloaded again
FX_HIST      <- c("1983-1986", "1987-1990", "1991-1994", "1995-1998", "1999-2002",
                  "2003-2006", "2007-2009", "2010-2013", "2014-2017", "2018-2022")

fx_download <- function(url, dest) {
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  # The RBA site rejects requests without a browser-like user agent.
  utils::download.file(url, dest, mode = "wb", quiet = TRUE,
                       headers = c(`User-Agent` = "Mozilla/5.0"))
}

# Reads one RBA exchange-rate sheet (xls or csv) and returns date + usd columns.
fx_read <- function(path) {
  raw <- if (grepl("\\.csv$", path)) {
    utils::read.csv(path, header = FALSE, colClasses = "character", fileEncoding = "UTF-8-BOM")
  } else {
    as.data.frame(readxl::read_excel(path, col_names = FALSE, col_types = "text", .name_repair = "minimal"))
  }
  id_row <- which(raw[[1]] == "Series ID")
  col    <- which(unlist(raw[id_row, ]) == "FXRUSD")
  d <- raw[(id_row + 1):nrow(raw), c(1, col)]
  names(d) <- c("date", "usd")
  d$date <- if (grepl("\\.csv$", path)) as.Date(d$date, format = "%d-%b-%Y")
            else as.Date(as.numeric(d$date), origin = "1899-12-30")
  d$usd <- suppressWarnings(as.numeric(d$usd))
  d[!is.na(d$date) & !is.na(d$usd), ]
}

fx_load <- function() {
  fresh <- file.exists(FX_CACHE) &&
    difftime(Sys.time(), file.mtime(FX_CACHE), units = "days") < FX_MAX_AGE
  if (fresh) {
    d <- utils::read.csv(FX_CACHE)
    d$date <- as.Date(d$date)
    return(d)
  }
  base <- "https://www.rba.gov.au/statistics/tables/"
  for (h in FX_HIST) {
    f <- file.path(FX_DIR, paste0(h, ".xls"))
    if (!file.exists(f)) fx_download(paste0(base, "xls-hist/", h, ".xls"), f)
  }
  cur <- file.path(FX_DIR, "f11.1-data.csv")
  tryCatch(fx_download(paste0(base, "csv/f11.1-data.csv"), cur),
           error = function(e) if (!file.exists(cur)) stop(e))
  d <- do.call(rbind, lapply(c(file.path(FX_DIR, paste0(FX_HIST, ".xls")), cur), fx_read))
  d <- d[!duplicated(d$date), ]
  d <- d[order(d$date), ]
  utils::write.csv(d, FX_CACHE, row.names = FALSE)
  d
}

FX <- fx_load()
message(sprintf("Exchange rates: %s daily A$/US$ rates, %s to %s",
                format(nrow(FX), big.mark = ","), min(FX$date), max(FX$date)))

# Average US$ per A$1 between two dates (inclusive). Vectorised.
fx_between <- function(from, to) {
  from <- as.Date(from); to <- as.Date(to)
  cs <- c(0, cumsum(FX$usd))
  i0 <- findInterval(from - 1, FX$date)   # rates strictly before `from`
  i1 <- findInterval(to, FX$date)         # rates up to and including `to`
  out <- (cs[i1 + 1] - cs[i0 + 1]) / (i1 - i0)
  out[i1 <= i0] <- NA
  # A period that is still running (e.g. the current year) uses the rates so far.
  out
}

# Period averages. Years are numbers; dates are anything as.Date() accepts.
fx_calendar_year  <- function(year) fx_between(as.Date(paste0(year, "-01-01")), as.Date(paste0(year, "-12-31")))
# Financial year named by the year it ends in (2024-25 -> 2025)
fx_financial_year <- function(fy_end) fx_between(as.Date(paste0(fy_end - 1, "-07-01")), as.Date(paste0(fy_end, "-06-30")))
# US fiscal year (1 October to 30 September), named by the year it ends in
fx_us_fiscal_year <- function(fy_end) fx_between(as.Date(paste0(fy_end - 1, "-10-01")), as.Date(paste0(fy_end, "-09-30")))
fx_quarter <- function(date) {
  date  <- as.Date(date)
  start <- as.Date(paste0(format(date, "%Y"), "-", sprintf("%02d", (as.integer(format(date, "%m")) - 1) %/% 3 * 3 + 1), "-01"))
  end   <- as.Date(vapply(start, function(s) as.character(seq(s, by = "3 months", length.out = 2)[2] - 1), ""))
  fx_between(start, end)
}
fx_month <- function(date) {
  start <- as.Date(format(as.Date(date), "%Y-%m-01"))
  end   <- as.Date(vapply(start, function(s) as.character(seq(s, by = "month", length.out = 2)[2] - 1), ""))
  fx_between(start, end)
}
# Rate on a date: the last published rate on or before it (for stocks).
fx_on <- function(date) FX$usd[pmax(1, findInterval(as.Date(date), FX$date))]

# Convert. `rate` is US$ per A$1, from one of the functions above.
aud_to_usd <- function(x, rate) x * rate
usd_to_aud <- function(x, rate) x / rate

# Labels for each currency, so chart code can write cur_lab[[cur]] etc.
CURRENCIES <- c("AUD", "USD")
cur_sym  <- c(AUD = "A$", USD = "US$")
cur_name <- c(AUD = "Australian dollars", USD = "US dollars")
fx_note  <- c(AUD = "", USD = "
Converted to US$ at the RBA's average daily exchange rate for each period.")
fx_note_aud_from_usd <- "
Converted to A$ at the RBA's average daily exchange rate for each period."

# File name for the US$ version of a chart: abc.png -> abc_usd.png
usd_file <- function(file) sub("\\.png$", "_usd.png", file)

# Saves a chart in both currencies. `make` is a function(cur) returning a
# ggplot for cur = "AUD" or "USD"; `save` is the script's own save function
# (called as save(plot, file, ...)).
save_both <- function(make, file, save, ...) {
  save(make("AUD"), file, ...)
  save(make("USD"), usd_file(file), ...)
  invisible(file)
}

# Periods with no rates yet (e.g. expected spending for next financial year)
# fall back to the average over the latest 365 days.
fx_latest_year <- function() fx_between(max(FX$date) - 364, max(FX$date))
fx_or_latest   <- function(rate) ifelse(is.na(rate), fx_latest_year(), rate)
