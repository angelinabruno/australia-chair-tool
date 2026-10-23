rm(list = ls())
# The dashboard folder is set once, in section 1 of '1. Refresh all data.R'.
# To run this script on its own, run that section first.
if (Sys.getenv("DASHBOARD_DIR") == "")
  stop("Dashboard folder not set. Run section 1 of '1. Refresh all data.R' first.")
setwd(file.path(Sys.getenv("DASHBOARD_DIR"), "Economy"))
source(file.path(Sys.getenv("DASHBOARD_DIR"), "0. Exchange rates.R"))   # A$/US$ toggle
source(file.path(Sys.getenv("DASHBOARD_DIR"), "0. Takeaways.R"))       # line under each chart

# Australian economy charts
#   1. Real GDP growth, Australia and Australia vs US
#   2. Real GDP index and labour productivity, Australia vs US
#   3. GVA by industry: levels and contributions to growth
#   5. Relative size: Australia vs US, US states and the OECD
#   (Two-way investment charts are made by '1i. Investment.R'.)
#
# Everything is downloaded (ABS via readabs; US data straight from BEA and
# BLS, no API keys needed; World Bank).

library(readabs)
library(lubridate)
library(WDI)
library(patchwork)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(showtext)


# Style -------------------------------------------------------------------

DPI <- 300
font_add_google("Source Sans 3", "source_sans")
showtext_auto()
showtext_opts(dpi = DPI)   # must match the dpi used in ggsave

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

# Horizontal bar charts: vertical gridlines only, no x axis line
theme_bars <- theme(
  panel.grid.major.y = element_blank(),
  panel.grid.major.x = element_line(colour = "#E6E7E8", linewidth = 0.4),
  axis.line.x  = element_blank(),
  axis.ticks.x = element_blank()
)

au_us_colours <- c("Australia"     = csis_colours[["navy"]],
                   "United States" = csis_colours[["red"]])

save_plot <- function(plot, file, width = 8, height = 4.5) {
  ggsave(file, plot, width = width, height = height, dpi = DPI, bg = "white")
}

# ---- US data: BEA and BLS --------------------------------------------------
# Downloads are kept in Economy/raw/ and fetched again when over a week old.
RAW_DIR <- "raw"
dir.create(RAW_DIR, showWarnings = FALSE)

fetch_us <- function(url, dest, max_age_days = 7) {
  if (!file.exists(dest) ||
      difftime(Sys.time(), file.mtime(dest), units = "days") > max_age_days) {
    message("Downloading ", basename(dest))
    utils::download.file(url, dest, mode = "wb", quiet = TRUE,
                         headers = c(`User-Agent` = "Mozilla/5.0"))
  }
  dest
}

# BEA NIPA quarterly flat file: one row per series code and quarter.
# A191RX = real GDP, millions of chained (2017) dollars, seasonally adjusted
# at annual rates (NIPA Table 1.1.6, line 1). Returns date (quarter start), value.
bea_nipa_q <- function(code) {
  f <- fetch_us("https://apps.bea.gov/national/Release/TXT/NipaDataQ.txt",
                file.path(RAW_DIR, "bea_NipaDataQ.txt"))
  d <- utils::read.csv(f, colClasses = "character", check.names = FALSE)
  names(d) <- c("code", "period", "value")
  d <- d[d$code == code, ]
  stopifnot("BEA series not found in NipaDataQ.txt" = nrow(d) > 0)
  data.frame(date  = as.Date(paste0(substr(d$period, 1, 4), "-",
                                    (as.integer(substr(d$period, 6, 6)) - 1) * 3 + 1, "-01")),
             value = as.numeric(gsub(",", "", d$value)))
}

# BLS public API, version 1 (no key: up to 10 years per request, 25 requests a
# day). Quarterly series; returns date (quarter start), value.
bls_q <- function(series, from_year = 1995) {
  to_year <- as.integer(format(Sys.Date(), "%Y"))
  starts  <- seq(from_year, to_year, by = 10)
  out <- lapply(starts, function(y0) {
    body <- sprintf('{"seriesid":["%s"],"startyear":"%d","endyear":"%d"}',
                    series, y0, min(y0 + 9, to_year))
    res <- httr2::request("https://api.bls.gov/publicAPI/v1/timeseries/data/") |>
      httr2::req_body_raw(body, type = "application/json") |>
      httr2::req_perform() |>
      httr2::resp_body_json(check_type = FALSE)   # BLS labels its JSON as text/plain
    if (res$status != "REQUEST_SUCCEEDED") stop("BLS: ", paste(unlist(res$message), collapse = " "))
    obs <- res$Results$series[[1]]$data
    data.frame(year   = vapply(obs, `[[`, "", "year"),
               period = vapply(obs, `[[`, "", "period"),
               value  = as.numeric(vapply(obs, `[[`, "", "value")))
  })
  d <- do.call(rbind, out)
  d <- d[grepl("^Q0[1-4]$", d$period), ]
  d$date <- as.Date(paste0(d$year, "-", (as.integer(substr(d$period, 3, 3)) - 1) * 3 + 1, "-01"))
  d <- d[order(d$date), c("date", "value")]
  d[!duplicated(d$date), ]
}

# Index each country's series to the average of base_year = 100. ABS dates
# quarters by their last month and BEA/BLS by their first, so both are floored
# to the quarter start.
index_to <- function(df, base_year) {
  df |>
    mutate(date = floor_date(date, "quarter")) |>
    arrange(country, date) |>
    group_by(country) |>
    mutate(base  = mean(value[year(date) == base_year], na.rm = TRUE),
           index = 100 * value / base) |>
    ungroup()
}


# 1. Real GDP growth ------------------------------------------------------

# A2304402X = GDP, chain volume measures, seasonally adjusted, quarterly
gdp_au <- read_abs_series("A2304402X") |>
  select(date, value) |>
  arrange(date) |>
  mutate(country = "Australia")

gdp_us <- bea_nipa_q("A191RX") |> mutate(country = "United States")

p_gdp_au <- gdp_au |>
  mutate(qoq = 100 * (value / lag(value) - 1),
         yoy = 100 * (value / lag(value, 4) - 1)) |>
  pivot_longer(c(qoq, yoy), names_to = "measure", values_to = "growth") |>
  filter(!is.na(growth)) |>
  mutate(measure = recode(measure, qoq = "Quarterly", yoy = "Through the year")) |>
  ggplot(aes(date, growth, colour = measure)) +
  geom_hline(yintercept = 0, colour = csis_colours[["grey"]], linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_colour_csis() +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 1)) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(title = "Australian real GDP growth",
       subtitle = "Chain volume measures, seasonally adjusted",
       x = NULL, y = NULL,
       caption = "Source: ABS, Australian National Accounts (cat. no. 5206.0).") +
  theme_csis()

save_plot(p_gdp_au, "gdp_growth.png")

gdp <- bind_rows(gdp_au, gdp_us) |>
  mutate(date = floor_date(date, "quarter")) |>
  arrange(country, date) |>
  group_by(country) |>
  mutate(yoy = 100 * (value / lag(value, 4) - 1)) |>
  ungroup() |>
  filter(!is.na(yoy))

stopifnot(n_distinct(gdp$country) == 2)

# Zoomed in so ordinary booms and busts are visible: the y axis stops at
# GROWTH_YLIM and the COVID-19 swings, which would flatten everything else,
# run off the scale (their size is given in the subtitle).
GROWTH_YLIM <- c(-4.5, 6.5)
gdp_recent  <- gdp |> filter(date >= as.Date("2000-01-01"))
off_scale   <- gdp_recent |>
  filter(yoy < GROWTH_YLIM[1] | yoy > GROWTH_YLIM[2]) |>
  group_by(country) |>
  summarise(lo = min(yoy), hi = max(yoy), .groups = "drop")
off_note <- if (nrow(off_scale) == 0) "" else
  paste0("
COVID-19 swings run off the scale (",
         paste(sprintf("%s %+.1f%% to %+.1f%%", sub("United States", "US", off_scale$country),
                       off_scale$lo, off_scale$hi), collapse = "; "), ").")

p_gdp <- gdp_recent |>
  ggplot(aes(date, yoy, colour = country)) +
  geom_hline(yintercept = 0, colour = csis_colours[["grey"]], linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = au_us_colours) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 1), breaks = seq(-4, 6, 2)) +
  scale_x_date(breaks = seq(as.Date("2000-01-01"), Sys.Date(), by = "5 years"), date_labels = "%Y") +
  coord_cartesian(ylim = GROWTH_YLIM) +
  labs(title = "Australian and US real GDP growth",
       subtitle = paste0("Through the year, seasonally adjusted.", off_note),
       x = NULL, y = NULL,
       caption = paste("Sources: ABS, Australian National Accounts (cat. no. 5206.0);",
                       "US BEA.")) +
  theme_csis()

save_plot(p_gdp, "gdp_growth_au_us.png")


# 2. GDP index and labour productivity ------------------------------------

gdp_base_year <- 2019

gdp_idx <- index_to(bind_rows(gdp_au, gdp_us), gdp_base_year)
stopifnot(!anyNA(gdp_idx$base))

p_gdp_idx <- gdp_idx |>
  filter(date >= as.Date("2000-01-01")) |>
  ggplot(aes(date, index, colour = country)) +
  geom_hline(yintercept = 100, colour = csis_colours[["ltgrey"]],
             linewidth = 0.4, linetype = "dashed") +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = au_us_colours) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  scale_x_date(breaks = seq(as.Date("2000-01-01"), Sys.Date(), by = "5 years"), date_labels = "%Y") +
  labs(title = "Real GDP, Australia and the United States",
       subtitle = paste0("Index, ", gdp_base_year, " = 100. Chain volume / chained dollars, ",
                         "seasonally adjusted."),
       x = NULL, y = NULL,
       caption = paste("Sources: ABS, Australian National Accounts (cat. no. 5206.0);",
                       "US BEA, National Income and Product Accounts, Table 1.1.6.")) +
  theme_csis()

save_plot(p_gdp_idx, "gdp_index_au_us.png")

# Growth and level together, for the dashboard's single "GDP" tab
p_gdp_both <- (p_gdp + labs(title = "Real GDP growth", caption = NULL)) /
  (p_gdp_idx + labs(title = "Real GDP level") + theme(legend.position = "none")) +
  plot_annotation(title = "Real GDP, Australia and the United States",
                  theme = theme_csis())
save_plot(p_gdp_both, "gdp_au_us.png", width = 8, height = 8)

# Australia: ABS A3606058X (output per hour worked)
# US: BLS PRS84006093, business sector labor productivity (output per hour
# of all workers), index
prod_base_year <- 2015

prod <- bind_rows(
  read_abs_series("A3606058X") |> select(date, value) |> mutate(country = "Australia"),
  bls_q("PRS84006093") |> mutate(country = "United States")
) |>
  index_to(prod_base_year)

stopifnot(n_distinct(prod$country) == 2, !anyNA(prod$base))

p_prod <- prod |>
  filter(date >= as.Date("2000-01-01")) |>
  ggplot(aes(date, index, colour = country)) +
  geom_hline(yintercept = 100, colour = csis_colours[["ltgrey"]],
             linewidth = 0.4, linetype = "dashed") +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = au_us_colours) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(title = "Labour Productivity, Australia vs. United States",
       subtitle = paste0("Output per hour worked. Index, ", prod_base_year, " = 100. ",
                         "Australia: market sector. United States: business sector."),
       x = NULL, y = NULL,
       caption = paste("Sources: ABS, Australian System of National Accounts;",
                       "US Bureau of Labor Statistics, Productivity and Costs.")) +
  theme_csis()

save_plot(p_prod, "productivity_au_us.png")

# Productivity levels: GDP per hour worked, whole economy, US$ at 2020 PPPs
# (constant prices), from the OECD Productivity Database. The index above
# shows growth; this shows the gap in levels.
oecd_pdb <- fetch_us(paste0("https://sdmx.oecd.org/public/rest/data/OECD.SDD.TPS,DSD_PDB@DF_PDB,/",
                            "AUS+USA.A.GDPHRS._T.USD_PPP_H.LR.N._Z.PPP?startPeriod=1990&format=csv"),
                     file.path(RAW_DIR, "oecd_gdp_per_hour.csv"))
prod_lv <- utils::read.csv(oecd_pdb) |>
  transmute(country = recode(REF_AREA, AUS = "Australia", USA = "United States"),
            year = as.integer(TIME_PERIOD), value = OBS_VALUE, base = BASE_PER) |>
  arrange(country, year)
stopifnot(n_distinct(prod_lv$country) == 2)
prod_lv_latest <- prod_lv |> group_by(country) |> slice_max(year, n = 1) |> ungroup()

p_prod_lv <- ggplot(prod_lv, aes(year, value, colour = country)) +
  geom_line(linewidth = 0.8) +
  geom_point(data = prod_lv_latest, size = 1.8) +
  geom_text(data = prod_lv_latest, aes(label = sprintf("$%.0f", value)),
            hjust = -0.3, size = 3.2, family = "source_sans", show.legend = FALSE) +
  scale_colour_manual(values = au_us_colours) +
  scale_y_continuous(labels = label_dollar(accuracy = 1), limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(breaks = seq(1990, 2030, 5), expand = expansion(mult = c(0.01, 0.06))) +
  labs(title = "GDP per hour worked, Australia and the United States",
       subtitle = paste0("Whole economy, US dollars at constant ", prod_lv$base[1],
                         " purchasing power parities"),
       x = NULL, y = NULL,
       caption = paste("Source: OECD Productivity Database (GDP per hour worked).",
                       "\nPPPs adjust for price differences, so the same in A$ and US$ views.")) +
  theme_csis()

save_plot(p_prod_lv, "productivity_level_au_us.png")

# Real GDP per person. Australia: ABS GDP per capita, chain volume measures,
# seasonally adjusted (5206.0 Table 1). US: BEA real GDP (A191RX) over
# population (B230RC, NIPA Table 2.1, thousands, midperiod).
pc_au <- read_abs("5206.0", tables = 1) |>
  filter(series == "GDP per capita: Chain volume measures ;",
         series_type == "Seasonally Adjusted") |>
  select(date, value) |> mutate(country = "Australia")
stopifnot(nrow(pc_au) > 0)
pc_us <- inner_join(bea_nipa_q("A191RX"), bea_nipa_q("B230RC"), by = "date",
                    suffix = c("_gdp", "_pop")) |>
  transmute(date, value = value_gdp / value_pop, country = "United States")
pc_idx <- index_to(bind_rows(pc_au, pc_us), gdp_base_year)

p_gdp_pc <- pc_idx |>
  filter(date >= as.Date("2000-01-01")) |>
  ggplot(aes(date, index, colour = country)) +
  geom_hline(yintercept = 100, colour = csis_colours[["ltgrey"]],
             linewidth = 0.4, linetype = "dashed") +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = au_us_colours) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  scale_x_date(breaks = seq(as.Date("2000-01-01"), Sys.Date(), by = "5 years"), date_labels = "%Y") +
  labs(title = "Real GDP per person, Australia and the United States",
       subtitle = paste0("Index, ", gdp_base_year, " = 100. Chain volume / chained dollars, ",
                         "seasonally adjusted, quarterly."),
       x = NULL, y = NULL,
       caption = paste("Sources: ABS, Australian National Accounts (cat. no. 5206.0), Table 1;",
                       "US BEA, NIPA Tables 1.1.6 and 2.1 (real GDP over population).")) +
  theme_csis()

save_plot(p_gdp_pc, "gdp_per_capita_au_us.png")


# 3. GVA by industry ------------------------------------------------------

# 5206.0 Table 45: GVA by industry, current prices, quarterly. Keeps the 19
# ANZSIC divisions, i.e. series labelled with a division letter, e.g.
# "Mining (B) ;". Ownership of dwellings and the all-industries total have no
# division letter, so they drop out here.
gva <- read_abs("5206.0", tables = 45) |>
  filter(series_type == "Seasonally Adjusted",
         grepl("\\([A-S]\\)", series),
         !grepl("\\(\\d{2,}\\)", series),       # no subdivisions
         !is.na(value)) |>
  transmute(date,
            industry = trimws(sub("\\s*\\([A-S]\\).*$", "", series)),
            value) |>
  summarise(value = max(value), .by = c(date, industry))

stopifnot(n_distinct(gva$industry) == 19)

# Levels: the latest financial year from the annual national accounts
# (5204.0 Table 5, current prices), so one quarter's seasonality can't move
# the ranking. The quarterly data above feed the contributions chart only.
gva_fy <- read_abs("5204.0", tables = 5) |>
  filter(grepl("\\([A-S]\\) ;  Current prices ;$", series), !is.na(value)) |>
  transmute(date, industry = trimws(sub("\\s*\\([A-S]\\).*$", "", series)), value)
gva_fy_all <- read_abs("5204.0", tables = 5) |>
  filter(series == "All industries ;  Current prices ;", !is.na(value)) |>
  select(date, total = value)
stopifnot(n_distinct(gva_fy$industry) == 19)

latest_fy  <- max(gva_fy$date)                         # June of the year it ends
fy_name    <- function(d) paste0(year(d) - 1, "-", format(d, "%y"))

level_ind <- gva_fy |>
  filter(date == latest_fy) |>
  left_join(gva_fy_all, by = "date") |>
  mutate(share = 100 * value / total) |>               # share of all-industry GVA
  arrange(desc(value)) |>
  mutate(industry = factor(industry, levels = industry))

stopifnot(nrow(level_ind) == 19)
print(head(level_ind, 3))   # check the title still holds

# Drawn twice, in A$ and US$ (at the financial year's average rate).
make_level_ind <- function(cur) {
  d <- level_ind |> mutate(value = if (cur == "USD") value * fx_financial_year(year(latest_fy)) else value)
  ggplot(d, aes(value, industry)) +
  geom_col(fill = csis_colours[["navy"]], width = 0.75) +
  geom_text(aes(label = sprintf("%s%sbn (%.1f%%)", cur_sym[[cur]],
                                format(round(value / 1000), big.mark = ","), share)),
            hjust = -0.12, size = 2.9, family = "source_sans",
            colour = csis_colours[["grey"]]) +
  scale_x_continuous(labels = label_number(scale = 1e-3, prefix = cur_sym[[cur]], suffix = "bn"),
                     expand = expansion(mult = c(0, 0.20))) +
  scale_y_discrete(limits = rev) +   # largest bar at the top
  labs(title = "Australian gross value added by industry",
       subtitle = paste0("Gross value added by ANZSIC division, current prices, ", cur_sym[[cur]], ", ",
                         fy_name(latest_fy), " financial year. Share of all-industry GVA in brackets."),
       x = NULL, y = NULL,
       caption = paste("Source: ABS, Australian System of National Accounts (cat. no. 5204.0), Table 5.",
                       "\nOwnership of dwellings (the rest of GVA) is not an ANZSIC division and is not shown.",
                       if (cur == "USD") fx_note[["USD"]] else "")) +
  theme_csis() +
  theme_bars
}

save_both(make_level_ind, "gva_level_industry.png", save_plot, width = 8, height = 7)

# Industry shares, Australia vs US. US: BEA GDP by industry, value added
# (TVA105-A, current dollars, calendar years). Sectors are matched as closely
# as the two classifications allow (ANZSIC divisions and NAICS sectors).
# Shares are of total value added in each country (US value added sums to
# GDP; Australia's to GVA at basic prices, i.e. before taxes on products).
bea_va <- fetch_us("https://apps.bea.gov/industry/Release/XLS/GDPxInd/ValueAdded.xlsx",
                   file.path(RAW_DIR, "bea_ValueAdded.xlsx"))
us_va_raw <- readxl::read_excel(bea_va, sheet = "TVA105-A", col_names = FALSE,
                                .name_repair = "minimal")
hdr <- which(us_va_raw[[1]] == "Line")
us_va_yr <- max(suppressWarnings(as.integer(unlist(us_va_raw[hdr, -(1:2)]))), na.rm = TRUE)
us_va <- data.frame(line  = suppressWarnings(as.integer(us_va_raw[[1]])),
                    name  = us_va_raw[[2]],
                    value = suppressWarnings(as.numeric(
                      us_va_raw[[which(unlist(us_va_raw[hdr, ]) == as.character(us_va_yr))]]))) |>
  filter(!is.na(line))
us_line <- function(...) sum(us_va$value[us_va$line %in% c(...)])
stopifnot("BEA value added table layout changed" =
            us_va$name[us_va$line == 89] == "Government" & us_va$name[us_va$line == 62] == "Housing")

# label, ANZSIC division (or "dwellings"), BEA lines
sector_map <- list(
  c("Agriculture, forestry and fishing", "Agriculture, forestry and fishing", 3),
  c("Mining",                            "Mining", 6),
  c("Manufacturing",                     "Manufacturing", 12),
  c("Electricity, gas and water",        "Electricity, gas, water and waste services", 10),
  c("Construction",                      "Construction", 11),
  c("Wholesale trade",                   "Wholesale trade", 34),
  c("Retail trade",                      "Retail trade", 35),
  c("Accommodation and food",            "Accommodation and food services", 85),
  c("Transport and warehousing",         "Transport, postal and warehousing", 40),
  c("Information and telecommunications","Information media and telecommunications", 49),
  c("Finance and insurance",             "Financial and insurance services", 55),
  c("Real estate and rental",            "Rental, hiring and real estate services", c(63, 64)),
  c("Housing (ownership of dwellings)",  "dwellings", 62),
  c("Professional services",             "Professional, scientific and technical services", c(66, 70)),
  c("Administrative and support",        "Administrative and support services", 71),
  c("Government / public administration","Public administration and safety", 89),
  c("Education",                         "Education and training", 75),
  c("Health care and social assistance", "Health care and social assistance", 76),
  c("Arts and recreation",               "Arts and recreation services", 82),
  c("Other services",                    "Other services", 88)
)
au_dw <- read_abs("5204.0", tables = 5) |>
  filter(series == "Ownership of dwellings ;  Current prices ;", date == latest_fy) |> pull(value)
au_total <- gva_fy_all$total[gva_fy_all$date == latest_fy]
us_total <- us_line(1)
va_share <- bind_rows(lapply(sector_map, function(s) {
  au_v <- if (s[2] == "dwellings") au_dw else level_ind$value[level_ind$industry == s[2]]
  data.frame(sector = s[1],
             country = c("Australia", "United States"),
             share = c(100 * au_v / au_total,
                       100 * us_line(as.integer(s[-(1:2)])) / us_total))
}))
stopifnot(abs(sum(va_share$share[va_share$country == "Australia"]) - 100) < 1,
          abs(sum(va_share$share[va_share$country == "United States"]) - 100) < 1)

va_order <- va_share |> filter(country == "Australia") |> arrange(share) |> pull(sector)
p_va_share <- va_share |>
  mutate(sector = factor(sector, levels = va_order),
         country = factor(country, levels = c("United States", "Australia"))) |>
  ggplot(aes(share, sector, fill = country)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  scale_fill_manual(values = au_us_colours, breaks = c("Australia", "United States")) +
  scale_x_continuous(labels = label_percent(scale = 1, accuracy = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Industry shares of the economy, Australia and the United States",
       subtitle = paste0("Share of total value added, current prices. Australia ", fy_name(latest_fy),
                         " financial year; United States ", us_va_yr, "."),
       x = NULL, y = NULL,
       caption = paste0("Sources: ABS, Australian System of National Accounts (cat. no. 5204.0), Table 5; ",
                        "US BEA, GDP by Industry (value added, TVA105).",
                        "
Sectors matched across ANZSIC and NAICS. US government-run schools and hospitals are in",
                        "
Government; in Australia they are in Education and Health.",
                        "\nUS professional services include management of companies; US utilities exclude waste services.")) +
  theme_csis() +
  theme_bars

save_plot(p_va_share, "gva_share_au_us.png", width = 8, height = 7.5)

# Contributions to through-the-year growth: each group's 4-quarter change over
# the total 4 quarters earlier, so the contributions add up to total growth.
gva_groups <- gva |>
  mutate(group = case_when(
    grepl("Mining", industry)                                              ~ "Mining",
    grepl("Manufact|Construct|Agricult|Electricity", industry)             ~ "Goods-producing",
    grepl("Financ|Profession|Rental|Administrative|Information", industry) ~ "Business services",
    grepl("Health|Education|Public admin", industry)                       ~ "Public & social",
    TRUE                                                                   ~ "Other services"
  )) |>
  group_by(date, group) |>
  summarise(value = sum(value), .groups = "drop")

gva_total <- gva_groups |>
  group_by(date) |>
  summarise(total = sum(value), .groups = "drop")

contrib <- gva_groups |>
  left_join(gva_total, by = "date") |>
  arrange(group, date) |>
  group_by(group) |>
  mutate(contribution = 100 * (value - lag(value, 4)) / lag(total, 4)) |>
  ungroup() |>
  filter(!is.na(contribution)) |>
  mutate(group = factor(group, levels = c("Mining", "Goods-producing", "Business services",
                                          "Public & social", "Other services")))

contrib_total <- contrib |>
  group_by(date) |>
  summarise(total = sum(contribution), .groups = "drop")

contrib_start <- as.Date("2015-01-01")

p_contrib <- contrib |>
  filter(date >= contrib_start) |>
  ggplot(aes(date, contribution, fill = group)) +
  geom_col(width = 80) +
  geom_line(data = filter(contrib_total, date >= contrib_start),
            aes(date, total), inherit.aes = FALSE,
            colour = csis_colours[["navy"]], linewidth = 0.6) +
  geom_hline(yintercept = 0, colour = csis_colours[["grey"]], linewidth = 0.4) +
  scale_fill_csis() +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 1)) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(title = "Contributions to nominal GVA growth",
       subtitle = "Through the year, current prices, seasonally adjusted. Navy line shows total.",
       x = NULL, y = NULL,
       caption = paste("Source: ABS, Australian National Accounts (cat. no. 5206.0),",
                       "Table 45. Excludes ownership of dwellings.")) +
  theme_csis()

save_plot(p_contrib, "gva_p_contrib.png", width = 9, height = 5)


# 5. Relative size --------------------------------------------------------

# One source for Australia's GDP in all three size charts: ABS current-price
# GDP (5206.0, original, the four quarters of the calendar year), converted
# at the RBA's average A$/US$ rate for the year. The US is from BEA
# (A191RC, current dollars, annual average of the quarterly annual rates).
au_gdp_cy <- read_abs_series("A2302467A") |>
  mutate(year = year(date)) |>
  group_by(year) |>
  filter(n() == 4) |>
  summarise(aud_bn = sum(value) / 1000, .groups = "drop") |>
  mutate(usd_tn = aud_bn * fx_calendar_year(year) / 1000)
us_gdp_cy <- bea_nipa_q("A191RC") |>
  mutate(year = year(date)) |>
  group_by(year) |>
  filter(n() == 4) |>
  summarise(usd_tn = mean(value) / 1e6, .groups = "drop")
au_gdp_usd_tn <- function(yr) au_gdp_cy$usd_tn[au_gdp_cy$year == yr]
GDP_SRC_AU <- "ABS (cat. no. 5206.0), converted at the RBA's average rate for the year"

# GDP per capita at PPP (World Bank): international dollars, no market rate
wb <- WDI(country = c("AU", "US"),
          indicator = c(pcap_ppp = "NY.GDP.PCAP.PP.CD"),   # GDP per capita, PPP
          extra = FALSE)

size_yr <- wb |>
  filter(!is.na(pcap_ppp)) |>
  count(year) |>
  filter(n == 2, year %in% au_gdp_cy$year, year %in% us_gdp_cy$year) |>
  pull(year) |>
  max()

size <- wb |> filter(year == size_yr) |>
  mutate(gdp_usd = 1e12 * if_else(country == "Australia", au_gdp_usd_tn(size_yr),
                                  us_gdp_cy$usd_tn[us_gdp_cy$year == size_yr]))
stopifnot(nrow(size) == 2)

with(size, cat(sprintf("US is %.1fx Australia in aggregate, %.2fx per capita (PPP)\n",
                       gdp_usd[country == "United States"] / gdp_usd[country == "Australia"],
                       pcap_ppp[country == "United States"] / pcap_ppp[country == "Australia"])))

size_bar <- function(d, label_fmt, x_labels, subtitle) {
  d |>
    mutate(country = reorder(country, v)) |>
    ggplot(aes(v, country, fill = country)) +
    geom_col(width = 0.6, show.legend = FALSE) +
    geom_text(aes(label = sprintf(label_fmt, v)), hjust = -0.2,
              size = 3.2, family = "source_sans", colour = csis_colours[["grey"]]) +
    scale_fill_manual(values = au_us_colours) +
    scale_x_continuous(labels = x_labels, expand = expansion(mult = c(0, 0.25))) +
    labs(subtitle = subtitle, x = NULL, y = NULL) +
    theme_csis() +
    theme_bars
}

# Drawn twice. The source is in US$; the A$ version converts aggregate GDP at
# the year's average rate. GDP per capita at PPP is in international dollars,
# which have no market exchange rate, so that panel is the same in both.
make_size <- function(cur) {
  conv  <- if (cur == "AUD") 1 / fx_calendar_year(size_yr) else 1
  sym   <- cur_sym[[cur]]
  p_agg  <- size_bar(size |> mutate(v = gdp_usd * conv / 1e12), paste0(sym, "%.1ftn"),
                     label_number(prefix = sym, suffix = "tn"), paste0("Aggregate GDP, current ", sym))
  p_pcap <- size_bar(size |> mutate(v = pcap_ppp / 1e3), "$%.0fk",
                     label_number(prefix = "$", suffix = "k"), "GDP per capita, PPP (international dollars)")

  (p_agg | p_pcap) +
    plot_annotation(
      title   = "Aggregate GDP and GDP per capita, Australia and the United States",
      caption = paste0("Sources: aggregate GDP, Australia ", GDP_SRC_AU, "; US BEA.",
                       "
GDP per capita, World Bank World Development Indicators. ", size_yr, ".",
                       if (cur == "AUD") fx_note_aud_from_usd else ""),
      theme   = theme_csis()
    )
}

save_both(make_size, "au_us_size.png", save_plot, width = 10, height = 3.8)

# Australia against US states. BEA regional accounts, SAGDP2 (GDP by state,
# millions of current dollars, annual), line 1 = all industry total.
sagdp_zip <- fetch_us("https://apps.bea.gov/regional/zip/SAGDP.zip",
                      file.path(RAW_DIR, "bea_SAGDP.zip"))
sagdp_csv <- grep("^SAGDP2__ALL_AREAS_.*\\.csv$", utils::unzip(sagdp_zip, list = TRUE)$Name, value = TRUE)
stopifnot("SAGDP2 all-areas file not found in SAGDP.zip" = length(sagdp_csv) == 1)
sagdp <- utils::read.csv(unz(sagdp_zip, sagdp_csv), colClasses = "character", check.names = FALSE)

state_gdp <- sagdp |>
  filter(trimws(LineCode) == "1", GeoName %in% state.name) |>
  select(GeoName, matches("^[0-9]{4}$")) |>
  pivot_longer(-GeoName, names_to = "year", values_to = "gdp") |>
  transmute(state = GeoName, year = as.integer(year),
            gdp = suppressWarnings(as.numeric(gdp))) |>
  filter(!is.na(gdp))
stopifnot(nrow(state_gdp) > 0)
message("States retrieved: ", n_distinct(state_gdp$state), " of 50")

states_yr <- max(state_gdp$year)

au_yr  <- min(states_yr, max(au_gdp_cy$year))
au_val <- au_gdp_usd_tn(au_yr)

comb <- state_gdp |>
  filter(year == states_yr) |>
  transmute(name = state, value = gdp / 1e6, is_au = FALSE) |>   # $m -> $tn
  bind_rows(data.frame(name = "AUSTRALIA", value = au_val, is_au = TRUE)) |>
  arrange(desc(value)) |>
  mutate(rank = row_number())

nearest <- comb |>
  filter(!is_au) |>
  slice_min(abs(value - au_val), n = 1) |>
  pull(name)
message(sprintf("Australia %.2f tn (%d), closest state: %s", au_val, au_yr, nearest))

states_plot <- comb |>
  filter(rank <= 15 | is_au) |>
  arrange(value) |>
  mutate(name = factor(name, levels = name))

# Drawn twice; the source is in US$, the A$ version uses the year's average rate.
make_states <- function(cur) {
  conv <- if (cur == "AUD") 1 / fx_calendar_year(states_yr) else 1
  sym  <- cur_sym[[cur]]
  ggplot(states_plot |> mutate(value = value * conv), aes(value, name, fill = is_au)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = sprintf("%s%.2ftn", sym, value)), hjust = -0.15,
            size = 2.9, family = "source_sans", colour = csis_colours[["grey"]]) +
  scale_fill_manual(values = c(`TRUE`  = csis_colours[["navy"]],
                               `FALSE` = "#E8A9AB"), guide = "none") +   # US states: light red
  scale_x_continuous(labels = label_number(prefix = sym, suffix = "tn", accuracy = 0.5),
                     expand = expansion(mult = c(0, 0.18))) +
  labs(title = "GDP of Australia and US states",
       subtitle = paste0("GDP, current ", sym, ", ", states_yr, ". Australia in navy, the 15 largest US states in red."),
       x = NULL, y = NULL,
       caption = paste0("Sources: US BEA, GDP by state (SAGDP2).
Australia, ", GDP_SRC_AU, ".",
                        if (au_yr != states_yr) paste0(" Australia ", au_yr, ".") else "",
                        if (cur == "AUD") fx_note_aud_from_usd else "")) +
  theme_csis() +
  theme_bars
}

save_both(make_states, "au_vs_us_states.png", save_plot, width = 8, height = 6)

# Australia's GDP rank among the 38 OECD members (World Bank, current US$)
oecd_members <- c(
  AU = "Australia", AT = "Austria", BE = "Belgium", CA = "Canada",
  CL = "Chile", CO = "Colombia", CR = "Costa Rica", CZ = "Czechia",
  DK = "Denmark", EE = "Estonia", FI = "Finland", FR = "France",
  DE = "Germany", GR = "Greece", HU = "Hungary", IS = "Iceland",
  IE = "Ireland", IL = "Israel", IT = "Italy", JP = "Japan",
  KR = "Korea", LV = "Latvia", LT = "Lithuania", LU = "Luxembourg",
  MX = "Mexico", NL = "Netherlands", NZ = "New Zealand", NO = "Norway",
  PL = "Poland", PT = "Portugal", SK = "Slovak Republic", SI = "Slovenia",
  ES = "Spain", SE = "Sweden", CH = "Switzerland", TR = "Türkiye",
  GB = "United Kingdom", US = "United States"
)

wb_oecd <- WDI(country = "all", indicator = c(gdp_usd = "NY.GDP.MKTP.CD"),
               start = 2020, end = year(Sys.Date()), extra = FALSE) |>
  filter(iso2c %in% names(oecd_members), !is.na(gdp_usd)) |>
  transmute(code    = iso2c,
            country = unname(oecd_members[iso2c]),
            year    = as.integer(year),
            gdp_usd = as.numeric(gdp_usd)) |>
  distinct(code, year, .keep_all = TRUE)

# latest year with data for every member
full_years <- wb_oecd |>
  count(year) |>
  filter(n == length(oecd_members)) |>
  pull(year)
if (!length(full_years)) stop("No year has GDP for all 38 OECD members; move the start year back.")
oecd_yr <- max(full_years)

# Australia from the ABS, as in the other two charts (World Bank for the rest)
au_oecd_wb <- wb_oecd$gdp_usd[wb_oecd$code == "AU" & wb_oecd$year == oecd_yr]
if (length(au_gdp_usd_tn(oecd_yr)))
  wb_oecd$gdp_usd[wb_oecd$code == "AU" & wb_oecd$year == oecd_yr] <- 1e12 * au_gdp_usd_tn(oecd_yr)
message(sprintf("Australia %d: ABS/RBA US$%.3ftn vs World Bank US$%.3ftn",
                oecd_yr, au_gdp_usd_tn(oecd_yr), au_oecd_wb / 1e12))

ranked <- wb_oecd |>
  filter(year == oecd_yr) |>
  arrange(desc(gdp_usd)) |>
  mutate(rank = row_number(), is_au = country == "Australia",
         who = case_when(is_au ~ "au", country == "United States" ~ "us", TRUE ~ "other"))

stopifnot(nrow(ranked) == length(oecd_members), sum(ranked$is_au) == 1)

au_rank <- ranked$rank[ranked$is_au]

ordinal <- function(n) {
  suffix <- if (n %% 100 %in% 11:13) "th"
  else switch(as.character(n %% 10), "1" = "st", "2" = "nd", "3" = "rd", "th")
  paste0(n, suffix)
}

message(sprintf("Australia is the %s-largest OECD economy in %d (US$%.2f tn)",
                ordinal(au_rank), oecd_yr, ranked$gdp_usd[ranked$is_au] / 1e12))

oecd_plot <- ranked |>
  filter(rank <= 15 | is_au) |>
  arrange(gdp_usd) |>
  mutate(country_label = factor(paste0(rank, ". ", country),
                                levels = paste0(rank, ". ", country)))

# Drawn twice; the source is in US$, the A$ version uses the year's average
# rate (one rate for every country, so the ranking is unchanged).
make_oecd_rank <- function(cur) {
  conv <- if (cur == "AUD") 1 / fx_calendar_year(oecd_yr) else 1
  sym  <- cur_sym[[cur]]
  d <- oecd_plot |> mutate(v = gdp_usd * conv / 1e12)
  ggplot(d, aes(v, country_label, fill = who)) +
  geom_col(width = 0.72) +
  geom_text(data = filter(d, who != "other"),
            aes(label = paste0(sym, number(v, accuracy = 0.01), "tn")),
            hjust = -0.15, size = 3.1, family = "source_sans",
            colour = csis_colours[["grey"]]) +
  scale_fill_manual(values = c(au = csis_colours[["navy"]], us = csis_colours[["red"]],
                               other = csis_colours[["ltgrey"]]), guide = "none") +
  scale_x_continuous(labels = label_dollar(prefix = sym, suffix = "tn", accuracy = 1),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Largest OECD economies by nominal GDP",
       subtitle = paste0("Nominal GDP, current ", sym, ", ", oecd_yr,
                         ". Ranking is among all 38 OECD members."),
       x = NULL, y = NULL,
       caption = paste0("Sources: World Bank World Development Indicators (GDP, current US$); OECD membership list.",
                        "\nAustralia: ", GDP_SRC_AU, ", as in the other size charts.",
                       if (cur == "AUD") fx_note_aud_from_usd else "")) +
  theme_csis() +
  theme_bars
}

save_both(make_oecd_rank, "oecd_gdp_rank_australia.png", save_plot, width = 8, height = 6.2)

# Takeaways under each chart ---------------------------------------------

local({
  ec <- function(file) file.path("Economy", file)
  # Australia vs the nearest US state, e.g. "about the size of Florida"
  near <- comb |> filter(!is_au) |> slice_min(abs(value - au_val), n = 1)
  au_r <- comb$rank[comb$is_au]
  line_states <- function(cur) {
    k <- if (cur == "AUD") 1 / fx_calendar_year(states_yr) else 1
    sprintf("Australia's economy (%s) is about the size of %s's (%s); it would rank %s among US states.",
            sprintf("%s%.2ftn", cur_sym[[cur]], au_val * k), near$name,
            sprintf("%s%.2ftn", cur_sym[[cur]], near$value * k), ordinal(au_r))
  }
  takeaway(ec("au_vs_us_states.png"), line_states("AUD"), line_states("USD"))

  r <- with(size, c(agg = gdp_usd[country == "United States"] / gdp_usd[country == "Australia"],
                    pc  = pcap_ppp[country == "United States"] / pcap_ppp[country == "Australia"]))
  takeaway(ec("au_us_size.png"), sprintf(
    "The US economy is %.0f times the size of Australia's; GDP per person is %.0f%% higher in the US (%d, at purchasing power parity).",
    r[["agg"]], 100 * (r[["pc"]] - 1), size_yr))

  takeaway(ec("oecd_gdp_rank_australia.png"), sprintf(
    "Australia is the %s-largest of the 38 OECD economies (%d).", ordinal(au_rank), oecd_yr))

  g <- gdp_idx |> group_by(country) |> slice_max(date, n = 1) |> ungroup()
  takeaway(ec("gdp_au_us.png"), sprintf(
    "Since %d, real GDP has grown %.0f%% in Australia and %.0f%% in the United States.",
    gdp_base_year, g$index[g$country == "Australia"] - 100, g$index[g$country == "United States"] - 100))

  pc <- pc_idx |> group_by(country) |> slice_max(date, n = 1) |> ungroup()
  takeaway(ec("gdp_per_capita_au_us.png"), sprintf(
    "Per person, real GDP has grown %.0f%% in Australia and %.0f%% in the United States since %d.",
    pc$index[pc$country == "Australia"] - 100, pc$index[pc$country == "United States"] - 100, gdp_base_year))

  pr <- prod |> group_by(country) |> slice_max(date, n = 1) |> ungroup()
  takeaway(ec("productivity_au_us.png"), sprintf(
    "Output per hour has risen %.0f%% in the Australian market sector and %.0f%% in the US business sector since %d.",
    pr$index[pr$country == "Australia"] - 100, pr$index[pr$country == "United States"] - 100, prod_base_year))

  lv <- prod_lv_latest
  takeaway(ec("productivity_level_au_us.png"), sprintf(
    "An hour of work produced about $%.0f of GDP in Australia and $%.0f in the United States in %d (constant PPP US$), a gap of %.0f%%.",
    lv$value[lv$country == "Australia"], lv$value[lv$country == "United States"], max(lv$year),
    100 * (lv$value[lv$country == "United States"] / lv$value[lv$country == "Australia"] - 1)))

  top <- level_ind[1, ]
  line_gva <- function(cur) sprintf(
    "%s is Australia's largest industry, at %s or %.0f%% of value added in %s.",
    top$industry, fmt_bn((top$value / 1000) * if (cur == "USD") fx_financial_year(year(latest_fy)) else 1,
                         cur_sym[[cur]], 0),
    top$share, fy_name(latest_fy))
  takeaway(ec("gva_level_industry.png"), line_gva("AUD"), line_gva("USD"))

  m <- va_share |> tidyr::pivot_wider(names_from = country, values_from = share)
  mi <- m[m$sector == "Mining", ]
  takeaway(ec("gva_share_au_us.png"), sprintf(
    "Mining makes up %.0f%% of Australia's economy against %.1f%% of the US economy.",
    mi$Australia, mi$`United States`))
})
