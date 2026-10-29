rm(list = ls())
# The dashboard folder is set once, in section 1 of '1. Refresh all data.R'.
# To run this script on its own, run that section first.
if (Sys.getenv("DASHBOARD_DIR") == "")
  stop("Dashboard folder not set. Run section 1 of '1. Refresh all data.R' first.")
setwd(file.path(Sys.getenv("DASHBOARD_DIR"), "Energy"))
source(file.path(Sys.getenv("DASHBOARD_DIR"), "0. Exchange rates.R"))   # A$/US$ toggle
source(file.path(Sys.getenv("DASHBOARD_DIR"), "0. Takeaways.R"))       # line under each chart

# ==============================================================================
# Australian energy: five charts for a US policy audience
#
# Data: DCCEEW (2026), Australian Energy Statistics, Table O
#   Electricity generation by fuel type, 2024-25 and calendar 2025
#   https://www.energy.gov.au/energy-data/australian-energy-statistics
#
# Workbook structure (verified against the file):
#   - Sheets "AUS FY", "NSW FY", ... "NT FY"   : 1989-90 to 2024-25
#   - Sheets "AUS CY", "NSW CY", ... "NT CY"   : 2015 to 2025 (est.)
#   - Sheet  "State summary 2025"              : fuels x states, calendar 2025
#   - Header is row 5, so skip = 4. Units are GWh throughout.
#   - Fuel names differ between FY and CY sheets: FY splits bioenergy into
#     "Bagasse, wood" + "Biogas" and carries an "Other [note a]" row; CY
#     collapses these to "Biomass". Handled in recode_fuel() below.
# ==============================================================================

library(data.table)   # for export_chart_csv(); loaded first so dplyr keeps first/last/between
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(forcats)
library(readr)

# Charts here use default fonts. If an earlier script in this R session turned
# on showtext (set for a different dpi), switch it off or text renders at the
# wrong size and titles get cut off.
if ("showtext" %in% loadedNamespaces()) showtext::showtext_auto(FALSE)

# Table O is downloaded the first time and reused after that. DCCEEW publishes
# a new edition each June at a new address: when it does, paste the new .xlsx
# link (from the Table O page linked above) into TABLE_O_URL, and update the
# years in the chart titles and the validation figures below.
TABLE_O_URL <- paste0("https://www.energy.gov.au/sites/default/files/2026-06/",
                      "table-o-electricity-generation-by-fuel%20type-2024-25-and-2025.xlsx")
path <- utils::URLdecode(basename(TABLE_O_URL))

if (!file.exists(path)) {
  message("Downloading ", path)
  httr2::request(TABLE_O_URL) |>
    httr2::req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64)") |>   # site blocks requests without one
    httr2::req_timeout(120) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform(path = path)
}
stopifnot(file.exists(path))


# ------------------------------------------------------------------------------
# 0. Chart CSV export helper
#
# Writes a metadata block (title, labels, styling) followed by the chart data.
#   extra     - grouping columns to keep (series / facet variables)
#   value_var - the numeric column to format; defaults to the y variable
#               (horizontal bar charts carry their value on x)
# ------------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

export_chart_csv <- function(p, file,
                             y_format = c("number", "percent"),
                             chart_type = "Line with point markers",
                             notes = NULL,
                             extra = NULL,
                             value_var = NULL) {
  y_format <- match.arg(y_format)
  
  xvar <- rlang::as_label(p$mapping$x)
  yvar <- rlang::as_label(p$mapping$y)
  value_var <- value_var %||% yvar
  
  dat <- as.data.table(p$data)[, c(extra, xvar, yvar), with = FALSE]
  setorderv(dat, c(extra, xvar))
  
  if (y_format == "percent") {
    dat[[value_var]] <- round(dat[[value_var]] * 100, 1)
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
              as.character(if (length(p$layers) >= 2) p$layers[[2]]$aes_params$size %||% "" else ""),
              "Bold",
              notes %||% "")
  )
  
  fwrite(meta, file, col.names = FALSE, bom = TRUE)   # BOM so Excel reads UTF-8 cleanly
  cat("\n", file = file, append = TRUE)
  fwrite(dat, file, append = TRUE, col.names = TRUE)
  invisible(file)
}


# ------------------------------------------------------------------------------
# 1. Parsing
# ------------------------------------------------------------------------------

STATES <- c("AUS", "NSW", "VIC", "QLD", "WA", "SA", "TAS", "NT")

DROP_ROWS <- c("Total non-renewable", "Total renewable", "Total",
               "Per cent renewable generation")

RENEWABLES <- c("Hydro", "Wind", "Rooftop solar", "Utility solar", "Bioenergy")

recode_fuel <- function(x) {
  x <- str_trim(str_remove(x, "\\s*\\[note [a-z]\\]$"))
  recode(x,
         "Small-scale solar PV" = "Rooftop solar",
         "Large-scale solar PV" = "Utility solar",
         "Natural gas"          = "Gas",
         "Oil products"         = "Oil",
         "Bagasse, wood"        = "Bioenergy",
         "Biogas"               = "Bioenergy",
         "Biomass"              = "Bioenergy",
         .default = x)
}

# Reads one region sheet. `basis` is "FY" or "CY".
read_region <- function(region, basis) {
  read_excel(path, sheet = paste(region, basis), skip = 4) |>
    rename(fuel = 1) |>
    filter(!is.na(fuel), !str_starts(fuel, "\\["), !fuel %in% DROP_ROWS) |>
    pivot_longer(-fuel, names_to = "period", values_to = "gwh") |>
    mutate(
      region = region,
      basis  = basis,
      fuel   = recode_fuel(fuel),
      gwh    = suppressWarnings(as.numeric(gwh)),
      year   = as.integer(str_extract(period, "^\\d{4}"))
    ) |>
    filter(!is.na(gwh)) |>
    # FY splits bioenergy across two rows; collapse so FY and CY are comparable.
    group_by(region, basis, period, year, fuel) |>
    summarise(gwh = sum(gwh), .groups = "drop")
}

gen_fy <- bind_rows(lapply(STATES, read_region, basis = "FY"))
gen_cy <- bind_rows(lapply(STATES, read_region, basis = "CY"))

state_2025 <- read_excel(path, sheet = "State summary 2025", skip = 4) |>
  rename(fuel = 1) |>
  filter(!is.na(fuel), !str_starts(fuel, "\\["), !fuel %in% DROP_ROWS) |>
  pivot_longer(-fuel, names_to = "region", values_to = "gwh") |>
  mutate(fuel = recode_fuel(fuel), gwh = suppressWarnings(as.numeric(gwh))) |>
  replace_na(list(gwh = 0)) |>
  group_by(region, fuel) |>
  summarise(gwh = sum(gwh), .groups = "drop")

# --- Validation gate: published 2025 figures are 286.8 TWh and 39.5% renewable
check <- gen_cy |>
  filter(region == "AUS", year == 2025) |>
  summarise(twh = sum(gwh) / 1000,
            renew_pct = 100 * sum(gwh[fuel %in% RENEWABLES]) / sum(gwh))
print(check)
stopifnot(abs(check$twh - 286.8) < 0.5, abs(check$renew_pct - 39.5) < 0.5)


# ------------------------------------------------------------------------------
# 2. Shared aesthetics
# ------------------------------------------------------------------------------

fuel_levels <- c("Black coal", "Brown coal", "Other", "Gas", "Oil",
                 "Bioenergy", "Hydro", "Wind", "Utility solar", "Rooftop solar",
                 "Geothermal")

fuel_cols <- c(
  "Black coal"    = "#2c2c2a", "Brown coal"    = "#5f5e5a", "Other" = "#b4b2a9",
  "Gas"           = "#eb6834", "Oil"           = "#993c1d",
  "Bioenergy"     = "#008300", "Hydro"         = "#2a78d6", "Wind" = "#1baf7a",
  "Utility solar" = "#fac775", "Rooftop solar" = "#eda100",
  "Geothermal"    = "#4a3aa7"
)

theme_csis <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.title            = element_text(face = "bold", size = 13),
      plot.subtitle         = element_text(colour = "grey35", size = 10),
      plot.caption          = element_text(colour = "grey45", size = 8, hjust = 0),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      panel.grid.minor      = element_blank(),
      panel.grid.major.x    = element_blank(),
      legend.position       = "bottom",
      legend.title          = element_blank(),
      legend.key.size       = unit(0.4, "cm")
    )
}

SRC <- "Source: DCCEEW (2026), Australian Energy Statistics, Table O."

as_fuel <- function(x) factor(x, levels = fuel_levels)

# Colour list for the CSV notes, e.g. "Black coal #2c2c2a; Wind #1baf7a"
fuel_col_note <- function(fuels) {
  fuels <- intersect(fuel_levels, fuels)
  paste(sprintf("%s %s", fuels, fuel_cols[fuels]), collapse = "; ")
}

# ------------------------------------------------------------------------------
# 1b. US electricity generation (EIA), for the Australia vs US charts
# ------------------------------------------------------------------------------
# National, calendar years: EIA Monthly Energy Review Table 7.2a (net
# generation by source, all sectors) and Table 10.6 (solar, which splits
# utility-scale and small-scale solar). Small-scale (rooftop) solar is
# included so the US matches Australia's Table O, which includes rooftop PV.
# EIA estimates small-scale solar from 2014; earlier years are utility only.
# By state, calendar years: EIA Electric Power Annual state file (net
# generation by state and source). It has no small-scale solar.
# All free downloads, no API key. Kept in Energy/raw/eia/, refreshed weekly.

EIA_DIR <- file.path("raw", "eia")
dir.create(EIA_DIR, recursive = TRUE, showWarnings = FALSE)
eia_get <- function(url, file) {
  dest <- file.path(EIA_DIR, file)
  if (!file.exists(dest) || difftime(Sys.time(), file.mtime(dest), units = "days") > 7)
    utils::download.file(url, dest, mode = "wb", quiet = TRUE, headers = c(`User-Agent` = "Mozilla/5.0"))
  dest
}
mer_annual <- function(table) {
  f <- eia_get(paste0("https://www.eia.gov/totalenergy/data/browser/csv.php?tbl=", table),
               paste0("mer_", table, ".csv"))
  read_csv(f, show_col_types = FALSE, col_types = cols(.default = "c")) |>
    filter(substr(YYYYMM, 5, 6) == "13") |>                  # annual totals
    transmute(msn = MSN, year = as.integer(substr(YYYYMM, 1, 4)),
              gwh = suppressWarnings(as.numeric(Value)))     # million kWh = GWh
}

us_raw <- bind_rows(mer_annual("T07.02A"),
                    mer_annual("T10.06") |> filter(msn %in% c("SOT5PUS", "SOT7PUS"))) |>
  filter(year >= 1990) |>
  tidyr::pivot_wider(names_from = msn, values_from = gwh, values_fill = 0)

US_MAP <- c(CLETPUS = "Coal", NGETPUS = "Gas", PAETPUS = "Oil", OJETPUS = "Other",
            NUETPUS = "Nuclear", HVETPUS = "Hydro", WYETPUS = "Wind",
            WDETPUS = "Bioenergy", WSETPUS = "Bioenergy", GEETPUS = "Geothermal",
            SOT5PUS = "Utility solar", SOT7PUS = "Rooftop solar")
us_gen <- us_raw |>
  mutate(other_rest = ELETPUS - (CLETPUS + NGETPUS + PAETPUS + OJETPUS + NUETPUS + HVETPUS +
                                   HPETPUS + WYETPUS + WDETPUS + WSETPUS + GEETPUS + SOETPUS)) |>
  select(year, all_of(names(US_MAP)), other_rest) |>
  pivot_longer(-year, names_to = "msn", values_to = "gwh") |>
  mutate(fuel = if_else(msn == "other_rest", "Other", US_MAP[msn])) |>
  group_by(year, fuel) |> summarise(gwh = sum(pmax(gwh, 0)), .groups = "drop") |>
  mutate(country = "United States")
us_last <- max(us_gen$year)
stopifnot(n_distinct(us_gen$fuel) >= 10)

# Both countries in one set of fuel groups: Australia's black and brown coal
# become Coal; the US adds Nuclear.
cmp_levels <- c("Coal", "Other", "Gas", "Oil", "Nuclear", "Bioenergy", "Geothermal",
                "Hydro", "Wind", "Utility solar", "Rooftop solar")
cmp_cols <- c(fuel_cols[c("Other", "Gas", "Oil", "Bioenergy", "Geothermal", "Hydro", "Wind",
                          "Utility solar", "Rooftop solar")],
              Coal = "#2c2c2a", Nuclear = "#b8478c")
cmp_lab  <- c("Rooftop solar" = "Rooftop / small-scale solar")
CMP_RENEW <- c("Hydro", "Wind", "Utility solar", "Rooftop solar", "Bioenergy", "Geothermal")
to_cmp <- function(f) if_else(f %in% c("Black coal", "Brown coal"), "Coal", f)

au_fy_cmp <- gen_fy |> filter(region == "AUS") |>
  mutate(fuel = to_cmp(fuel)) |> group_by(year, fuel) |>
  summarise(gwh = sum(gwh), .groups = "drop") |> mutate(country = "Australia")
mix_ts <- bind_rows(au_fy_cmp, us_gen) |>
  tidyr::complete(tidyr::nesting(country, year), fuel, fill = list(gwh = 0)) |>   # no gaps in the areas
  group_by(country, year) |> mutate(share = gwh / sum(gwh)) |> ungroup() |>
  mutate(fuel = factor(fuel, levels = cmp_levels))

US_SRC <- paste0("US: EIA, Monthly Energy Review, Tables 7.2a and 10.6 (calendar years; small-scale solar ",
                 "estimated from 2014, so earlier US solar is utility-scale only).")


# ------------------------------------------------------------------------------
# 3. CHART 1 - generation mix over time, Australia and the US
#
# Shares of total generation, so the two very different-sized grids can be
# compared. Coal peaked around 2008-09 in Australia; in the US gas replaced
# coal while nuclear held steady.
# ------------------------------------------------------------------------------

p1 <- mix_ts |>
  mutate(country = recode(country,
                          Australia = "Australia (financial years, by the year they start)",
                          `United States` = "United States (calendar years)")) |>
  ggplot(aes(year, share, fill = fuel)) +
  geom_area(colour = "white", linewidth = 0.15) +
  facet_wrap(~country, ncol = 2) +
  scale_fill_manual(values = cmp_cols, labels = function(x) ifelse(x %in% names(cmp_lab), cmp_lab[x], x),
                    drop = TRUE) +
  scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(c(0, 0))) +
  scale_x_continuous(breaks = seq(1990, 2025, 10), expand = expansion(c(0, 0.02))) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(
    title    = "Electricity generation by fuel, Australia and the United States",
    subtitle = sprintf("Share of total generation, 1990 to %d. Australia from 1989-90 to 2024-25.", us_last),
    x = NULL, y = NULL,
    caption  = paste0("Sources: Australia: DCCEEW, Australian Energy Statistics, Table O (includes rooftop solar).\n",
                      US_SRC, "\nAustralia's black and brown coal are combined. US pumped storage is left out.")
  ) +
  theme_csis() +
  theme(strip.text = element_text(face = "bold", hjust = 0), panel.spacing.x = unit(1.5, "lines"))

ggsave("01_fuel_mix_timeseries.png", p1, width = 10, height = 5.5, dpi = 150, bg = "white")

export_chart_csv(p1, "01_fuel_mix_timeseries.csv",
                 y_format = "percent", chart_type = "Stacked area (100%), two panels",
                 extra = c("country", "fuel"),
                 notes = paste0("Shares of total generation. Australia year = start year of the financial year ",
                                "(1989 = 1989-90); US calendar years."))



# ------------------------------------------------------------------------------
# 4. CHART 2 - eight grids, not one
#
# So what: "the Australian grid" does not exist. WA is a separate market
# entirely. Any read on AUKUS basing at Henderson, critical-minerals
# processing, or data-centre siting has to be state-level. Bars are sorted by
# renewable share so the ranking is legible, which a choropleth cannot do.
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 5b. Generation mix by state: Australian states and the main US data centre
#     states on one chart
# ------------------------------------------------------------------------------
# Australia: Table O state summary, calendar 2025 (includes rooftop solar).
# US: EIA Electric Power Annual, net generation by state (all producers) plus
# EIA's estimate of small-scale solar by state (Table 3.21), so both sides
# include rooftop solar. The latest US state year is a year behind Australia's.
DC_STATES <- c(VA = "Virginia", TX = "Texas", CA = "California", OR = "Oregon",
               AZ = "Arizona", GA = "Georgia", OH = "Ohio", IL = "Illinois", "US-TOTAL" = "United States")
STATE_MAP <- c("Coal" = "Coal", "Natural Gas" = "Gas", "Petroleum" = "Oil", "Other Gases" = "Other",
               "Nuclear" = "Nuclear", "Hydroelectric Conventional" = "Hydro", "Wind" = "Wind",
               "Solar Thermal and Photovoltaic" = "Utility solar",
               "Wood and Wood Derived Fuels" = "Bioenergy", "Other Biomass" = "Bioenergy",
               "Geothermal" = "Geothermal", "Other" = "Other")
st_file <- eia_get("https://www.eia.gov/electricity/data/state/annual_generation_state.xls",
                   "annual_generation_state.xls")
us_states <- read_excel(st_file, skip = 1) |>
  rename(year = 1, state = 2, producer = 3, source = 4, mwh = 5) |>
  mutate(state = toupper(state)) |>                     # the US total is "US-TOTAL" in some years, "US-Total" in others
  filter(producer == "Total Electric Power Industry", state %in% names(DC_STATES),
         source %in% names(STATE_MAP)) |>
  mutate(year = as.integer(year), fuel = STATE_MAP[source], mwh = pmax(as.numeric(mwh), 0))
st_year <- max(us_states$year)

# Small-scale solar by state (thousand MWh = GWh): EPA Table 3.21 has the latest
# year and the year before; use the column for st_year.
ss_file <- eia_get("https://www.eia.gov/electricity/annual/xls/epa_03_21.xlsx", "epa_03_21.xlsx")
ss_raw <- suppressMessages(read_excel(ss_file, col_names = FALSE))
ss_hdr <- apply(ss_raw[3:6, ], 2, function(c) paste(na.omit(c), collapse = " | "))
ss_col <- which(grepl(paste0("Year ", st_year), ss_hdr) &
                  cumsum(grepl("Small Scale Generation", ss_hdr)) > 0)[1]
stopifnot("EIA Table 3.21 layout changed" = !is.na(ss_col))
ss_names <- c(DC_STATES[DC_STATES != "United States"], "US-TOTAL" = "U.S. Total")
us_small <- tibble(state = names(ss_names),
                   gwh = suppressWarnings(as.numeric(unlist(ss_raw[match(ss_names, ss_raw[[1]]), ss_col])))) |>
  mutate(fuel = "Rooftop solar")
stopifnot(!anyNA(us_small$gwh))

state_mix <- bind_rows(
  state_2025 |> filter(region != "AUS") |>
    transmute(group = "Australia
2025", place = region,
              fuel = to_cmp(fuel), gwh),
  state_2025 |> filter(region == "AUS") |>
    transmute(group = "Australia
2025", place = "Australia",
              fuel = to_cmp(fuel), gwh),
  us_states |> filter(year == st_year) |>
    transmute(group = sprintf("Main US data
centre states
%d", st_year),
              place = DC_STATES[state], fuel, gwh = mwh / 1000),
  us_small |> transmute(group = sprintf("Main US data
centre states
%d", st_year),
                        place = DC_STATES[state], fuel, gwh)
) |>
  group_by(group, place, fuel) |> summarise(gwh = sum(gwh), .groups = "drop") |>
  group_by(group, place) |>
  mutate(share = gwh / sum(gwh), twh = sum(gwh) / 1000,
         renew = sum(gwh[fuel %in% CMP_RENEW]) / sum(gwh)) |>
  ungroup() |>
  mutate(fuel  = factor(fuel, levels = cmp_levels),
         total = place %in% c("Australia", "United States"),
         label = sprintf("%s  (%s TWh)", place, comma(twh, accuracy = if_else(twh < 10, 0.1, 1))),
         # national totals at the bottom of each panel, states by renewable share
         label = fct_reorder(label, renew + if_else(total, -10, 0)))

p2 <- ggplot(state_mix, aes(share, label, fill = fuel)) +
  geom_col(width = 0.72) +
  geom_text(data = distinct(state_mix, group, label, renew),
            aes(x = 1.02, y = label, label = percent(renew, accuracy = 1)),
            inherit.aes = FALSE, hjust = 0, size = 3, colour = "grey20") +
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = cmp_cols, labels = function(x) ifelse(x %in% names(cmp_lab), cmp_lab[x], x),
                    drop = TRUE) +
  scale_x_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0), limits = c(0, 1.09)) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(
    title    = "Electricity generation mix by state",
    subtitle = "Share of generation by fuel, calendar years, including rooftop solar. Right-hand figure is renewable share.",
    x = NULL, y = NULL,
    caption  = paste0("Sources: DCCEEW, Australian Energy Statistics, Table O (calendar 2025); EIA, Electric Power ",
                      "Annual, net generation by state and Table 3.21\n(small-scale solar), calendar ", st_year,
                      ". Generation is where power is made, not where it is used. New South Wales and Victoria have ",
                      "about 96% of\nAustralia's data centre demand (AEMO, 2025-26).")
  ) +
  theme_csis() +
  theme(strip.text.y = element_text(angle = 0, hjust = 0, face = "bold", size = 8.5),
        strip.placement = "outside", panel.spacing.y = unit(0.8, "lines"))

ggsave("02_state_fuel_mix.png", p2, width = 11, height = 7.2, dpi = 150, bg = "white")

renew_note <- state_mix |>
  distinct(place, renew) |>
  arrange(desc(renew)) |>
  with(paste(sprintf("%s %s", place, percent(renew, accuracy = 1)), collapse = "; "))

export_chart_csv(p2, "02_state_fuel_mix.csv",
                 y_format = "percent", chart_type = "Stacked horizontal bar (100%), two panels",
                 value_var = "share", extra = c("group", "fuel"),
                 notes = paste0("Value is on the x-axis. Renewable share: ", renew_note, "."))


# ------------------------------------------------------------------------------
# 5. CHART 3 - electricity generation by fuel, Australia vs US, latest year
#
# (Was primary energy vs electricity; the primary energy bar was removed.
# The file name is kept so the dashboard link does not change.)
# ------------------------------------------------------------------------------

# Same calendar year for both: Australia from Table O's calendar-year sheet,
# the US from EIA.
mix_year <- min(us_last, max(gen_cy$year[gen_cy$region == "AUS"]))
comparison <- bind_rows(
  gen_cy |> filter(region == "AUS", year == mix_year) |> mutate(fuel = to_cmp(fuel), country = "Australia"),
  us_gen |> filter(year == mix_year)
) |>
  group_by(country, fuel) |> summarise(gwh = sum(gwh), .groups = "drop") |>
  group_by(country) |> mutate(share = gwh / sum(gwh)) |> ungroup() |>
  mutate(fuel = factor(fuel, levels = cmp_levels),
         country = factor(country, levels = c("United States", "Australia")))

p3 <- ggplot(comparison, aes(share, country, fill = fuel)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = ifelse(share >= 0.04, percent(share, accuracy = 1), "")),
            position = position_stack(vjust = 0.5), colour = "white", size = 3.2) +
  scale_fill_manual(values = cmp_cols, labels = function(x) ifelse(x %in% names(cmp_lab), cmp_lab[x], x),
                    drop = TRUE) +
  scale_x_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0)) +
  guides(fill = guide_legend(nrow = 2)) +
  labs(
    title    = "Electricity generation by fuel, Australia and the United States",
    subtitle = sprintf("Share of total generation, calendar %d", mix_year),
    x = NULL, y = NULL,
    caption = paste0("Sources: DCCEEW, Australian Energy Statistics, Table O (calendar years, includes rooftop solar);\n",
                     "EIA, Monthly Energy Review, Tables 7.2a and 10.6 (includes small-scale solar). ",
                     "Labels shown for shares of 4% or more.")
  ) +
  theme_csis()

ggsave("03_primary_vs_electricity.png", p3, width = 9, height = 3.8, dpi = 150, bg = "white")

export_chart_csv(p3, "03_primary_vs_electricity.csv",
                 y_format = "percent", chart_type = "Stacked horizontal bar (100%)",
                 value_var = "share", extra = "fuel",
                 notes = sprintf("Calendar %d for both countries. File name kept from the earlier primary energy chart.",
                                 mix_year))

# ------------------------------------------------------------------------------
# 5c. Renewable share over time, Australia vs US
# ------------------------------------------------------------------------------
renew_ts <- bind_rows(
  mix_ts |>
    group_by(country, year) |>
    summarise(share = sum(share[fuel %in% CMP_RENEW]), .groups = "drop") |>
    mutate(series = if_else(country == "Australia",
                            "Australia: renewables (financial years, by the year they start)",
                            "United States: renewables (calendar years)")),
  # US renewables plus nuclear: low-carbon generation (Australia has no nuclear)
  mix_ts |>
    filter(country == "United States") |>
    group_by(country, year) |>
    summarise(share = sum(share[fuel %in% c(CMP_RENEW, "Nuclear")]), .groups = "drop") |>
    mutate(series = "United States: renewables plus nuclear")
) |>
  filter(year >= 2000) |>
  mutate(series = factor(series, levels = c("Australia: renewables (financial years, by the year they start)",
                                            "United States: renewables (calendar years)",
                                            "United States: renewables plus nuclear")))
renew_last <- renew_ts |> group_by(series) |> slice_max(year, n = 1) |> ungroup()

p9 <- ggplot(renew_ts, aes(year, share, colour = series, linetype = series)) +
  geom_line(linewidth = 1) +
  geom_point(data = renew_last, size = 2.4, show.legend = FALSE) +
  geom_text(data = renew_last, aes(label = percent(share, accuracy = 1)), hjust = -0.3, size = 3.4,
            show.legend = FALSE) +
  scale_colour_manual(values = c("#00205B", "#D2232A", "#D2232A"), name = NULL) +
  scale_linetype_manual(values = c("solid", "solid", "22"), name = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, NA),
                     expand = expansion(c(0, 0.05))) +
  scale_x_continuous(breaks = seq(2000, 2025, 5), expand = expansion(c(0.01, 0.06))) +
  guides(colour = guide_legend(ncol = 1), linetype = guide_legend(ncol = 1)) +
  labs(
    title    = "Renewable share of electricity generation, Australia and the United States",
    subtitle = "Hydro, wind, solar (including rooftop), bioenergy and geothermal, share of total generation",
    x = NULL, y = NULL,
    caption  = paste0("Sources: Australia: DCCEEW, Australian Energy Statistics, Table O.\n", US_SRC,
                      "\nNuclear is low-carbon but not renewable; Australia has no nuclear generation.")
  ) +
  theme_csis() +
  theme(legend.key.width = unit(1.2, "cm"))

ggsave("09_renewable_share_au_us.png", p9, width = 9, height = 5.4, dpi = 150, bg = "white")



# ------------------------------------------------------------------------------
# 6. CHART 4 - targets: Australia's 82% and the former US 2035 goal
#
# So what: the Commonwealth target is 82% renewable electricity by 2030.
# Realised share against the straight line to that target is the single most
# useful number for judging whether the commitment is financeable, and it
# drives the transmission and storage procurement US firms bid into.
# ------------------------------------------------------------------------------

renew_hist <- gen_fy |>
  filter(region == "AUS") |>
  group_by(year) |>
  summarise(share = sum(gwh[fuel %in% RENEWABLES]) / sum(gwh), .groups = "drop") |>
  filter(year >= 2000)

last_pt     <- slice_max(renew_hist, year)
target_line <- tibble(year = c(last_pt$year, 2030), share = c(last_pt$share, 0.82))

# The US has no current national target. The closest equivalent is the former
# federal goal of 100% carbon pollution-free electricity by 2035 (set in 2021,
# reversed by executive order in January 2025). It counted nuclear as well as
# renewables, so it is set against the US renewables-plus-nuclear share.
us_lowc <- mix_ts |>
  filter(country == "United States", year >= 2000) |>
  group_by(year) |>
  summarise(share = sum(share[fuel %in% c(CMP_RENEW, "Nuclear")]), .groups = "drop")
us_last_pt <- slice_max(us_lowc, year)
us_goal    <- tibble(year = c(us_last_pt$year, 2035), share = c(us_last_pt$share, 1))

tgt_cols <- c("Australia: renewables" = "#00205B",
              "United States: renewables plus nuclear" = "#D2232A")
p4 <- ggplot() +
  geom_line(data = renew_hist |> mutate(s = "Australia: renewables"),
            aes(year, share, colour = s), linewidth = 1.1) +
  geom_line(data = us_lowc |> mutate(s = "United States: renewables plus nuclear"),
            aes(year, share, colour = s), linewidth = 1.1, linetype = "22") +
  geom_line(data = target_line, aes(year, share), linetype = "11", colour = "#00205B", linewidth = 0.6) +
  geom_line(data = us_goal, aes(year, share), linetype = "11", colour = "#D2232A", linewidth = 0.6, alpha = 0.6) +
  geom_point(data = tibble(year = 2030, share = 0.82), aes(year, share),
             shape = 21, fill = "white", colour = "#00205B", size = 3) +
  geom_point(data = tibble(year = 2035, share = 1), aes(year, share),
             shape = 21, fill = "white", colour = "#D2232A", size = 3) +
  annotate("text", x = 2029.6, y = 0.82, label = "Australia: 82% renewable\nby 2030 (current target)",
           hjust = 1, vjust = 0.4, size = 3, colour = "#00205B", lineheight = 0.95) +
  annotate("text", x = 2034.6, y = 1, label = "US: 100% carbon-free by 2035\n(former goal, revoked 2025)",
           hjust = 1, vjust = 0.4, size = 3, colour = "#D2232A", lineheight = 0.95) +
  scale_colour_manual(values = tgt_cols, name = NULL) +
  guides(colour = guide_legend(override.aes = list(linetype = c("solid", "22")))) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1.05)) +
  scale_x_continuous(breaks = seq(2000, 2035, 5)) +
  labs(
    title    = "Clean electricity targets, Australia and the United States",
    subtitle = paste("Share of generation against each country's target. Australia: renewables, financial years by the",
                     "\nyear they start. US: renewables plus nuclear (carbon-free), calendar years. Dotted lines: straight paths to target."),
    x = NULL, y = NULL,
    caption = paste0(SRC, "
", US_SRC,
                     "\nAustralia's 82% target applies to the main grids (NEM and WA's SWIS); this all-Australia share is a close proxy. ",
                     "The US has no current national target;\nthe 2035 goal (2021) was reversed in January 2025. ",
                     "Several states have their own, e.g. California 60% renewable by 2030 and 100% clean by 2045.")
  ) +
  theme_csis() +
  theme(legend.key.width = unit(1.2, "cm"))

ggsave("04_renewables_vs_target.png", p4, width = 9.5, height = 5.4, dpi = 150, bg = "white")

write_csv(bind_rows(renew_hist |> mutate(series = "Australia: renewables (year = start of financial year)"),
                    us_lowc |> mutate(series = "United States: renewables plus nuclear (calendar year)"),
                    tibble(year = c(2030, 2035), share = c(0.82, 1),
                           series = c("Australia target (82% by 2030)",
                                      "US former goal (100% carbon-free by 2035, revoked 2025)"))),
          "04_renewables_vs_target.csv")


# ------------------------------------------------------------------------------
# 7. CHART 5 - rooftop solar as the largest single renewable asset class
#
# So what: rooftop PV generated 34.8 TWh in calendar 2025 against 21.4 TWh
# utility-scale solar and 40.2 TWh wind. Millions of household systems sit
# outside utility dispatch control - a distributed asset base with a very
# different resilience and cyber profile from a centralised fleet. Relevant to
# anyone writing on grid security or minimum-demand events.
# ------------------------------------------------------------------------------

p5 <- gen_fy |>
  filter(region == "AUS", fuel %in% RENEWABLES, year >= 2005) |>
  mutate(fuel = as_fuel(fuel),
         twh  = gwh / 1000) |>          # named column so the CSV export can find it
  mutate(fuel = droplevels(fuel)) |>   # legend lists only the renewable fuels shown
  ggplot(aes(year, twh, fill = fuel)) +
  geom_col(width = 0.75) +
  scale_fill_manual(values = fuel_cols, drop = TRUE) +
  scale_y_continuous(labels = comma, expand = expansion(c(0, 0.05))) +
  scale_x_continuous(breaks = seq(2005, 2025, 5)) +
  labs(
    title    = "Australian renewable electricity generation by technology",
    subtitle = "Renewable generation by technology, TWh, financial years shown by the year they start",
    x = NULL, y = "TWh", caption = SRC
  ) +
  theme_csis()

ggsave("05_renewables_composition.png", p5, width = 9, height = 5, dpi = 150, bg = "white")

export_chart_csv(p5, "05_renewables_composition.csv",
                 y_format = "number", chart_type = "Stacked vertical bar",
                 extra = "fuel",
                 notes = paste0("Values are TWh (not counts). year = start year of the financial year. ",
                                "Fill colours: ", fuel_col_note(RENEWABLES), "."))


# ------------------------------------------------------------------------------
# 8. Tidy exports
# ------------------------------------------------------------------------------

write_csv(gen_fy,     "australia_generation_fy_tidy.csv")
write_csv(gen_cy,     "australia_generation_cy_tidy.csv")
write_csv(state_2025, "australia_generation_state_2025.csv")

message("Done - five charts, five chart CSVs and three tidy CSVs written to ", getwd())

# ------------------------------------------------------------------------------
# 9. CHART 6 - cost of new solar and wind, Australia vs United States
#
# So what: levelised cost of electricity (LCOE) is the lifetime cost of a new
# plant per MWh, so it compares the two countries on the same basis whatever
# their market design. IRENA's figures exclude subsidies and tax credits.
#
# Data: IRENA (2026), Renewable Power Generation Costs in 2025, data file.
#   Fig 2.11 - onshore wind weighted-average LCOE by country, 2010-2025
#   Fig. 3.7 - utility-scale solar PV weighted-average LCOE by country, 2010-2025
# Values are 2025 US$/MWh (the onshore wind sheet's axis label says USD/kWh,
# but its numbers are per MWh, matching the report's text).
# IRENA publishes a new edition each July at a new address: when it does,
# paste the new data-file link into IRENA_URL and check the two sheet names.
# The file sits behind a download form on the report page; the direct link
# below still downloads it.
# ------------------------------------------------------------------------------

IRENA_URL  <- paste0("https://www.irena.org/-/media/Files/IRENA/Agency/Publication/",
                     "2026/Jul/IRENA_TEC_RPGC_in_2025_data_file_2026.xlsx")
irena_path <- basename(IRENA_URL)
BROWSER_UA <- paste("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
                    "(KHTML, like Gecko) Chrome/124.0 Safari/537.36")

if (!file.exists(irena_path)) {
  message("Downloading ", irena_path)
  httr2::request(IRENA_URL) |>
    httr2::req_user_agent(BROWSER_UA) |>
    httr2::req_timeout(300) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform(path = irena_path)
}
stopifnot(identical(readBin(irena_path, "raw", 2), charToRaw("PK")))   # a real .xlsx, not a block page

# Each sheet has a header row of years 2010-2025 with country names in the
# column to the left of the first year.
read_irena_lcoe <- function(sheet, technology) {
  raw <- read_excel(irena_path, sheet = sheet, col_names = FALSE,
                    col_types = "text", .name_repair = "minimal")
  raw <- as.data.frame(raw)
  hdr <- which(apply(raw, 1, function(r) sum(grepl("^20[0-9]{2}$", r)) >= 10))[1]
  yr_cols <- which(grepl("^20[0-9]{2}$", unlist(raw[hdr, ])))
  yr_cols <- yr_cols[c(TRUE, diff(yr_cols) == 1)]                 # first run of years only
  ctry_col <- min(yr_cols) - 1
  out <- raw[(hdr + 1):nrow(raw), c(ctry_col, yr_cols)]
  names(out) <- c("country", unlist(raw[hdr, yr_cols]))
  out |>
    filter(country %in% c("Australia", "United States")) |>
    pivot_longer(-country, names_to = "year", values_to = "lcoe") |>
    mutate(year = as.integer(year), lcoe = as.numeric(lcoe), technology = technology) |>
    filter(!is.na(lcoe))
}

lcoe <- bind_rows(
  read_irena_lcoe("Fig. 3.7", "Utility-scale solar PV"),
  read_irena_lcoe("Fig 2.11", "Onshore wind")
) |>
  mutate(technology = factor(technology, levels = c("Utility-scale solar PV", "Onshore wind")))

stopifnot(nrow(lcoe) > 40, n_distinct(lcoe$country) == 2)

lcoe_latest <- lcoe |> group_by(technology, country) |> slice_max(year, n = 1) |> ungroup()
lcoe_yr     <- max(lcoe_latest$year)

au_us_cols <- c("Australia" = "#00205B", "United States" = "#D2232A")   # as on every page

# From 2015: the 2010-12 solar costs (up to $490/MWh) would flatten the recent comparison.
lcoe_plot <- lcoe |> filter(year >= 2015)

# Drawn twice. IRENA's figures are in constant (real) US$ of lcoe_yr, so the
# A$ version converts every year at that one year's average rate.
lcoe_rate <- fx_calendar_year(lcoe_yr)
make_p6 <- function(cur) {
conv <- if (cur == "AUD") 1 / lcoe_rate else 1
lcoe_plot   <- lcoe_plot   |> mutate(lcoe = lcoe * conv)
lcoe_latest <- lcoe_latest |> mutate(lcoe = lcoe * conv)
ggplot(lcoe_plot, aes(year, lcoe, colour = country)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.4) +
  ggrepel::geom_text_repel(data = lcoe_latest, aes(label = paste0(cur_sym[[cur]], round(lcoe))),
                           nudge_x = 0.6, direction = "y", hjust = 0, size = 3.2,
                           segment.colour = NA, min.segment.length = Inf, show.legend = FALSE) +
  facet_wrap(~ technology) +
  scale_colour_manual(values = au_us_cols) +
  scale_y_continuous(labels = label_dollar(prefix = cur_sym[[cur]]), limits = c(0, NA),
                     expand = expansion(c(0, 0.05))) +
  scale_x_continuous(breaks = seq(2015, 2025, 2), expand = expansion(mult = c(0.02, 0.14))) +
  labs(
    title    = "Levelised cost of new solar and wind, Australia and the United States",
    subtitle = sprintf(paste0("Weighted-average levelised cost of electricity for new projects, ",
                              "%d %s per MWh, 2015-%d"), lcoe_yr, cur_sym[[cur]], lcoe_yr),
    x = NULL, y = NULL,
    caption  = paste0("Source: IRENA (2026), Renewable Power Generation Costs in 2025, data file ",
                      "(Fig. 2.11 and Fig. 3.7).\nLCOE excludes subsidies and tax credits, ",
                      "so US figures are before federal tax credits.",
                      if (cur == "AUD") sprintf("
Converted to A$ at the RBA's %d average exchange rate.", lcoe_yr) else "")
  ) +
  theme_csis() +
  theme(strip.text = element_text(face = "bold", size = 10))
}

save_both(make_p6, "06_lcoe_au_us.png",
          function(p, f) ggsave(f, p, width = 9, height = 5, dpi = 150, bg = "white"))
p6 <- make_p6("USD")   # the chart CSV below keeps IRENA's own US$ figures

export_chart_csv(p6, "06_lcoe_au_us.csv",
                 y_format = "number", chart_type = "Faceted line with point markers",
                 extra = c("technology", "country"),
                 notes = paste0("Values are ", lcoe_yr, " US$ per MWh (not counts). Facets by technology. ",
                                "Colours: Australia #eda100; United States #00205B. ",
                                "Latest value labelled at the end of each line."))


# ------------------------------------------------------------------------------
# 10. CHART 7 - wholesale electricity prices, Australia vs California and Texas
#
# So what: spot prices are what generators earn and what large users pay
# before network charges, so they are the closest like-for-like measure of
# power costs between the two countries.
#
# Data (all free, no key needed; raw files are kept in Energy/raw/):
#   - AEMO aggregated price and demand data: regional reference price (RRP)
#     for each 5-minute dispatch interval
#   - CAISO OASIS: hourly day-ahead price (LMP) at the SP15 trading hub
#     (Southern California)
#   - RBA table F11: monthly A$/US$ exchange rate (end of month)
#
#   - ERCOT: 15-minute real-time price at the Texas hub average, from its
#     yearly historical price files (see the ERCOT notes below)
#
# Texas (ERCOT) is the closest US match to the NEM: an energy-only market
# with lots of wind and solar. PJM's data needs a registered API key, so it
# is not included. Each region's price is a simple time-weighted average
# of all intervals, so all markets are averaged the same way (AEMO's own
# headline figures are demand-weighted; those are in the tidy CSV as vwa_aud).
#
# OASIS only keeps about 39 months of hourly prices, so the chart starts at
# PRICE_START. Downloaded months are kept in Energy/raw/ and never fetched
# again, so the series grows with each refresh: do not delete that folder.
# Complete quarters only.
# ------------------------------------------------------------------------------

PRICE_START <- as.Date("2023-07-01")
last_month  <- seq(as.Date(format(Sys.Date(), "%Y-%m-01")), by = "-1 month", length.out = 2)[2]
NEM_REGIONS <- c(NSW = "NSW1", QLD = "QLD1", VIC = "VIC1", SA = "SA1")
dir.create(file.path("raw", "aemo"),  recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("raw", "caiso"), recursive = TRUE, showWarnings = FALSE)

# Downloads url to dest unless a good copy is already there (and, if
# max_age_days is set, younger than that).
fetch_cached <- function(url, dest, check = function(p) file.size(p) > 1000,
                         max_age_days = Inf) {
  fresh <- file.exists(dest) &&
    difftime(Sys.time(), file.mtime(dest), units = "days") < max_age_days
  if (fresh && check(dest)) return(dest)
  httr2::request(url) |>
    httr2::req_user_agent(BROWSER_UA) |>
    httr2::req_timeout(180) |>
    httr2::req_retry(max_tries = 4, backoff = function(i) 5 * i) |>
    httr2::req_perform(path = dest)
  if (!check(dest)) stop("Download did not return data: ", url)
  dest
}

price_months <- seq(PRICE_START, last_month, by = "1 month")

# --- AEMO: monthly summaries of each region's 5-minute dispatch prices ---
nem_monthly <- expand_grid(month_start = price_months, region = names(NEM_REGIONS)) |>
  purrr::pmap(function(month_start, region) {
    file <- sprintf("PRICE_AND_DEMAND_%s_%s.csv", format(month_start, "%Y%m"), NEM_REGIONS[[region]])
    path <- fetch_cached(paste0("https://www.aemo.com.au/aemo/data/nem/priceanddemand/", file),
                         file.path("raw", "aemo", file))
    d <- read_csv(path, show_col_types = FALSE, col_select = c(TOTALDEMAND, RRP))
    tibble(month_start, region,
           minutes = nrow(d) * 5,
           twa_aud = mean(d$RRP),
           vwa_aud = sum(d$RRP * d$TOTALDEMAND) / sum(d$TOTALDEMAND))
  }) |>
  purrr::list_rbind()

# --- CAISO: hourly day-ahead SP15 prices, one request per month ---
# Requests run from local (Pacific) midnight to midnight: OASIS rejects
# windows over 31 days, which a UTC window crossing daylight saving can be.
# OASIS allows one request every few seconds, so new months are fetched slowly.
caiso_monthly <- tibble(month_start = price_months) |>
  purrr::pmap(function(month_start) {
    next_month <- seq(month_start, by = "1 month", length.out = 2)[2]
    utc_stamp  <- function(d) format(as.POSIXct(format(d), tz = "America/Los_Angeles"),
                                     "%Y%m%dT%H:%M-0000", tz = "UTC")
    dest <- file.path("raw", "caiso", sprintf("SP15_DAM_%s.zip", format(month_start, "%Y%m")))
    if (!file.exists(dest)) Sys.sleep(5)
    url <- paste0("https://oasis.caiso.com/oasisapi/SingleZip?queryname=PRC_LMP",
                  "&market_run_id=DAM&version=12&node=TH_SP15_GEN-APND",
                  "&startdatetime=", utc_stamp(month_start),
                  "&enddatetime=",   utc_stamp(next_month),
                  "&resultformat=6")
    fetch_cached(url, dest, check = function(p) {
      file.size(p) > 1000 && any(grepl("\\.csv$", utils::unzip(p, list = TRUE)$Name))
    })
    csv <- utils::unzip(dest, list = TRUE)$Name[1]
    d <- read_csv(unz(dest, csv), show_col_types = FALSE) |>
      filter(XML_DATA_ITEM == "LMP_PRC",
             format(as.Date(OPR_DT), "%Y-%m") == format(month_start, "%Y-%m"))
    tibble(month_start, minutes = nrow(d) * 60, usd = mean(d$MW))   # MW column holds the price
  }) |>
  purrr::list_rbind()

# --- ERCOT (Texas): 15-minute real-time prices at the hub average (HB_HUBAVG) ---
# Source: ERCOT "Historical RTM Load Zone and Hub Prices" (NP6-785-ER), one
# zip per year, free with no login:
#   https://www.ercot.com/mp/data-products/data-product-details?id=NP6-785-ER
# The script reads ERCOT's file list and downloads each year it needs into
# Energy/raw/ercot/. Past years are kept and not fetched again; the current
# year's file, which ERCOT updates daily, is re-fetched when over a week old.
# ercot.com refuses requests that don't look like they come from its own page,
# hence the Referer header. If the download ever fails, save the yearly zips
# from the page above into Energy/raw/ercot/ by hand and the script uses those.
# If there is no ERCOT data at all, Texas is left off the chart.

ERCOT_HUB   <- "HB_HUBAVG"
ERCOT_DIR   <- file.path("raw", "ercot")
ERCOT_PAGE  <- "https://www.ercot.com/mp/data-products/data-product-details?id=NP6-785-ER"
dir.create(ERCOT_DIR, recursive = TRUE, showWarnings = FALSE)

ercot_request <- function(url) {
  httr2::request(url) |>
    httr2::req_user_agent(BROWSER_UA) |>
    httr2::req_headers(Referer = ERCOT_PAGE) |>
    httr2::req_timeout(300) |>
    httr2::req_retry(max_tries = 3, backoff = function(i) 5 * i)
}

# Download the yearly zips covering PRICE_START to last month
fetch_ercot_files <- function() {
  docs <- ercot_request("https://www.ercot.com/misapp/servlets/IceDocListJsonWS?reportTypeId=13061") |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>                 # ERCOT sends no content type, so parse the text
    jsonlite::fromJSON()
  docs <- docs$ListDocsByRptTypeRes$DocumentList$Document
  years <- as.integer(format(PRICE_START, "%Y")):as.integer(format(last_month, "%Y"))
  for (y in years) {
    doc <- docs[docs$FriendlyName == paste0("RTMLZHBSPP_", y), ]
    if (nrow(doc) == 0) { message("ERCOT: no file listed for ", y); next }
    dest <- file.path(ERCOT_DIR, paste0("RTMLZHBSPP_", y, ".zip"))
    current_year <- y == as.integer(format(Sys.Date(), "%Y"))
    stale <- !file.exists(dest) ||
      (current_year && difftime(Sys.time(), file.mtime(dest), units = "days") > 7)
    if (!stale) next
    message("ERCOT: downloading RTMLZHBSPP_", y)
    ercot_request(paste0("https://www.ercot.com/misdownload/servlets/mirDownload?doclookupId=",
                         doc$DocID[1])) |>
      httr2::req_perform(path = dest)
    if (!identical(readBin(dest, "raw", 2), charToRaw("PK"))) {
      unlink(dest); stop("ERCOT returned something other than a zip for ", y)
    }
  }
}

# One row per 15-minute interval at the hub, from every workbook in the folder
read_ercot_files <- function() {
  for (z in list.files(ERCOT_DIR, "\\.zip$", full.names = TRUE))
    utils::unzip(z, exdir = ERCOT_DIR, overwrite = TRUE)
  books <- list.files(ERCOT_DIR, "\\.xlsx$", full.names = TRUE)
  if (length(books) == 0) return(NULL)
  purrr::map(books, function(b) {
    purrr::map(excel_sheets(b), function(sh) {
      d <- read_excel(b, sheet = sh)
      names(d) <- str_replace_all(str_to_lower(names(d)), "[^a-z]+", "_")
      if (!all(c("delivery_date", "delivery_hour", "delivery_interval",
                 "settlement_point_name", "settlement_point_price") %in% names(d)))
        return(NULL)
      if (!"repeated_hour_flag" %in% names(d)) d$repeated_hour_flag <- "N"
      d |>
        filter(settlement_point_name == ERCOT_HUB) |>
        transmute(date     = as.Date(delivery_date, tryFormats = c("%m/%d/%Y", "%Y-%m-%d")),
                  hour     = as.integer(delivery_hour),
                  interval = as.integer(delivery_interval),
                  repeated = as.character(repeated_hour_flag),
                  price    = as.numeric(settlement_point_price))
    }) |> purrr::list_rbind()
  }) |> purrr::list_rbind() |>
    distinct(date, hour, interval, repeated, .keep_all = TRUE)   # same year saved twice
}

tryCatch(fetch_ercot_files(),
         error = function(e) message("ERCOT download failed (", conditionMessage(e),
                                     "); using any files already in ", ERCOT_DIR))
ercot_raw <- read_ercot_files()

ercot_monthly <- if (!is.null(ercot_raw) && nrow(ercot_raw) > 0) {
  ercot_raw |>
    mutate(month_start = as.Date(format(date, "%Y-%m-01"))) |>
    filter(month_start %in% price_months) |>
    group_by(month_start) |>
    summarise(minutes = n() * 15, usd = mean(price), days = n_distinct(date), .groups = "drop") |>
    # whole months only (the current-year file stops part-way through a month)
    filter(days == vapply(month_start, function(m)
      as.integer(format(seq(m, by = "1 month", length.out = 2)[2] - 1, "%d")), integer(1))) |>
    select(-days)
} else NULL

has_ercot <- !is.null(ercot_monthly) && nrow(ercot_monthly) > 0
if (has_ercot) {
  message("ERCOT: ", nrow(ercot_monthly), " months of ", ERCOT_HUB, " prices")
} else {
  message("ERCOT: no data found, so Texas is left off the wholesale price chart.")
}

# --- Exchange rate: each month's average of the RBA's daily A$/US$ rates ---
# (from '0. Exchange rates.R', the same rule as every other chart's toggle)
fx <- tibble(month_start = price_months, aud_usd = fx_month(price_months)) |>
  filter(!is.na(aud_usd))

missing_fx <- setdiff(price_months, fx$month_start)
if (length(missing_fx) > 0)
  warning("No RBA exchange rate yet for ", paste(format(as.Date(missing_fx, origin = "1970-01-01"),
                                                        "%b %Y"), collapse = ", "),
          "; those months are left out.")

# --- Quarterly averages in US$/MWh and A$/MWh ---
price_qtr <- bind_rows(
  nem_monthly |>
    inner_join(fx, by = "month_start") |>
    mutate(usd = twa_aud * aud_usd, aud = twa_aud),
  caiso_monthly |> mutate(region = "California (SP15)") |>
    inner_join(fx, by = "month_start") |> mutate(aud = usd / aud_usd),
  if (has_ercot) ercot_monthly |> mutate(region = "Texas (ERCOT)") |>
    inner_join(fx, by = "month_start") |> mutate(aud = usd / aud_usd)
) |>
  mutate(quarter = as.Date(paste0(format(month_start, "%Y-"),
                                  sprintf("%02d", (as.integer(format(month_start, "%m")) - 1) %/% 3 * 3 + 1),
                                  "-01"))) |>
  group_by(region, quarter) |>
  summarise(n_months  = n(),
            price_usd = weighted.mean(usd, minutes),
            price_aud = weighted.mean(aud, minutes),
            twa_aud   = if (all(is.na(twa_aud))) NA_real_ else weighted.mean(twa_aud, minutes),
            vwa_aud   = if (all(is.na(vwa_aud))) NA_real_ else weighted.mean(vwa_aud, minutes),
            .groups = "drop") |>
  filter(n_months == 3) |>
  mutate(region = factor(region, levels = intersect(c("Texas (ERCOT)", "California (SP15)", names(NEM_REGIONS)),
                                                   unique(region))))

stopifnot(n_distinct(price_qtr$region) == 5 + has_ercot)
us_markets <- c("Texas (ERCOT)", "California (SP15)")

qtr_label    <- function(d) paste0(format(d, "%Y"), " Q", (as.integer(format(d, "%m")) - 1) %/% 3 + 1)
price_latest <- price_qtr |> filter(quarter == max(quarter))
price_q      <- max(price_latest$quarter)

# Australia in navy shades, the US in red shades. The chart shows the two
# largest NEM regions and the two US markets; QLD and SA go in the caption.
price_cols <- c("Texas (ERCOT)" = "#D2232A", "California (SP15)" = "#F08A8E",
                NSW = "#00205B", VIC = "#5B8BD0")
CHART_REGIONS <- names(price_cols)
note_regions  <- price_latest |> filter(!region %in% CHART_REGIONS)

# Drawn twice: US$ (NEM converted) and A$ (US markets converted).
make_p7 <- function(cur) {
price_qtr    <- price_qtr    |> filter(region %in% CHART_REGIONS) |>
  mutate(price = if (cur == "USD") price_usd else price_aud, region = droplevels(region))
price_latest <- price_latest |> filter(region %in% CHART_REGIONS) |>
  mutate(price = if (cur == "USD") price_usd else price_aud)
others <- note_regions |> mutate(price = if (cur == "USD") price_usd else price_aud)
ggplot(price_qtr, aes(quarter, price, colour = region)) +
  geom_line(aes(linewidth = region %in% us_markets)) +
  geom_point(size = 1.4) +
  ggrepel::geom_text_repel(data = price_latest, aes(label = paste0(cur_sym[[cur]], round(price))),
                           nudge_x = 25, direction = "y", hjust = 0, size = 3.1,
                           segment.colour = NA, min.segment.length = Inf, show.legend = FALSE) +
  scale_linewidth_manual(values = c(`TRUE` = 1.4, `FALSE` = 0.7), guide = "none") +
  scale_colour_manual(values = price_cols) +
  scale_y_continuous(labels = label_dollar(prefix = cur_sym[[cur]]), limits = c(0, NA),
                     expand = expansion(c(0, 0.05))) +
  scale_x_date(breaks = seq(min(price_qtr$quarter), price_q, by = "6 months"),
               labels = qtr_label, expand = expansion(mult = c(0.02, 0.08))) +
  labs(
    title    = if (has_ercot) "Wholesale electricity prices, Australian NEM regions, California and Texas (ERCOT)"
               else "Wholesale electricity prices, Australian NEM regions and California",
    subtitle = sprintf(paste0("Average wholesale price, %s per MWh, calendar quarters, %s to %s.
",
                              "NEM and Texas: real-time prices; California: day-ahead prices."),
                       cur_sym[[cur]], qtr_label(min(price_qtr$quarter)), qtr_label(price_q)),
    x = NULL, y = NULL,
    caption  = paste0("Sources: AEMO aggregated price and demand data (5-minute regional prices); ",
                      "CAISO OASIS (hourly day-ahead price, SP15 hub);\n",
                      if (has_ercot) "ERCOT (15-minute real-time price, hub average HB_HUBAVG); " else "",
                      "RBA daily A$/US$ exchange rates, averaged for each month to convert that month.\n",
                      "Simple time-weighted averages of all intervals. ",
                      if (nrow(others)) paste0("Not shown, ", qtr_label(price_q), ": ",
                                               paste(sprintf("%s %s%.0f", others$region, cur_sym[[cur]], others$price),
                                                     collapse = ", "), ".") else "",
                      if (has_ercot) "" else " Texas (ERCOT) not shown: no ERCOT data available on this run.")
  ) +
  theme_csis()
}

save_both(make_p7, "07_wholesale_prices_au_us.png",
          function(p, f) ggsave(f, p, width = 9, height = 5.2, dpi = 150, bg = "white"))
p7 <- make_p7("USD")   # the chart CSV below stays in US$

export_chart_csv(p7, "07_wholesale_prices_au_us.csv",
                 y_format = "number", chart_type = "Line with point markers",
                 extra = "region",
                 notes = paste0("Values are US$ per MWh (not counts). quarter = first day of the quarter. ",
                                "Colours: Texas (ERCOT) #D2232A and California (SP15) #00205B (thicker lines); NSW #eda100; ",
                                "QLD #993c1d; VIC #2a78d6; SA #1baf7a. Latest value labelled."))

write_csv(lcoe |> arrange(technology, country, year), "lcoe_au_us_tidy.csv")
write_csv(price_qtr |> arrange(region, quarter), "wholesale_prices_au_us_tidy.csv")

# Takeaways under each chart
local({
  en <- function(file) file.path("Energy", file)
  coal <- gen_fy |> filter(region == "AUS") |> group_by(year) |>
    summarise(coal = sum(gwh[grepl("coal", fuel, ignore.case = TRUE)]) / sum(gwh), .groups = "drop")
  pk <- coal |> slice_max(coal, n = 1); lt <- coal |> slice_max(year, n = 1)
  fy <- function(y) paste0(y, "-", sprintf("%02d", (y + 1) %% 100))
  us_c <- mix_ts |> filter(country == "United States") |> group_by(year) |>
    summarise(coal = sum(share[fuel == "Coal"]), .groups = "drop")
  takeaway(en("01_fuel_mix_timeseries.png"), sprintf(
    "Coal's share of electricity fell from a peak of %.0f%% in %s to %.0f%% in %s in Australia, and from %.0f%% in %d to %.0f%% in %d in the US.",
    100 * pk$coal, fy(pk$year), 100 * lt$coal, fy(lt$year),
    100 * max(us_c$coal), us_c$year[which.max(us_c$coal)], 100 * us_c$coal[us_c$year == us_last], us_last))
  takeaway(en("04_renewables_vs_target.png"), sprintf(
    "Renewables supplied %.0f%% of Australian electricity in %s; reaching 82%% by 2030 needs about %.0f percentage points a year.",
    100 * last_pt$share, fy(last_pt$year), 100 * (0.82 - last_pt$share) / (2030 - last_pt$year)))
  rn <- gen_fy |> filter(region == "AUS", fuel %in% RENEWABLES, year == max(year)) |> arrange(desc(gwh))
  takeaway(en("05_renewables_composition.png"), sprintf(
    "%s was the largest renewable source in %s, at %.1f TWh.", rn$fuel[1], fy(max(gen_fy$year)), rn$gwh[1] / 1000))
  l7 <- function(cur) {
    p <- price_latest |> mutate(v = if (cur == "USD") price_usd else price_aud)
    g <- function(r) p$v[p$region == r]
    sprintf("In %s wholesale power averaged %s%.0f/MWh in NSW and %s%.0f in Victoria, against %s%.0f in Texas and %s%.0f in California.",
            qtr_label(price_q), cur_sym[[cur]], g("NSW"), cur_sym[[cur]], g("VIC"),
            cur_sym[[cur]], if (has_ercot) g("Texas (ERCOT)") else NA, cur_sym[[cur]], g("California (SP15)"))
  }
  takeaway(en("07_wholesale_prices_au_us.png"), l7("AUD"), l7("USD"))
  sm <- state_mix |> distinct(group, place, renew) |> filter(!place %in% c("Australia", "United States"))
  au_s <- sm |> filter(grepl("^Australia", group)) |> arrange(desc(renew))
  takeaway(en("02_state_fuel_mix.png"), sprintf(
    "Renewables supplied %.0f%% of electricity in New South Wales and %.0f%% in Victoria, where most Australian data centres are, against %.0f%% in Virginia.",
    100 * au_s$renew[au_s$place == "NSW"], 100 * au_s$renew[au_s$place == "VIC"],
    100 * sm$renew[sm$place == "Virginia"]))
  cm <- comparison |> group_by(country) |>
    summarise(coal = sum(share[fuel == "Coal"]), gas = sum(share[fuel == "Gas"]),
              ren = sum(share[fuel %in% CMP_RENEW]), nuc = sum(share[fuel == "Nuclear"]))
  g <- function(c, v) 100 * cm[[v]][cm$country == c]
  takeaway(en("03_primary_vs_electricity.png"), sprintf(
    "In %d coal made up %.0f%% of Australia's electricity and %.0f%% of the US's; gas %.0f%% and %.0f%%; renewables %.0f%% and %.0f%% (plus %.0f%% nuclear in the US).",
    mix_year, g("Australia", "coal"), g("United States", "coal"), g("Australia", "gas"), g("United States", "gas"),
    g("Australia", "ren"), g("United States", "ren"), g("United States", "nuc")))
  rl <- renew_last |> mutate(k = c("au", "us", "us_nuc")[as.integer(series)])
  v <- function(k) 100 * rl$share[rl$k == k]
  takeaway(en("09_renewable_share_au_us.png"), sprintf(
    "Renewables supplied %.0f%% of Australia's electricity in %s and %.0f%% of the US's in %d; with nuclear, the US's low-carbon share was %.0f%%.",
    v("au"), fy(rl$year[rl$k == "au"]), v("us"), rl$year[rl$k == "us"], v("us_nuc")))
  l6 <- function(cur) {
    k <- if (cur == "AUD") 1 / lcoe_rate else 1
    v <- function(t, c) lcoe_latest$lcoe[lcoe_latest$technology == t & lcoe_latest$country == c] * k
    sprintf("New utility-scale solar cost %s%.0f/MWh in Australia and %s%.0f in the US in %d; onshore wind %s%.0f and %s%.0f.",
            cur_sym[[cur]], v("Utility-scale solar PV", "Australia"), cur_sym[[cur]], v("Utility-scale solar PV", "United States"),
            lcoe_yr, cur_sym[[cur]], v("Onshore wind", "Australia"), cur_sym[[cur]], v("Onshore wind", "United States"))
  }
  takeaway(en("06_lcoe_au_us.png"), l6("AUD"), l6("USD"))
})

message("Done - charts 6 and 7 written to ", getwd())