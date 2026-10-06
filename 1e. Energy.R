rm(list = ls())
# The dashboard folder is set once, in section 1 of '1. Refresh all data.R'.
# To run this script on its own, run that section first.
if (Sys.getenv("DASHBOARD_DIR") == "")
  stop("Dashboard folder not set. Run section 1 of '1. Refresh all data.R' first.")
setwd(file.path(Sys.getenv("DASHBOARD_DIR"), "Energy"))

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

path <- "table-o-electricity-generation-by-fuel type-2024-25-and-2025.xlsx"
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
# 3. CHART 1 - the coal exit, 35 years
#
# So what: coal peaked around 2008-09 and has fallen since. This is the fastest
# turnover of a major grid in the OECD, and every closure date is a reliability
# question that shapes gas demand and storage procurement.
# ------------------------------------------------------------------------------

p1 <- gen_fy |>
  filter(region == "AUS") |>
  mutate(fuel = as_fuel(fuel),
         twh  = gwh / 1000) |>          # named column so the CSV export can find it
  ggplot(aes(year, twh, fill = fuel)) +
  geom_area(colour = "white", linewidth = 0.15) +
  scale_fill_manual(values = fuel_cols, drop = FALSE) +
  scale_y_continuous(labels = comma, expand = expansion(c(0, 0.02))) +
  scale_x_continuous(breaks = seq(1990, 2025, 5)) +
  labs(
    title    = "Australian electricity generation by fuel type",
    subtitle = "Electricity generation by fuel, TWh, financial years 1989-90 to 2024-25",
    x = NULL, y = "TWh", caption = SRC
  ) +
  theme_csis()

ggsave("01_fuel_mix_timeseries.png", p1, width = 9, height = 5.5, dpi = 150, bg = "white")

export_chart_csv(p1, "01_fuel_mix_timeseries.csv",
                 y_format = "number", chart_type = "Stacked area",
                 extra = "fuel",
                 notes = paste0("Values are TWh (not counts). year = start year of the financial year ",
                                "(1989 = 1989-90). Stacked in fuel order. Fill colours: ",
                                fuel_col_note(unique(p1$data$fuel)), "."))


# ------------------------------------------------------------------------------
# 4. CHART 2 - eight grids, not one
#
# So what: "the Australian grid" does not exist. WA is a separate market
# entirely. Any read on AUKUS basing at Henderson, critical-minerals
# processing, or data-centre siting has to be state-level. Bars are sorted by
# renewable share so the ranking is legible, which a choropleth cannot do.
# ------------------------------------------------------------------------------

state_shares <- state_2025 |>
  filter(region != "AUS") |>
  group_by(region) |>
  mutate(share = gwh / sum(gwh),
         twh   = sum(gwh) / 1000,
         renew = sum(gwh[fuel %in% RENEWABLES]) / sum(gwh)) |>
  ungroup() |>
  mutate(fuel  = as_fuel(fuel),
         label = sprintf("%s  (%.1f TWh)", region, twh),
         label = fct_reorder(label, renew))

p2 <- ggplot(state_shares, aes(share, label, fill = fuel)) +
  geom_col(width = 0.72) +
  geom_text(
    data = distinct(state_shares, label, renew),
    aes(x = 1.02, y = label, label = percent(renew, accuracy = 1)),
    inherit.aes = FALSE, hjust = 0, size = 3.1, colour = "grey20"
  ) +
  scale_fill_manual(values = fuel_cols, drop = FALSE) +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     expand = c(0, 0), limits = c(0, 1.09)) +
  labs(
    title    = "Electricity generation mix by state and territory, 2025",
    subtitle = "Share of generation by fuel, calendar 2025. Right-hand figure is renewable share.",
    x = NULL, y = NULL, caption = SRC
  ) +
  theme_csis()

ggsave("02_state_fuel_mix.png", p2, width = 9.5, height = 5, dpi = 150, bg = "white")

renew_note <- state_shares |>
  distinct(region, renew) |>
  arrange(desc(renew)) |>
  with(paste(sprintf("%s %s", region, percent(renew, accuracy = 1)), collapse = "; "))

export_chart_csv(p2, "02_state_fuel_mix.csv",
                 y_format = "percent", chart_type = "Stacked horizontal bar (100%)",
                 value_var = "share", extra = "fuel",
                 notes = paste0("Value is on the x-axis. Bars sorted by renewable share (highest at top). ",
                                "Right-hand labels, renewable share: ", renew_note, ". Fill colours: ",
                                fuel_col_note(unique(state_shares$fuel)), "."))


# ------------------------------------------------------------------------------
# 5. CHART 3 - electricity is only part of the story
#
# So what: oil is Australia's largest primary energy source and barely touches
# the grid. The gap between these two bars is transport, and it is imported
# refined product. This is the strategic vulnerability Table O cannot show.
#
# Primary energy figures are AES Table C, 2023-24 - the most recent published
# year. Table C runs a year behind Table O; 2024-25 is due September 2026.
# ------------------------------------------------------------------------------

elec_2025 <- gen_cy |>
  filter(region == "AUS", year == 2025) |>
  mutate(grp = case_when(
    fuel %in% c("Black coal", "Brown coal") ~ "Coal",
    fuel == "Gas" ~ "Gas",
    fuel == "Oil" ~ "Oil",
    TRUE          ~ "Renewables")) |>
  count(grp, wt = gwh, name = "gwh") |>
  mutate(share = gwh / sum(gwh), basis = "Electricity only\n(2025)") |>
  select(basis, fuel = grp, share)

comparison <- bind_rows(
  tibble(basis = "All primary energy\n(2023-24)",
         fuel  = c("Coal", "Oil", "Gas", "Renewables"),
         share = c(0.25, 0.41, 0.25, 0.09)),
  elec_2025
) |>
  mutate(fuel = fct_relevel(fuel, "Coal", "Oil", "Gas", "Renewables"))

p3 <- ggplot(comparison, aes(share, basis, fill = fuel)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = ifelse(share > 0.05, percent(share, accuracy = 1), "")),
            position = position_stack(vjust = 0.5), colour = "white", size = 3.2) +
  scale_fill_manual(values = c(Coal = "#2c2c2a", Oil = "#993c1d",
                               Gas  = "#eb6834", Renewables = "#1baf7a")) +
  scale_x_continuous(labels = percent_format(accuracy = 1), expand = c(0, 0)) +
  labs(
    title    = "Fuel shares of primary energy and of electricity generation",
    subtitle = "Oil is 41% of primary energy and under 2% of generation. The difference is transport fuel, almost all imported.",
    x = NULL, y = NULL,
    caption = "Source: DCCEEW, Australian Energy Statistics, Tables C (2023-24) and O (2025)."
  ) +
  theme_csis()

ggsave("03_primary_vs_electricity.png", p3, width = 9, height = 3.6, dpi = 150, bg = "white")

export_chart_csv(p3, "03_primary_vs_electricity.csv",
                 y_format = "percent", chart_type = "Stacked horizontal bar (100%)",
                 value_var = "share", extra = "fuel",
                 notes = paste0("Value is on the x-axis. Primary energy shares (2023-24) are hard-coded from ",
                                "AES Table C; electricity shares (2025) computed from Table O. In-bar labels ",
                                "shown only for segments above 5%. Fill colours: Coal #2c2c2a; Oil #993c1d; ",
                                "Gas #eb6834; Renewables #1baf7a."))


# ------------------------------------------------------------------------------
# 6. CHART 4 - the 82% target
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

p4 <- ggplot(renew_hist, aes(year, share)) +
  geom_line(colour = "#1baf7a", linewidth = 1.1) +
  geom_line(data = target_line, linetype = "22", colour = "grey45", linewidth = 0.8) +
  geom_point(data = last_pt, colour = "#1baf7a", size = 3) +
  geom_point(data = tibble(year = 2030, share = 0.82),
             shape = 21, fill = "white", colour = "grey30", size = 3) +
  annotate("text", x = 2029.6, y = 0.82, label = "82% by 2030\npolicy target",
           hjust = 1, vjust = 0.4, size = 3.1, colour = "grey30", lineheight = 0.95) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 0.9)) +
  scale_x_continuous(breaks = seq(2000, 2030, 5)) +
  labs(
    title    = "Renewable share of electricity generation and the 82% target for 2030",
    subtitle = "Renewable share of total generation, financial years, against a straight-line path to target",
    x = NULL, y = NULL, caption = SRC
  ) +
  theme_csis()

ggsave("04_renewables_vs_target.png", p4, width = 9, height = 5, dpi = 150, bg = "white")

export_chart_csv(p4, "04_renewables_vs_target.csv",
                 y_format = "percent", chart_type = "Line with reference path",
                 notes = sprintf(paste0("year = start year of the financial year. y-axis limits in the metadata ",
                                        "are fractions (0-0.9 = 0-90%%). Dashed grey45 (#737373, 0.8 mm) ",
                                        "straight line from %d (%.1f%%) to 2030 (82%%), with an open circle and ",
                                        "label '82%% by 2030 policy target' at the 2030 point. Marker (size 3) on ",
                                        "the latest year."),
                                 last_pt$year, last_pt$share * 100))


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
  ggplot(aes(year, twh, fill = fuel)) +
  geom_col(width = 0.75) +
  scale_fill_manual(values = fuel_cols, drop = FALSE) +
  scale_y_continuous(labels = comma, expand = expansion(c(0, 0.05))) +
  scale_x_continuous(breaks = seq(2005, 2025, 5)) +
  labs(
    title    = "Renewable electricity generation by technology",
    subtitle = "Renewable generation by technology, TWh, financial years",
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