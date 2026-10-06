rm(list = ls())
# The dashboard folder is set once, in section 1 of '1. Refresh all data.R'.
# To run this script on its own, run that section first.
if (Sys.getenv("DASHBOARD_DIR") == "")
  stop("Dashboard folder not set. Run section 1 of '1. Refresh all data.R' first.")
setwd(file.path(Sys.getenv("DASHBOARD_DIR"), "Economy"))

# Australian economy charts
#   1. Real GDP growth, Australia and Australia vs US
#   2. Real GDP index and labour productivity, Australia vs US
#   3. GVA by industry: levels and contributions to growth
#   5. Relative size: Australia vs US, US states and the OECD
#   (Two-way investment charts are made by '1i. Investment.R'.)
#
# Everything is downloaded (ABS via readabs, FRED, World Bank).

library(readabs)
library(quantmod)   # load before dplyr so dplyr's first()/last() aren't masked
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

# FRED series as a data frame (date, value)
fred <- function(id) {
  x <- getSymbols(id, src = "FRED", auto.assign = FALSE)
  data.frame(date = index(x), value = as.numeric(x[, 1]))
}

# Index each country's series to the average of base_year = 100. ABS dates
# quarters by their last month and FRED by their first, so both are floored
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

gdp_us <- fred("GDPC1") |> mutate(country = "United States")

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

p_gdp <- gdp |>
  filter(date >= as.Date("2000-01-01")) |>
  ggplot(aes(date, yoy, colour = country)) +
  geom_hline(yintercept = 0, colour = csis_colours[["grey"]], linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = au_us_colours) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 1)) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(title = "Australian and US real GDP growth",
       subtitle = "Through the year, seasonally adjusted",
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
  filter(date >= as.Date("2001-01-01")) |>
  ggplot(aes(date, index, colour = country)) +
  geom_hline(yintercept = 100, colour = csis_colours[["ltgrey"]],
             linewidth = 0.4, linetype = "dashed") +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = au_us_colours) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
  labs(title = "Real GDP, Australia and the United States",
       subtitle = paste0("Index, ", gdp_base_year, " = 100. Chain volume / chained dollars, ",
                         "seasonally adjusted."),
       x = NULL, y = NULL,
       caption = paste("Sources: ABS, Australian National Accounts (cat. no. 5206.0);",
                       "US BEA via FRED (GDPC1).")) +
  theme_csis()

save_plot(p_gdp_idx, "gdp_index_au_us.png")

# Australia: ABS A3606058X (output per hour worked)
# US: FRED OPHPBS, business sector real output per hour of all persons
prod_base_year <- 2015

prod <- bind_rows(
  read_abs_series("A3606058X") |> select(date, value) |> mutate(country = "Australia"),
  fred("OPHPBS") |> mutate(country = "United States")
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
                       "US Bureau of Labor Statistics.")) +
  theme_csis()

save_plot(p_prod, "productivity_au_us.png")


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

latest_q <- max(gva$date)

level_ind <- gva |>
  filter(date == latest_q) |>
  mutate(share = 100 * value / sum(value)) |>
  arrange(desc(value)) |>
  mutate(industry = factor(industry, levels = industry))

stopifnot(nrow(level_ind) == 19)
print(head(level_ind, 3))   # check the title still holds

p_level_ind <- ggplot(level_ind, aes(value, industry)) +
  geom_col(fill = csis_colours[["navy"]], width = 0.75) +
  geom_text(aes(label = sprintf("$%sbn (%.1f%%)",
                                format(round(value / 1000), big.mark = ","), share)),
            hjust = -0.12, size = 2.9, family = "source_sans",
            colour = csis_colours[["grey"]]) +
  scale_x_continuous(labels = label_number(scale = 1e-3, prefix = "$", suffix = "bn"),
                     expand = expansion(mult = c(0, 0.20))) +
  scale_y_discrete(limits = rev) +   # largest bar at the top
  labs(title = "Mining is Australia's largest industry by gross value added",
       subtitle = paste0("Gross value added by ANZSIC division, current prices, ",
                         format(latest_q, "%B %Y"), " quarter. Share of total in brackets."),
       x = NULL, y = NULL,
       caption = "Source: ABS, Australian National Accounts (cat. no. 5206.0), Table 45.") +
  theme_csis() +
  theme_bars

save_plot(p_level_ind, "gva_level_industry.png", width = 8, height = 7)

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

# Aggregate GDP vs GDP per capita (World Bank)
wb <- WDI(country = c("AU", "US"),
          indicator = c(gdp_usd  = "NY.GDP.MKTP.CD",       # GDP, current US$
                        pcap_ppp = "NY.GDP.PCAP.PP.CD"),   # GDP per capita, PPP
          extra = FALSE)

size_yr <- wb |>
  filter(!is.na(gdp_usd), !is.na(pcap_ppp)) |>
  count(year) |>
  filter(n == 2) |>
  pull(year) |>
  max()

size <- wb |> filter(year == size_yr)
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

p_agg  <- size_bar(size |> mutate(v = gdp_usd / 1e12), "$%.1ftn",
                   label_number(prefix = "$", suffix = "tn"), "Aggregate GDP, current US$")
p_pcap <- size_bar(size |> mutate(v = pcap_ppp / 1e3), "$%.0fk",
                   label_number(prefix = "$", suffix = "k"), "GDP per capita, PPP")

p_size <- (p_agg | p_pcap) +
  plot_annotation(
    title   = "Australia is a high-income small country",
    caption = paste0("Source: World Bank World Development Indicators, ", size_yr, "."),
    theme   = theme_csis()
  )

save_plot(p_size, "au_us_size.png", width = 10, height = 3.8)

# Australia against US states. FRED state GDP series are <state code>NGSP,
# annual, millions of current dollars.
get_state_gdp <- function(name) {
  id <- paste0(state.abb[match(name, state.name)], "NGSP")
  x  <- try(getSymbols(id, src = "FRED", auto.assign = FALSE), silent = TRUE)
  if (inherits(x, "try-error")) return(NULL)
  data.frame(state = name, year = as.integer(format(index(x), "%Y")),
             gdp = as.numeric(x[, 1]))
}

state_gdp <- bind_rows(lapply(state.name, get_state_gdp))
stopifnot(nrow(state_gdp) > 0)
message("States retrieved: ", n_distinct(state_gdp$state), " of 50")

states_yr <- max(state_gdp$year)

au_wb  <- WDI(country = "AU", indicator = c(gdp = "NY.GDP.MKTP.CD")) |> filter(!is.na(gdp))
au_yr  <- min(states_yr, max(au_wb$year))
au_val <- au_wb$gdp[au_wb$year == au_yr] / 1e12

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

p_states <- ggplot(states_plot, aes(value, name, fill = is_au)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = sprintf("$%.2ftn", value)), hjust = -0.15,
            size = 2.9, family = "source_sans", colour = csis_colours[["grey"]]) +
  scale_fill_manual(values = c(`TRUE`  = csis_colours[["red"]],
                               `FALSE` = csis_colours[["ltgrey"]]), guide = "none") +
  scale_x_continuous(labels = label_number(prefix = "$", suffix = "tn", accuracy = 0.5),
                     expand = expansion(mult = c(0, 0.18))) +
  labs(title = paste0("Australia's economy is comparable to ", nearest),
       subtitle = paste0("GDP, current US$, ", states_yr, ". Australia shown in red."),
       x = NULL, y = NULL,
       caption = "Sources: US BEA; World Bank World Development Indicators.") +
  theme_csis() +
  theme_bars

save_plot(p_states, "au_vs_us_states.png", width = 8, height = 6)

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

ranked <- wb_oecd |>
  filter(year == oecd_yr) |>
  arrange(desc(gdp_usd)) |>
  mutate(rank = row_number(), is_au = country == "Australia")

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

p_oecd_rank <- ggplot(oecd_plot, aes(gdp_usd / 1e12, country_label, fill = is_au)) +
  geom_col(width = 0.72) +
  geom_text(data = filter(oecd_plot, is_au),
            aes(label = paste0("$", number(gdp_usd / 1e12, accuracy = 0.01), "tn")),
            hjust = -0.15, size = 3.1, family = "source_sans",
            colour = csis_colours[["grey"]]) +
  scale_fill_manual(values = c(`TRUE`  = csis_colours[["red"]],
                               `FALSE` = csis_colours[["ltgrey"]]), guide = "none") +
  scale_x_continuous(labels = label_dollar(suffix = "tn", accuracy = 1),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(title = paste0("Australia is the ", ordinal(au_rank),
                      "-largest economy in the OECD"),
       subtitle = paste0("Nominal GDP, current US$, ", oecd_yr,
                         ". Ranking is among all 38 OECD members."),
       x = NULL, y = NULL,
       caption = paste("Source: World Bank World Development Indicators (GDP, current US$);",
                       "OECD membership list.")) +
  theme_csis() +
  theme_bars

save_plot(p_oecd_rank, "oecd_gdp_rank_australia.png", width = 8, height = 6.2)