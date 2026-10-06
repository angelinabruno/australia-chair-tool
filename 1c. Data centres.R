rm(list = ls())
# The dashboard folder is set once, in section 1 of '1. Refresh all data.R'.
# To run this script on its own, run that section first.
if (Sys.getenv("DASHBOARD_DIR") == "")
  stop("Dashboard folder not set. Run section 1 of '1. Refresh all data.R' first.")
setwd(file.path(Sys.getenv("DASHBOARD_DIR"), "Data centres"))

# ABS 5625.0 Private New Capital Expenditure and Expected Expenditure
# Data centre proxy: Information Media & Telecommunications (IMT)
#
# All series current price, original - expected expenditure is only published
# on this basis, so actuals are kept the same for comparability.
#
# Coverage:
#   Actual      - B&S and EPM, all industries
#   Expected ST - B&S: Total/Mining/Non-Mining. EPM: full industry breakdown
#   Expected LT - B&S: Total/Mining/Non-Mining. EPM: Total/Mining/Non-Mining/IMT
# No expected B&S series for IMT, so the forward view is equipment only.

# install.packages(c("readabs","dplyr","tidyr","ggplot2","scales","showtext"))
library(readabs)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(showtext)


# 1. Style ----------------------------------------------------------------

font_add_google("Source Sans 3", "source_sans")
showtext_auto()
showtext_opts(dpi = 300)   # needs to match ggsave dpi

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

theme_set(theme_csis())

SRC <- paste("Source: ABS, Private New Capital Expenditure and Expected",
             "Expenditure, Australia (cat. 5625.0).")

save_csis <- function(plot, file, width = 9, height = 5.5) {
  ggsave(file, plot,
         width = width, height = height, dpi = 300, bg = "white")
}


# 2. Series ---------------------------------------------------------------

BS  <- "Buildings and Structures"
EPM <- "Equipment, Plant and Machinery"
IMT <- "Information Media and Telecommunications"

registry <- tribble(
  ~series_id,     ~measure,      ~asset, ~industry,                          ~basis,
  # Actual, B&S
  "A124792377V",  "Actual",      BS,     "Total",                            "CP",
  "A3517168V",    "Actual",      BS,     "Mining",                           "CP",
  "A124792079W",  "Actual",      BS,     "Non-Mining",                       "CP",
  "A3517189F",    "Actual",      BS,     IMT,                                "CP",
  # Actual, EPM
  "A124792395X",  "Actual",      EPM,    "Total",                            "CP",
  "A3517210L",    "Actual",      EPM,    "Mining",                           "CP",
  "A124792081J",  "Actual",      EPM,    "Non-Mining",                       "CP",
  "A3517213V",    "Actual",      EPM,    "Manufacturing",                    "CP",
  "A3517216A",    "Actual",      EPM,    "Electricity, Gas, Water & Waste",  "CP",
  "A3517219J",    "Actual",      EPM,    "Construction",                     "CP",
  "A3517222W",    "Actual",      EPM,    "Wholesale Trade",                  "CP",
  "A3517225C",    "Actual",      EPM,    "Retail Trade",                     "CP",
  "A83728708X",   "Actual",      EPM,    "Accommodation & Food Services",    "CP",
  "A3517228K",    "Actual",      EPM,    "Transport, Postal & Warehousing",  "CP",
  "A3517231X",    "Actual",      EPM,    IMT,                                "CP",
  # Expected ST, B&S
  "A124795991X",  "Expected ST", BS,     "Total",                            "CP",
  "A3517169W",    "Expected ST", BS,     "Mining",                           "CP",
  "A124796663F",  "Expected ST", BS,     "Non-Mining",                       "CP",
  # Expected ST, EPM
  "A124795992A",  "Expected ST", EPM,    "Total",                            "CP",
  "A3517211R",    "Expected ST", EPM,    "Mining",                           "CP",
  "A124796664J",  "Expected ST", EPM,    "Non-Mining",                       "CP",
  "A3517214W",    "Expected ST", EPM,    "Manufacturing",                    "CP",
  "A3517217C",    "Expected ST", EPM,    "Electricity, Gas, Water & Waste",  "CP",
  "A3517220T",    "Expected ST", EPM,    "Construction",                     "CP",
  "A3517223X",    "Expected ST", EPM,    "Wholesale Trade",                  "CP",
  "A3517226F",    "Expected ST", EPM,    "Retail Trade",                     "CP",
  "A83728686W",   "Expected ST", EPM,    "Accommodation & Food Services",    "CP",
  "A3517229L",    "Expected ST", EPM,    "Transport, Postal & Warehousing",  "CP",
  "A3517232A",    "Expected ST", EPM,    IMT,                                "CP",
  # Expected LT, B&S
  "A124794221K",  "Expected LT", BS,     "Total",                            "CP",
  "A3517170F",    "Expected LT", BS,     "Mining",                           "CP",
  "A124794893A",  "Expected LT", BS,     "Non-Mining",                       "CP",
  # Expected LT, EPM
  "A124794222L",  "Expected LT", EPM,    "Total",                            "CP",
  "A3517212T",    "Expected LT", EPM,    "Mining",                           "CP",
  "A124794894C",  "Expected LT", EPM,    "Non-Mining",                       "CP",
  "A3517233C",    "Expected LT", EPM,    IMT,                                "CP",
  # Actual, chain volume, SA
  "A124797535F",  "Actual",      BS,     "Total",                            "CVM_SA",
  "A3515896F",    "Actual",      BS,     IMT,                                "CVM_SA"
)


# 3. Load -----------------------------------------------------------------

raw <- read_abs(series_id = registry$series_id)

# offline: CSV with columns series_id, date, value
# raw <- readr::read_csv("data/abs_5625_series.csv")

dat <- raw %>%
  select(series_id, date, value) %>%
  inner_join(registry, by = "series_id") %>%
  mutate(date = as.Date(date)) %>%
  arrange(series_id, date)

missing <- setdiff(registry$series_id, unique(dat$series_id))
if (length(missing)) warning("Not returned: ", paste(missing, collapse = ", "))


# 4. Transforms -----------------------------------------------------------

# Original series are seasonal (June qtr peak), so use trailing 4-qtr sums.
# This also puts actuals on the same annual basis as expected expenditure.
roll4 <- function(x) as.numeric(stats::filter(x, rep(1, 4), sides = 1))

fy_start <- function(d) {
  y <- as.integer(format(d, "%Y")); m <- as.integer(format(d, "%m"))
  as.integer(ifelse(m >= 7, y, y - 1))
}
fy_label <- function(s) paste0(s, "-", sprintf("%02d", (s + 1) %% 100))

# Reference FY for expected series:
#   ST at date d -> FY containing d
#   LT at date d -> following FY
# Check: Dec-2025 release had first estimate for 2026-27, fy_start = 2025, +1 = 2026.
cp <- dat %>%
  filter(basis == "CP") %>%
  group_by(series_id) %>%
  mutate(roll4 = roll4(value)) %>%
  ungroup() %>%
  mutate(ref_fy = case_when(
    measure == "Actual"      ~ fy_start(date),
    measure == "Expected ST" ~ fy_start(date),
    measure == "Expected LT" ~ fy_start(date) + 1L
  ))

# check counts per reference year (should be 4 actuals per FY)
cp %>%
  filter(industry == IMT, asset == EPM, ref_fy >= 2018, !is.na(value)) %>%
  count(ref_fy, measure) %>%
  pivot_wider(names_from = measure, values_from = n, values_fill = 0) %>%
  arrange(ref_fy) %>%
  print(n = Inf)


# 5. IMT capex by asset ---------------------------------------------------

p1 <- cp %>%
  filter(measure == "Actual", industry == IMT) %>%
  ggplot(aes(date, roll4, colour = asset)) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = c(csis_colours[["navy"]], csis_colours[["red"]])) +
  scale_y_continuous(labels = label_comma(), expand = expansion(c(0, 0.05))) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  labs(
    title    = "Data centre build-out is evident from increased building and equipment expenditure",
    subtitle = "Information Media & Telecommunications capital expenditure, rolling four-quarter sum",
    y = "A$ million", x = NULL,
    caption  = paste(SRC, "Current price, original.")
  )
save_csis(p1, "01_divj_by_asset.png")


# 6. IMT share of total ---------------------------------------------------

p2 <- cp %>%
  filter(measure == "Actual", industry %in% c(IMT, "Total")) %>%
  select(date, asset, industry, roll4) %>%
  pivot_wider(names_from = industry, values_from = roll4) %>%
  mutate(share = .data[[IMT]] / Total) %>%
  filter(!is.na(share)) %>%
  ggplot(aes(date, share, colour = asset)) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = c(csis_colours[["navy"]], csis_colours[["red"]])) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     expand = expansion(c(0, 0.05)), limits = c(0, NA)) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  labs(
    title    = "Information Media & Telecommunications' share of national capex has broken out",
    subtitle = "Share of all-industry capital expenditure, rolling four-quarter sum",
    y = "Share of total", x = NULL,
    caption  = paste(SRC, "Current price, original.")
  )
save_csis(p2, "02_divj_share.png")


# 7. EPM by industry, actual and expected ---------------------------------

hl <- c(IMT, "Mining")

spaghetti <- function(df, ttl, sub) {
  ggplot() +
    geom_line(data = filter(df, !industry %in% hl),
              aes(date, plot_val, group = industry),
              colour = csis_colours[["ltgrey"]], linewidth = 0.5) +
    geom_line(data = filter(df, industry %in% hl),
              aes(date, plot_val, colour = industry), linewidth = 0.9) +
    scale_colour_manual(values = setNames(c(csis_colours[["red"]],
                                            csis_colours[["navy"]]), hl)) +
    scale_y_continuous(labels = label_comma(), expand = expansion(c(0, 0.05))) +
    scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
    labs(title = ttl, subtitle = sub, y = "A$ million", x = NULL,
         caption = paste(SRC, "Current price, original."))
}

p3 <- cp %>%
  filter(measure == "Actual", asset == EPM,
         !industry %in% c("Total", "Non-Mining"), !is.na(roll4)) %>%
  mutate(plot_val = roll4) %>%
  spaghetti("Information Media & Telecommunications is now rivalling mining on equipment capex",
            paste("Actual equipment, plant and machinery by industry,",
                  "rolling four-quarter sum; grey lines are other industries"))
save_csis(p3, "03_epm_actual_by_industry.png")

p4 <- cp %>%
  filter(measure == "Expected ST", asset == EPM,
         !industry %in% c("Total", "Non-Mining"), !is.na(value)) %>%
  mutate(plot_val = value) %>%
  spaghetti("Information Media & Telecommunications now leads expected equipment spend",
            paste("Short term expected equipment capex (current financial year)",
                  "by industry; grey lines are other industries"))
save_csis(p4, "04_epm_expected_by_industry.png")


# 8. Actual vs expected ---------------------------------------------------

# Expected values are full-year estimates reported each quarter, so they're
# plotted against the 4-qtr sum of actuals.
lbl <- c("Actual" = "Actual (rolling 4-qtr sum)",
         "Expected ST" = "Expected, current year",
         "Expected LT" = "Expected, next year")

plot_actual_vs_expected <- function(ind, ast, ttl, sub, from = as.Date("1987-01-01")) {
  d <- cp %>%
    filter(industry == ind, asset == ast, date >= from) %>%
    mutate(plot_val = if_else(measure == "Actual", roll4, value),
           series   = factor(lbl[measure], levels = unname(lbl))) %>%
    filter(!is.na(plot_val))
  
  ggplot(d, aes(date, plot_val, colour = series)) +
    geom_line(linewidth = 0.9) +
    scale_colour_manual(values = setNames(
      c(csis_colours[["navy"]], csis_colours[["teal"]], csis_colours[["gold"]]),
      unname(lbl)), drop = FALSE) +
    scale_y_continuous(labels = label_comma(), expand = expansion(c(0, 0.05))) +
    scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    labs(title = ttl, subtitle = sub, y = "A$ million (annualised)", x = NULL,
         caption = paste(SRC, "Current price, original. Expected series are",
                         "full-year estimates as reported each quarter."))
}

p5 <- plot_actual_vs_expected(
  "Total", BS,
  "Expectations lead the cycle, and understate it",
  "Buildings and structures, all industries")
save_csis(p5, "05_actual_vs_expected_bs_total.png")

p6 <- plot_actual_vs_expected(
  IMT, EPM,
  "Expected equipment spend in Information Media & Telecommunications has surged",
  "Equipment, plant and machinery, Information Media & Telecommunications",
  from = as.Date("2005-01-01"))
save_csis(p6, "06_actual_vs_expected_epm_divj.png")


# 9. Realisation ratios ---------------------------------------------------

# first LT estimate (~1 year ahead) vs FY outturn
actual_fy <- cp %>%
  filter(measure == "Actual", !is.na(value)) %>%
  group_by(industry, asset, ref_fy) %>%
  summarise(actual = sum(value), nq = n(), .groups = "drop") %>%
  filter(nq == 4)   # complete years only

first_lt <- cp %>%
  filter(measure == "Expected LT", !is.na(value)) %>%
  group_by(industry, asset, ref_fy) %>%
  slice_min(date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(industry, asset, ref_fy, first_est = value)

realisation <- actual_fy %>%
  inner_join(first_lt, by = c("industry", "asset", "ref_fy")) %>%
  mutate(ratio = actual / first_est,
         fy    = fy_label(ref_fy))

realisation %>%
  filter(industry == IMT, asset == EPM, ref_fy >= 2015) %>%
  select(fy, first_est, actual, ratio) %>%
  print(n = Inf)

rz <- realisation %>% filter(industry == IMT, asset == EPM, ref_fy >= 2012)

p7 <- ggplot(rz, aes(x = fy)) +
  geom_col(aes(y = actual, fill = "Actual outturn"), width = 0.6) +
  geom_point(aes(y = first_est, colour = "First estimate, a year ahead"),
             size = 2.6) +
  geom_text(aes(y = pmax(actual, first_est),
                label = sprintf("%.2fx", ratio)),
            vjust = -0.8, size = 3, family = "source_sans",
            colour = csis_colours[["grey"]]) +
  scale_fill_manual(values = c("Actual outturn" = csis_colours[["navy"]])) +
  scale_colour_manual(values = c("First estimate, a year ahead" = csis_colours[["red"]])) +
  scale_y_continuous(labels = label_comma(), expand = expansion(c(0, 0.12))) +
  labs(
    title    = "Firms have consistently under-forecast Information Media & Telecommunications equipment spend",
    subtitle = "Labels show actual as a multiple of the first estimate made a year ahead",
    y = "A$ million", x = NULL,
    caption  = paste(SRC, "Current price, original.")
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_csis(p7, "07_realisation_ratio_divj_epm.png")


# 10. IMT capex by financial year -----------------------------------------

fy_divj <- actual_fy %>%
  filter(industry == IMT, ref_fy >= 2010) %>%
  mutate(fy = fy_label(ref_fy))

p8 <- ggplot(fy_divj, aes(fy, actual, fill = asset)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = c(csis_colours[["navy"]], csis_colours[["red"]])) +
  scale_y_continuous(labels = label_comma(), expand = expansion(c(0, 0.05))) +
  labs(
    title    = "Information Media & Telecommunications capital expenditure by financial year",
    subtitle = "Complete financial years only",
    y = "A$ million", x = NULL,
    caption  = paste(SRC, "Current price, original.")
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
save_csis(p8, "08_divj_financial_year.png")


# 11. Real B&S capex (CVM, SA) --------------------------------------------

# kept separate from the current price charts
p9 <- dat %>%
  filter(basis == "CVM_SA") %>%
  ggplot(aes(date, value, colour = industry)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~industry, scales = "free_y", ncol = 1) +
  scale_colour_manual(values = setNames(c(csis_colours[["red"]],
                                          csis_colours[["navy"]]),
                                        c(IMT, "Total"))) +
  scale_y_continuous(labels = label_comma()) +
  labs(
    title    = "Real building and structures capex",
    subtitle = "Chain volume measures, seasonally adjusted, quarterly",
    y = "A$ million", x = NULL,
    caption  = paste(SRC, "Chain volume measures, seasonally adjusted.")
  ) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", hjust = 0,
                                  colour = csis_colours[["navy"]]))
save_csis(p9, "09_real_momentum.png", height = 7)