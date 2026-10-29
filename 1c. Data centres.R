rm(list = ls())
# The dashboard folder is set once, in section 1 of '1. Refresh all data.R'.
# To run this script on its own, run that section first.
if (Sys.getenv("DASHBOARD_DIR") == "")
  stop("Dashboard folder not set. Run section 1 of '1. Refresh all data.R' first.")
setwd(file.path(Sys.getenv("DASHBOARD_DIR"), "Data centres"))
source(file.path(Sys.getenv("DASHBOARD_DIR"), "0. Exchange rates.R"))   # A$/US$ toggle
source(file.path(Sys.getenv("DASHBOARD_DIR"), "0. Takeaways.R"))       # line under each chart

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
library(patchwork)


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

# US$ versions for the currency toggle. Actual quarters use that quarter's
# average rate; expected full-year estimates use their financial year's average
# (or the latest 12 months for years not yet under way).
cp_usd <- cp %>%
  mutate(rate  = fx_or_latest(if_else(measure == "Actual", fx_quarter(date),
                                      fx_financial_year(ref_fy + 1L))),
         value = value * rate) %>%
  group_by(series_id) %>%
  mutate(roll4 = roll4(value)) %>%
  ungroup() %>%
  select(-rate)
cp_aud <- cp
cp_for <- function(cur) if (cur == "USD") cp_usd else cp_aud
# Quarters the ABS suppresses ("np") are NA. They are kept, not dropped, so
# lines break at them instead of drawing straight across.
GAPS <- "Gaps: not published by ABS."
y_lab  <- function(cur, extra = "") paste0(cur_sym[[cur]], " million", extra)
src_cp <- function(cur, extra = "") paste0(SRC, " Current price, original. ", GAPS,
                                           if (nzchar(extra)) paste0("
", extra) else "",
                                          if (cur == "USD") fx_note[["USD"]] else "")

# check counts per reference year (should be 4 actuals per FY)
cp %>%
  filter(industry == IMT, asset == EPM, ref_fy >= 2018, !is.na(value)) %>%
  count(ref_fy, measure) %>%
  pivot_wider(names_from = measure, values_from = n, values_fill = 0) %>%
  arrange(ref_fy) %>%
  print(n = Inf)


# 5. IMT capex by asset ---------------------------------------------------

# Charts in A$ or US$ are drawn twice through save_both(); each make_*()
# takes cur = "AUD" or "USD".
make_p1 <- function(cur) cp_for(cur) %>%
  filter(measure == "Actual", industry == IMT) %>%
  ggplot(aes(date, roll4, colour = asset)) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = c(csis_colours[["navy"]], csis_colours[["teal"]])) +
  scale_y_continuous(labels = label_comma(), expand = expansion(c(0, 0.05))) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  labs(
    title    = "Information Media & Telecommunications capital expenditure by asset type",
    subtitle = "Information Media & Telecommunications capital expenditure, rolling four-quarter sum",
    y = y_lab(cur), x = NULL,
    caption  = src_cp(cur)
  )
save_both(make_p1, "01_divj_by_asset.png", save_csis)


# 6. IMT share of total ---------------------------------------------------

p2 <- cp %>%
  filter(measure == "Actual", industry %in% c(IMT, "Total")) %>%
  select(date, asset, industry, roll4) %>%
  pivot_wider(names_from = industry, values_from = roll4) %>%
  mutate(share = .data[[IMT]] / Total) %>%
  filter(date >= min(date[!is.na(share)])) %>%   # NA quarters stay, so the line breaks
  ggplot(aes(date, share, colour = asset)) +
  geom_line(linewidth = 0.9) +
  scale_colour_manual(values = c(csis_colours[["navy"]], csis_colours[["teal"]])) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     expand = expansion(c(0, 0.05)), limits = c(0, NA)) +
  scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
  labs(
    title    = "Information Media & Telecommunications share of all-industry capex",
    subtitle = "Share of all-industry capital expenditure, rolling four-quarter sum",
    y = "Share of total", x = NULL,
    caption  = paste(SRC, "Current price, original.", GAPS)
  )
save_csis(p2, "02_divj_share.png")


# 7. EPM by industry, actual and expected ---------------------------------

hl <- c(IMT, "Mining")

spaghetti <- function(df, ttl, sub, cur) {
  ggplot() +
    geom_line(data = filter(df, !industry %in% hl),
              aes(date, plot_val, group = industry),
              colour = csis_colours[["ltgrey"]], linewidth = 0.5) +
    geom_line(data = filter(df, industry %in% hl),
              aes(date, plot_val, colour = industry), linewidth = 0.9) +
    scale_colour_manual(values = setNames(c(csis_colours[["teal"]],
                                            csis_colours[["navy"]]), hl)) +
    scale_y_continuous(labels = label_comma(), expand = expansion(c(0, 0.05))) +
    scale_x_date(date_breaks = "3 years", date_labels = "%Y") +
    labs(title = ttl, subtitle = sub, y = y_lab(cur), x = NULL,
         caption = src_cp(cur))
}

make_p3 <- function(cur) cp_for(cur) %>%
  filter(measure == "Actual", asset == EPM,
         !industry %in% c("Total", "Non-Mining")) %>%
  mutate(plot_val = roll4) %>%
  spaghetti("Equipment, plant and machinery capex by industry",
            paste("Actual equipment, plant and machinery by industry,",
                  "rolling four-quarter sum; grey lines are other industries"), cur)
save_both(make_p3, "03_epm_actual_by_industry.png", save_csis)

# 8. Expected spending, like for like -------------------------------------

# The ABS asks about each financial year seven times, so a year's estimate
# grows as plans firm up and actual spending replaces expectations. Plotting
# every survey's expectation as one line gives a saw-tooth; the ABS instead
# compares the same estimate number across years (5625.0, Table 17).
#
# The short-term (ST) and long-term (LT) expected series each cover only part
# of a year, depending on the survey quarter, so the ABS builds each
# financial-year estimate from them. For the year starting in July of year t:
#   Est. 1  LT reported in the Dec qtr of t-1 (12 months expected)
#   Est. 2  LT reported in the Mar qtr of t
#   Est. 3  ST + LT reported in the Jun qtr of t (6 + 6 months expected)
#   Est. 4  Sep qtr actual + ST + LT reported in the Sep qtr (3 actual + 9 expected)
#   Est. 5  Sep and Dec qtr actuals + ST reported in the Dec qtr (6 + 6)
#   Est. 6  Sep, Dec and Mar qtr actuals + ST reported in the Mar qtr (9 + 3)
#   Est. 7  the four quarters' actuals
# Checked against Table 17 (all industries) when the script runs, below.
qtr_m <- function(d) as.integer(format(d, "%m"))

estimates_for <- function(cur) {
  w <- cp_for(cur) %>%
    mutate(fy = fy_start(date), m = qtr_m(date)) %>%
    select(industry, asset, measure, fy, m, value)
  get <- function(meas, month, shift = 0L) w %>%
    filter(measure == meas, m == month) %>%
    transmute(industry, asset, ref_fy = fy + shift, v = value)
  a <- function(month) get("Actual", month)
  sum_parts <- function(est, ...) Reduce(function(x, y)
    inner_join(x, y, by = c("industry", "asset", "ref_fy")), list(...)) %>%
    mutate(value = rowSums(across(starts_with("v")))) %>%
    transmute(industry, asset, ref_fy, est, value)
  bind_rows(
    get("Expected LT", 12, 1L) %>% transmute(industry, asset, ref_fy, est = 1L, value = v),
    get("Expected LT", 3, 1L)  %>% transmute(industry, asset, ref_fy, est = 2L, value = v),
    sum_parts(3L, get("Expected ST", 6, 1L), get("Expected LT", 6, 1L)),
    sum_parts(4L, a(9), get("Expected ST", 9), get("Expected LT", 9)),
    sum_parts(5L, a(9), a(12), get("Expected ST", 12)),
    sum_parts(6L, a(9), a(12), a(3), get("Expected ST", 3)),
    sum_parts(7L, a(9), a(12), a(3), a(6))
  ) %>%
    filter(!is.na(value))
}

# Check against the ABS's own estimates for all industries (Table 17).
local({
  t17 <- tryCatch(read_abs("5625.0", tables = 17, path = file.path(getwd(), "raw")),
                  error = function(e) NULL)
  if (is.null(t17)) return(message("Table 17 not reached; estimates not checked"))
  abs_e <- t17 %>%
    filter(grepl("Equipment, Plant", series), grepl("Total, including Education", series)) %>%
    mutate(est = as.integer(sub("^Estimate ([1-7]).*", "\\1", series)),
           ref_fy = fy_start(as.Date(date))) %>%
    select(ref_fy, est, abs = value)
  mine <- estimates_for("AUD") %>% filter(industry == "Total", asset == EPM)
  chk <- inner_join(mine, abs_e, by = c("ref_fy", "est")) %>% filter(ref_fy >= 2015)
  if (!nrow(chk)) return(message("Table 17 layout not recognised; estimates not checked"))
  gap <- max(abs(chk$value / chk$abs - 1), na.rm = TRUE)
  message(sprintf("Estimates vs ABS Table 17 (all industries, EPM, %d values): largest gap %.1f%%",
                  nrow(chk), 100 * gap))
  if (gap > 0.02) warning("Estimates differ from ABS Table 17 by up to ",
                          round(100 * gap, 1), "%. Check the mapping in section 8.")
})

# Latest estimate made: its number, and the year it is for
latest_est <- estimates_for("AUD") %>%
  filter(est < 7, industry == "Total", asset == EPM) %>%
  arrange(ref_fy, est) %>% slice_tail(n = 1)
est_name <- function(n) c("first", "second", "third", "fourth", "fifth", "sixth",
                          "seventh")[n]

# 04: each industry's estimate for a year, using the same estimate number as
# the latest one, so every year is measured at the same point in the survey.
make_p4 <- function(cur) {
  d <- estimates_for(cur) %>%
    filter(asset == EPM, est == latest_est$est,
           !industry %in% c("Total", "Non-Mining"), ref_fy >= 2005) %>%
    mutate(fy_end = ref_fy + 1L)
  ggplot() +
    geom_line(data = filter(d, !industry %in% hl),
              aes(fy_end, value, group = industry),
              colour = csis_colours[["ltgrey"]], linewidth = 0.5) +
    geom_line(data = filter(d, industry %in% hl),
              aes(fy_end, value, colour = industry), linewidth = 0.9) +
    geom_point(data = filter(d, industry %in% hl, ref_fy == max(ref_fy)),
               aes(fy_end, value, colour = industry), size = 2.2) +
    scale_colour_manual(values = setNames(c(csis_colours[["teal"]],
                                            csis_colours[["navy"]]), hl)) +
    scale_y_continuous(labels = label_comma(), expand = expansion(c(0, 0.05))) +
    scale_x_continuous(breaks = pretty_breaks(8),
                       labels = function(x) fy_label(x - 1L)) +
    labs(title = "Expected equipment, plant and machinery capex by industry",
         subtitle = paste0("Estimate ", latest_est$est, " (the ", est_name(latest_est$est),
                           " of seven) for each financial year; grey lines are other industries"),
         y = y_lab(cur), x = NULL,
         caption = src_cp(cur, paste("Each year is shown at the same estimate number,",
                                     "as the ABS compares them.")))
}
save_both(make_p4, "04_epm_expected_by_industry.png", save_csis)

# 05, 06: each financial year's estimates in order, one line per year, so the
# latest year can be read against earlier years at the same stage.
plot_estimate_paths <- function(ind, ast, ttl, sub, n_years = 8, cur = "AUD") {
  d <- estimates_for(cur) %>%
    filter(industry == ind, asset == ast) %>%
    filter(ref_fy > max(ref_fy) - n_years) %>%
    mutate(fy = fy_label(ref_fy),
           latest = ref_fy == max(ref_fy))
  last_pts <- d %>% group_by(fy) %>% slice_max(est, n = 1) %>% ungroup()
  ggplot(d, aes(est, value, group = fy)) +
    geom_line(data = filter(d, !latest), colour = csis_colours[["ltgrey"]], linewidth = 0.6) +
    geom_line(data = filter(d, latest), colour = csis_colours[["navy"]], linewidth = 1.1) +
    geom_point(data = filter(d, latest), colour = csis_colours[["navy"]], size = 1.8) +
    ggrepel::geom_text_repel(data = last_pts,
              aes(label = fy, colour = if_else(latest, "latest", "other")),
              direction = "y", hjust = 0, nudge_x = 0.12, min.segment.length = Inf,
              size = 2.8, family = "source_sans", show.legend = FALSE) +
    scale_colour_manual(values = c(latest = csis_colours[["navy"]],
                                   other = csis_colours[["grey"]])) +
    scale_x_continuous(breaks = 1:7, labels = c(paste("Est.", 1:6), "Actual"),
                       expand = expansion(mult = c(0.02, 0.1))) +
    scale_y_continuous(labels = label_comma(), expand = expansion(c(0, 0.05)),
                       limits = c(0, NA)) +
    labs(title = ttl, subtitle = sub, y = y_lab(cur), x = NULL,
         caption = src_cp(cur, paste("Estimates 1-3 are made before the year starts, 4-6 during it;",
                                     "Actual = the four quarters (estimate 7).")))
}


make_p6 <- function(cur) plot_estimate_paths(
  IMT, EPM,
  "Information Media & Telecommunications equipment capex: estimates and outturn",
  "Equipment, plant and machinery; each line is one financial year, latest in navy",
  cur = cur)
save_both(make_p6, "06_actual_vs_expected_epm_divj.png", save_csis)


# 9. Realisation ratios ---------------------------------------------------

# Full-year actuals (complete years only)
actual_fy_for <- function(cur) cp_for(cur) %>%
  filter(measure == "Actual", !is.na(value)) %>%
  group_by(industry, asset, ref_fy) %>%
  summarise(actual = sum(value), nq = n(), .groups = "drop") %>%
  filter(nq == 4)   # complete years only

realisation_for <- function(cur) {
  # Realisation ratio for estimate 1 (as in ABS Table 18): the year's actual
  # spending (estimate 7) over the first estimate, made in the Dec quarter
  # about 18 months before the year ends.
  e <- estimates_for(cur) %>% filter(est %in% c(1L, 7L))
  inner_join(filter(e, est == 1L) %>% select(industry, asset, ref_fy, first_est = value),
             filter(e, est == 7L) %>% select(industry, asset, ref_fy, actual = value),
             by = c("industry", "asset", "ref_fy")) %>%
    mutate(ratio = actual / first_est,
           fy    = fy_label(ref_fy))
}

realisation_for("AUD") %>%
  filter(industry == IMT, asset == EPM, ref_fy >= 2015) %>%
  select(fy, first_est, actual, ratio) %>%
  print(n = Inf)

make_p7 <- function(cur) {
# IMT against all industries (the benchmark): how far the outturn ended up
# above or below the first estimate made a year ahead.
rz <- realisation_for(cur) %>%
  filter(industry %in% c(IMT, "Total"), asset == EPM, ref_fy >= 2012) %>%
  mutate(who = if_else(industry == IMT, "Information Media & Telecommunications",
                       "All industries"))
all_fy <- seq(min(rz$ref_fy), max(rz$ref_fy))
# A suppressed quarter means no full-year outturn (e.g. 2015-16, March 2016 not published)
np_fy  <- setdiff(all_fy, rz$ref_fy[rz$industry == IMT])
rz <- rz %>% mutate(fy = factor(fy, levels = fy_label(all_fy)),
                    who = factor(who, levels = c("Information Media & Telecommunications",
                                                 "All industries")))

ggplot(rz, aes(fy, ratio, fill = who)) +
  geom_hline(yintercept = 1, colour = csis_colours[["grey"]], linewidth = 0.5) +
  geom_col(position = position_dodge(width = 0.75, preserve = "single"), width = 0.7) +
  geom_text(data = filter(rz, industry == IMT), aes(label = sprintf("%.2fx", ratio)),
            position = position_nudge(x = -0.19), vjust = -0.5, size = 2.8,
            family = "source_sans", colour = csis_colours[["navy"]]) +
  annotate("text", x = fy_label(np_fy), y = 0, label = "Not\npublished", vjust = -0.3,
           size = 2.8, lineheight = 0.9, family = "source_sans",
           colour = csis_colours[["grey"]]) +
  scale_x_discrete(drop = FALSE) +
  scale_fill_manual(values = c("Information Media & Telecommunications" = csis_colours[["navy"]],
                               "All industries" = csis_colours[["ltgrey"]])) +
  scale_y_continuous(labels = function(x) paste0(x, "x"), expand = expansion(c(0, 0.12))) +
  labs(
    title    = "Equipment capex: actual outturn as a multiple of the first estimate",
    subtitle = paste("Realisation ratio: actual spending in the year divided by estimate 1,",
                     "made six months before the year starts; 1x = as expected"),
    y = NULL, x = NULL,
    caption  = src_cp(cur)
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
save_both(make_p7, "07_realisation_ratio_divj_epm.png", save_csis)

# Pairs shown as one image each in the dashboard's "Australian capex detail"
# panel: firms' plans against what they spent, and spending by industry
make_p6_7 <- function(cur) {
  (make_p6(cur) + labs(caption = NULL)) / make_p7(cur) +
    plot_annotation(title = "Equipment capex: what firms planned and what they spent",
                    theme = theme_csis())
}
save_both(make_p6_7, "06_expected_and_realisation_epm_divj.png", save_csis, height = 10)

make_p3_4 <- function(cur) {
  (make_p3(cur) + labs(caption = NULL)) / make_p4(cur) +
    plot_annotation(title = "Equipment, plant and machinery capex by industry: actual and expected",
                    theme = theme_csis())
}
save_both(make_p3_4, "03_epm_by_industry_actual_expected.png", save_csis, height = 10)


# 10. IMT capex by financial year -----------------------------------------

make_p8 <- function(cur) {
fy_divj <- actual_fy_for(cur) %>%
  filter(industry == IMT, ref_fy >= 2010)
# Years with a suppressed quarter have no full-year total; keep them on the
# axis with a label rather than closing up the gap.
all_fy <- seq(min(fy_divj$ref_fy), max(fy_divj$ref_fy))
np_fy  <- setdiff(all_fy, fy_divj$ref_fy)
fy_divj <- fy_divj %>% mutate(fy = factor(fy_label(ref_fy), levels = fy_label(all_fy)))

ggplot(fy_divj, aes(fy, actual, fill = asset)) +
  geom_col(width = 0.7) +
  annotate("text", x = fy_label(np_fy), y = 0, label = "Not\npublished", vjust = -0.3,
           size = 2.8, lineheight = 0.9, family = "source_sans",
           colour = csis_colours[["grey"]]) +
  scale_x_discrete(drop = FALSE) +
  scale_fill_manual(values = c(csis_colours[["navy"]], csis_colours[["teal"]])) +
  scale_y_continuous(labels = label_comma(), expand = expansion(c(0, 0.05))) +
  labs(
    title    = "Information Media & Telecommunications capital expenditure by financial year",
    subtitle = "Complete financial years only",
    y = y_lab(cur), x = NULL,
    caption  = src_cp(cur)
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
save_both(make_p8, "08_divj_financial_year.png", save_csis)



# 12. Takeaways under each chart ------------------------------------------

local({
  dc <- function(file) file.path("Data centres", file)
  fx <- fx_financial_year
  # Full-year IMT spending, both assets
  fy <- actual_fy_for("AUD") %>% filter(industry == IMT) %>%
    group_by(ref_fy) %>% summarise(v = sum(actual) / 1000, n = n(), .groups = "drop") %>%
    filter(n == 2) %>% arrange(ref_fy)
  l <- fy[nrow(fy), ]; p <- fy[nrow(fy) - 1, ]
  mny <- function(cur, x, yr) fmt_bn(if (cur == "USD") x * fx(yr + 1L) else x, cur_sym[[cur]])
  line8 <- function(cur) sprintf(
    "Information media and telecommunications firms, which include data centres, spent %s on new capital in %s, up from %s the year before.",
    mny(cur, l$v, l$ref_fy), fy_label(l$ref_fy), mny(cur, p$v, p$ref_fy))
  takeaway(dc("08_divj_financial_year.png"), line8("AUD"), line8("USD"))
  takeaway(dc("01_divj_by_asset.png"), line8("AUD"), line8("USD"))

  sh <- cp %>% filter(measure == "Actual", industry %in% c(IMT, "Total")) %>%
    select(date, asset, industry, roll4) %>%
    pivot_wider(names_from = industry, values_from = roll4) %>%
    mutate(share = .data[[IMT]] / Total) %>% filter(!is.na(share)) %>%
    group_by(asset) %>% mutate(record = share == max(share)) %>%
    slice_max(date, n = 1) %>% ungroup()
  epm <- sh$share[sh$asset == EPM]; bs <- sh$share[sh$asset == BS]
  takeaway(dc("02_divj_share.png"), sprintf(
    "In the year to %s the industry accounted for %.0f%% of all private equipment spending and %.0f%% of building spending%s.",
    format(max(sh$date), "%B %Y"), 100 * epm, 100 * bs,
    if (all(sh$record)) ", both the highest since the series began in 1987" else ""))

  rz <- realisation_for("AUD") %>% filter(asset == EPM, industry %in% c(IMT, "Total")) %>%
    filter(ref_fy == max(ref_fy[industry == IMT]))
  takeaway(dc("07_realisation_ratio_divj_epm.png"), sprintf(
    "In %s the industry spent %.1f times its first estimate on equipment; across all industries the ratio was %.1f.",
    rz$fy[1], rz$ratio[rz$industry == IMT], rz$ratio[rz$industry == "Total"]))

  e <- estimates_for("AUD") %>% filter(industry == IMT, asset == EPM, est == latest_est$est) %>%
    arrange(ref_fy)
  el <- e[nrow(e), ]; ep <- e[nrow(e) - 1, ]
  line6 <- function(cur) sprintf(
    "At estimate %d, firms expect to spend %s on equipment in %s, %.1f times the %s they expected at the same stage a year earlier.",
    el$est, mny(cur, el$value / 1000, el$ref_fy), fy_label(el$ref_fy),
    el$value / ep$value, mny(cur, ep$value / 1000, ep$ref_fy))
  takeaway(dc("06_actual_vs_expected_epm_divj.png"), line6("AUD"), line6("USD"))
  takeaway(dc("04_epm_expected_by_industry.png"), line6("AUD"), line6("USD"))


  e3 <- cp %>% filter(measure == "Actual", asset == EPM, industry == IMT, !is.na(roll4)) %>%
    slice_max(date, n = 1)
  m3 <- cp %>% filter(measure == "Actual", asset == EPM, industry == "Mining", date == e3$date)
  e3u <- cp_usd %>% filter(measure == "Actual", asset == EPM, date == e3$date,
                           industry %in% c(IMT, "Mining"))
  line3 <- function(i, m, s) sprintf(
    "Equipment spending by information media and telecommunications reached %s in the year to %s, against %s for mining.",
    fmt_bn(i / 1000, s), format(e3$date, "%B %Y"), fmt_bn(m / 1000, s))
  takeaway(dc("03_epm_actual_by_industry.png"), line3(e3$roll4, m3$roll4, "A$"),
           line3(e3u$roll4[e3u$industry == IMT], e3u$roll4[e3u$industry == "Mining"], "US$"))

})


# 13. Power, building and costs: data centres directly -------------------

# ABS capex covers the whole Information Media & Telecommunications industry.
# These charts look at data centres themselves: their electricity demand and
# grid connections (AEMO, Transgrid), the buildings (ABS building activity and
# approvals, "Commercial buildings n.e.c.", where the ABS classifies data
# centres), the computers that go in them (ABS imports) and what power costs
# in US data centre states (EIA).
#
# Figures that come from reports rather than data files are kept by hand in
# dc_reference_data.csv and dc_projects_tracker.csv (in this folder), each
# with its source and link. Update them when AEMO publishes each Quarterly
# Energy Dynamics and when the other sources are revised.

ref <- utils::read.csv("dc_reference_data.csv", comment.char = "#", stringsAsFactors = FALSE)
ref_get <- function(ds) ref[ref$dataset == ds, ]
to_qtr  <- function(d) as.Date(paste0(format(d, "%Y"), "-",
                                      sprintf("%02d", (as.integer(format(d, "%m")) - 1) %/% 3 * 3 + 3), "-01"))

# 13a. Data centre demand by region (AEMO QED)
dem <- ref_get("aemo_qed_demand") |>
  mutate(period = factor(period, levels = unique(period)),
         region = factor(region, levels = c("New South Wales", "Victoria")))
p10 <- ggplot(dem, aes(region, value, fill = period)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = paste0(value, " MW")), position = position_dodge(width = 0.75),
            vjust = -0.5, size = 3.4, family = "source_sans", colour = csis_colours[["grey"]]) +
  scale_fill_manual(values = c(csis_colours[["ltgrey"]], csis_colours[["navy"]])) +
  scale_y_continuous(labels = label_comma(suffix = " MW"), expand = expansion(c(0, 0.12))) +
  labs(title = "Average electricity demand of data centres by NEM region",
       subtitle = "Average demand over the quarter, megawatts",
       x = NULL, y = NULL,
       caption = paste0("Source: AEMO, Quarterly Energy Dynamics Q2 2026. ",
                        "Q2 2025 for New South Wales is derived from AEMO's +35% year-on-year change.\n",
                        "Other regions are small (South Australia 2%, Queensland 1% of FY26 data centre demand; AEMO 2026 ESOO)."))
save_csis(p10, "10_dc_demand_by_region.png", height = 5)

# 13b. Connection pipeline by stage (AEMO QED)
pipe <- ref_get("aemo_qed_pipeline") |>
  left_join(ref_get("aemo_qed_pipeline_projects") |> select(item, projects = value), by = "item") |>
  mutate(item = factor(item, levels = rev(item)))
pipe_reg <- ref_get("aemo_qed_pipeline_region")
p11 <- ggplot(pipe, aes(value, item)) +
  geom_col(fill = csis_colours[["navy"]], width = 0.6) +
  geom_text(aes(label = sprintf("%.1f GW (%d projects)", value, projects)),
            hjust = -0.08, size = 3.4, family = "source_sans", colour = csis_colours[["grey"]]) +
  scale_x_continuous(labels = label_number(suffix = " GW"), expand = expansion(c(0, 0.35))) +
  labs(title = "Data centres in AEMO's transmission connection process",
       subtitle = sprintf("Maximum connection capacity by stage, %s: %d projects, %.1f GW",
                          pipe$period[1], sum(pipe$projects), sum(pipe$value)),
       x = NULL, y = NULL,
       caption = paste0("Source: AEMO, Quarterly Energy Dynamics Q2 2026. Projects that have formally applied to AEMO; ",
                        "loads under 5 MW are connected by network businesses.\nCapacity by region: ",
                        paste(sprintf("%s %s%%", pipe_reg$region, pipe_reg$value), collapse = ", "), ".")) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#E6E7E8", linewidth = 0.4))
save_csis(p11, "11_dc_connection_pipeline.png", height = 4)

# 13c. Data centres' share of grid electricity, Australia vs US
# Australia: AEMO's 2026 ESOO (NEM, Step Change). US: LBNL's 2023 estimate and
# its 2028 range, and EPRI's February 2026 range for 2030 (newer and higher).
# Definitions differ a little: AEMO's share is of NEM operational consumption,
# LBNL's and EPRI's of total US electricity use.
gs <- bind_rows(ref_get("grid_share"), ref_get("grid_share_low"), ref_get("grid_share_high"),
                ref_get("grid_share_range")) |>
  mutate(year = as.integer(substr(period, 1, 4)) + if_else(grepl("-", period), 1L, 0L),
         country = if_else(region == "United States", "United States", "Australia"))
gs_line <- filter(gs, dataset == "grid_share")
gs_rng  <- bind_rows(
  tibble(year = 2028, src = "LBNL", lo = gs$value[gs$dataset == "grid_share_low"],
         hi = gs$value[gs$dataset == "grid_share_high"]),
  gs |> filter(dataset == "grid_share_range") |>
    summarise(year = first(year), src = "EPRI", lo = min(value), hi = max(value)))
p12 <- ggplot() +
  geom_linerange(data = gs_rng, aes(x = year, ymin = lo, ymax = hi),
                 colour = csis_colours[["red"]], linewidth = 3, alpha = 0.35) +
  geom_line(data = filter(gs_line, country == "Australia"), aes(year, value, colour = country),
            linewidth = 0.9, linetype = "22") +
  geom_point(data = gs_line, aes(year, value, colour = country), size = 2.6) +
  geom_text(data = gs_line, aes(year, value, colour = country, label = paste0(value, "%")),
            vjust = -1, size = 3.3, family = "source_sans", show.legend = FALSE) +
  geom_text(data = gs_rng, aes(x = year + 0.3, y = hi,
                               label = sprintf("US %d, %s: %s-%s%%", year, src, lo, hi)),
            hjust = 0, vjust = 1, size = 3.1, family = "source_sans", colour = csis_colours[["red"]]) +
  scale_colour_manual(values = c(Australia = csis_colours[["navy"]], `United States` = csis_colours[["red"]])) +
  scale_y_continuous(labels = label_percent(scale = 1, accuracy = 1), limits = c(0, 18),
                     expand = expansion(c(0, 0.02))) +
  scale_x_continuous(breaks = seq(2022, 2036, 2), limits = c(2022, 2037)) +
  labs(title = "Data centres' share of electricity use, Australia and the United States",
       subtitle = "Actual and forecast share of electricity consumption; bars are forecast ranges",
       x = NULL, y = NULL,
       caption = paste("Sources: AEMO, 2026 ESOO data centre forecasting overview (NEM operational consumption, Step Change,",
                       "financial years shown by the year they end);
Lawrence Berkeley National Laboratory,",
                       "2024 United States Data Center Energy Usage Report; EPRI, Powering Intelligence 2026",
                       "(share of total US electricity use).
Australia covers the NEM only (not Western Australia or",
                       "the Northern Territory)."))
save_csis(p12, "12_dc_share_of_grid_au_us.png", height = 5)

# 13d. Transgrid: Sydney basin enquiries vs capacity
tg <- ref_get("transgrid_sydney")
tg$label <- factor(c("Connection enquiries\n(more than)", "Signed connection\nagreements",
                     "Extra capacity if data\ncentres pay for upgrades"),
                   levels = c("Connection enquiries\n(more than)", "Signed connection\nagreements",
                              "Extra capacity if data\ncentres pay for upgrades"))
tg$fill <- c("req", "have", "maybe")
p13 <- ggplot(tg, aes(label, value, fill = fill)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(value, " GW")), vjust = -0.5, size = 3.6,
            family = "source_sans", colour = csis_colours[["grey"]]) +
  scale_fill_manual(values = c(req = csis_colours[["ltgrey"]], have = csis_colours[["navy"]],
                               maybe = csis_colours[["teal"]]), guide = "none") +
  scale_y_continuous(labels = label_number(suffix = " GW"), expand = expansion(c(0, 0.1))) +
  labs(title = "Data centre demand for grid connections in Sydney, and the capacity available",
       subtitle = "Transgrid transmission network, Sydney basin, gigawatts",
       x = NULL, y = NULL,
       caption = paste("Sources: Transgrid, Sydney basin network capacity update, 20 August 2026;
",
                       "Transgrid, New transmission capacity in Sydney to be funded by data centres, 26 August 2026.",
                       "\nTransgrid says it cannot offer further capacity without network augmentation, which data centres would fund."))
save_csis(p13, "13_transgrid_sydney_capacity.png", height = 5)

# 13e. Buildings: value of work done and approvals, Commercial buildings n.e.c.
# 8752.0 work done is quarterly (rolling four-quarter sum); 8731.0 approvals
# are monthly and jump around with single large projects, so they are shown
# as a trailing 12-month moving average with the monthly values faint behind.
# The two are on different time bases, so each has its own panel.
# Private sector, current prices, original.
roll12 <- function(x) as.numeric(stats::filter(x, rep(1 / 12, 12), sides = 1))   # trailing mean

bld_ids <- c(done = "A751834F", approved = "A119227564K")
bld <- read_abs(series_id = unname(bld_ids)) |>
  transmute(series_id, date = as.Date(date), value = value / 1000) |>   # $'000 -> $ million
  mutate(measure = names(bld_ids)[match(series_id, bld_ids)]) |>
  arrange(measure, date) |>
  filter(date >= as.Date("2008-01-01"))
bld_for <- function(cur) bld |>
  mutate(value = if (cur == "USD")
                   value * fx_or_latest(if_else(measure == "done", fx_quarter(date), fx_month(date)))
                 else value) |>
  group_by(measure) |>
  mutate(smooth = if (first(measure) == "done") roll4(value) else roll12(value)) |>
  ungroup() |>
  filter(date >= as.Date("2010-01-01"))
bld_lbl <- c(approved = "Value of building approved, per month (12-month moving average)",
             done     = "Value of work done, per year (rolling four-quarter sum)")
make_p14 <- function(cur) {
  d <- bld_for(cur) |> mutate(panel = factor(bld_lbl[measure], levels = bld_lbl))
  ggplot(d, aes(date)) +
    geom_line(data = filter(d, measure == "approved"), aes(y = value / 1000),
              colour = csis_colours[["teal"]], alpha = 0.25, linewidth = 0.4) +
    geom_line(aes(y = smooth / 1000, colour = measure), linewidth = 0.9) +
    facet_wrap(~panel, ncol = 1, scales = "free_y") +
    scale_colour_manual(values = c(approved = csis_colours[["teal"]], done = csis_colours[["navy"]]),
                        guide = "none") +
    scale_y_continuous(labels = label_dollar(prefix = cur_sym[[cur]], suffix = "bn", accuracy = 0.5),
                       expand = expansion(c(0, 0.05)), limits = c(0, NA)) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    labs(title = "Building work on commercial buildings not elsewhere classified",
         subtitle = paste("Private sector. Approvals: 12-month moving average, monthly values faint. The ABS includes",
                          "\ndata centres in this category, alongside other commercial buildings it does not list separately"),
         x = NULL, y = NULL,
         caption = paste("Sources: ABS, Building Activity, Australia (cat. 8752.0), Table 51;",
                         "ABS, Building Approvals, Australia (cat. 8731.0), Table 51.\nCurrent prices, original.",
                         if (cur == "USD") fx_note[["USD"]] else "")) +
    theme(strip.text = element_text(face = "bold", hjust = 0, colour = csis_colours[["navy"]]))
}
save_both(make_p14, "14_commercial_nec_building.png", save_csis, height = 6)

# 13f. Computers and parts imported (SITC 752 + 759), next to IMT equipment capex
# ABS Data API, merchandise imports by SITC and country of origin, monthly,
# A$ thousand. Monthly imports are shown as a trailing 12-month moving
# average (monthly values faint); IMT capex is quarterly (rolling four-quarter
# sum), so it has its own panel.
imp_url <- paste0("https://data.api.abs.gov.au/rest/data/ABS,MERCH_IMP,1.0.0/",
                  "752+759.TOT+USA.TOT.M?startPeriod=2009-01")
imp_file <- file.path("raw", "abs_merch_imp_752_759.csv")
dir.create("raw", showWarnings = FALSE)
if (!file.exists(imp_file) || difftime(Sys.time(), file.mtime(imp_file), units = "days") > 7)
  utils::download.file(imp_url, imp_file, mode = "wb", quiet = TRUE,
                       headers = c(Accept = "application/vnd.sdmx.data+csv"))
imp <- utils::read.csv(imp_file) |>
  transmute(origin = COUNTRY_ORIGIN, date = as.Date(paste0(TIME_PERIOD, "-01")),
            value = OBS_VALUE / 1000) |>                            # A$ million
  group_by(origin, date) |>
  summarise(value = sum(value), .groups = "drop") |>                # 752 + 759
  arrange(origin, date)
imt_epm <- cp |>
  filter(measure == "Actual", industry == IMT, asset == EPM, date >= as.Date("2009-01-01")) |>
  transmute(date, value)
imp_lbl <- c(TOT = "All countries", USA = "From the US")
imp_for <- function(cur) imp |>
  mutate(value = if (cur == "USD") value * fx_or_latest(fx_month(date)) else value) |>
  group_by(origin) |> mutate(smooth = roll12(value)) |> ungroup() |>
  filter(date >= as.Date("2010-01-01"))
make_p15 <- function(cur) {
  p_imp <- "Imports of computers and parts, per month (12-month moving average)"
  p_cap <- "Information Media & Telecommunications equipment capex, per year (rolling four-quarter sum)"
  d_imp <- imp_for(cur) |> mutate(series = imp_lbl[origin], panel = p_imp)
  d_cap <- imt_epm |>
    mutate(value = if (cur == "USD") value * fx_or_latest(fx_quarter(date)) else value,
           smooth = roll4(value), series = "Equipment capex, information media and telecoms", panel = p_cap) |>
    filter(date >= as.Date("2010-01-01"))
  d <- bind_rows(d_imp, d_cap) |> mutate(panel = factor(panel, levels = c(p_imp, p_cap)))
  ggplot(d, aes(date)) +
    geom_line(data = filter(d, panel == p_imp), aes(y = value / 1000, group = series, colour = series),
              alpha = 0.25, linewidth = 0.4, show.legend = FALSE) +
    geom_line(aes(y = smooth / 1000, colour = series), linewidth = 0.9) +
    facet_wrap(~panel, ncol = 1, scales = "free_y") +
    scale_colour_manual(values = c(`All countries` = csis_colours[["navy"]],
                                   `From the US` = csis_colours[["red"]],
                                   `Equipment capex, information media and telecoms` = csis_colours[["gold"]])) +
    scale_y_continuous(labels = label_dollar(prefix = cur_sym[[cur]], suffix = "bn", accuracy = 0.5),
                       expand = expansion(c(0, 0.05)), limits = c(0, NA)) +
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
    labs(title = "Imports of computers and parts, and equipment spending by the industry",
         subtitle = "Current prices. Imports: 12-month moving average, monthly values faint",
         x = NULL, y = NULL,
         caption = paste("Sources: ABS, merchandise imports by SITC and country of origin (ABS Data API),",
                         "SITC 752 (computers) and 759 (parts);\nABS 5625.0 for capex.",
                         GAPS, if (cur == "USD") fx_note[["USD"]] else "")) +
    theme(strip.text = element_text(face = "bold", hjust = 0, colour = csis_colours[["navy"]]))
}
save_both(make_p15, "15_computer_imports.png", save_csis, height = 6.5)

# 13g. Cost to run: industrial electricity prices in US data centre states
eia <- ref_get("eia_industrial_price")
eia_yr <- as.integer(eia$period[1])
eia_k  <- function(cur) if (cur == "AUD") 10 / fx_calendar_year(eia_yr) else 10   # c/kWh -> $/MWh
make_p16 <- function(cur) {
  d <- eia |> mutate(v = value * eia_k(cur), us = region == "US average",
                     region = reorder(region, v))
  ggplot(d, aes(v, region, fill = us)) +
    geom_col(width = 0.65) +
    geom_text(aes(label = sprintf("%s%.0f", cur_sym[[cur]], v)), hjust = -0.15, size = 3.2,
              family = "source_sans", colour = csis_colours[["grey"]]) +
    scale_fill_manual(values = c(`TRUE` = csis_colours[["red"]], `FALSE` = "#E8A9AB"), guide = "none") +
    scale_x_continuous(labels = label_dollar(prefix = cur_sym[[cur]]), expand = expansion(c(0, 0.15))) +
    labs(title = "US benchmark: industrial electricity prices in data centre states",
         subtitle = paste0("Average retail price to industrial customers, ", eia_yr, ", ", cur_sym[[cur]], " per MWh"),
         x = NULL, y = NULL,
         caption = paste0("Source: US EIA, Electric Power Annual, Table 2.10 (published in US cents per kWh).",
                          "\nAll-in retail prices (energy, network and other charges). No comparable official",
                          "\nseries is published for Australian industrial users.",
                          if (cur == "AUD") fx_note_aud_from_usd else "")) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = "#E6E7E8", linewidth = 0.4))
}
save_both(make_p16, "16_us_industrial_power_prices.png", save_csis, height = 5)

# Takeaways for section 13
local({
  dc <- function(file) file.path("Data centres", file)
  nsw <- dem[dem$region == "New South Wales", ]; vic <- dem[dem$region == "Victoria", ]
  takeaway(dc("10_dc_demand_by_region.png"), sprintf(
    "Data centres drew %d MW on average in New South Wales and %d MW in Victoria in %s, up from %d MW and %d MW a year earlier.",
    nsw$value[2], vic$value[2], nsw$period[2], nsw$value[1], vic$value[1]))
  takeaway(dc("11_dc_connection_pipeline.png"), sprintf(
    "%d data centre projects totalling %.1f GW are seeking transmission connections; data centres in NSW and Victoria drew about %.1f GW on average in %s.",
    sum(pipe$projects), sum(pipe$value), sum(dem$value[dem$period == levels(dem$period)[2]]) / 1000,
    levels(dem$period)[2]))
  au <- gs_line[gs_line$country == "Australia", ]; au <- au[order(au$year), ]
  takeaway(dc("12_dc_share_of_grid_au_us.png"), sprintf(
    "AEMO expects data centres' share of NEM electricity use to rise from %s%% in %s to %s%% in 2029-30 and %s%% by %s; EPRI puts the US at %s-%s%% by 2030.",
    au$value[1], au$period[1], au$value[au$period == "2029-30"], au$value[nrow(au)], au$period[nrow(au)],
    gs_rng$lo[gs_rng$src == "EPRI"], gs_rng$hi[gs_rng$src == "EPRI"]))
  takeaway(dc("13_transgrid_sydney_capacity.png"), sprintf(
    "Transgrid has more than %s GW of data centre enquiries in Sydney, has signed about %s GW and says up to %s GW more is possible only if data centres pay for the upgrades.",
    tg$value[1], tg$value[2], tg$value[3]))
  line14 <- function(cur) {
    a <- bld_for(cur) |> filter(measure == "approved", !is.na(smooth)) |> arrange(date)
    al <- a[nrow(a), ]; ap <- a[nrow(a) - 12, ]
    w <- bld_for(cur) |> filter(measure == "done", !is.na(smooth)) |> arrange(date)
    wl <- w[nrow(w), ]
    sprintf(paste("Approvals averaged %s a month in the 12 months to %s, %.0f%% more than a year earlier;",
                  "work done was %s in the year to %s."),
            fmt_bn(al$smooth / 1000, cur_sym[[cur]], 2), format(al$date, "%B %Y"),
            100 * (al$smooth / ap$smooth - 1),
            fmt_bn(wl$smooth / 1000, cur_sym[[cur]]), format(wl$date, "%B %Y"))
  }
  takeaway(dc("14_commercial_nec_building.png"), line14("AUD"), line14("USD"))
  line15 <- function(cur) {
    it <- imp_for(cur) |> filter(!is.na(smooth))
    tot <- it |> filter(origin == "TOT") |> arrange(date)
    il <- tot[nrow(tot), ]; ip <- tot[nrow(tot) - 12, ]
    us <- it |> filter(origin == "USA", date == il$date)
    sprintf(paste("Imports of computers and parts averaged %s a month in the 12 months to %s, %.0f%% more",
                  "than a year earlier; %.0f%% came from the US."),
            fmt_bn(il$smooth / 1000, cur_sym[[cur]], 2), format(il$date, "%B %Y"),
            100 * (il$smooth / ip$smooth - 1), 100 * us$smooth / il$smooth)
  }
  takeaway(dc("15_computer_imports.png"), line15("AUD"), line15("USD"))
  line16 <- function(cur) sprintf(
    "Industrial users paid an average %s%.0f/MWh in Virginia, the largest US data centre market, and %s%.0f/MWh in Texas in %d.",
    cur_sym[[cur]], eia$value[eia$region == "Virginia"] * eia_k(cur),
    cur_sym[[cur]], eia$value[eia$region == "Texas"] * eia_k(cur), eia_yr)
  takeaway(dc("16_us_industrial_power_prices.png"), line16("AUD"), line16("USD"))
})
