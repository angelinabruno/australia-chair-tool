# ==============================================================================
# Old industry charts, banked from '1b. Economy.R' (October 2026)
#
# Replaced on the dashboard by Economy/industry_shares_au_us.png (OECD
# harmonised value added by ISIC Rev.4, Australia and US on one chart).
#   gva_level_industry.png (+ _usd)  Australian GVA by ANZSIC division, latest
#                                    financial year, ABS 5204.0 Table 5 (A$)
#   gva_share_au_us.png              Industry shares, ABS 5204.0 vs BEA value
#                                    added, matched with a hand concordance
#   market_nonmarket_au_us.png       Market vs non-market (ISIC O, P, Q) from the
#                                    OECD data; dropped as not informative
#
# Not run by '1. Refresh all data.R'. To redraw, run '1b. Economy.R' up to
# and including its industry-shares section first (it sets up the style,
# fetch_us(), RAW_DIR, au_us_colours, save_plot(), ind_share and ind_yr), then
# run this file from the Economy folder.
# ==============================================================================

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


NON_MARKET <- c("Public administration and defence", "Education", "Health and social work")

# Market and non-market: the OECD productivity-statistics convention, with
# public administration, education and health (ISIC O, P, Q) as non-market.
mkt <- ind_share |>
  mutate(part = if_else(group %in% NON_MARKET, "Non-market (public administration, education, health)",
                        "Market (all other industries)")) |>
  group_by(country, part) |>
  summarise(share = sum(share), .groups = "drop") |>
  mutate(part = factor(part, levels = c("Non-market (public administration, education, health)",
                                        "Market (all other industries)")),
         country = factor(country, levels = c("United States", "Australia")))
p_mkt <- ggplot(mkt, aes(share, country, fill = part)) +
  geom_col(width = 0.55) +
  geom_text(aes(label = sprintf("%.0f%%", share)), position = position_stack(vjust = 0.5),
            size = 3.6, family = "source_sans", colour = "white", fontface = "bold") +
  scale_fill_manual(values = c("Market (all other industries)" = csis_colours[["navy"]],
                               "Non-market (public administration, education, health)" = csis_colours[["teal"]]),
                    breaks = c("Market (all other industries)",
                               "Non-market (public administration, education, health)")) +
  scale_x_continuous(labels = label_percent(scale = 1), expand = expansion(c(0, 0.01))) +
  guides(fill = guide_legend(ncol = 1)) +
  labs(title = "Market and non-market shares of the economy, Australia and the United States",
       subtitle = sprintf("Share of value added excluding owner-occupied housing, %d (Australia %s)", ind_yr, au_yr_lab),
       x = NULL, y = NULL,
       caption = paste0("Source: OECD National Accounts (Table 6A, ISIC Rev.4). Non-market = ISIC O, P and Q, ",
                        "as in OECD productivity statistics.\nThis is a split by activity, not ownership: ",
                        "private schools and hospitals count as non-market, and government businesses\n",
                        "(e.g. postal, water) as market. OECD sector accounts have US but not Australian ",
                        "general government value added.")) +
  theme_csis() +
  theme_bars

save_plot(p_mkt, "market_nonmarket_au_us.png", width = 8.5, height = 3.6)
