rm(list = ls())
# The dashboard folder is set once, in section 1 of '1. Refresh all data.R'.
# To run this script on its own, run that section first.
if (Sys.getenv("DASHBOARD_DIR") == "")
  stop("Dashboard folder not set. Run section 1 of '1. Refresh all data.R' first.")
setwd(file.path(Sys.getenv("DASHBOARD_DIR"), "Tech"))
source(file.path(Sys.getenv("DASHBOARD_DIR"), "0. Takeaways.R"))       # line under each chart

# ==============================================================================
# Australia in critical and emerging technologies
# Modules:
#   A  Research output share by technology, Australia vs comparators  (OpenAlex)
#   B  High-impact share - where Australia punches above weight       (OpenAlex)
#   C  Top Australian institutions per technology                     (OpenAlex)
#   D  Collaboration partners: US vs China co-authorship              (OpenAlex)
#   E  Frontier AI model development by country                       (Epoch AI)
#   F  R&D intensity vs comparators                                   (OECD MSTI)
#
# Technology categories follow DISR's List of Critical Technologies in the
# National Interest (updated July 2026) so output maps onto Australian policy
# language and AUKUS Pillar 2 advanced capabilities.
#
# CAVEAT ON METHOD: this uses keyword matching on titles and abstracts. ASPI's
# Critical Technology Tracker uses curated Web of Science query strings plus
# citation percentiles and is more defensible for publication.
# ==============================================================================


library(httr2)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)
source(file.path(Sys.getenv("DASHBOARD_DIR"), "0. Chart data.R"))   # <chart>_data.csv beside each chart
library(scales)
library(forcats)
library(readr)

# Charts here use default fonts. If an earlier script in this R session turned
# on showtext (set for a different dpi), switch it off or text renders at the
# wrong size and titles get cut off.
if ("showtext" %in% loadedNamespaces()) showtext::showtext_auto(FALSE)

# ------------------------------------------------------------------------------
# 0. Configuration
# ------------------------------------------------------------------------------

# Free key from https://openalex.org - see https://docs.openalex.org
# Store in .Renviron as OPENALEX_KEY=... rather than hardcoding.
OA_KEY   <- Sys.getenv("OPENALEX_KEY")
OA_EMAIL <- "abruno@csis.org"   

YEARS <- 2015:2025

COMPARATORS <- c(
  au = "Australia", us = "United States", cn = "China", gb = "United Kingdom",
  jp = "Japan",     kr = "South Korea",   in_ = "India", ca = "Canada",
  de = "Germany"
)

# One colour per country across the dashboard: Australia navy, US red.
AU_COL <- "#00205B"; US_COL <- "#D2232A"; CN_COL <- "#F2A900"; OTHER_COL <- "#b4b2a9"

# Keyword definitions.
# Quoted phrases are treated as phrases by OpenAlex; OR joins alternatives.
TECHNOLOGIES <- c(
  "Artificial intelligence" =
    '"machine learning" OR "deep learning" OR "neural network" OR "artificial intelligence" OR "large language model"',
  "Quantum computing" =
    '"quantum computing" OR "quantum computer" OR "qubit" OR "quantum error correction"',
  "Quantum sensing and comms" =
    '"quantum sensing" OR "quantum key distribution" OR "quantum communication" OR "quantum metrology"',
  "Advanced comms (5G/6G)" =
    '"5G network" OR "6G network" OR "millimeter wave communication" OR "massive MIMO" OR "network slicing"',
  "Cybersecurity" =
    '"cybersecurity" OR "intrusion detection" OR "post-quantum cryptography" OR "malware detection"',
  "Semiconductors" =
    '"semiconductor device" OR "integrated circuit design" OR "photonic integrated circuit" OR "compound semiconductor"',
  "Autonomous systems and robotics" =
    '"autonomous vehicle" OR "unmanned aerial vehicle" OR "swarm robotics" OR "robot navigation"',
  "Critical minerals processing" =
    '"rare earth extraction" OR "lithium extraction" OR "hydrometallurgy" OR "critical mineral processing"'
)


# ------------------------------------------------------------------------------
# 0b. Disk cache
#
# Every fetch below goes through cached(). First run hits the network and
# writes an .rds into ./cache/; later runs read from disk. Nothing re-downloads
# unless the cache file is older than MAX_AGE_DAYS or REFRESH is TRUE.
#
#   REFRESH <- TRUE                     # force a full refresh
#   cached("name", expr, refresh = TRUE) # force one dataset
#   unlink("cache", recursive = TRUE)    # nuke everything
# ------------------------------------------------------------------------------

CACHE_DIR    <- "cache"
MAX_AGE_DAYS <- 30
REFRESH      <- FALSE

dir.create(CACHE_DIR, showWarnings = FALSE)

cached <- function(name, expr, refresh = REFRESH, max_age = MAX_AGE_DAYS) {
  f <- file.path(CACHE_DIR, paste0(name, ".rds"))
  
  if (!refresh && file.exists(f)) {
    age <- as.numeric(difftime(Sys.time(), file.mtime(f), units = "days"))
    if (age <= max_age) {
      message(sprintf("cache hit : %-22s (%.1f days old)", name, age))
      return(readRDS(f))
    }
    message(sprintf("cache stale: %-22s (%.1f days old, refetching)", name, age))
  } else {
    message("fetching  : ", name)
  }
  
  value <- force(expr)
  saveRDS(value, f)
  attr(value, "fetched_at") <- Sys.time()
  value
}

# What is currently cached, and how old
cache_status <- function() {
  files <- list.files(CACHE_DIR, pattern = "\\.rds$", full.names = TRUE)
  if (!length(files)) return(message("Cache is empty."))
  tibble(
    dataset  = tools::file_path_sans_ext(basename(files)),
    fetched  = file.mtime(files),
    days_old = round(as.numeric(difftime(Sys.time(), file.mtime(files), units = "days")), 1),
    size_kb  = round(file.size(files) / 1024, 1)
  ) |> arrange(desc(fetched))
}


# ------------------------------------------------------------------------------
# 1. OpenAlex helper
#
# Uses the group_by endpoint, which returns aggregate counts without paging
# through individual records. One request per query rather than thousands.
# ------------------------------------------------------------------------------

oa_group <- function(filters, group_by, retries = 3) {
  req <- request("https://api.openalex.org/works") |>
    req_url_query(
      filter   = paste(filters, collapse = ","),
      group_by = group_by,
      mailto   = OA_EMAIL,
      per_page = 200
    ) |>
    req_user_agent(paste0("CSIS Australia Chair research (", OA_EMAIL, ")")) |>
    req_retry(max_tries = retries) |>
    req_throttle(capacity = 10, fill_time_s = 1)
  
  if (nzchar(OA_KEY)) req <- req_url_query(req, api_key = OA_KEY)
  
  out <- req |> req_perform() |> resp_body_json()
  tibble(
    key   = map_chr(out$group_by, "key"),
    label = map_chr(out$group_by, "key_display_name"),
    n     = map_int(out$group_by, "count")
  )
}

# Shared filters applied to every query.
base_filters <- function(extra = character()) {
  c(paste0("publication_year:", min(YEARS), "-", max(YEARS)),
    "type:article",
    "is_retracted:false",
    extra)
}

kw_filter <- function(tech) paste0("title_and_abstract.search:", TECHNOLOGIES[[tech]])

# The keyword lists, so readers can see exactly what each field counts
write_csv(tibble(technology = names(TECHNOLOGIES), openalex_query = unname(TECHNOLOGIES),
                 searched_in = "title and abstract", years = paste0(min(YEARS), "-", max(YEARS))),
          "technology_keywords.csv")
KW_NOTE <- "Fields are keyword searches of titles and abstracts; the keywords are in Tech/technology_keywords.csv."

# OpenAlex group_by returns `key` in different shapes depending on the field:
# a bare code ("AU"), a lowercase code ("au"), or a full URI
# ("https://openalex.org/countries/AU"). Match on all of them plus the display
# name so the pipeline does not silently return zero rows.
country_is <- function(df, code, name) {
  df |> filter(
    str_to_upper(str_extract(key, "[A-Za-z]{2}$")) == str_to_upper(code) |
      str_to_lower(label) == str_to_lower(name)
  )
}

# Diagnostic - run after any fetch to see what the API actually returned.
peek <- function(df, n = 8) {
  message("rows: ", nrow(df))
  print(head(distinct(df, key, label), n))
  invisible(df)
}


# ------------------------------------------------------------------------------
# 2. MODULE A - research output by country and technology
#
# Establishes the baseline. Australia is roughly 0.3% of world
# population and produces ~3% of research output, so interesting question is
# which technologies it over- or under-indexes on relative to that baseline.
# ------------------------------------------------------------------------------

fetch_country_counts <- function(tech) {
  message("Fetching: ", tech)
  oa_group(base_filters(kw_filter(tech)),
           group_by = "authorships.countries") |>
    mutate(technology = tech)
}

country_counts <- cached(
  "A_country_counts",
  map_dfr(names(TECHNOLOGIES), fetch_country_counts)
)

write_csv(country_counts, "tech_country_counts_raw.csv")

peek(country_counts)

au_share <- country_counts |>
  group_by(technology) |>
  mutate(world = sum(n)) |>
  ungroup() |>
  country_is("AU", "Australia") |>
  transmute(technology, au_papers = n, share = n / world)

stopifnot("Module A returned no Australian rows - inspect peek() output above" =
            nrow(au_share) > 0)

p_a <- au_share |>
  mutate(technology = fct_reorder(technology, share)) |>
  ggplot(aes(share, technology)) +
  geom_col(fill = AU_COL, width = 0.65) +
  geom_vline(xintercept = mean(au_share$share), linetype = "22", colour = "grey45") +
  geom_text(aes(label = sprintf("%s papers (%.1f%%)", comma(au_papers), 100 * share)),
            hjust = -0.08, size = 3, colour = "grey30") +
  scale_x_continuous(labels = percent_format(accuracy = 0.1),
                     expand = expansion(c(0, 0.3))) +
  labs(
    title    = "Australian share of world publications by critical technology",
    subtitle = paste0("Australian share of world publications, ", min(YEARS), "-", max(YEARS),
                      ". Dashed line is Australia's average across these fields."),
    x = NULL, y = NULL,
    caption = paste("Source: OpenAlex. Labels show the number of Australian papers and the share of the world total.
", KW_NOTE)
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        plot.title.position = "plot",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())

ggsave("A_australian_share_by_technology.png", p_a, width = 9, height = 5.5,
       dpi = 150, bg = "white")


# ------------------------------------------------------------------------------
# 3. MODULE B - quality, not just volume
#
# Raw counts flatter large systems. Restricting to the top decile of
# cited work is closer to ASPI's method.

# ------------------------------------------------------------------------------

fetch_top_decile <- function(tech) {
  message("Top-decile: ", tech)
  oa_group(base_filters(c(kw_filter(tech), "cited_by_percentile_year.min:90")),
           group_by = "authorships.countries") |>
    mutate(technology = tech)
}

top_decile <- cached(
  "B_top_decile",
  map_dfr(names(TECHNOLOGIES), fetch_top_decile)
)

quality_gap <- bind_rows(
  country_counts |> mutate(measure = "All papers"),
  top_decile     |> mutate(measure = "Top 10% most cited")
) |>
  group_by(technology, measure) |>
  mutate(share = n / sum(n)) |>
  ungroup() |>
  country_is("AU", "Australia")

stopifnot("Module B returned no Australian rows" = nrow(quality_gap) > 0)

p_b <- quality_gap |>
  mutate(technology = fct_reorder(technology, share, .fun = max)) |>
  ggplot(aes(share, technology, colour = measure)) +
  geom_line(aes(group = technology), colour = "grey70", linewidth = 0.6) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("All papers" = "#b4b2a9",
                                 "Top 10% most cited" = AU_COL)) +
  scale_x_continuous(labels = percent_format(accuracy = 0.1)) +
  labs(
    title    = "Australian share of world publications: all papers and top 10% cited",
    subtitle = "Share of world output, all papers vs top decile by citations",
    x = NULL, y = NULL, colour = NULL,
    caption = paste("Source: OpenAlex.", KW_NOTE)
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        plot.title.position = "plot",
        legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())

ggsave("B_quality_vs_volume.png", p_b, width = 9, height = 5.5, dpi = 150, bg = "white")


# ------------------------------------------------------------------------------
# 4. MODULE C - which Australian institutions
#
# Capability is concentrated in a handful of universities and CSIRO.
# Directly relevant to research-security and export-control conversations.
# ------------------------------------------------------------------------------

fetch_au_institutions <- function(tech) {
  message("Institutions: ", tech)
  oa_group(base_filters(c(kw_filter(tech), "institutions.country_code:au")),
           group_by = "institutions.id") |>
    mutate(technology = tech) |>
    slice_max(n, n = 6)
}

au_institutions <- cached(
  "C_au_institutions",
  map_dfr(names(TECHNOLOGIES), fetch_au_institutions)
)

write_csv(au_institutions, "C_australian_institutions.csv")

p_c <- au_institutions |>
  filter(technology %in% c("Artificial intelligence", "Quantum computing",
                           "Cybersecurity", "Advanced comms (5G/6G)")) |>
  mutate(label = str_remove(label, "^The "),
         # Facet-local ordering without tidytext: make each label unique per
         # facet, order globally, then strip the suffix at render time.
         label = fct_reorder(paste(label, technology, sep = "\u001f"), n)) |>
  ggplot(aes(n, label)) +
  geom_col(fill = "#1baf7a", width = 0.65) +
  facet_wrap(~technology, scales = "free", ncol = 2) +
  scale_y_discrete(labels = function(x) str_remove(x, "\u001f.*$")) +
  scale_x_continuous(labels = comma, expand = expansion(c(0, 0.1))) +
  labs(
    title    = "Leading Australian institutions by publications in each field",
    subtitle = paste0("Publications ", min(YEARS), "-", max(YEARS), ", top six institutions per field"),
    x = NULL, y = NULL, caption = "Source: OpenAlex."
  ) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 13),
        plot.title.position = "plot",
        strip.text = element_text(face = "bold", hjust = 0),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())

ggsave("C_australian_institutions.png", p_c, width = 10, height = 7, dpi = 150, bg = "white")


# ------------------------------------------------------------------------------
# 5. MODULE D - collaboration partners
#
# For each technology,what share of Australian papers are co-authored with US institutions versus
# Chinese ones? Feeds directly into research-security debates, the Defence
# Trade Controls Act amendments, and AUKUS Pillar 2 technology-sharing.
# ------------------------------------------------------------------------------

fetch_partner <- function(tech, partner) {
  oa_group(
    base_filters(c(kw_filter(tech),
                   "institutions.country_code:au",
                   paste0("institutions.country_code:", partner))),
    group_by = "publication_year"
  ) |>
    mutate(technology = tech, partner = partner)
}

partners <- cached(
  "D_collaboration",
  expand_grid(tech = names(TECHNOLOGIES), partner = c("us", "cn")) |>
    pmap_dfr(function(tech, partner) {
      message("Collab: ", tech, " x ", partner)
      fetch_partner(tech, partner)
    })
)

au_totals <- country_counts |>
  country_is("AU", "Australia") |>
  select(technology, au_total = n)

collab <- partners |>
  group_by(technology, partner) |>
  summarise(n = sum(n), .groups = "drop") |>
  left_join(au_totals, by = "technology") |>
  mutate(share = n / au_total,
         partner = recode(partner, us = "with United States", cn = "with China"))

p_d <- collab |>
  mutate(technology = fct_reorder(technology, share, .fun = max)) |>
  ggplot(aes(share, technology, fill = partner)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.65) +
  scale_fill_manual(values = c("with United States" = US_COL,
                               "with China" = CN_COL)) +
  scale_x_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(c(0, 0.05))) +
  labs(
    title    = "Australian publications with US and Chinese co-authors",
    subtitle = paste0("Share of Australian publications ", min(YEARS), "-", max(YEARS),
                      " with at least one co-author institution in each country"),
    x = NULL, y = NULL, fill = NULL,
    caption = paste("Source: OpenAlex. Shares overlap - a paper can have both US and Chinese co-authors.
", KW_NOTE)
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        plot.title.position = "plot",
        legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())

ggsave("D_collaboration_partners.png", p_d, width = 9, height = 5.5, dpi = 150, bg = "white")



# ------------------------------------------------------------------------------
# 6. MODULE E - frontier AI model development
#
# Research output and model development are different things. Australia
# ranks respectably on AI papers and close to nowhere on notable models. The
# gap, strong research base, no frontier development capacity, is arguably the
# single most important framing for a US audience thinking about AUKUS Pillar 2
# and about where Australia is a partner versus a customer.
#
#Cite as: Epoch AI, 'Data on AI Models', epoch.ai.
# ------------------------------------------------------------------------------

# Epoch updates this dataset roughly daily, so cache it for a shorter window.
models <- cached(
  "E_epoch_models",
  read_csv("https://epoch.ai/data/notable_ai_models.csv", show_col_types = FALSE),
  max_age = 7
)

glimpse(models)   # column names change occasionally - check before relying on them

# Epoch renames columns periodically. Find the country column by pattern rather
# than hardcoding it, and fail loudly if it disappears entirely.
country_col <- names(models)[str_detect(names(models), regex("country", ignore_case = TRUE))][1]
date_col    <- names(models)[str_detect(names(models), regex("publication date", ignore_case = TRUE))][1]
stopifnot(!is.na(country_col), !is.na(date_col))
message("Using columns: ", country_col, " / ", date_col)

model_countries <- models |>
  filter(!is.na(.data[[country_col]])) |>
  separate_longer_delim(all_of(country_col), delim = ",") |>
  mutate(country = str_squish(.data[[country_col]]),
         year    = lubridate::year(.data[[date_col]])) |>
  filter(year >= 2015, nzchar(country)) |>
  # Epoch uses long-form UN names; shorten the ones that wreck an axis.
  mutate(country = recode(country,
                          "United States of America" = "United States",
                          "Korea (Republic of)"      = "South Korea",
                          "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
                          "Russian Federation"       = "Russia",
                          "Taiwan (Province of China)" = "Taiwan",
                          .default = country)) |>
  count(country, sort = TRUE)

print(model_countries, n = 25)   # confirm "Australia" appears as expected

p_e <- model_countries |>
  filter(row_number() <= 12 | country == "Australia") |>   # always show Australia
  mutate(who = case_when(country == "Australia" ~ "au", country == "United States" ~ "us", TRUE ~ "other"),
         country = fct_reorder(country, n)) |>
  ggplot(aes(n, country, fill = who)) +
  geom_col(width = 0.65) +
  geom_text(aes(label = comma(n)), hjust = -0.2, size = 3, colour = "grey30") +
  scale_fill_manual(values = c(au = AU_COL, us = US_COL, other = OTHER_COL), guide = "none") +
  scale_x_continuous(labels = comma, expand = expansion(c(0, 0.1))) +
  labs(
    title    = "Notable AI models by country",
    subtitle = "Notable AI models by country of developing organisation, 2015 onward",
    x = NULL, y = NULL,
    caption = "Source: Epoch AI, 'Data on AI Models' (CC-BY)."
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"),
        plot.title.position = "plot",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())

ggsave("E_ai_models_by_country.png", p_e, width = 9, height = 5.5, dpi = 150, bg = "white")


# ------------------------------------------------------------------------------
# 7. MODULE F - R&D intensity
#
# Australian GERD as a share of GDP has been drifting below the OECD average, and
# BERD is unusually low for a high-income economy. Every capability finding above
# sits downstream of this.
# ------------------------------------------------------------------------------

# GERD as a share of GDP from the OECD's Main Science and Technology
# Indicators (MSTI). For Australia these are the ABS's own R&D surveys (ABS
# 8104.0 and 8112.0), which run every second year, so its line has fewer points.
MSTI_URL <- paste0("https://sdmx.oecd.org/public/rest/data/OECD.STI.STP,DSD_MSTI@DF_MSTI,/",
                   "AUS+USA+CHN+GBR+JPN+KOR+CAN+DEU+ISR.A.G.PT_B1GQ......?startPeriod=2005&format=csv")

fetch_msti <- function() {
  old <- options(timeout = 300); on.exit(options(old), add = TRUE)
  tryCatch(read_csv(MSTI_URL, show_col_types = FALSE) |>
             transmute(iso3c = REF_AREA, year = as.integer(TIME_PERIOD), gerd = OBS_VALUE),
           error = function(e) { message("MSTI failed: ", conditionMessage(e)); NULL })
}

rd <- cached("F_msti_rd", fetch_msti(), max_age = 180)

if (is.null(rd) || nrow(rd) == 0) {
  unlink(file.path(CACHE_DIR, "F_msti_rd.rds"))   # don't cache a failure
  message("\n--- Module F skipped: OECD MSTI unavailable. ---\n")
} else {

  iso2 <- c(AUS = "AU", USA = "US", CHN = "CN", GBR = "GB", JPN = "JP", KOR = "KR",
            CAN = "CA", DEU = "DE", ISR = "IL")
  rd_plot <- rd |>
    filter(!is.na(gerd)) |>
    mutate(who = case_when(iso3c == "AUS" ~ "au", iso3c == "USA" ~ "us", TRUE ~ "other"),
           iso2c = iso2[iso3c]) |>
    arrange(iso3c, year)

  # One label per country, at its most recent year
  rd_labels <- rd_plot |>
    group_by(iso3c) |>
    slice_max(year, n = 1, with_ties = FALSE) |>
    ungroup()

  p_f <- ggplot(rd_plot, aes(year, gerd, group = iso3c)) +
    geom_line(aes(colour = who, linewidth = who)) +
    geom_point(data = filter(rd_plot, who == "au"), aes(colour = who), size = 1.8) +
    geom_text(
      data = rd_labels,
      aes(label = iso2c, colour = who),
      hjust = -0.3, size = 3, show.legend = FALSE
    ) +
    scale_colour_manual(values = c(au = AU_COL, us = US_COL, other = "#a8a69c"),
                        guide = "none") +
    scale_linewidth_manual(values = c(au = 1.2, us = 1.2, other = 0.6),
                           guide = "none") +
    scale_x_continuous(expand = expansion(c(0.02, 0.06))) +
    labs(
      title    = "R&D intensity, Australia and comparators",
      subtitle = "Gross domestic expenditure on R&D, per cent of GDP. Australia in navy, the United States in red",
      x = NULL, y = NULL,
      caption = paste("Source: OECD, Main Science and Technology Indicators (from the ABS R&D surveys for Australia,",
                      "\nwhich are run every second year).")
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold"),
          plot.title.position = "plot",
          panel.grid.minor = element_blank())

  ggsave("F_rd_intensity.png", p_f, width = 9, height = 5.5, dpi = 150, bg = "white")

  au_l <- rd_labels |> filter(iso3c == "AUS"); us_l <- rd_labels |> filter(iso3c == "USA")
  oecd_rank <- rd_labels |> arrange(desc(gerd)) |> mutate(r = row_number())
  takeaway("Tech/F_rd_intensity.png", sprintf(
    "Australia spent %.2f%% of GDP on R&D in %d, against %.2f%% in the United States in %d.",
    au_l$gerd, au_l$year, us_l$gerd, us_l$year))

}   # end module F conditional

# Takeaways for modules A-E
local({
  top <- au_share |> arrange(desc(share))
  takeaway("Tech/A_australian_share_by_technology.png", sprintf(
    "Australia's largest share of world research among these fields is in %s (%.1f%% of papers, %s papers); its smallest is in %s (%.1f%%).",
    top$technology[1], 100 * top$share[1], comma(top$au_papers[1]),
    top$technology[nrow(top)], 100 * top$share[nrow(top)]))
  q <- quality_gap |> select(technology, measure, share) |>
    pivot_wider(names_from = measure, values_from = share) |>
    mutate(ratio = `Top 10% most cited` / `All papers`) |> arrange(desc(ratio))
  takeaway("Tech/B_quality_vs_volume.png", if (any(q$ratio > 1)) sprintf(
    "Australia's share of the most-cited papers is higher than its share of all papers in %d of %d fields, most of all in %s.",
    sum(q$ratio > 1), nrow(q), q$technology[1]) else sprintf(
    "Australia's share of the most-cited papers is below its share of all papers in all %d fields; the gap is smallest in %s.",
    nrow(q), q$technology[1]))
  ai <- au_institutions |> filter(technology == "Artificial intelligence") |> slice_max(n, n = 1)
  takeaway("Tech/C_australian_institutions.png", sprintf(
    "%s publishes the most Australian research on artificial intelligence (%s papers, %d-%d).",
    str_remove(ai$label, "^The "), comma(ai$n), min(YEARS), max(YEARS)))
  w <- collab |> select(technology, partner, share) |>
    pivot_wider(names_from = partner, values_from = share)
  takeaway("Tech/D_collaboration_partners.png", sprintf(
    "Australian researchers co-author more with Chinese than US institutions in %d of %d fields.",
    sum(w$`with China` > w$`with United States`), nrow(w)))
  au_m <- model_countries$n[model_countries$country == "Australia"]
  us_m <- model_countries$n[model_countries$country == "United States"]
  takeaway("Tech/E_ai_models_by_country.png", sprintf(
    "Organisations in the United States developed %s notable AI models since 2015; Australian organisations developed %s.",
    comma(us_m), if (length(au_m)) comma(au_m) else "none"))
})



message("Done - charts written to ", getwd())
print(cache_status())