# The dashboard folder is set once, in section 1 of '1. Refresh all data.R'.
# To run this script on its own, run that section first.
if (Sys.getenv("DASHBOARD_DIR") == "")
  stop("Dashboard folder not set. Run section 1 of '1. Refresh all data.R' first.")
setwd(file.path(Sys.getenv("DASHBOARD_DIR"), "Migration"))


# ---------------------------------------------------------------------------
# ABS Overseas Migration — net overseas migration of USA-born, 2004-05 to date
#
# Source cube: 34070DO001 "Net overseas migration by country of birth,
# state/territory - financial years". Released 19 December 2025 for 2024-25.
#
# Cube structure (confirmed against 34070DO001_202425.xlsx):
#   - Sheet "Contents", then "Table 1.1" .. "Table 1.9"
#   - Table 1.1 = Australia; 1.2 NSW, 1.3 Vic, 1.4 Qld, 1.5 SA,
#     1.6 WA, 1.7 Tas, 1.8 NT, 1.9 ACT
#   - Header row is row 14 on Table 1.1 but row 13 on 1.2-1.9, so it is
#     located by searching for "SACC code" rather than hard-coded. The
#     region name sits in the "Table x.x ..." title row just above it.
#   - Columns: SACC code | Country of birth | 2004-05 ... 2024-25(e)
#   - 250 country rows, then blank / "Total Australian-born" /
#     "Total overseas-born" / "Total" / copyright. Country rows are the
#     ones with a 4-digit SACC code.
#   - Table 1.2 is padded out to 58 columns with empty ones; trimmed below.
#
# The USA is SACC code 8104 and is labelled "USA", NOT "United States".
#
# NOTE: the Contents sheet is deliberately not used. Excel stores the table
# numbers there as numerics, so readxl renders "1.1" as "1.1000000000000001"
# and sheet lookups fail. Sheet names come from excel_sheets() instead.
#
# Caveats carried through to the output:
#   - Estimates are rounded to the nearest 10 to confidentialise, so
#     components may not sum to totals.
#   - The latest year is preliminary, based on a propensity model rather
#     than actual traveller outcomes; small cells may be revised heavily.
#   - Uses the 12/16 month rule; not used in official ERP before Sep qtr 2006.
# ---------------------------------------------------------------------------

library(readabs)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(ggplot2)
source(file.path(Sys.getenv("DASHBOARD_DIR"), "0. Chart data.R"))   # <chart>_data.csv beside each chart

# --- Config ----------------------------------------------------------------

ABS_CATALOGUE <- "overseas-migration"
CUBE_ID       <- "34070DO001"
DATA_DIR      <- file.path("data-raw", "abs")
OUT_DIR       <- "output"

# Set to a path to use a file you already have; leave NULL to download.
LOCAL_FILE <- NULL
# LOCAL_FILE <- file.path(DATA_DIR, "34070DO001_202425.xlsx")

# SACC codes to extract. 8104 = USA.
FOCUS_CODES <- c("8104")

dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIR,  recursive = TRUE, showWarnings = FALSE)


# --- Step 1: get the cube --------------------------------------------------

get_cube <- function() {
  if (!is.null(LOCAL_FILE) && file.exists(LOCAL_FILE)) {
    message("Using local file: ", LOCAL_FILE)
    return(LOCAL_FILE)
  }
  message("Downloading cube ", CUBE_ID, " from catalogue '", ABS_CATALOGUE, "'")
  download_abs_data_cube(
    catalogue_string = ABS_CATALOGUE,
    cube             = CUBE_ID,
    path             = DATA_DIR
  )
}

cube_path <- get_cube()

data_sheets <- str_subset(excel_sheets(cube_path), "^Table\\s")
message("Data sheets: ", paste(data_sheets, collapse = ", "))


# --- Step 2: parse one NOM sheet -------------------------------------------

read_nom_sheet <- function(path, sheet) {
  
  raw <- suppressMessages(
    read_excel(path, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
  )
  raw <- as.data.frame(lapply(raw, as.character), stringsAsFactors = FALSE)
  
  col1 <- raw[[1]]
  
  # Header row: the one whose first cell starts with "SACC code"
  hdr <- which(str_detect(col1, "^SACC code"))[1]
  if (is.na(hdr)) stop("No 'SACC code' header row found in ", sheet)
  
  # Region: from the "Table x.x Net overseas migration by country of birth,
  # <REGION>, 2004-05 to ..." line immediately above the header
  title_rows <- which(str_detect(col1, "^Table\\s") & seq_along(col1) < hdr)
  if (length(title_rows) == 0) stop("No title row found in ", sheet)
  title <- col1[max(title_rows)]
  
  region <- title |>
    str_remove("^.*country of birth,\\s*") |>
    str_remove(",\\s*\\d{4}-\\d{2}.*$") |>
    str_trim()
  
  headers <- as.character(raw[hdr, ])
  
  # Trim the trailing empty padding columns (Table 1.2 has ~35 of them)
  keep <- which(!is.na(headers) & headers != "" & headers != "NA")
  
  dat <- raw[(hdr + 1):nrow(raw), keep, drop = FALSE]
  names(dat) <- headers[keep]
  names(dat)[1:2] <- c("sacc_code", "country")
  
  dat |>
    # Country rows only: drops blanks, "Total overseas-born", copyright line
    filter(str_detect(sacc_code, "^\\d{4}$")) |>
    pivot_longer(
      cols = -c(sacc_code, country),
      names_to  = "period_raw",
      values_to = "value"
    ) |>
    mutate(
      sheet       = sheet,
      region      = region,
      # "2024-25(e)" -> preliminary flag, then strip the footnote marker
      preliminary = str_detect(period_raw, "\\("),
      period      = str_remove(period_raw, "\\(.*\\)"),
      fy_end      = as.integer(str_sub(period, 1, 4)) + 1L,
      value       = suppressWarnings(as.numeric(value))
    ) |>
    select(region, sacc_code, country, period, fy_end, preliminary, value)
}

nom_all <- map_dfr(data_sheets, \(s) read_nom_sheet(cube_path, s))

# Sanity checks
stopifnot(
  nrow(nom_all) > 0,
  "8104" %in% nom_all$sacc_code,
  n_distinct(nom_all$region) == length(data_sheets),
  "Australia" %in% nom_all$region
)

message("Parsed ", nrow(nom_all), " rows across ",
        n_distinct(nom_all$region), " regions, ",
        n_distinct(nom_all$sacc_code), " countries, ",
        min(nom_all$fy_end), "-", max(nom_all$fy_end))

print(distinct(nom_all, region))


# --- Step 3: the USA series ------------------------------------------------

nom_usa <- nom_all |>
  filter(sacc_code %in% FOCUS_CODES) |>
  mutate(measure = "Net overseas migration") |>
  arrange(region, fy_end)

print(nom_usa |> filter(region == "Australia"), n = 30)

write.csv(nom_usa, file.path(OUT_DIR, "abs_nom_usa_born.csv"), row.names = FALSE)
write.csv(nom_all, file.path(OUT_DIR, "abs_nom_all_countries.csv"), row.names = FALSE)


# --- Step 4: charts --------------------------------------------------------

abs_caption <- paste0(
  "Source: ABS, Overseas Migration, cube ", CUBE_ID,
  ". Years ending 30 June. Rounded to the nearest 10. ",
  "Latest year preliminary. Retrieved ", Sys.Date(), "."
)

p_national <- nom_usa |>
  filter(region == "Australia") |>
  ggplot(aes(x = fy_end, y = value)) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey60") +
  geom_line(linewidth = 0.9, colour = "#2c5f8a") +
  geom_point(aes(shape = preliminary), size = 2, colour = "#2c5f8a") +
  scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1),
                     labels = c("Final", "Preliminary"), name = NULL) +
  scale_x_continuous(breaks = seq(2005, 2035, 5)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Net migration of Americans to Australia",
    subtitle = "Net overseas migration of people born in the USA, years ending 30 June",
    x = NULL, y = NULL, caption = abs_caption
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.caption = element_text(hjust = 0))

print(p_national)

p_states <- nom_usa |>
  filter(region != "Australia") |>
  ggplot(aes(x = fy_end, y = value)) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey70") +
  geom_line(linewidth = 0.7, colour = "#2c5f8a") +
  facet_wrap(~ region, ncol = 4, scales = "free_y") +
  scale_x_continuous(breaks = seq(2005, 2035, 10)) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title    = "Net migration of Americans to Australia, by state and territory",
    subtitle = "Net overseas migration of people born in the USA, years ending 30 June",
    x = NULL, y = NULL, caption = abs_caption
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.caption = element_text(hjust = 0))

print(p_states)

ggsave(file.path(OUT_DIR, "nom_usa_national.png"), p_national,
       width = 9, height = 5, dpi = 300)
ggsave(file.path(OUT_DIR, "nom_usa_states.png"), p_states,
       width = 11, height = 6, dpi = 300)
# The dashboard reads the charts from Migration/ itself, so copy them up a level
file.copy(file.path(OUT_DIR, c("nom_usa_national.png", "nom_usa_states.png",
                             "nom_usa_national_data.csv", "nom_usa_states_data.csv")), ".", overwrite = TRUE)


# --- Optional: USA against peer countries of birth -------------------------
# Look up the codes you want first, then uncomment:
#   nom_all |> distinct(sacc_code, country) |> print(n = 250)

# peers <- c("8104", "8102", "1201")   # USA, Canada, New Zealand
# nom_all |>
#   filter(region == "Australia", sacc_code %in% peers) |>
#   ggplot(aes(fy_end, value, colour = country)) +
#   geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey60") +
#   geom_line(linewidth = 0.9) +
#   scale_y_continuous(labels = scales::comma) +
#   labs(title = "Net overseas migration by country of birth, Australia",
#        x = NULL, y = NULL, colour = NULL, caption = abs_caption) +
#   theme_minimal(base_size = 12)


# ==============================================================================
# Two-way skilled and student flows
#
# Net overseas migration by country of birth (above) mixes in Australians born
# in the US and dual citizens returning home. The charts below use visa data
# by citizenship instead, which is a better measure of two-way flows of
# workers, researchers and students.
#
#   1. E-3 visas issued to Australians (US State Department)
#   2. Student (F-1) and exchange visitor (J-1) visas issued to Australians
#   3. US citizens on Australian temporary skilled visas, by occupation (Home Affairs)
#   4. US citizens on temporary work, study and graduate visas in Australia (Home Affairs)
#   5. E-3 jobs by occupation, employer and city (US Department of Labor)
#
# Charts are saved in Migration/ (where the dashboard reads them); raw files
# and extracts are kept in Migration/data-raw/ and reused.
#
# Two helper scripts in this folder do the heavy file reading, using the
# Python standard library (Python is needed only when a new DOL or Home
# Affairs file arrives; extracts are cached):
#   extract_e3_lca.py       - E-3 rows from a DOL LCA disclosure workbook
#   extract_pivot_cache.py  - the data inside a Home Affairs pivot-table workbook
# ==============================================================================

library(readr)
library(forcats)
library(scales)
library(showtext)
library(sysfonts)

font_add_google("Source Sans 3", "source_sans")
showtext_auto()
showtext_opts(dpi = 150)   # match ggsave() dpi or text renders too small

csis_colours <- c(navy = "#00205B", red = "#D2232A", teal = "#0098A8",
                  gold = "#F2A900", grey = "#565A5C", ltgrey = "#B1B3B3")

theme_csis <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "source_sans") %+replace%
    theme(
      plot.title    = element_text(face = "bold", size = rel(1.2), hjust = 0,
                                   colour = csis_colours[["navy"]], margin = margin(b = 4)),
      plot.subtitle = element_text(hjust = 0, colour = csis_colours[["grey"]], margin = margin(b = 10)),
      plot.caption  = element_text(size = rel(0.75), hjust = 0, colour = csis_colours[["grey"]],
                                   margin = margin(t = 10), lineheight = 1.1),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "#E6E7E8", linewidth = 0.4),
      axis.text          = element_text(colour = csis_colours[["grey"]]),
      legend.position      = "top",
      legend.justification = "left",
      legend.title         = element_blank(),
      complete = TRUE
    )
}

save_csis <- function(plot, file, width = 9, height = 5.2) {
  ggsave(file, plot, width = width, height = height, dpi = 150, bg = "white")
}

BROWSER_UA <- paste("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
                    "(KHTML, like Gecko) Chrome/124.0 Safari/537.36")
PYTHON <- Sys.which(c("python", "python3", "py"))
PYTHON <- PYTHON[nzchar(PYTHON)][1]

run_python <- function(script, args) {
  if (is.na(PYTHON)) stop("Python is needed to read new DOL / Home Affairs files but was not found")
  status <- system2(PYTHON, c("-I", shQuote(script), vapply(args, shQuote, "")))
  if (status != 0) stop(script, " failed (exit code ", status, ")")
}


# ------------------------------------------------------------------------------
# A. US State Department: nonimmigrant visas issued, by nationality and class
# ------------------------------------------------------------------------------
# One workbook per US fiscal year (October-September). The State Department's
# own site blocks scripts, but its adoption.state.gov mirror serves the same
# files. File names have changed over the years, so a few spellings are tried.
# Years already downloaded are reused; a new year is picked up once published.

NIV_DIR  <- file.path("data-raw", "state_dept")
dir.create(NIV_DIR, recursive = TRUE, showWarnings = FALSE)
NIV_BASE <- "https://adoption.state.gov/content/dam/visas/Statistics/Non-Immigrant-Statistics/NIVDetailTables/"
NIV_CLASSES <- c("E3", "E3D", "E3R", "F1", "J1")

fetch_niv <- function(fy) {
  yy <- sprintf("%02d", fy %% 100)
  have <- list.files(NIV_DIR, sprintf("^FY%s\\.xlsx?$", yy), full.names = TRUE)
  if (length(have)) return(have[1])
  for (name in c(sprintf("FY%sNIVDetailTable.xlsx", yy), sprintf("FY%sNIVDetailTable.xls", yy),
                 sprintf("FY%s%%20NIV%%20Detail%%20Table.xlsx", yy),
                 sprintf("FY%s%%20NIV%%20Detail%%20Table.xls", yy))) {
    ext  <- if (grepl("xlsx$", name)) "xlsx" else "xls"
    dest <- file.path(NIV_DIR, sprintf("FY%s.%s", yy, ext))
    ok <- tryCatch({
      httr2::request(paste0(NIV_BASE, name)) |>
        httr2::req_user_agent(BROWSER_UA) |>
        httr2::req_timeout(120) |>
        httr2::req_perform(path = dest)
      sig <- readBin(dest, "raw", 4)
      identical(sig[1:2], charToRaw("PK")) || identical(sig, as.raw(c(0xD0, 0xCF, 0x11, 0xE0)))
    }, error = function(e) FALSE)
    if (ok) return(dest)
    unlink(dest)
  }
  NULL
}

# Australia's row of one year's table: the header row is the one holding the
# visa class codes, and the Australia row is matched on its first cell.
read_niv_australia <- function(path, fy) {
  d <- suppressMessages(as.data.frame(read_excel(path, col_names = FALSE, col_types = "text")))
  hdr <- which(apply(d, 1, function(r) any(trimws(r) %in% c("E3", "E-3"))))[1]
  au  <- which(trimws(d[[1]]) == "Australia")
  if (is.na(hdr) || !length(au)) { message("NIV FY", fy, ": layout not recognised"); return(NULL) }
  h <- toupper(gsub("[^A-Za-z0-9]", "", unlist(d[hdr, ])))
  v <- unlist(d[au[1], ])
  tibble(fy = fy, class = NIV_CLASSES,
         visas = vapply(NIV_CLASSES, function(k) {
           j <- which(h == k)[1]
           if (is.na(j)) NA_real_ else suppressWarnings(as.numeric(gsub("[^0-9]", "", v[j])))
         }, numeric(1)))
}

niv_years <- 2006:(as.integer(format(Sys.Date(), "%Y")) - 1)
niv <- purrr::map(niv_years, function(fy) {
  p <- fetch_niv(fy)
  if (is.null(p)) return(NULL)
  read_niv_australia(p, fy)
}) |> purrr::list_rbind() |>
  mutate(visas = replace_na(visas, 0))

niv_last <- max(niv$fy)
message("State Department visa tables: FY", min(niv$fy), "-FY", niv_last)
write_csv(niv, "visas_issued_australians_tidy.csv")

NIV_CAPTION <- paste0("Source: US Department of State, Report of the Visa Office, nonimmigrant visas issued ",
                      "by nationality and visa class (via the adoption.state.gov mirror).\n",
                      "US fiscal years ending 30 September. Visas issued, not people admitted.")

# --- Chart 1: E-3 visas against the cap ---
E3_CAP <- 10500
e3 <- niv |>
  filter(class %in% c("E3", "E3R", "E3D")) |>
  mutate(type = recode(class, E3 = "New visas (principal applicants, counted against the cap)",
                              E3R = "Renewals", E3D = "Spouses and children"),
         type = factor(type, levels = c("New visas (principal applicants, counted against the cap)",
                                        "Renewals", "Spouses and children")))

p_e3 <- ggplot(e3, aes(fy, visas, colour = type)) +
  geom_hline(yintercept = E3_CAP, linetype = "22", colour = csis_colours[["grey"]], linewidth = 0.6) +
  annotate("text", x = min(e3$fy), y = E3_CAP, label = "Annual cap: 10,500 new visas",
           hjust = 0, vjust = -0.6, size = 3.4, family = "source_sans", colour = csis_colours[["grey"]]) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.6) +
  scale_colour_manual(values = c(csis_colours[["navy"]], csis_colours[["teal"]], csis_colours[["ltgrey"]])) +
  scale_y_continuous(labels = comma, limits = c(0, NA), expand = expansion(c(0, 0.08))) +
  scale_x_continuous(breaks = seq(2006, niv_last, 2)) +
  guides(colour = guide_legend(ncol = 1)) +
  labs(title = "Skilled professional visas issued to Australians",
       subtitle = sprintf("Visas issued by US fiscal year, FY%d to FY%d. A US work visa reserved for Australian professionals.",
                          min(e3$fy), niv_last),
       x = NULL, y = NULL,
       caption = paste0(NIV_CAPTION, " Visa class E-3; renewals and dependants do not count against the cap.")) +
  theme_csis()

save_csis(p_e3, "e3_visas_australians.png")

# --- Chart 2: student and exchange visitor visas ---
fj <- niv |>
  filter(class %in% c("F1", "J1")) |>
  mutate(type = recode(class, F1 = "F-1 students",
                              J1 = "J-1 exchange visitors (researchers, scholars, interns and others)"))

p_fj <- ggplot(fj, aes(fy, visas, colour = type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.6) +
  scale_colour_manual(values = c(csis_colours[["navy"]], csis_colours[["gold"]])) +
  scale_y_continuous(labels = comma, limits = c(0, NA), expand = expansion(c(0, 0.08))) +
  scale_x_continuous(breaks = seq(2006, niv_last, 2)) +
  guides(colour = guide_legend(ncol = 1)) +
  labs(title = "Student and exchange visas issued to Australians",
       subtitle = sprintf("Visas issued by US fiscal year, FY%d to FY%d", min(fj$fy), niv_last),
       x = NULL, y = NULL, caption = NIV_CAPTION) +
  theme_csis()

save_csis(p_fj, "student_exchange_visas_australians.png")


# ------------------------------------------------------------------------------
# B. Home Affairs: US citizens on temporary visas in Australia
# ------------------------------------------------------------------------------
# data.gov.au datasets (pivot-table workbooks, refreshed quarterly/monthly):
#   temporary-entrants-visa-holders  (BP0019) - temporary visa holders by citizenship
#   visa-temporary-work-skilled      (BP0014) - skilled visa holders, with occupation
# The newest workbook is downloaded when its name changes; the US-citizen
# records are extracted once and cached as CSV.

HA_DIR <- file.path("data-raw", "home_affairs")
dir.create(HA_DIR, recursive = TRUE, showWarnings = FALSE)
US_CITIZEN <- "United States of America"

fetch_home_affairs <- function(package, pattern) {
  meta <- tryCatch(
    httr2::request(paste0("https://data.gov.au/data/api/3/action/package_show?id=", package)) |>
      httr2::req_retry(max_tries = 3) |> httr2::req_perform() |> httr2::resp_body_json(),
    error = function(e) NULL)
  urls <- if (is.null(meta)) character() else
    vapply(meta$result$resources, function(r) r$url, "")
  url <- grep(pattern, urls, value = TRUE)[1]
  if (is.na(url)) {                      # offline or renamed: fall back to the newest local copy
    have <- sort(list.files(HA_DIR, pattern, full.names = TRUE), decreasing = TRUE)
    if (!length(have)) stop("No Home Affairs file found for ", package)
    return(have[1])
  }
  dest <- file.path(HA_DIR, basename(url))
  if (!file.exists(dest)) {
    message("Downloading ", basename(url))
    httr2::request(url) |> httr2::req_user_agent(BROWSER_UA) |>
      httr2::req_timeout(900) |> httr2::req_retry(max_tries = 3) |>
      httr2::req_perform(path = dest)
  }
  dest
}

us_extract <- function(xlsx) {
  out <- sub("\\.xlsx$", "_us.csv", xlsx)
  if (!file.exists(out)) {
    message("Extracting US-citizen records from ", basename(xlsx), " (one-off, can take a few minutes)")
    run_python("extract_pivot_cache.py", c(xlsx, out, paste0("Citizenship Country=", US_CITIZEN)))
  }
  read_csv(out, show_col_types = FALSE)
}

HA_CAPTION <- "Source: Department of Home Affairs, via data.gov.au"

# --- Chart 3: US citizens on temporary skilled visas, by occupation ---
skilled <- us_extract(fetch_home_affairs("visa-temporary-work-skilled", "holders.*\\.xlsx$")) |>
  mutate(snapshot = as.Date(substr(`Snapshot Date`, 1, 10)))
sk_last <- max(skilled$snapshot)

sk_occ <- skilled |>
  filter(snapshot == sk_last, `Applicant Type` == "Primary") |>
  count(occupation = `Nominated Occupation (Unit Group)`, wt = Total, name = "holders") |>
  filter(!is.na(occupation), !occupation %in% c("Not Applicable", "Not Specified", "Not Recorded")) |>
  slice_max(holders, n = 15) |>
  mutate(occupation = str_remove(occupation, "^\\d+\\s*-?\\s*"),
         occupation = fct_reorder(str_wrap(occupation, 40), holders))

sk_total <- skilled |>
  filter(snapshot == sk_last, `Applicant Type` == "Primary") |>
  summarise(n = sum(Total)) |> pull(n)

p_sk <- ggplot(sk_occ, aes(holders, occupation)) +
  geom_col(fill = csis_colours[["navy"]], width = 0.7) +
  geom_text(aes(label = comma(holders)), hjust = -0.2, size = 3.3, family = "source_sans",
            colour = csis_colours[["grey"]]) +
  scale_x_continuous(labels = comma, expand = expansion(c(0, 0.12))) +
  labs(title = "Skilled visas held by Americans in Australia, by occupation",
       subtitle = sprintf("US citizens holding temporary skilled visas (primary applicants) at %s, top 15 nominated occupations (of %s in total)",
                          trimws(format(sk_last, "%e %B %Y")), comma(sk_total)),
       x = NULL, y = NULL,
       caption = paste0(HA_CAPTION, ", Temporary Resident (Skilled) visa holders (BP0014). ",
                        "Covers Temporary Skill Shortage / Skills in Demand and\nearlier 457 visas; ",
                        "occupations are those nominated by the sponsoring employer.")) +
  theme_csis() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#E6E7E8", linewidth = 0.4),
        axis.text.y = element_text(colour = csis_colours[["navy"]], size = rel(0.9)))

save_csis(p_sk, "us_citizens_skilled_visas_occupation.png", height = 6)

# --- Chart 4: US citizens on temporary work, study and graduate visas ---
TEMP_GROUPS <- c("Temporary Resident (Skilled Employment)" = "Skilled employment",
                 "Temporary Resident (Other Employment)"   = "Other employment (incl. research, entertainment, sport)",
                 "Temporary Graduate"                      = "Temporary graduate",
                 "Student"                                 = "Student",
                 "Working Holiday Maker"                   = "Work and Holiday")

temp <- us_extract(fetch_home_affairs("temporary-entrants-visa-holders", "bp0019.*\\.xlsx$")) |>
  filter(`Visa Category` %in% names(TEMP_GROUPS)) |>
  mutate(snapshot = as.Date(substr(`Snapshot Date`, 1, 10)),
         group = factor(TEMP_GROUPS[`Visa Category`], levels = rev(unname(TEMP_GROUPS)))) |>
  count(snapshot, group, wt = `Visa Holders`, name = "holders")

# Smoothed with a four-quarter moving average of the quarter-end snapshots,
# which takes out the seasonal swing in student and working holiday numbers.
temp_ma <- temp |>
  filter(snapshot == lubridate::ceiling_date(snapshot, "quarter") - 1) |>
  arrange(group, snapshot) |>
  group_by(group) |>
  mutate(holders = as.numeric(stats::filter(holders, rep(1 / 4, 4), sides = 1))) |>
  ungroup() |>
  filter(!is.na(holders))

p_temp <- ggplot(temp_ma, aes(snapshot, holders, fill = group)) +
  geom_area(colour = "white", linewidth = 0.2) +
  scale_fill_manual(values = setNames(c(csis_colours[["navy"]], "#3E5C8A", csis_colours[["teal"]],
                                        csis_colours[["gold"]], csis_colours[["ltgrey"]]),
                                      unname(TEMP_GROUPS))) +
  scale_y_continuous(labels = comma, expand = expansion(c(0, 0.05))) +
  scale_x_date(breaks = seq(as.Date("2012-01-01"), max(temp_ma$snapshot), by = "2 years"),
               date_labels = "%Y", expand = expansion(mult = c(0.01, 0.02))) +
  guides(fill = guide_legend(ncol = 2, reverse = TRUE)) +
  labs(title = "Temporary visas held by Americans in Australia, by type",
       subtitle = sprintf(paste0("US citizens holding work, study and graduate visas, primary and secondary applicants.\n",
                                 "Four-quarter moving average, %s to %s"),
                          format(min(temp_ma$snapshot), "%B %Y"), format(max(temp_ma$snapshot), "%B %Y")),
       x = NULL, y = NULL,
       caption = paste0(HA_CAPTION, ", temporary visa holders in Australia (BP0019). ",
                        "Excludes visitor, bridging, transit and other temporary visas.")) +
  theme_csis()

save_csis(p_temp, "us_citizens_temporary_visas.png")


# ------------------------------------------------------------------------------
# C. US Department of Labor: E-3 Labor Condition Applications (LCAs)
# ------------------------------------------------------------------------------
# Every E-3 needs an LCA from the employer, recording the job, occupation (SOC
# code), employer and worksite. DOL publishes them in large workbooks that
# dol.gov will not serve to scripts, so they are downloaded by hand into
#   Migration/data-raw/dol/
# from https://www.dol.gov/agencies/eta/foreign-labor/performance
# Up to FY2019 there is one file per fiscal year; from FY2020 there is one per
# quarter (not cumulative), so a year counts as complete only when all four
# quarters are present. The latest complete year is charted.
# E-3 rows are extracted once per file (extract_e3_lca.py) and cached.

DOL_DIR <- file.path("data-raw", "dol")
E3_DIR  <- file.path("data-raw", "dol_e3")
dir.create(E3_DIR, recursive = TRUE, showWarnings = FALSE)

dol_files <- tibble(path = list.files(DOL_DIR, "\\.xlsx$", full.names = TRUE)) |>
  mutate(name = basename(path),
         fy   = as.integer(str_match(name, "FY ?(20\\d{2}|\\d{2})")[, 2]),
         fy   = if_else(fy < 100, 2000L + fy, fy),
         qtr  = as.integer(str_match(name, "_Q([1-4])")[, 2]),
         quarterly = fy >= 2020)

complete_fy <- dol_files |>
  group_by(fy) |>
  summarise(complete = if (first(quarterly)) all(1:4 %in% qtr) else TRUE, .groups = "drop") |>
  filter(complete) |>
  pull(fy)

lca_fy <- max(complete_fy)
use_files <- dol_files |> filter(fy %in% complete_fy)   # every file for each complete year
message("DOL LCA data: FY", paste(range(complete_fy), collapse = "-"), " (", nrow(use_files),
        " files); latest complete year FY", lca_fy)

# Certified E-3 LCAs for every complete year. A case can appear in two files
# (FY2009 has two systems' files; quarterly files overlap slightly), so keep
# one row per case number within each year.
e3_all <- purrr::map2(use_files$path, use_files$fy, function(p, fy) {
  out <- file.path(E3_DIR, sub("\\.xlsx$", "_e3.csv", basename(p)))
  if (!file.exists(out)) {
    message("Extracting E-3 rows from ", basename(p), " (one-off, can take several minutes)")
    run_python("extract_e3_lca.py", c(p, out))
  }
  read_csv(out, show_col_types = FALSE, col_types = cols(.default = col_character())) |>
    mutate(fy = fy)
}) |> purrr::list_rbind() |>
  distinct(fy, case_number, .keep_all = TRUE) |>
  filter(str_to_lower(str_squish(case_status)) == "certified") |>   # not "Certified - Withdrawn"
  mutate(positions = suppressWarnings(as.numeric(positions)),
         positions = if_else(is.na(positions) | positions < 1, 1, positions))

e3_lca <- e3_all |> filter(fy == lca_fy)

# SOC major groups (first two digits of the SOC code)
SOC_MAJOR <- c("11" = "Management", "13" = "Business and financial operations",
               "15" = "Computer and mathematical", "17" = "Architecture and engineering",
               "19" = "Life, physical and social science", "21" = "Community and social service",
               "23" = "Legal", "25" = "Education and library", "27" = "Arts, design, entertainment, sports and media",
               "29" = "Healthcare practitioners", "31" = "Healthcare support",
               "41" = "Sales", "43" = "Office and administrative support")

lca_occ <- e3_lca |>
  mutate(group = SOC_MAJOR[substr(soc_code, 1, 2)],
         group = replace_na(group, "Other")) |>
  count(group, name = "lcas") |>
  mutate(share = lcas / sum(lcas)) |>
  arrange(desc(lcas)) |>
  mutate(group = fct_reorder(group, lcas))

p_lca_occ <- ggplot(lca_occ, aes(lcas, group)) +
  geom_col(fill = csis_colours[["navy"]], width = 0.7) +
  geom_text(aes(label = sprintf("%s (%s)", comma(lcas), percent(share, accuracy = 1))),
            hjust = -0.1, size = 3.3, family = "source_sans", colour = csis_colours[["grey"]]) +
  scale_x_continuous(labels = comma, expand = expansion(c(0, 0.2))) +
  labs(title = "Skilled professional jobs certified for Australians, by occupation",
       subtitle = sprintf("Certified Labor Condition Applications, US fiscal year %d (%s in total)",
                          lca_fy, comma(nrow(e3_lca))),
       x = NULL, y = NULL,
       caption = paste0("Source: US Department of Labor, Office of Foreign Labor Certification, LCA disclosure data. ",
                        "Occupation groups are 2018 SOC major groups. Visa class E-3.\n",
                        "Employers file an LCA for each new job, transfer and extension, so LCAs ",
                        "exceed visas issued and are not a count of people.")) +
  theme_csis() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#E6E7E8", linewidth = 0.4),
        axis.text.y = element_text(colour = csis_colours[["navy"]]))

save_csis(p_lca_occ, "e3_jobs_by_occupation.png", height = 5.6)

# E-3 jobs over time, by occupation group, from FY2011. Earlier files use
# older occupation codes (not SOC), and the FY2010 file has no visa-class
# column, so E-3 cases can't be picked out of it.
TREND_GROUPS <- c("Computer and mathematical", "Management", "Business and financial operations",
                  "Architecture and engineering", "Arts, design, entertainment, sports and media")
lca_trend <- e3_all |>
  mutate(code2 = str_match(soc_code, "^(\\d{2})-")[, 2],
         group = case_when(is.na(code2) ~ "Not classified (occupation code missing or unreadable)",
                           SOC_MAJOR[code2] %in% TREND_GROUPS ~ SOC_MAJOR[code2],
                           TRUE ~ "Other occupations"),
         group = factor(group, levels = c(TREND_GROUPS, "Other occupations",
                                          "Not classified (occupation code missing or unreadable)"))) |>
  count(fy, group, name = "lcas") |>
  filter(fy >= 2011)

p_lca_trend <- ggplot(lca_trend, aes(fy, lcas, fill = group)) +
  geom_col(width = 0.75) +
  scale_fill_manual(values = c(csis_colours[["navy"]], "#3E5C8A", csis_colours[["teal"]],
                               csis_colours[["gold"]], "#E37A63", csis_colours[["ltgrey"]], "#DADCDF")) +
  scale_y_continuous(labels = comma, expand = expansion(c(0, 0.05))) +
  scale_x_continuous(breaks = seq(min(lca_trend$fy), max(lca_trend$fy), 2)) +
  guides(fill = guide_legend(ncol = 2)) +
  labs(title = "Skilled professional jobs certified for Australians, over time",
       subtitle = sprintf("Certified Labor Condition Applications by occupation group and US fiscal year, FY%d to FY%d",
                          min(lca_trend$fy), max(lca_trend$fy)),
       x = NULL, y = NULL,
       caption = paste0("Source: US Department of Labor, Office of Foreign Labor Certification, LCA disclosure data. ",
                        "Occupation groups are SOC major groups. Visa class E-3.\n",
                        "Employers file an LCA for each new job, transfer and extension, so LCAs ",
                        "exceed visas issued and are not a count of people.")) +
  theme_csis()

save_csis(p_lca_trend, "e3_jobs_trend.png", height = 5.6)
write_csv(lca_trend |> mutate(group = as.character(group)), "e3_jobs_trend_tidy.csv")

# Top employers and worksite cities (two panels)
# Drop company-type suffixes; DOL already gives names in mixed case
tidy_name <- function(x) str_squish(str_remove(x, regex(",?\\s*(inc|llc|l\\.l\\.c|llp|corp|corporation|ltd)\\.?$",
                                                         ignore_case = TRUE)))
top_emp <- e3_lca |>
  mutate(item = tidy_name(employer)) |>
  count(item, name = "lcas") |> slice_max(lcas, n = 12, with_ties = FALSE) |>
  mutate(panel = "Top employers")
top_city <- e3_lca |>
  mutate(item = paste0(str_to_title(worksite_city), ", ", toupper(worksite_state))) |>
  count(item, name = "lcas") |> slice_max(lcas, n = 12, with_ties = FALSE) |>
  mutate(panel = "Top worksite cities")

emp_city <- bind_rows(top_emp, top_city) |>
  mutate(item = paste(item, panel, sep = "___"),   # keeps facet orders separate
         item = fct_reorder(item, lcas))

p_lca_ec <- ggplot(emp_city, aes(lcas, item)) +
  geom_col(aes(fill = panel), width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = comma(lcas)), hjust = -0.2, size = 3.1, family = "source_sans",
            colour = csis_colours[["grey"]]) +
  facet_wrap(~ panel, scales = "free") +
  scale_y_discrete(labels = function(x) str_wrap(sub("___.*$", "", x), 28)) +
  scale_fill_manual(values = c("Top employers" = csis_colours[["navy"]],
                               "Top worksite cities" = csis_colours[["teal"]])) +
  scale_x_continuous(labels = comma, expand = expansion(c(0, 0.18))) +
  labs(title = "Skilled professional jobs certified for Australians, by employer and city",
       subtitle = sprintf("Certified Labor Condition Applications, top employers and worksite cities, US fiscal year %d", lca_fy),
       x = NULL, y = NULL,
       caption = paste0("Source: US Department of Labor, Office of Foreign Labor Certification, LCA disclosure data. ",
                        "Employer names lightly standardised; first listed worksite. Visa class E-3.\n",
                        "Employers file an LCA for each new job, transfer and extension, so LCAs ",
                        "are not a count of people.")) +
  theme_csis() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "#E6E7E8", linewidth = 0.4),
        strip.text = element_text(face = "bold", colour = csis_colours[["navy"]], hjust = 0),
        axis.text.y = element_text(colour = csis_colours[["navy"]], size = rel(0.85)))

save_csis(p_lca_ec, "e3_employers_cities.png", width = 11, height = 6)

# --- Tidy exports ---
write_csv(sk_occ |> mutate(occupation = as.character(occupation)), "us_citizens_skilled_visas_occupation_tidy.csv")
write_csv(temp, "us_citizens_temporary_visas_tidy.csv")
write_csv(lca_occ |> mutate(group = as.character(group)), "e3_jobs_by_occupation_tidy.csv")
write_csv(emp_city |> mutate(item = sub("___.*$", "", as.character(item))), "e3_employers_cities_tidy.csv")

message("Done - five two-way flow charts written to ", getwd())


# ------------------------------------------------------------------------------
# Takeaways under each chart (read back from the tidy CSVs written above)
# ------------------------------------------------------------------------------
source(file.path(Sys.getenv("DASHBOARD_DIR"), "0. Takeaways.R"))
local({
  mg <- function(file) file.path("Migration", file)
  rd <- function(f) utils::read.csv(f, stringsAsFactors = FALSE)
  v <- rd("visas_issued_australians_tidy.csv")
  e3 <- aggregate(visas ~ fy, v[grepl("^E3", v$class), ], sum); e3 <- e3[order(e3$fy), ]
  takeaway(mg("e3_visas_australians.png"), sprintf(
    "The US issued %s E-3 skilled professional visas to Australians and their families in fiscal year %d.",
    format(e3$visas[nrow(e3)], big.mark = ","), e3$fy[nrow(e3)]))
  se <- aggregate(visas ~ fy, v[grepl("^(F|J|M)", v$class), ], sum); se <- se[order(se$fy), ]
  if (nrow(se)) takeaway(mg("student_exchange_visas_australians.png"), sprintf(
    "Australians received %s US student and exchange visas in fiscal year %d.",
    format(se$visas[nrow(se)], big.mark = ","), se$fy[nrow(se)]))
  oc <- rd("e3_jobs_by_occupation_tidy.csv"); oc <- oc[order(-oc$lcas), ]
  takeaway(mg("e3_jobs_by_occupation.png"), sprintf(
    "%s jobs made up %.0f%% of US jobs certified for Australian skilled professionals.",
    oc$group[1], 100 * oc$share[1]))
  tr <- rd("e3_jobs_trend_tidy.csv"); tt <- aggregate(lcas ~ fy, tr, sum); tt <- tt[order(tt$fy), ]
  takeaway(mg("e3_jobs_trend.png"), sprintf(
    "US employers had %s jobs certified for Australian skilled professionals in fiscal year %d, against %s in %d.",
    format(tt$lcas[nrow(tt)], big.mark = ","), tt$fy[nrow(tt)], format(tt$lcas[1], big.mark = ","), tt$fy[1]))
  ec <- rd("e3_employers_cities_tidy.csv")
  emp <- ec[ec$panel == ec$panel[1], ]; emp <- emp[order(-emp$lcas), ]
  takeaway(mg("e3_employers_cities.png"), sprintf(
    "%s certified the most jobs for Australian skilled professionals (%d), followed by %s and %s.",
    emp$item[1], emp$lcas[1], emp$item[2], emp$item[3]))
  sk <- rd("us_citizens_skilled_visas_occupation_tidy.csv"); sk <- sk[order(-sk$holders), ]
  takeaway(mg("us_citizens_skilled_visas_occupation.png"), sprintf(
    "%s are the most common occupation among Americans on skilled visas in Australia (%d holders).",
    sk$occupation[1], sk$holders[1]))
  tv <- rd("us_citizens_temporary_visas_tidy.csv"); tl <- tv[tv$snapshot == max(tv$snapshot), ]
  tl <- tl[order(-tl$holders), ]
  takeaway(mg("us_citizens_temporary_visas.png"), sprintf(
    "%s Americans held temporary visas in Australia at %s; the largest group (%s) held %s visas.",
    format(sum(tl$holders), big.mark = ","), format(as.Date(tl$snapshot[1]), "%d %B %Y"),
    format(tl$holders[1], big.mark = ","), tolower(tl$group[1])))
  nm <- rd("abs_nom_usa_born.csv"); nm <- nm[nm$region == "Australia", ]; nm <- nm[order(nm$fy_end), ]
  takeaway(mg("nom_usa_national.png"), sprintf(
    "Net overseas migration of US-born people to Australia was %s in %s%s.",
    format(nm$value[nrow(nm)], big.mark = ","), nm$period[nrow(nm)],
    if (isTRUE(nm$preliminary[nrow(nm)])) " (preliminary)" else ""))
})
