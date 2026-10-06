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
    title    = "Net overseas migration of USA-born people, Australia",
    subtitle = "Persons, years ending 30 June",
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
    title    = "Net overseas migration of USA-born people, by state and territory",
    subtitle = "Persons, years ending 30 June",
    x = NULL, y = NULL, caption = abs_caption
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.caption = element_text(hjust = 0))

print(p_states)

ggsave(file.path(OUT_DIR, "nom_usa_national.png"), p_national,
       width = 9, height = 5, dpi = 300)
ggsave(file.path(OUT_DIR, "nom_usa_states.png"), p_states,
       width = 11, height = 6, dpi = 300)


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