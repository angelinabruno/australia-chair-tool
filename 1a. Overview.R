rm(list = ls())
# The dashboard folder is set once, in section 1 of '1. Refresh all data.R'.
# To run this script on its own, run that section first.
if (Sys.getenv("DASHBOARD_DIR") == "")
  stop("Dashboard folder not set. Run section 1 of '1. Refresh all data.R' first.")
setwd(file.path(Sys.getenv("DASHBOARD_DIR"), "Overview"))
# ============================================================
# AUSTRALIA OVERLAID ON THE CONTIGUOUS UNITED STATES
# ============================================================
#
# Designed for dashboard / landing-page use.
#
# ============================================================


# ------------------------------------------------------------
# 0. PACKAGES
# ------------------------------------------------------------

packages <- c(
  "sf",
  "ggplot2",
  "rnaturalearth"
)

missing_packages <- packages[
  !packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(
    missing_packages,
    repos = "https://cloud.r-project.org"
  )
}

library(sf)
library(ggplot2)
library(rnaturalearth)


# ------------------------------------------------------------
# 1. GET COUNTRY BOUNDARIES
# ------------------------------------------------------------

countries <- ne_countries(
  scale = 50,
  returnclass = "sf"
)

usa <- countries[
  countries$admin == "United States of America",
]

aus <- countries[
  countries$admin == "Australia",
]


# ------------------------------------------------------------
# 2. GET STATE / TERRITORY BOUNDARIES
#
# Uses Natural Earth 1:50m data and avoids
# rnaturalearthhires.
# ------------------------------------------------------------

states_world <- ne_download(
  scale = 50,
  type = "states",
  category = "cultural",
  returnclass = "sf"
)

usa_states <- states_world[
  states_world$admin == "United States of America",
]

aus_states <- states_world[
  states_world$admin == "Australia",
]


# ------------------------------------------------------------
# 3. DEFINE CONTIGUOUS US BOUNDING BOX
# ------------------------------------------------------------

us_crop_box <- st_bbox(
  c(
    xmin = -125,
    ymin = 24,
    xmax = -66,
    ymax = 50
  ),
  crs = st_crs(usa)
)

usa_contiguous <- st_crop(
  usa,
  us_crop_box
)

usa_states_contiguous <- st_crop(
  usa_states,
  us_crop_box
)


# ------------------------------------------------------------
# 4. CROP AUSTRALIA TO MAINLAND + TASMANIA
#
# This removes distant offshore territories/islands that
# otherwise create the small dot far below the map and cause
# excessive whitespace.
# ------------------------------------------------------------

aus_crop_box <- st_bbox(
  c(
    xmin = 112,
    ymin = -44.5,
    xmax = 154,
    ymax = -10
  ),
  crs = st_crs(aus)
)

aus_main <- st_crop(
  aus,
  aus_crop_box
)

aus_states_main <- st_crop(
  aus_states,
  aus_crop_box
)


# ------------------------------------------------------------
# 5. EQUAL-AREA PROJECTION
#
# EPSG:8857 = Equal Earth
#
# Both countries are projected into the same equal-area
# coordinate system before Australia is shifted.
# ------------------------------------------------------------

map_crs <- 8857

usa_proj <- st_transform(
  usa_contiguous,
  map_crs
)

usa_states_proj <- st_transform(
  usa_states_contiguous,
  map_crs
)

aus_proj <- st_transform(
  aus_main,
  map_crs
)

aus_states_proj <- st_transform(
  aus_states_main,
  map_crs
)


# ------------------------------------------------------------
# 6. FIND CENTRE OF CONTIGUOUS US
# ------------------------------------------------------------

bbox_us <- st_bbox(usa_proj)

us_centre_x <- (
  as.numeric(bbox_us["xmin"]) +
    as.numeric(bbox_us["xmax"])
) / 2

us_centre_y <- (
  as.numeric(bbox_us["ymin"]) +
    as.numeric(bbox_us["ymax"])
) / 2


# ------------------------------------------------------------
# 7. FIND CENTRE OF AUSTRALIA
# ------------------------------------------------------------

bbox_aus <- st_bbox(aus_proj)

aus_centre_x <- (
  as.numeric(bbox_aus["xmin"]) +
    as.numeric(bbox_aus["xmax"])
) / 2

aus_centre_y <- (
  as.numeric(bbox_aus["ymin"]) +
    as.numeric(bbox_aus["ymax"])
) / 2


# ------------------------------------------------------------
# 8. CALCULATE TRANSLATION
#
# Australia is moved only — NOT resized.
#
# Small manual adjustments improve visual placement over
# the continental US.
# ------------------------------------------------------------

dx <- us_centre_x - aus_centre_x
dy <- us_centre_y - aus_centre_y

# ------------------------------------------------------------
# OPTIONAL VISUAL NUDGE
#
# Positive x = move Australia east
# Negative x = move west
#
# Positive y = move north
# Negative y = move south
# ------------------------------------------------------------

us_width <- as.numeric(
  bbox_us["xmax"] - bbox_us["xmin"]
)

us_height <- as.numeric(
  bbox_us["ymax"] - bbox_us["ymin"]
)

# Move Australia slightly east and slightly south
# for a more balanced overlay.

dx <- dx + us_width * 0.015
dy <- dy - us_height * 0.03


# ------------------------------------------------------------
# 9. SHIFT AUSTRALIA
# ------------------------------------------------------------

aus_overlay <- aus_proj

aus_geom_shifted <-
  st_geometry(aus_proj) + c(dx, dy)

st_crs(aus_geom_shifted) <-
  st_crs(aus_proj)

st_geometry(aus_overlay) <-
  aus_geom_shifted


# ------------------------------------------------------------
# 10. SHIFT AUSTRALIAN STATE BOUNDARIES
# ------------------------------------------------------------

aus_states_overlay <- aus_states_proj

aus_states_geom_shifted <-
  st_geometry(aus_states_proj) + c(dx, dy)

st_crs(aus_states_geom_shifted) <-
  st_crs(aus_states_proj)

st_geometry(aus_states_overlay) <-
  aus_states_geom_shifted


# ------------------------------------------------------------
# 11. CREATE COMBINED BOUNDING BOX
#
# Plot limits are based on BOTH countries so neither one
# gets clipped.
# ------------------------------------------------------------

combined_geometry <- c(
  st_geometry(usa_proj),
  st_geometry(aus_overlay)
)

st_crs(combined_geometry) <-
  st_crs(usa_proj)

combined_bbox <- st_bbox(
  combined_geometry
)

plot_width <- as.numeric(
  combined_bbox["xmax"] -
    combined_bbox["xmin"]
)

plot_height <- as.numeric(
  combined_bbox["ymax"] -
    combined_bbox["ymin"]
)


# ------------------------------------------------------------
# 12. TIGHT PADDING
#
# Less whitespace than previous version.
# ------------------------------------------------------------

x_padding <- plot_width * 0.025
y_padding <- plot_height * 0.045


# ------------------------------------------------------------
# 13. CREATE MAP
# ------------------------------------------------------------

australia_us_map <- ggplot() +
  
  # ----------------------------------------------------------
# US BASE
# ----------------------------------------------------------

geom_sf(
  data = usa_proj,
  fill = "#E1E5E9",
  colour = "#AEB6BE",
  linewidth = 0.7
) +
  
  # ----------------------------------------------------------
# US STATE BOUNDARIES
# ----------------------------------------------------------

geom_sf(
  data = usa_states_proj,
  fill = NA,
  colour = "white",
  linewidth = 0.5,
  alpha = 0.95
) +
  
  # ----------------------------------------------------------
# AUSTRALIA OVERLAY
# ----------------------------------------------------------

geom_sf(
  data = aus_overlay,
  fill = "#4396C6",
  colour = "#003F60",
  linewidth = 1.15,
  alpha = 0.72
) +
  
  # ----------------------------------------------------------
# AUSTRALIAN STATE / TERRITORY BOUNDARIES
# ----------------------------------------------------------

geom_sf(
  data = aus_states_overlay,
  fill = NA,
  colour = "#003F60",
  linewidth = 0.65,
  alpha = 0.95
) +
  
  # ----------------------------------------------------------
# MAP LIMITS
# ----------------------------------------------------------

coord_sf(
  crs = st_crs(usa_proj),
  
  xlim = c(
    as.numeric(combined_bbox["xmin"]) - x_padding,
    as.numeric(combined_bbox["xmax"]) + x_padding
  ),
  
  ylim = c(
    as.numeric(combined_bbox["ymin"]) - y_padding,
    as.numeric(combined_bbox["ymax"]) + y_padding
  ),
  
  expand = FALSE,
  datum = NA
) +
  
  # ----------------------------------------------------------
# TEXT
# ----------------------------------------------------------

labs(
  title = "Australia is almost as large as the US",
#  subtitle = "Australia shown at the same geographic scale",
#  caption = "Source: Natural Earth"
) +
  
  # ----------------------------------------------------------
# THEME
# ----------------------------------------------------------

theme_void(
  base_size = 14
) +
  
  theme(
    
    plot.title = element_text(
      size = 24,
      face = "bold",
      hjust = 0,
      margin = margin(
        b = 5
      )
    ),
    
    plot.subtitle = element_text(
      size = 13,
      colour = "#555555",
      hjust = 0,
      margin = margin(
        b = 10
      )
    ),
    
    plot.caption = element_text(
      size = 8.5,
      colour = "#777777",
      hjust = 0,
      margin = margin(
        t = 6
      )
    ),
    
    plot.margin = margin(
      t = 12,
      r = 12,
      b = 10,
      l = 12
    )
  )


# ------------------------------------------------------------
# 14. DISPLAY
# ------------------------------------------------------------

print(australia_us_map)


# ------------------------------------------------------------
# 15. SAVE DASHBOARD VERSION
#
# A slightly wider aspect ratio should work well in a
# Quarto dashboard card.
# ------------------------------------------------------------

ggsave(
  filename = "australia_over_us.png",
  plot = australia_us_map,
  width = 11,
  height = 6.2,
  units = "in",
  dpi = 300,
  bg = "white"
)


# ------------------------------------------------------------
# 16. TRANSPARENT VERSION
#
# Useful if your dashboard card already supplies the
# background colour.
# ------------------------------------------------------------

ggsave(
  filename = "australia_over_us_transparent.png",
  plot = australia_us_map,
  width = 11,
  height = 6.2,
  units = "in",
  dpi = 300,
  bg = "transparent"
)


# ------------------------------------------------------------
# 17. CONFIRM OUTPUT LOCATION
# ------------------------------------------------------------

cat(
  "\nSaved to:\n",
  normalizePath(
    "australia_over_us.png",
    mustWork = FALSE
  ),
  "\n"
)



# ─────────────────────────────────────────────────────────────
#  US–Australia dashboard map
#  One chart: great-circle distances (miles) from HMAS Stirling
#  to US Navy bases, plus Pacific island capitals.
#  Packages: ggplot2, ggrepel, maps (no sf / ozmaps needed)
# ─────────────────────────────────────────────────────────────

library(ggplot2)
library(ggrepel)
library(maps)

world2 <- map_data("world2")   # Pacific-centred world (0–360 longitude)

# ── Helpers ──────────────────────────────────────────────────
# Great-circle distance in statute miles (haversine)
miles_between <- function(lon1, lat1, lon2, lat2, r = 3958.8) {
  d2r  <- pi / 180
  dlat <- (lat2 - lat1) * d2r
  dlon <- (lon2 - lon1) * d2r
  a <- sin(dlat / 2)^2 + cos(lat1 * d2r) * cos(lat2 * d2r) * sin(dlon / 2)^2
  2 * r * asin(pmin(1, sqrt(a)))
}

# Points along the great-circle path, returned in 0–360 longitude
gc_path <- function(lon1, lat1, lon2, lat2, n = 120) {
  d2r <- pi / 180
  to_xyz <- function(lon, lat) c(cos(lat * d2r) * cos(lon * d2r),
                                 cos(lat * d2r) * sin(lon * d2r),
                                 sin(lat * d2r))
  p1 <- to_xyz(lon1, lat1); p2 <- to_xyz(lon2, lat2)
  omega <- acos(pmin(1, sum(p1 * p2)))
  t <- seq(0, 1, length.out = n)
  pts <- sapply(t, function(f) (sin((1 - f) * omega) * p1 +
                                  sin(f * omega) * p2) / sin(omega))
  lon <- atan2(pts[2, ], pts[1, ]) / d2r
  lat <- atan2(pts[3, ], sqrt(pts[1, ]^2 + pts[2, ]^2)) / d2r
  data.frame(lon = ifelse(lon < 0, lon + 360, lon), lat = lat, t = t)
}

# ── US and Australian sites ──────────────────────────────────
sites <- data.frame(
  id   = c("stirling", "osborne", "darwin",
           "guam", "pearl", "sandiego", "kitsap"),
  name = c("HMAS Stirling (Perth)\nSRF-West from 2027",
           "Osborne (Adelaide)\nSSN-AUKUS build",
           "Darwin\nUS sub port visits",
           "Naval Base Guam",
           "Pearl Harbor (Hawaii)",
           "Naval Base San Diego",
           "Naval Base Kitsap-Bangor (WA)"),
  lon  = c(115.68, 138.50, 130.84, 144.66, -157.95, -117.13, -122.71),
  lat  = c(-32.23, -34.79, -12.46,  13.45,   21.35,   32.68,   47.72),
  country = c(rep("Australia", 3), rep("United States", 4)),
  stringsAsFactors = FALSE
)
sites$lon2 <- ifelse(sites$lon < 0, sites$lon + 360, sites$lon)

# ── Distance routes ──────────────────────────────────────────
# Add or remove rows to change which pairs are measured.
# The mileage is written into the destination's label.
route_pairs <- data.frame(
  from = c("stirling", "stirling", "stirling", "stirling"),
  to   = c("guam",     "pearl",    "sandiego", "kitsap"),
  stringsAsFactors = FALSE
)

route_paths <- list()
route_pairs$miles <- NA
for (i in seq_len(nrow(route_pairs))) {
  a <- sites[sites$id == route_pairs$from[i], ]
  b <- sites[sites$id == route_pairs$to[i], ]
  path <- gc_path(a$lon, a$lat, b$lon, b$lat)
  path$route <- paste(a$id, b$id, sep = "-")
  route_paths[[i]] <- path
  route_pairs$miles[i] <- miles_between(a$lon, a$lat, b$lon, b$lat)
}
route_paths <- do.call(rbind, route_paths)

print(route_pairs)   # quick check of the numbers in the console

# Append "x,xxx mi from Stirling" to each destination label
fmt_mi <- function(m) paste0(format(round(m, -1), big.mark = ","), " mi")
for (i in seq_len(nrow(route_pairs))) {
  k <- sites$id == route_pairs$to[i]
  sites$name[k] <- paste0(sites$name[k], "\n", fmt_mi(route_pairs$miles[i]),
                          " from Stirling")
}

# Label nudges — order: Stirling, Osborne, Darwin, Guam, Pearl,
# San Diego, Kitsap. Tweak here if anything sits awkwardly.
sites$nx <- c(-3,  0, -4,  -8,   0, -10, -6)
sites$ny <- c(-7, -5, -3,   6,  -6,  -6,  6)

# ── Pacific islands (capitals) ───────────────────────────────
islands <- data.frame(
  name = c("Palau", "Micronesia (FSM)", "Marshall Is.",
           "Papua New Guinea", "Solomon Is.", "Vanuatu", "Fiji",
           "Nauru", "Kiribati", "Tuvalu", "Samoa", "Tonga",
           "New Caledonia (FR)", "French Polynesia (FR)",
           "Cook Is. (NZ)", "Niue (NZ)", "American Samoa (US)"),
  lon  = c(134.62, 158.16, 171.38,
           147.18, 159.95, 168.32, 178.44,
           166.92, 172.98, 179.20, -171.77, -175.20,
           166.46, -149.57,
           -159.78, -169.92, -170.70),
  lat  = c(  7.50,   6.92,   7.09,
             -9.44,  -9.43, -17.73, -18.14,
             -0.55,   1.33,  -8.52, -13.83, -21.14,
             -22.28, -17.53,
             -21.21, -19.06, -14.28),
  group = c(rep("Compact of Free Association (US)", 3),
            rep("Pacific island country", 9),
            rep("Territory / associated state", 5)),
  stringsAsFactors = FALSE
)
islands$lon2 <- ifelse(islands$lon < 0, islands$lon + 360, islands$lon)

# ── One label layer so every label avoids every other ────────
col_country <- c("Australia" = "#1b7837", "United States" = "#2166ac")

lab_df <- rbind(
  data.frame(x = sites$lon2, y = sites$lat, label = sites$name,
             col = col_country[sites$country], size = 3.8, face = "bold",
             nx = sites$nx, ny = sites$ny),
  data.frame(x = islands$lon2, y = islands$lat, label = islands$name,
             col = "grey25", size = 2.9, face = "plain", nx = 0, ny = 0)
)

# ── Chart ────────────────────────────────────────────────────
p <- ggplot() +
  geom_polygon(data = world2, aes(long, lat, group = group),
               fill = "grey94", colour = "grey75", linewidth = 0.2) +
  geom_path(data = route_paths, aes(lon, lat, group = route),
            colour = "grey40", linetype = "22", linewidth = 0.5) +
  geom_point(data = islands, aes(lon2, lat, fill = group),
             shape = 24, size = 2.6, colour = "grey20", stroke = 0.3) +
  geom_point(data = sites, aes(lon2, lat, colour = country), size = 3.6) +
  geom_text_repel(
    data = lab_df, aes(x, y, label = label),
    colour = lab_df$col, size = lab_df$size, fontface = lab_df$face,
    nudge_x = lab_df$nx, nudge_y = lab_df$ny,
    bg.colour = "white", bg.r = 0.12,          # halo so route lines don't cut text
    lineheight = 0.9, box.padding = 0.45, point.padding = 0.3,
    segment.colour = "grey55", segment.size = 0.25,
    min.segment.length = 0.2, seed = 7, max.overlaps = Inf
  ) +
  scale_colour_manual(values = col_country, name = NULL) +
  scale_fill_manual(values = c(
    "Compact of Free Association (US)" = "#762a83",
    "Pacific island country"           = "#35978f",
    "Territory / associated state"     = "#dfc27d"
  ), name = NULL) +
  guides(colour = guide_legend(order = 1, override.aes = list(size = 4)),
         fill   = guide_legend(order = 2, override.aes = list(size = 3.5))) +
  coord_quickmap(xlim = c(105, 250), ylim = c(-47, 56), expand = FALSE) +
  labs(
    title    = "US-Australia: Distances Across the Pacific",
    subtitle = "Great-circle distance from HMAS Stirling (SRF-West) to US Navy bases",
    x = NULL, y = NULL,
    caption  = "Distances in statute miles, great-circle. Island markers show capital cities. Pacific-centred projection."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(face = "bold", size = 22),
    plot.subtitle   = element_text(size = 13),
    legend.position = "bottom",
    legend.box      = "horizontal",
    legend.text     = element_text(size = 11),
    panel.grid      = element_line(colour = "grey92"),
    axis.text       = element_blank(),   # 0–360 longitudes read oddly on a dashboard
    plot.caption    = element_text(colour = "grey40", size = 9)
  )

print(p)

ggsave("US-Australia distance map.png", p,
       width = 12, height = 8, dpi = 600, bg = "white")



library(ggplot2)
library(ggrepel)
library(maps)

world2 <- map_data("world2")   # Pacific-centred world (0–360 longitude)

# ── Helpers ──────────────────────────────────────────────────
# Great-circle distance in statute miles (haversine)
miles_between <- function(lon1, lat1, lon2, lat2, r = 3958.8) {
  d2r  <- pi / 180
  dlat <- (lat2 - lat1) * d2r
  dlon <- (lon2 - lon1) * d2r
  a <- sin(dlat / 2)^2 + cos(lat1 * d2r) * cos(lat2 * d2r) * sin(dlon / 2)^2
  2 * r * asin(pmin(1, sqrt(a)))
}

# Points along the great-circle path, returned in 0–360 longitude
gc_path <- function(lon1, lat1, lon2, lat2, n = 120) {
  d2r <- pi / 180
  to_xyz <- function(lon, lat) c(cos(lat * d2r) * cos(lon * d2r),
                                 cos(lat * d2r) * sin(lon * d2r),
                                 sin(lat * d2r))
  p1 <- to_xyz(lon1, lat1); p2 <- to_xyz(lon2, lat2)
  omega <- acos(pmin(1, sum(p1 * p2)))
  t <- seq(0, 1, length.out = n)
  pts <- sapply(t, function(f) (sin((1 - f) * omega) * p1 +
                                  sin(f * omega) * p2) / sin(omega))
  lon <- atan2(pts[2, ], pts[1, ]) / d2r
  lat <- atan2(pts[3, ], sqrt(pts[1, ]^2 + pts[2, ]^2)) / d2r
  data.frame(lon = ifelse(lon < 0, lon + 360, lon), lat = lat, t = t)
}

# ── Cities ───────────────────────────────────────────────────
sites <- data.frame(
  id   = c("perth", "adelaide", "darwin", "canberra",
           "guam", "honolulu", "sandiego", "seattle", "washington"),
  name = c("Perth", "Adelaide", "Darwin", "Canberra",
           "Guam", "Honolulu", "San Diego", "Seattle", "Washington DC"),
  lon  = c(115.86, 138.60, 130.84, 149.13,
           144.75, -157.86, -117.16, -122.33, -77.04),
  lat  = c(-31.95, -34.93, -12.46, -35.28,
           13.47,   21.31,   32.72,   47.61,  38.91),
  country = c(rep("Australia", 4), rep("United States", 5)),
  stringsAsFactors = FALSE
)
sites$lon2 <- ifelse(sites$lon < 0, sites$lon + 360, sites$lon)

# ── Distance routes ──────────────────────────────────────────
# Add or remove rows to change which pairs are measured.
# The mileage is written into the destination's label.
route_pairs <- data.frame(
  from = c("perth", "perth",    "perth",     "perth",   "canberra"),
  to   = c("guam",  "honolulu", "sandiego",  "seattle", "washington"),
  stringsAsFactors = FALSE
)

route_paths <- list()
route_pairs$miles <- NA
for (i in seq_len(nrow(route_pairs))) {
  a <- sites[sites$id == route_pairs$from[i], ]
  b <- sites[sites$id == route_pairs$to[i], ]
  path <- gc_path(a$lon, a$lat, b$lon, b$lat)
  path$route <- paste(a$id, b$id, sep = "-")
  route_paths[[i]] <- path
  route_pairs$miles[i] <- miles_between(a$lon, a$lat, b$lon, b$lat)
}
route_paths <- do.call(rbind, route_paths)

print(route_pairs)   # quick check of the numbers in the console

# Append "x,xxx mi from <origin>" to each destination label
fmt_mi <- function(m) paste0(format(round(m, -1), big.mark = ","), " mi")
for (i in seq_len(nrow(route_pairs))) {
  k    <- sites$id == route_pairs$to[i]
  from <- sites$name[sites$id == route_pairs$from[i]]
  sites$name[k] <- paste0(sites$name[k], "\n",
                          fmt_mi(route_pairs$miles[i]), " from ", from)
}

# Label nudges — order: Perth, Adelaide, Darwin, Canberra, Guam,
# Honolulu, San Diego, Seattle, Washington DC. Tweak if anything
# sits awkwardly.
sites$nx <- c(-6, 2, -5,  6,  -8,  0, -10, -8,  2)
sites$ny <- c(-7, -6, -3, -7,  7, -7,  -7,  7, 10)

# ── Pacific islands (capitals) ───────────────────────────────
islands <- data.frame(
  name = c("Palau", "Micronesia (FSM)", "Marshall Is.",
           "Papua New Guinea", "Solomon Is.", "Vanuatu", "Fiji",
           "Nauru", "Kiribati", "Tuvalu", "Samoa", "Tonga",
           "New Caledonia (FR)", "French Polynesia (FR)",
           "Cook Is. (NZ)", "Niue (NZ)", "American Samoa (US)"),
  lon  = c(134.62, 158.16, 171.38,
           147.18, 159.95, 168.32, 178.44,
           166.92, 172.98, 179.20, -171.77, -175.20,
           166.46, -149.57,
           -159.78, -169.92, -170.70),
  lat  = c(  7.50,   6.92,   7.09,
             -9.44,  -9.43, -17.73, -18.14,
             -0.55,   1.33,  -8.52, -13.83, -21.14,
             -22.28, -17.53,
             -21.21, -19.06, -14.28),
  group = c(rep("Compact of Free Association (US)", 3),
            rep("Pacific island country", 9),
            rep("Territory / associated state", 5)),
  stringsAsFactors = FALSE
)
islands$lon2 <- ifelse(islands$lon < 0, islands$lon + 360, islands$lon)

# ── One label layer so every label avoids every other ────────
col_country <- c("Australia" = "#1b7837", "United States" = "#2166ac")

lab_df <- rbind(
  data.frame(x = sites$lon2, y = sites$lat, label = sites$name,
             col = col_country[sites$country], size = 3.8, face = "bold",
             nx = sites$nx, ny = sites$ny),
  data.frame(x = islands$lon2, y = islands$lat, label = islands$name,
             col = "grey25", size = 2.9, face = "plain", nx = 0, ny = 0)
)

# ── Chart ────────────────────────────────────────────────────
p <- ggplot() +
  geom_polygon(data = world2, aes(long, lat, group = group),
               fill = "grey94", colour = "grey75", linewidth = 0.2) +
  geom_path(data = route_paths, aes(lon, lat, group = route),
            colour = "grey40", linetype = "22", linewidth = 0.5) +
  geom_point(data = islands, aes(lon2, lat, fill = group),
             shape = 24, size = 2.6, colour = "grey20", stroke = 0.3) +
  geom_point(data = sites, aes(lon2, lat, colour = country), size = 3.6) +
  geom_text_repel(
    data = lab_df, aes(x, y, label = label),
    colour = lab_df$col, size = lab_df$size, fontface = lab_df$face,
    nudge_x = lab_df$nx, nudge_y = lab_df$ny,
    bg.colour = "white", bg.r = 0.12,          # halo so route lines don't cut text
    lineheight = 0.9, box.padding = 0.45, point.padding = 0.3,
    segment.colour = "grey55", segment.size = 0.25,
    min.segment.length = 0.2, seed = 7, max.overlaps = Inf
  ) +
  scale_colour_manual(values = col_country, name = NULL) +
  scale_fill_manual(values = c(
    "Compact of Free Association (US)" = "#762a83",
    "Pacific island country"           = "#35978f",
    "Territory / associated state"     = "#dfc27d"
  ), name = NULL) +
  guides(colour = guide_legend(order = 1, override.aes = list(size = 4)),
         fill   = guide_legend(order = 2, override.aes = list(size = 3.5))) +
  coord_quickmap(xlim = c(105, 295), ylim = c(-47, 62), expand = FALSE) +
  labs(
    title    = "US-Australia: Distances Across the Pacific",
    subtitle = "",
    x = NULL, y = NULL,
    caption  = ""
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title      = element_text(face = "bold", size = 22),
    plot.subtitle   = element_text(size = 13),
    legend.position = "bottom",
    legend.box      = "horizontal",
    legend.text     = element_text(size = 11),
    panel.grid      = element_line(colour = "grey92"),
    axis.text       = element_blank(),   # 0–360 longitudes read oddly on a dashboard
    plot.caption    = element_text(colour = "grey40", size = 9)
  )

print(p)

ggsave("US-Australia distance map.png", p,
       width = 14, height = 7.5, dpi = 600, bg = "white")







