# ==============================================================================
# Goods imports by sector chart, banked from '1f. Trade.R' (October 2026)
# Removed from the Trade overview page. Not run by the refresh. To redraw, run
# '1f. Trade.R' up to and including the sector_shares line in section 5, then
# this file from the Trade folder.
# ==============================================================================

# 5. Chart — import sector shares
# ---------------------------------------------------------------
# Mirrors the export sector chart. Note the y-axis cannot be shared
# between the two: imports are ~76% manufactures against ~13% for
# exports, so a common scale would flatten the export series to
# illegibility. Keep them as separate panels, not a faceted pair.

imp_sectors <- sector_shares |> filter(trade_type == "Total Imports")

recon_imp <- imp_sectors |>
  group_by(year) |>
  summarise(share_sum = sum(share), .groups = "drop")
if (any(abs(recon_imp$share_sum - 1) > 1e-6)) {
  warning("Import sector shares do not sum to 1.")
} else {
  message("Import sector shares reconcile.")
}

lvls_s <- imp_sectors |>
  filter(year == max(year)) |>
  arrange(desc(share)) |>
  pull(sector)

imp_plot <- imp_sectors |> mutate(sector = factor(sector, levels = lvls_s))
ends_s   <- imp_plot |> filter(year == max(year))

p_imports_sector <- ggplot(imp_plot, aes(year, share, colour = sector, group = sector)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.4) +
  geom_text_repel(
    data = ends_s,
    aes(label = str_wrap(sector, 16)),
    family = "source_sans", size = 3.2, fontface = "bold", lineheight = 0.9,
    hjust = 0, direction = "y", nudge_x = 0.4,
    segment.size = 0.3, segment.alpha = 0.5,
    min.segment.length = 0, seed = 42
  ) +
  scale_colour_csis() +
  scale_x_continuous(breaks = seq(2006, 2025, by = 2),
                     expand = expansion(mult = c(0.02, 0.20))) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
  labs(
    #title    = "Australia imports mostly manufactured goods",
    title = "Share of total goods imports",
    subtitle = "By broad sector, 2006-2025",
    caption  = paste0(
      DFAT_SRC,
      "\nSectors are DFAT's four-way aggregation of SITC Rev. 4 3-digit groups."
    ),
    x = NULL, y = "Share of total goods imports"
  ) +
  theme_csis() +
  theme(legend.position = "none")

ggsave("dfat_import_shares_by_sector.png", p_imports_sector,
       width = 8.5, height = 5, dpi = 150, bg = "white")

