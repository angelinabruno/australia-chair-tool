# ==============================================================================
# Services trade balance chart, banked from '1f. Trade.R' (October 2026)
# Removed from the Trade overview page. Not run by the refresh. To redraw, run
# '1f. Trade.R' up to its style section (theme_csis, csis_colours, save_csis,
# ABS_SOURCE, the services data and the exchange-rate helpers), then this file
# from the Trade folder.
# ==============================================================================

# ---------------------------------------------------------------
# 3. Chart — services balance
# ---------------------------------------------------------------
# Columns rather than a line: this is a signed balance, and the sign
# flip in 2020 is the whole story. Fill encodes direction, so no
# legend is needed once the axis label states the convention.

# Drawn twice, in A$ and US$ (each calendar year at its average rate).
make_services <- function(cur) {
services_plot <- services |>
  mutate(value = if (cur == "USD") aud_to_usd(value, fx_calendar_year(year)) else value,
         direction = if_else(value >= 0, "Surplus", "Deficit"))

ggplot(services_plot, aes(year, value, fill = direction)) +
  geom_col(width = 0.72) +
  geom_hline(yintercept = 0, colour = csis_colours[["grey"]], linewidth = 0.5) +
  annotate(
    "text",
    x = 2020, y = services_plot$value[services_plot$year == 2020] + 4,
    label = "Surplus in 2020",
    family = "source_sans", size = 3, lineheight = 0.95,
    colour = csis_colours[["teal"]], fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(Deficit = unname(csis_colours["red"]),
               Surplus = unname(csis_colours["teal"]))
  ) +
  scale_x_continuous(
    breaks = seq(2005, 2025, by = 5),
    minor_breaks = NULL,
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    labels = label_dollar(prefix = cur_sym[[cur]], suffix = "b", accuracy = 1),
    expand = expansion(mult = c(0.10, 0.14))
  ) +
  labs(
    title    = "Australia's balance on trade in services",
    subtitle = "Exports less imports, 2005-2025",
    caption  = paste0(
      ABS_SOURCE,
      "\nNote: a negative balance indicates imports exceed exports.",
      if (cur == "USD") fx_note[["USD"]] else ""
    ),
    x = NULL, y = paste0("Balance (", cur_sym[[cur]], " billion)")
  ) +
  theme_csis() +
  theme(
    legend.position    = "none",
    panel.grid.major.y = element_line(colour = "#E6E7E8", linewidth = 0.4)
  )
}



save_both(make_services, "abs_services_balance.png", save_csis, height = 4.4)
