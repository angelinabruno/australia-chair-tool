# Draft "booms and busts" charts, banked on 22 October 2026 and not on the dashboard.
# Taken from "1b. Economy.R" (section 2). To use one, paste its block back into that
# script after the gdp_au_us.png chart (it needs `gdp`, theme_csis(), au_us_colours
# and save_plot() from there; the second chart also needs the zoo package), then
# add the PNG to the .qmd.

# Booms and busts: how much through-the-year growth swings in each decade.
# The standard deviation is the typical distance of a quarter's growth rate
# from that decade's average, so a taller bar means bigger booms and busts.
vol <- gdp |>
  mutate(decade = floor(year(date) / 10) * 10) |>
  filter(decade >= 1960) |>
  group_by(country, decade) |>
  summarise(sd = sd(yoy), quarters = n(), .groups = "drop") |>
  mutate(decade_lab = ifelse(decade == max(decade),
                             paste0(decade, "s\n(to ", format(max(gdp$date), "%Y"), ")"),
                             paste0(decade, "s")))
write.csv(vol, "gdp_volatility_au_us.csv", row.names = FALSE)

p_vol <- ggplot(vol, aes(decade_lab, sd, fill = country)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f", sd)), position = position_dodge(width = 0.75),
            vjust = -0.4, size = 3, family = "source_sans", colour = csis_colours[["grey"]]) +
  scale_fill_manual(values = au_us_colours) +
  scale_y_continuous(labels = label_number(accuracy = 1, suffix = " pts"),
                     expand = expansion(c(0, 0.1))) +
  labs(title = "How much real GDP growth swings, Australia and the United States",
       subtitle = paste0("Standard deviation of through-the-year real GDP growth in each decade, ",
                         "percentage points.
Taller bars mean bigger booms and busts."),
       x = NULL, y = NULL,
       caption = paste("Sources: ABS, Australian National Accounts (cat. no. 5206.0);",
                       "US BEA, National Income and Product Accounts, Table 1.1.6.")) +
  theme_csis() +
  theme(panel.grid.major.x = element_blank())
save_plot(p_vol, "gdp_volatility_au_us.png", width = 8, height = 4.8)

# The same idea without statistics: in each rolling ten-year window, the gap
# between the strongest and the weakest through-the-year growth rate. A wider
# gap means bigger booms and busts.
SWING_YEARS <- 10
swing <- gdp |>
  arrange(country, date) |>
  group_by(country) |>
  mutate(hi  = zoo::rollmax(yoy, SWING_YEARS * 4, fill = NA, align = "right"),
         lo  = -zoo::rollmax(-yoy, SWING_YEARS * 4, fill = NA, align = "right"),
         gap = hi - lo) |>
  ungroup() |>
  filter(!is.na(gap), date >= as.Date("1975-01-01"))
write.csv(swing |> select(country, date, hi, lo, gap), "gdp_swings_au_us.csv", row.names = FALSE)

swing_end <- swing |> group_by(country) |> slice_max(date, n = 1) |> ungroup()

p_swing <- ggplot(swing, aes(date, gap, colour = country)) +
  geom_line(linewidth = 0.9) +
  geom_text(data = swing_end, aes(label = sprintf("%.0f pts", gap)), hjust = -0.15,
            size = 3.2, family = "source_sans", show.legend = FALSE) +
  scale_colour_manual(values = au_us_colours) +
  scale_y_continuous(labels = label_number(accuracy = 1, suffix = " pts"),
                     limits = c(0, NA), expand = expansion(c(0, 0.05))) +
  scale_x_date(breaks = seq(as.Date("1975-01-01"), Sys.Date(), by = "5 years"),
               date_labels = "%Y", expand = expansion(mult = c(0.01, 0.07))) +
  labs(title = "Booms and busts: the gap between the best and worst years of growth",
       subtitle = paste0("Highest minus lowest through-the-year real GDP growth over the previous ",
                         SWING_YEARS, " years, percentage points.\nA higher line means bigger booms and busts."),
       x = NULL, y = NULL,
       caption = paste("Sources: ABS, Australian National Accounts (cat. no. 5206.0);",
                       "US BEA, National Income and Product Accounts, Table 1.1.6.")) +
  theme_csis()
save_plot(p_swing, "gdp_swings_au_us.png", width = 8, height = 4.8)
