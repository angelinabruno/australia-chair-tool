# ==============================================================================
# Two charts banked from '1c. Data centres.R' (October 2026): buildings capex
# estimates for all industries, and real building and structures capex. Neither
# is specific to data centres, so they were taken off the Data centres page.
# Not run by the refresh. To redraw, run '1c. Data centres.R' up to section 9,
# then this file from the Data centres folder.
# ==============================================================================

make_p5 <- function(cur) plot_estimate_paths(
  "Total", BS,
  "Buildings and structures capex: estimates and outturn by financial year",
  "All industries; each line is one financial year, latest in navy", cur = cur)
save_both(make_p5, "05_actual_vs_expected_bs_total.png", save_csis)

# 11. Real B&S capex (CVM, SA) --------------------------------------------

# kept separate from the current price charts. Real (constant-price) values
# are converted at one rate, the latest complete financial year's average,
# so growth rates are the same in both currencies.
real_fy   <- fy_start(Sys.Date())   # latest complete FY, named by the year it ends in
real_rate <- fx_financial_year(real_fy)

make_p9 <- function(cur) dat %>%
  filter(basis == "CVM_SA") %>%
  mutate(value = if (cur == "USD") value * real_rate else value) %>%
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
    y = y_lab(cur), x = NULL,
    caption  = paste(SRC, "Chain volume measures, seasonally adjusted.",
                     if (cur == "USD") sprintf("
Converted to US$ at the %s average rate.",
                                               fy_label(real_fy - 1L)) else "")
  ) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", hjust = 0,
                                  colour = csis_colours[["navy"]]))
save_both(make_p9, "09_real_momentum.png", save_csis, height = 7)

# Takeaways (switched off on the dashboard)
  b <- estimates_for("AUD") %>% filter(industry == "Total", asset == BS, est == latest_est$est) %>%
    arrange(ref_fy)
  takeaway(dc("05_actual_vs_expected_bs_total.png"), sprintf(
    "Estimate %d for all-industry building spending in %s is %.0f%% above the same estimate a year earlier.",
    latest_est$est, fy_label(max(b$ref_fy)), 100 * (b$value[nrow(b)] / b$value[nrow(b) - 1] - 1)))

  r9 <- dat %>% filter(basis == "CVM_SA", industry == IMT) %>% arrange(date)
  g9 <- r9$value[nrow(r9)] / r9$value[nrow(r9) - 4] - 1
  takeaway(dc("09_real_momentum.png"), sprintf(
    "After removing price changes, the industry's building spending was %.0f%% higher in the %s quarter than a year earlier.",
    100 * g9, format(max(r9$date), "%B %Y")))
