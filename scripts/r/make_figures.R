################################################################################
##
## [ PROJ ] Low-income enrollment across states with MRP
## [ FILE ] make_figures.R
## [ AUTH ] Benjamin Skinner (@btskinner)
## [ INIT ] 15 January 2021
##
################################################################################

## libraries
libs <- c("tidyverse", "crosswalkr", "patchwork")
sapply(libs, require, character.only = TRUE)

## file paths
dat_dir <- file.path("..", "..", "data")
est_dir <- file.path("..", "..", "estimates")
cln_dir <- file.path(dat_dir, "clean")
fig_dir <- file.path("..", "..", "figures")

## settings for figure consistency
plot_169_w <- 7.5
plot_169_h <- plot_169_w * 9/16
plot_pg_h <- 10
plot_pg_w <- 7.5

## -----------------------------------------------------------------------------
## read in data
## -----------------------------------------------------------------------------

## MRP estimates:
## n := national
## s := state
## q := quantile
## d := difference
## l := low income
## w := weights
## e := enrollment estimate
## i := ipeds
df_nq <- readRDS(file.path(cln_dir, "ps_national.RDS"))
df_nd <- readRDS(file.path(cln_dir, "ps_national_diff.RDS"))
df_sq <- readRDS(file.path(cln_dir, "ps_state.RDS"))
df_sl <- readRDS(file.path(cln_dir, "ps_state_lo.RDS"))
df_ne <- readRDS(file.path(cln_dir, "acs_2013_enroll.RDS"))
df_ip <- readRDS(file.path(cln_dir, "ipeds_1314_lowinc.RDS"))
df_sw <- readRDS(file.path(cln_dir, "state_estimates_w_error.RDS"))
df_nw <- readRDS(file.path(cln_dir, "national_estimates_w_error.RDS"))

## -----------------------------------------------------------------------------
## validation: national
## -----------------------------------------------------------------------------

## make plot data by combining MRP with national estimates
plot_df <- df_nq |>
  mutate(v = v * 100,
         est = "MRP") |>
  pivot_wider(names_from = "q",
              names_prefix = "q",
              values_from = "v") |>
  select(lowinc, est, starts_with("q")) |>
  bind_rows(df_nw |>
            mutate(est = ps_ontatt * 100,
                   se = se * 100,
                   q50 = est,
                   q2.5 = est - qnorm(0.975) * se,
                   q97.5 = est + qnorm(0.975) * se,
                   est = "HSLS weights (W4W1STU)") |>
            select(lowinc, est, starts_with("q"))
            ) |>
  mutate(lowinc = factor(lowinc,
                         levels = 1:0,
                         labels = c("Low income",
                                    "Mid-High income")))

## points with error lines
p1 <- ggplot(plot_df, aes(x = lowinc, y = q50, colour = est)) +
  geom_linerange(aes(xmin = lowinc, xmax = lowinc,
                     ymin = q2.5, ymax = q97.5),
                 position = position_dodge(width = 1),
                 size = .5) +
  geom_point(aes(group = est),
             colour = "white",
             position = position_dodge(width = 1),
             size = 2) +
  geom_point(position = position_dodge(width = 1),
             size = 1) +
  geom_vline(xintercept = seq(1.5, plot_df |>
                                     distinct(lowinc) |>
                                     pull() |>
                                     length(),
                              by = 1),
             colour = "gray",
             size = .5,
             alpha = .5) +
  scale_y_continuous(breaks = seq(45,80,5),
                     minor_breaks = seq(45,80,1)) +
  scale_colour_discrete(guide = "none") +
  labs(title = "National",
       y = "Enrollment (%)",
       x = NULL) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank())

## -----------------------------------------------------------------------------
## validation: states
## -----------------------------------------------------------------------------

## --------------------------------
## validation: 10 states
## --------------------------------

## convert estimates / s.e.s using weights into confidence intervals
df_sw_ <- df_sw |>
  filter(w == "w4w1stu",
         y == "ps_ontatt") |>
  mutate(est = est * 100,
         se = se * 100,
         q50 = est,
         q2.5 = est - qnorm(0.975) * se,
         q97.5 = est + qnorm(0.975) * se,
         est = "HSLS weights (W4W1STU)") |>
  left_join(stcrosswalk |> select(stfips, stname),
            by = c("x1state" = "stfips")) |>
  select(stname, lowinc, starts_with("q"), est)

## states to keep from MRP estimates
keep_states <- c(6,12,13,26,37,39,42,47,48,53)

## subset MRP estimates and set up
df_sq10 <- df_sq |>
  filter(fips %in% keep_states) |>
  mutate(v = v * 100,
         est = "MRP") |>
  pivot_wider(names_from = "q",
              names_prefix = "q",
              values_from = "v") |>
  select(stname, lowinc, q2.5, q50, q97.5, est)

## bind
plot_df <- bind_rows(df_sw_, df_sq10) |>
  mutate(lowinc = factor(lowinc,
                         levels = 1:0,
                         labels = c("Low income",
                                    "Mid-High income")))

## make plot
p2 <- ggplot(plot_df,
            aes(x = stname, y = q50, colour = factor(est))) +
  facet_wrap(~lowinc) +
  geom_linerange(aes(xmin = stname,
                     xmax = stname,
                     ymin = q2.5,
                     ymax = q97.5),
                 position = position_dodge(width = 1),
                 size = .5) +
  geom_point(aes(x = stname,
                 y = q50,
                 group = factor(est)),
             colour = "white",
             position = position_dodge(width = 1),
             size = 2) +
  geom_point(aes(x = stname,
                 y = q50,
                 colour = factor(est)),
             position = position_dodge(width = 1),
             size = 1) +
  geom_vline(xintercept = seq(1.5, plot_df |>
                                     distinct(stname) |>
                                     pull() |>
                                     length(),
                              by = 1),
             colour = "gray",
             size = .5,
             alpha = .5) +
  scale_colour_discrete(name = NULL) +
  labs(title = "Representative states",
       y = "Enrollment (%)",
       x = "State") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        panel.grid.major.x = element_blank())

## combine plots to one page
p <- p1 / p2

## save
ggsave(filename = file.path(fig_dir, "validation.pdf"),
       plot = p,
       width = 7.5,
       height = 7.5,
       units = "in",
       dpi = "retina")

## -----------------------------------------------------------------------------
## state comparison: coef lines
## -----------------------------------------------------------------------------

## get order by lowest lowinc enrollment for sorting
df_sq_lo <- df_sq |>
  filter(q == "50", lowinc == 1) |>
  select(st, lowinc, v) |>
  arrange(v) |>
  mutate(st_order = row_number()) |>
  select(st, st_order)

## make plot data
plot_df <- df_sq |>
  mutate(v = v * 100) |>
  pivot_wider(names_from = "q",
              names_prefix = "q",
              values_from = "v") |>
  left_join(crosswalkr::stcrosswalk |> select(stname, stabbr),
            by = "stname") |>
  left_join(df_sq_lo, by = "st") |>
  arrange(st_order, lowinc) |>
  mutate(lowinc = factor(lowinc,
                         levels = 1:0,
                         labels = c("Low income",
                                    "Mid-High income")),
         st_order = factor(st_order, st_order, stabbr))

## points with error lines
p <- ggplot(plot_df, aes(x = st_order, y = q50, colour = lowinc)) +
  geom_linerange(aes(xmin = st_order, xmax = st_order,
                     ymin = q2.5, ymax = q97.5,
                     colour = lowinc),
                 position = position_dodge(width = 1)) +
  geom_point(aes(group = lowinc),
             colour = "white",
             position = position_dodge(width = 1),
             size = 2) +
  geom_point(position = position_dodge(width = 1), size = .9) +
  geom_vline(xintercept = seq(1.5, plot_df |>
                                     distinct(st_order) |>
                                     pull() |>
                                     length(),
                              by = 1),
             colour = "gray",
             size = .5,
             alpha = .5) +
  scale_y_continuous(breaks = seq(0,100,10),
                     minor_breaks = seq(0,100,5)) +
  scale_colour_discrete(name = NULL) +
  labs(title = NULL,
       y = "Enrollment (%)",
       x = "State") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5),
        panel.grid.major.x = element_blank())

ggsave(filename = file.path(fig_dir,
                            "state_coef_lines.pdf"),
       plot = p,
       width = plot_169_w,
       height = plot_169_h,
       units = "in",
       dpi = "retina")

## -----------------------------------------------------------------------------
## comparison: ipeds
## -----------------------------------------------------------------------------

## set up ACS
df_acs <- df_ne |>
  filter(lo == 1) |>
  rename(pct_enroll_acs = enroll_pct) |>
  left_join(stcrosswalk |> select(stfips, stabbr),
            by = c("fips" = "stfips")) |>
  select(stabbr, pct_enroll_acs)

## set up IPEDS
df_ipeds <- df_ip |>
  rename(pct_lo30 = pct_lo_mean) |>
  select(stabbr, pct_pell, pct_lo30)

## set up MRP data
df_lowinc <- df_sl |>
  mutate(mrp = v * 100) |>
  left_join(stcrosswalk |> select(stabbr, stname), by = "stname") |>
  select(stabbr, mrp)

## combine to make plot data
## plot_df <- df_lowinc |>
##   left_join(df_acs, by = "stabbr") |>
##   left_join(df_ipeds, by = "stabbr") |>
##   mutate(diff_acs = pct_enroll_acs - mrp,
##          diff_pell = pct_pell - mrp,
##          diff_lo30 = pct_lo30 - mrp) |>
##   select(stabbr, starts_with("diff")) |>
##   pivot_longer(cols = starts_with("diff"),
##                names_to = "type",
##                values_to = "val") |>
##   arrange(stabbr, type) |>
##   group_by(stabbr, type) |>
##   summarise(q50 = quantile(val, .5),
##             q2.5 = quantile(val, 0.025),
##             q97.5 = quantile(val, 0.975),
##             .groups = "drop")

plot_df <- df_lowinc |>
  left_join(df_acs, by = "stabbr") |>
  mutate(diff_acs = pct_enroll_acs - mrp) |>
  select(stabbr, starts_with("diff")) |>
  pivot_longer(cols = starts_with("diff"),
               names_to = "type",
               values_to = "val") |>
  arrange(stabbr, type) |>
  group_by(stabbr, type) |>
  summarise(q50 = quantile(val, .5),
            q2.5 = quantile(val, 0.025),
            q97.5 = quantile(val, 0.975),
            .groups = "drop")

## make plot: ACS
p1 <- ggplot(plot_df |>
             arrange(q50) |>
             mutate(x = row_number()),
             aes(x = factor(x, levels = 1:51, labels = stabbr),
                 y = q50)) +
  geom_linerange(aes(ymin = q2.5,
                     ymax = q97.5)) +
  geom_point(colour = "white", size = 2) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_y_continuous(breaks = seq(-20,70,10),
                     minor_breaks  = seq(-20,70,5)) +
  labs(title = NULL,
       y = expression("%"~ACS - MRP~"(p.p.)"),
       x = NULL) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, vjust = 0.5))

## plot data for bottom two figures using IPEDS data
plot_df <- df_lowinc |>
  group_by(stabbr) |>
  summarise(q50 = quantile(mrp, .5),
            q2.5 = quantile(mrp, 0.025),
            q97.5 = quantile(mrp, 0.975),
            .groups = "drop") |>
  left_join(df_ipeds, by = "stabbr")

## make plot: Pell
p2 <- ggplot(plot_df, aes(x = pct_pell, y = q50)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_linerange(aes(ymin = q2.5,
                     ymax = q97.5)) +
  geom_point(colour = "white", size = 2, show.legend = FALSE) +
  geom_text(aes(label = stabbr), size = 2, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0,60,10),
                     minor_breaks = seq(0,60,5),
                     limits = c(20,60)) +
  scale_y_continuous(breaks = seq(0,80,10),
                     minor_breaks = seq(0,80,5),
                     limits = c(20,80)) +
  labs(title = NULL,
       y = "MRP",
       x = "% Pell") +
  theme_bw() +
  theme(axis.text = element_text(size = 8))

## make plot: < $30k population
p3 <- ggplot(plot_df, aes(x = pct_lo30, y = q50)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_linerange(aes(ymin = q2.5,
                     ymax = q97.5)) +
  geom_point(colour = "white", size = 2, show.legend = FALSE) +
  geom_text(aes(label = stabbr), size = 2, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(0,80,10),
                     minor_breaks = seq(0,80,5),
                     limits = c(10,80)) +
  scale_y_continuous(breaks = seq(0,80,10),
                     minor_breaks = seq(0,80,5),
                     limits = c(20,80)) +
  labs(title = NULL,
       y = "MRP",
       x = "% FinAid cohort < $30k") +
  theme_bw() +
  theme(axis.text = element_text(size = 8))

## patchwork together
p <- p1 / (p2 | p3) +
  plot_annotation(tag_levels = "A")

## save
ggsave(filename = file.path(fig_dir,
                            "state_acs_ipeds_comparison.pdf"),
       plot = p,
       width = plot_pg_w,
       height = plot_pg_h * 0.75,
       units = "in",
       dpi = "retina")

## =============================================================================
## END SCRIPT
################################################################################
