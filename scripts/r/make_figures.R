################################################################################
##
## [ PROJ ] Low-income enrollment across states with MRP
## [ FILE ] make_figures.R
## [ AUTH ] Benjamin Skinner (@btskinner)
## [ INIT ] 15 January 2021
##
################################################################################

## libraries
libs <- c("tidyverse", "crosswalkr", "patchwork", "rstan", "tidybayes", "haven", "Cairo")
sapply(libs, require, character.only = TRUE)

## file paths
dat_dir <- file.path("..", "..", "data")
est_dir <- file.path("..", "..", "estimates")
cln_dir <- file.path(dat_dir, "clean")
fig_dir <- file.path("..", "..", "figures")

## read in macros
source("macros.R")

## settings for figure consistency
plot_169_w <- 7.5
plot_169_h <- plot_169_w * 9/16
plot_pg_h <- 10
plot_pg_w <- 7.5

## -----------------------------------------------------------------------------
## read in data
## -----------------------------------------------------------------------------

## MRP estimates:
## b := bayes
## n := national
## s := state
## q := quantile
## d := difference
## p := poststrat full distribution
## w := weights
## e := enrollment estimate
## i := ipeds
df_nq <- readRDS(file.path(cln_dir, "ps_national.RDS"))
df_nd <- readRDS(file.path(cln_dir, "ps_national_diff.RDS"))
df_np <- readRDS(file.path(cln_dir, "ps_national_post.RDS"))
df_sq <- readRDS(file.path(cln_dir, "ps_state.RDS"))
df_sd <- readRDS(file.path(cln_dir, "ps_state_diff.RDS"))
df_sp <- readRDS(file.path(cln_dir, "ps_state_post.RDS"))
df_ne <- readRDS(file.path(cln_dir, "acs_2013_enroll.RDS"))
df_ip <- readRDS(file.path(cln_dir, "ipeds_1314_lowinc.RDS"))
df_sb <- readRDS(file.path(cln_dir, "state_estimates_bayes_weights.RDS"))
df_sw <- readRDS(file.path(cln_dir, "state_estimates_w_error.RDS"))
df_nw <- readRDS(file.path(cln_dir, "national_estimates_w_error.RDS"))

## population counts
pop <- readRDS(file.path(cln_dir, "acs_lo_poststrat.RDS"))

## get estimates
fit <- read_stan_csv(list.files(est_dir, "hsl_ps", full.names = TRUE)) |>
  tidy_draws()

## get public use data
df_pu <- readRDS(file.path(cln_dir, "pu_hsl.RDS"))

## -----------------------------------------------------------------------------
## estimates: coef plots
## -----------------------------------------------------------------------------

## first level stats
plot_df <- fit |>
  select(starts_with("b_"), starts_with("a_ra")) |>
  pivot_longer(cols = everything(),
               names_to = "coef",
               values_to = "est") |>
  mutate(coef = add_param_labels(coef),
         coef = fl_params_to_factor(coef))

p1 <- ggplot(plot_df, aes(x = est, y = fct_rev(coef))) +
  stat_pointinterval(.width = c(.5, .95), size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-2,2,1),
                     minor_breaks = seq(-2,2,0.5),
                     limits = c(-2,2)) +
  labs(x = expression(alpha*","~beta),
       y = NULL) +
  theme_bw()

## second level parameters
plot_df <- fit |>
  select(starts_with("g")) |>
  pivot_longer(cols = everything(),
               names_to = "coef",
               values_to = "est") |>
  mutate(coef = add_param_labels(coef),
         coef = sl_params_to_factor(coef))

p2 <- ggplot(plot_df, aes(x = est, y = fct_rev(coef))) +
  stat_pointinterval(.width = c(.5, .95), size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-1,1,1),
                     minor_breaks = seq(-1,1,0.5),
                     limits = c(-1,1)) +
  labs(x = expression(gamma),
       y = NULL) +
  theme_bw()

## states
plot_df <- fit |>
  select("a", starts_with("a_rg"), starts_with("a_st")) |>
  pivot_longer(cols = everything(),
               names_to = "coef",
               values_to = "est") |>
  mutate(coef = add_param_labels(coef),
         coef = st_params_to_factor(coef))

p3 <- ggplot(plot_df, aes(x = est, y = fct_rev(coef))) +
  stat_pointinterval(.width = c(.5, .95), size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(-2,2,1),
                     minor_breaks = seq(-2,2,0.5),
                     limits = c(-2,2)) +
  labs(x = expression(alpha*","~alpha[region]*","~alpha[state]*","~alpha[stata.lowinc]),
       y = NULL) +
  theme_bw() +
  theme(axis.text.y = element_text(size = 7))

## put together
p <- (p1 / p2) | p3

## save
ggsave(filename = file.path(fig_dir, paste0("regression_parameters.pdf")),
       plot = p,
       width = 7.5,
       height = 10,
       units = "in",
       dpi = "retina")

for (samp in c(1,3,5)) {

  ## ---------------------------------------------------------------------------
  ## validation: coef lines states
  ## ---------------------------------------------------------------------------

  ## --------------------------------
  ## validation: 10 states
  ## --------------------------------

  ## get weighted estimates
  df_sb_ <- df_sb |>
    ungroup() |>
    mutate(v = .epred * 100,
           est = case_when(
             w == "w1student_norm" ~ "Base year weights (W1STUDENT)",
             w == "w4w1stu_norm" ~ "Longitudinal weights (W4W1STU)")
           ) |>
    left_join(stcrosswalk |> select(stfips, stname),
              by = c("x1state" = "stfips")) |>
    select(stname, lowinc, v, est)

  ## states to keep from MRP estimates
  keep_states <- c(6,12,13,26,37,39,42,47,48,53)

  ## subset MRP estimates and set up
  df_sp10 <- df_sp |>
    filter(sample == samp,
           fips %in% keep_states) |>
    mutate(v = v * 100,
           est = "MRP") |>
    select(stname, lowinc, v, est)

  ## bind
  df_st <- bind_rows(df_sb_, df_sp10) |>
    mutate(lowinc = factor(lowinc,
                           levels = 1:0,
                           labels = c("Low income",
                                      "Mid-high income")))

  ## loop through states, income type, and estimates
  df_ov_list <- list()
  df_50_list <- list()
  for (i in df_st |> distinct(stname) |> pull(stname)) {
    for (j in df_st |> distinct(lowinc) |> pull(lowinc)) {
      for (k in df_st |> filter(est != "MRP") |> distinct(est) |> pull(est)) {
        ## name
        nm <- paste(i, j, k, sep = "_")
        ## vectors
        mrp <- df_st |>
          filter(stname == i,
                 lowinc == j,
                 est == "MRP") |>
          pull(v)
        w <- df_st |>
          filter(stname == i,
                 lowinc == j,
                 est == k) |>
          pull(v)
        ## overlap
        df_ov_list[nm] <- overlapping::overlap(
          list(x1 = mrp,
               x2 = w))
        ## distance between medians
        df_50_list[nm] <- round(abs(median(mrp - w)), 2)
      }
    }
  }

  ## convert lists to tibble and join
  df_ov <- stack(df_ov_list) |>
    as_tibble() |>
    separate(ind, c("stname", "lowinc", "est"), "_") |>
    rename(ov = values) |>
    mutate(ov = round(ov, 2))
  df_50 <- stack(df_50_list) |>
    as_tibble() |>
    separate(ind, c("stname", "lowinc", "est"), "_") |>
    rename(diff = values)
  df_ov50 <- df_ov |>
    left_join(df_50, by = c("stname", "lowinc", "est")) |>
    mutate(label = case_when(
      grepl("Base",est) ~ paste0("Base: OVL = ",
                                 sprintf(fmt = "%#.2f", ov),
                                 "; |\u0394\u03b8| = ",
                                 sprintf(fmt = "%#.2f", diff)),
      grepl("Long",est) ~ paste0("Long: OVL = ",
                                 sprintf(fmt = "%#.2f", ov),
                                 "; |\u0394\u03b8| = ",
                                 sprintf(fmt = "%#.2f", diff))))

  ## save data for table
  saveRDS(df_ov50 |>
            select(stname, lowinc, est, ov, diff) |>
            arrange(stname, lowinc, est),
          file.path(cln_dir, paste0("df_ov50_", samp, ".RDS")))

  ## low income estimates
  p2a <- ggplot(df_st |> filter(lowinc == "Low income"),
               aes(x = v, colour = factor(est), linetype = factor(est))) +
    facet_wrap(~stname, ncol = 5) +
    geom_density() +
    scale_colour_discrete(name = "") +
    scale_linetype_manual(name = "", values = c("dashed", "dotted", "solid")) +
    geom_text(data = df_ov50 |> filter(!grepl("W4", est), grepl("Low", lowinc)),
              aes(x = 20, y = .39, label = label),
              colour = "black",
              hjust = 0, size = 2) +
    geom_text(data = df_ov50 |> filter(grepl("W4", est), grepl("Low", lowinc)),
              aes(x = 20, y = .35, label = label),
              colour = "black",
              hjust = 0, size = 2) +
    scale_x_continuous(limits = c(20,70)) +
    scale_y_continuous(limits = c(0,.4)) +
    labs(title = "Low income",
         x = quote(widehat(theta)[state]),
         y = NULL) +
    theme_bw() +
    theme(legend.position = "bottom",
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          strip.background = element_blank())

  ## mid-high income estimates
  p2b <- ggplot(df_st |> filter(lowinc == "Mid-high income"),
               aes(x = v, colour = factor(est), linetype = factor(est))) +
    facet_wrap(~stname, ncol = 5) +
    geom_density() +
    scale_colour_discrete(name = "") +
    scale_linetype_manual(name = "", values = c("dashed", "dotted", "solid")) +
    geom_text(data = df_ov50 |> filter(!grepl("W4", est), grepl("Mid", lowinc)),
              aes(x = 40, y = .39, label = label),
              colour = "black",
              hjust = 0, size = 2) +
    geom_text(data = df_ov50 |> filter(grepl("W4", est), grepl("Mid", lowinc)),
              aes(x = 40, y = .35, label = label),
              colour = "black",
              hjust = 0, size = 2) +
    scale_x_continuous(limits = c(40,90)) +
    scale_y_continuous(limits = c(0,.4)) +
    labs(title = "Middle-high income",
         x = quote(widehat(theta)[state]),,
         y = NULL) +
    theme_bw() +
    theme(legend.position = "bottom",
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          strip.background = element_blank())

  ## patchwork together
  p2 <- p2a / p2b +
    plot_layout(guides = "collect") &
    theme(legend.position = "bottom")

  ## save
  ggsave(filename = file.path(fig_dir, paste0("coef_validation_bayes_", samp, ".pdf")),
         plot = p2,
         width = 6.5,
         height = 7.5,
         units = "in",
         dpi = "retina",
         device = cairo_pdf)

  ## ---------------------------------------------------------------------------
  ## state comparison: coef lines (diff)
  ## ---------------------------------------------------------------------------

  ## get order by lowest lowinc enrollment for sorting
  df_sd_q <- df_sd |>
    filter(sample == samp) |>
    group_by(st) |>
    summarise(q025 = quantile(v, .025),
              q500 = quantile(v, .500),
              q975 = quantile(v, .975)) |>
    pivot_longer(cols = starts_with("q"),
                 names_to = "q",
                 values_to = "v") |>
    mutate(v = v * 100)

  df_sd_q_lo <- df_sd_q |>
    filter(q == "q500") |>
    select(st, v) |>
    arrange(v) |>
    mutate(st_order = row_number()) |>
    select(st, st_order)

  plot_df <- df_sd_q |>
    left_join(df_sd |> distinct(st, stname),
              by = "st") |>
    left_join(df_sd_q_lo, by = "st") |>
    pivot_wider(names_from = "q",
                values_from = "v") |>
    left_join(stcrosswalk |> select(stname, stabbr),
              by = "stname") |>
    mutate(stabbr = ifelse(is.na(stabbr), "DC", stabbr)) |>
    arrange(st_order) |>
    mutate(st_order = factor(st_order, st_order, stabbr))

  ## points with error lines
  p <- ggplot(plot_df, aes(x = st_order, y = q500)) +
    geom_linerange(aes(xmin = st_order, xmax = st_order,
                       ymin = q025, ymax = q975)) +
    geom_point(colour = "white",
               size = 2) +
    geom_point() +
    geom_vline(xintercept = seq(1.5, plot_df |>
                                       distinct(st_order) |>
                                       pull() |>
                                       length(),
                                by = 1),
               colour = "gray",
               linewidth = .5,
               alpha = .5) +
    scale_y_continuous(breaks = seq(0,100,5),
                       minor_breaks = seq(0,100,1)) +
    labs(title = NULL,
         y = expression(widehat(theta)[mid/high]~"-"~widehat(theta)[low]),
         x = "State") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5),
          panel.grid.major.x = element_blank())

  ggsave(filename = file.path(fig_dir,
                              paste0("state_diff_coef_lines_", samp, ".pdf")),
         plot = p,
         width = plot_169_w,
         height = plot_169_h,
         units = "in",
         dpi = "retina")

  ## save for patchwork below
  st_coef_diff <- p

  ## ---------------------------------------------------------------------------
  ## state comparison: coef lines (lo / hi)
  ## ---------------------------------------------------------------------------

  ## get order by lowest lowinc enrollment for sorting
  df_sq_lo <- df_sq |>
    filter(q == "50", lowinc == 1, sample == samp) |>
    select(st, lowinc, v) |>
    arrange(v) |>
    mutate(st_order = row_number()) |>
    select(st, st_order)

  ## make plot data
  plot_df <- df_sq |>
    filter(sample == samp) |>
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
                                      "Mid-high income")),
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
    geom_point(aes(shape = lowinc, colour = lowinc),
               position = position_dodge(width = 1), size = 1.2) +
    geom_vline(xintercept = seq(1.5, plot_df |>
                                       distinct(st_order) |>
                                       pull() |>
                                       length(),
                                by = 1),
               colour = "gray",
               linewidth = .5,
               alpha = .5) +
    scale_y_continuous(breaks = seq(0,100,10),
                       minor_breaks = seq(0,100,5)) +
    scale_colour_discrete(name = NULL) +
    scale_shape_discrete(name = NULL) +
    labs(title = NULL,
         y = expression(widehat(theta)),
         x = "State") +
    theme_bw() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5),
          panel.grid.major.x = element_blank())

  ggsave(filename = file.path(fig_dir,
                              paste0("state_coef_lines_", samp, ".pdf")),
         plot = p,
         width = plot_169_w,
         height = plot_169_h,
         units = "in",
         dpi = "retina")

  ## save for patchwork
  st_coef_hilo <- p

  ## -------------------------
  ## patch state coef
  ## -------------------------

  p <- st_coef_hilo / st_coef_diff  +
    plot_annotation(tag_levels = "A")

  ggsave(filename = file.path(fig_dir,
                              paste0("state_coef_patch_", samp, ".pdf")),
         plot = p,
         width = plot_pg_w,
         height = plot_pg_h * 0.75,
         units = "in",
         dpi = "retina")

  ## ---------------------------------------------------------------------------
  ## comparison: ipeds
  ## ---------------------------------------------------------------------------

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
  df_lowinc <- df_sp |>
    filter(sample == samp, lowinc == 1) |>
    mutate(mrp = v * 100) |>
    left_join(stcrosswalk |> select(stabbr, stname), by = "stname") |>
    select(stabbr, mrp)

  ## combine to make plot data
  plot_df <- df_lowinc |>
    group_by(stabbr) |>
    summarise(q50 = quantile(mrp, .5),
              q2.5 = quantile(mrp, 0.025),
              q97.5 = quantile(mrp, 0.975),
              .groups = "drop") |>
    left_join(df_acs, by = "stabbr") |>
    left_join(df_ipeds, by = "stabbr") |>
    mutate(acs_p_cor = cor(q50, pct_enroll_acs),
           acs_s_cor = cor(q50, pct_enroll_acs, method = "spearman"),
           pell_p_cor = cor(q50, pct_pell),
           pell_s_cor = cor(q50, pct_pell, method = "spearman"),
           lo30_p_cor = cor(q50, pct_lo30),
           lo30_s_cor = cor(q50, pct_lo30, method = "spearman"),
           across(ends_with("cor"), ~ sprintf("%#0.2f", round(.x, 2))))

  ## make plot: ACS
  p1 <- ggplot(plot_df, aes(x = pct_enroll_acs, y = q50)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    geom_linerange(aes(ymin = q2.5,
                       ymax = q97.5),
                   linewidth = 0.2) +
    geom_point(colour = "white", size = 1.5, show.legend = FALSE) +
    geom_text(aes(label = stabbr), size = 1.5, show.legend = FALSE) +
    scale_x_continuous(breaks = seq(0,100,10),
                       minor_breaks = seq(0,100,5),
                       limits = c(30,100)) +
    scale_y_continuous(breaks = seq(0,100,10),
                       minor_breaks = seq(0,100,5),
                       limits = c(20,80)) +
    labs(title = NULL,
         y = "MRP",
         x = "% ACS") +
    theme_bw() +
    theme(axis.text = element_text(size = 8))

  ## make plot: Pell
  p2 <- ggplot(plot_df, aes(x = pct_pell, y = q50)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    geom_linerange(aes(ymin = q2.5,
                       ymax = q97.5),
                   linewidth = 0.2) +
    geom_point(colour = "white", size = 1.5, show.legend = FALSE) +
    geom_text(aes(label = stabbr), size = 1.5, show.legend = FALSE) +
    scale_x_continuous(breaks = seq(0,100,10),
                       minor_breaks = seq(0,100,5),
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
                       ymax = q97.5),
                   linewidth = 0.2) +
    geom_point(colour = "white", size = 1.5, show.legend = FALSE) +
    geom_text(aes(label = stabbr), size = 1.5, show.legend = FALSE) +
    scale_x_continuous(breaks = seq(0,100,10),
                       minor_breaks = seq(0,100,5),
                       limits = c(10,80)) +
    scale_y_continuous(breaks = seq(0,80,10),
                       minor_breaks = seq(0,80,5),
                       limits = c(20,80)) +
    labs(title = NULL,
         y = "MRP",
         x = "% FinAid cohort < $30k") +
    theme_bw() +
    theme(axis.text = element_text(size = 8))

  ## grob plot for correlation
  p4 <- ggplot(plot_df |>
                 filter(row_number() == 1) |>
                 select(ends_with("cor")) |>
                 pivot_longer(cols = everything(),
                              names_to = "stat",
                              values_to = "label") |>
                 mutate(label = case_when(
                   grepl("acs_p",stat) ~ paste0("ACS: \u03c1 (Pearson) = ",
                                                label),
                   grepl("acs_s",stat) ~ paste0("ACS: \u03c1 (Spearman) = ",
                                                label),
                   grepl("pell_p",stat) ~ paste0("Pell: \u03c1 (Pearson) = ",
                                                 label),
                   grepl("pell_s",stat) ~ paste0("Pell: \u03c1 (Spearman) = ",
                                                 label),
                   grepl("lo30_p",stat) ~ paste0("FinAid: \u03c1 (Pearson) = ",
                                                 label),
                   grepl("lo30_s",stat) ~ paste0("FinAid: \u03c1 (Spearman) = ",
                                                 label))) |>
               mutate(x = 10, y = c(80,75,60,55,40,35))) +
    geom_text(aes(x = x, y = y, label = label),
              colour = "black",
              hjust = 0, size = 4) +
    geom_text(aes(x = 10, y = 90, label = c("Correlation with MRP estimate:")),
              hjust = 0) +
    scale_x_continuous(limits = c(10,50)) +
    scale_y_continuous(limits = c(15,90)) +
    theme_void()

  ## patchwork together
  p <- (p1 | p2) / (p3 | p4) +
    plot_annotation(tag_levels = list(c("A", "B", "C", "")))

  ## save
  ggsave(filename = file.path(fig_dir,
                              paste0("state_acs_ipeds_comparison_",
                                     samp,
                                     ".pdf")),
         plot = p,
         width = plot_pg_w,
         height = plot_pg_h * 0.75,
         units = "in",
         dpi = "retina",
         device = cairo_pdf)
}

## =============================================================================
## END SCRIPT
################################################################################
