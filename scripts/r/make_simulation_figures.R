################################################################################
##
## [ PROJ ] Low-income enrollment across states with MRP
## [ FILE ] make_simulation_figures.R
## [ AUTH ] Benjamin Skinner (@btskinner)
## [ INIT ] 20 January 2022
##
################################################################################

## libraries
libs <- c("tidyverse", "crosswalkr", "patchwork")
sapply(libs, require, character.only = TRUE)

## file paths
dat_dir <- file.path("..", "..", "data")
sim_dir <- file.path(dat_dir, "simulation")
fig_dir <- file.path("..", "..", "figures")

## function
std_err_mean <- function(x) sd(x) / sqrt(length(x))

## -----------------------------------------------------------------------------
## macros for figures
## -----------------------------------------------------------------------------

## settings for figure consistency
plot_169_w <- 16
plot_169_h <- 9
plot_sq_75 <- 7.5
plot_pg_h <- 9
plot_pg_w <- 7.5

## labels
samp_label <- c("srsl" = "Simple random sample (1%)",
                "srss" = "Simple random sample (0.1%)",
                "wrsl" = "Weighted random sample (1%)",
                "wrss" = "Weighted random sample (0.1%)")
tau_labeller <- as_labeller(c(n = "D == 0", y = "D == 1"),
                            default = label_parsed)

## -----------------------------------------------------------------------------
## read in data
## -----------------------------------------------------------------------------

## population
pop <- readRDS(file.path(sim_dir, "sim_population.RDS")) |>
  mutate(t = factor(t, levels = c(1,0)))

## sample data
df_sam <- grep("sim_w|sim_s",
               list.files(sim_dir, full.names = TRUE),
               value = TRUE) |>
  map(~ readRDS(.x) |>
        mutate(sam = gsub("^.+_([a-z]+)\\.RDS$", "\\1", .x),
               t = factor(t, levels = c(1,0)))) |>
  bind_rows()

## estimates
df_est <- grep("ps",
               list.files(sim_dir, full.names = TRUE),
               value = TRUE) |>
  map(~ readRDS(.x) |>
        mutate(sam = gsub("^.+_([a-z]+)\\.RDS$", "\\1", .x),
               lev = gsub("^.+sim_ps_([a-z]+)_.+\\.RDS$", "\\1", .x),
               t = factor(t, levels = c(1,0)))) |>
  bind_rows() |>
  filter(lev == "state")

## -----------------------------------------------------------------------------
## national population differences
## -----------------------------------------------------------------------------

## make figure data
plot_df <- pop |>
  mutate(y = y * 100) |>
  group_by(t) |>
  summarise(se = std_err_mean(y),
            y = mean(y),
            .groups = "drop") |>
  mutate(sam = "pop",
         weight = "n") |>
  bind_rows(df_sam |>
              mutate(y = y * 100) |>
              group_by(sam, t) |>
              summarise(se = std_err_mean(y),
                        y = mean(y),
                        .groups = "drop") |>
              mutate(weight = "n")) |>
  bind_rows(df_sam |>
              mutate(y = y * 100) |>
              filter(grepl("wrss|wrsl", sam)) |>
              group_by(sam, t) |>
              summarise(se = sqrt(Hmisc::wtd.var(y, 1/w, normwt = TRUE)) / sqrt(n()),
                        y = Hmisc::wtd.mean(y, 1/w, normwt = TRUE),
                        .groups = "drop") |>
              mutate(weight = "y")) |>
  mutate(lo95 = y - qnorm(.975) * se,
         hi95 = y + qnorm(.975) * se,
         t = factor(t,
                    levels = c(0,1),
                    labels = c("n","y")),
         sam = paste(sam, weight, sep = "_"),
         lab = factor(sam,
                      levels = c("pop_n",
                                 "srsl_n",
                                 "srss_n",
                                 "wrsl_n",
                                 "wrss_n",
                                 "wrsl_y",
                                 "wrss_y"),
                      labels = c("Population",
                                 "Simple\nrandom\nsample\n(1%)",
                                 "Simple\nrandom\nsample\n(0.1%)",
                                 "Weighted\nrandom\nsample\n(1%)\nw/o weights",
                                 "Weighted\nrandom\nsample\n(0.1%)\nw/o weights",
                                 "Weighted\nrandom\nsample\n(1%)\nw/ weights",
                                 "Weighted\nrandom\nsample\n(0.1%)\nw/ weights")))

## make plot
p <- ggplot(plot_df |> filter(weight == "n"),
            aes(x = lab, y = y, fill = sam)) +
  facet_wrap(~ t,
             labeller = tau_labeller) +
  geom_col() +
  geom_vline(xintercept = 1.5) +
  geom_vline(xintercept = c(3.5, 5.5),
             linetype = "dashed") +
  scale_fill_discrete(guide = "none") +
  scale_y_continuous(breaks = seq(0,100,10),
                     minor_breaks = seq(0,100,1),
                     limits = c(0,100),
                     expand = c(0,0)) +
  labs(title = NULL,
       y = bquote(Theta~","~theta),
       x = NULL) +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size = 18),
        strip.text = element_text(size = 18))

## save
ggsave(filename = file.path(fig_dir, "sim_national_popsam.pdf"),
       plot = p,
       width = plot_169_w,
       height = plot_169_h,
       units = "in",
       dpi = "retina")

## -----------------------------------------------------------------------------
## state population differences
## -----------------------------------------------------------------------------

## make figure data
plot_df <- pop |>
  group_by(state, t) |>
  summarise(y = mean(y) * 100,
            .groups = "drop")

## make plot
p1 <- ggplot(plot_df, aes(x = state,
                         y = y,
                         fill = t)) +
  geom_bar(stat = "identity", position = "dodge2") +
  geom_vline(xintercept = seq(1.5, plot_df |>
                                   distinct(state) |>
                                   pull() |>
                                   length(),
                              by = 1),
             colour = "gray",
             size = .5,
             alpha = .5) +
  scale_fill_discrete(guide = "none") +
  scale_y_continuous(breaks = seq(0,100,10),
                     minor_breaks = seq(0,100,5),
                     limits = c(0,100)) +
  labs(title = "Simulated population",
       y = bquote(Theta[state]),
       x = NULL) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = 7, angle = 45, vjust = 0.5))

## -----------------------------------------------------------------------------
## state sample differences
## -----------------------------------------------------------------------------

## set up samples for comparison
plot_df <- df_sam |>
    group_by(sam, state, t) |>
    summarise(y = mean(y) * 100,
              .groups = "drop")

## plot, with facet for each sample
p2 <- ggplot(plot_df,
            aes(x = state, y = y, fill = t)) +
  facet_wrap(~sam,
             labeller = labeller(sam = samp_label),
             nrow = 4) +
  geom_bar(stat = "identity", position = "dodge2") +
  geom_vline(xintercept = seq(1.5, plot_df |>
                                   distinct(state) |>
                                   pull() |>
                                   length(),
                              by = 1),
             colour = "gray",
             size = .5,
             alpha = .5) +
  labs(title = "Samples from simulated population",
       y = bquote(theta[state]),
       x = "State") +
  scale_fill_discrete(name = "D") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = 7, angle = 45, vjust = 0.5))

## combine plots to one page
p <- p1 / p2 +
  plot_layout(heights = c(1,2))

## save
ggsave(filename = file.path(fig_dir, "sim_state_popsam.pdf"),
       plot = p,
       width = plot_pg_w,
       height = plot_pg_h,
       units = "in",
       dpi = "retina")

## -----------------------------------------------------------------------------
## 45 degree plots
## -----------------------------------------------------------------------------

## -------------------------------------
## population to sample
## -------------------------------------

## join plot_df from before with population data
plot_df <- df_sam |>
    group_by(sam, state, t) |>
    summarise(y = mean(y) * 100,
              .groups = "drop") |>
  left_join(pop |>
            group_by(state, t) |>
            summarise(y_pop = mean(y) * 100,
                      .groups = "drop"))

## each sample gets a facet
p <- ggplot(plot_df,
            aes(x = y,
                y = y_pop,
                colour = t)) +
  facet_wrap(~sam,
             labeller = labeller(sam = samp_label)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point() +
  geom_point(colour = "white", show.legend = FALSE) +
  geom_text(aes(label = state), show.legend = FALSE) +
  scale_x_continuous(limits = c(0,100)) +
  scale_y_continuous(limits = c(0,100)) +
  labs(title = NULL,
       y = bquote(Theta[state]),
       x = bquote(theta[state])) +
  scale_colour_discrete(name = "D") +
  theme_bw() +
  theme(legend.position = "bottom")

## save
ggsave(filename = file.path(fig_dir, "sim_45_sam.pdf"),
       plot = p,
       width = plot_sq_75,
       height = plot_sq_75,
       units = "in",
       dpi = "retina")

## -------------------------------------
## population to estimate
## -------------------------------------

## join plot_df with population data
plot_df <- df_est |>
  left_join(pop |>
            group_by(state, t) |>
            summarise(y_pop = mean(y) * 100,
                      .groups = "drop"))

## each sample gets a facet
p <- ggplot(plot_df |> filter(q == "50") |> mutate(v = v * 100),
            aes(x = v,
                y = y_pop,
                colour = t)) +
  facet_wrap(~sam,
             labeller = labeller(sam = samp_label)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_point() +
  geom_point(colour = "white", show.legend = FALSE) +
  geom_text(aes(label = state), show.legend = FALSE) +
  scale_x_continuous(limits = c(0,100)) +
  scale_y_continuous(limits = c(0,100)) +
  labs(title = NULL,
       y = bquote(Theta[state]),
       x = bquote(hat(theta)[state])) +
  scale_colour_discrete(name = "D") +
  theme_bw() +
  theme(legend.position = "bottom")

## save
ggsave(filename = file.path(fig_dir, "sim_45_est.pdf"),
       plot = p,
       width = plot_sq_75,
       height = plot_sq_75,
       units = "in",
       dpi = "retina")

## -----------------------------------------------------------------------------
## line plot comparison between population, sample, and MRP
## -----------------------------------------------------------------------------

## combine data for plot to compare
plot_df <- df_est |>
  mutate(v = v * 100) |>
  pivot_wider(names_from = "q",
              names_prefix = "q",
              values_from = "v") |>
  select(-stnum) |>
  left_join(df_sam |>
              group_by(sam, state, t) |>
              summarise(y_samp = mean(y),
                        .groups = "drop"),
            by = c("state", "t", "sam")) |>
  left_join(pop |>
            group_by(state, t) |>
            summarise(y_pop = mean(y),
                      .groups = "drop"),
            by = c("state", "t")) |>
  mutate(y_samp = y_samp * 100,
         y_pop = y_pop * 100) |>
  pivot_longer(cols = c("q50", "y_samp", "y_pop"),
               names_to = "type",
               values_to = "est") |>
  mutate(q2.5 = ifelse(type != "q50", NA, q2.5),
         q25 = ifelse(type != "q50", NA, q25),
         q75 = ifelse(type != "q50", NA, q75),
         q97.5 = ifelse(type != "q50", NA, q97.5),
         type = factor(type,
                       levels = c("y_pop", "y_samp", "q50")),
         t = factor(t,
                    levels = c(0,1),
                    labels = c("n","y")))

## plot: SRSL
p1 <- ggplot(plot_df |>
               filter(sam == "srsl"),
             aes(x = state, y = est, colour = type)) +
  facet_wrap(~ t,
             labeller = tau_labeller,
             ncol = 1) +
  geom_linerange(aes(xmin = state, xmax = state,
                     ymin = q2.5, ymax = q97.5,
                     colour = type),
                 position = position_dodge(width = 1)) +
  geom_point(aes(shape = type),
             position = position_dodge(width = 1),
             size = .8) +
  geom_vline(xintercept = seq(1.5, plot_df |>
                                     distinct(state) |>
                                     pull() |>
                                     length(),
                              by = 1),
             colour = "gray",
             size = .5,
             alpha = .5) +
  labs(title = "Simple random sample (0.1%) from simulated population",
       y = bquote(Theta~","~theta~","~hat(theta)),
       x = NULL) +
  guides(colour = "none", shape = "none") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = 7, angle = 45, vjust = 0.5))

## plot: WRSL
p2 <- ggplot(plot_df |>
               filter(sam == "wrsl"),
             aes(x = state, y = est, colour = type)) +
  facet_wrap(~ t,
             labeller = tau_labeller,
             ncol = 1) +
  geom_linerange(aes(xmin = state, xmax = state,
                     ymin = q2.5, ymax = q97.5,
                     colour = type),
                 position = position_dodge(width = 1)) +
  geom_point(aes(shape = type),
             position = position_dodge(width = 1),
             size = .8) +
  geom_vline(xintercept = seq(1.5, plot_df |>
                                     distinct(state) |>
                                     pull() |>
                                     length(),
                              by = 1),
             colour = "gray",
             size = .5,
             alpha = .5) +
  labs(title = "Weighted random sample (0.1%) from simulated population",
       y = bquote(Theta~","~theta~","~hat(theta)),
       x = "State") +
  scale_shape_discrete(name = "",
                       labels = c(y_pop = bquote("Population"~(Theta)),
                                  y_samp = bquote("Sample (1%)"~(theta)),
                                  q50 = bquote("MRP"~(hat(theta))))) +
  scale_colour_discrete(name = "",
                        labels = c(y_pop = bquote("Population"~(Theta)),
                                  y_samp = bquote("Sample (1%)"~(theta)),
                                  q50 = bquote("MRP"~(hat(theta))))) +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = 7, angle = 45, vjust = 0.5))

## combine
p <- p1 / p2

## save
ggsave(filename = file.path(fig_dir, "sim_full_comp.pdf"),
       plot = p,
       width = plot_pg_w,
       height = plot_pg_h,
       units = "in",
       dpi = "retina")

## =============================================================================
## END SCRIPT
################################################################################
