################################################################################
##
## [ PROJ ] Low-income enrollment across states with MRP
## [ FILE ] sens.R
## [ AUTH ] Benjamin Skinner (@btskinner)
## [ INIT ] 11 January 2022
##
################################################################################

## libraries
libs <- c("tidyverse", "survey", "brms", "tidybayes")
sapply(libs, require, character.only = TRUE)

## directories
dat_dir <- file.path("..", "..", "data")
cln_dir <- file.path(dat_dir, "clean")
est_dir <- file.path("..", "..", "estimates")
fig_dir <- file.path("..", "..", "figures")

## -----------------------------------------------------------------------------
## get data
## -----------------------------------------------------------------------------

df <- readRDS(file.path(cln_dir, "hsl_weights.RDS")) |>
  filter(!is.na(ps_ontatt))

## -----------------------------------------------------------------------------
## freq: compute average enrollments by income and 10 states
## -----------------------------------------------------------------------------

keep_states <- c(6,12,13,26,37,39,42,47,48,53)

out <- df |>
  group_by(x1state, lowinc) |>
  summarise(ps_ontatt_avg_w1student = weighted.mean(ps_ontatt, w1student),
            ps_ontatt_avg_w4w1stu = weighted.mean(ps_ontatt, w4w1stu),
            .groups = "drop") |>
  filter(x1state %in% keep_states)

saveRDS(out, file.path(est_dir, "state_estimates_point.RDS"))

## -----------------------------------------------------------------------------
## freq: with survey package
## -----------------------------------------------------------------------------

## base year weight
svydat <- svrepdesign(weights = ~w1student, data = df,
                      repweights = "w1student[0-9]+", type = "BRR")

est_base <- map(keep_states,
              ~ svyby(~ps_ontatt, ~lowinc,
                      subset(svydat, x1state == .x), svymean, na.rm = T) |>
                  as_tibble() |>
                  mutate(x1state = .x,
                         y = "ps_ontatt") |>
                  rename(est = ps_ontatt)) |>
    bind_rows() |>
    mutate(w = "w1student")

## longitudinal weight
svydat <- svrepdesign(weights = ~w4w1stu, data = df,
                      repweights = "w4w1stu[0-9]+", type = "BRR")

est_long <- map(keep_states,
                ~ svyby(~ps_ontatt, ~lowinc,
                        subset(svydat, x1state == .x), svymean, na.rm = T) |>
                    as_tibble() |>
                    mutate(x1state = .x,
                           y = "ps_ontatt") |>
                    rename(est = ps_ontatt)) |>
    bind_rows() |>
    mutate(w = "w4w1stu")

## bind rows and save
est <- bind_rows(est_base, est_long)

saveRDS(est, file.path(est_dir, "state_estimates_w_error.RDS"))

## -----------------------------------------------------------------------------
## bayes
## -----------------------------------------------------------------------------

## create normed weights for brm
df <- df |>
  mutate(w1student_norm = w1student / sum(w1student) * nrow(df),
         w4w1stu_norm = w4w1stu / sum(w4w1stu) * nrow(df))

est_bayes_base <- map(keep_states,
                      ~ brm(ps_ontatt|weights(w1student_norm) ~ lowinc,
                            data = df |> filter(x1state == .x),
                            family = bernoulli("logit"),
                            backend = "cmdstan",
                            cores = 4) |>
                        epred_draws(newdata = tibble(lowinc = c(0,1))) |>
                        mutate(x1state = .x,
                               w = "w1student_norm")) |>
  bind_rows()


est_bayes_long <- map(keep_states,
                      ~ brm(ps_ontatt|weights(w4w1stu_norm) ~ lowinc,
                            data = df |> filter(x1state == .x),
                            family = bernoulli("logit"),
                            backend = "cmdstan",
                            cores = 4) |>
                        epred_draws(newdata = tibble(lowinc = c(0,1))) |>
                        mutate(x1state = .x,
                               w = "w4w1stu_norm")) |>
  bind_rows()

est_bayes <- bind_rows(est_bayes_base, est_bayes_long)

saveRDS(est_bayes, file.path(est_dir, "state_estimates_bayes_weights.RDS"))

## =============================================================================
## END SCRIPT
################################################################################
