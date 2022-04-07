################################################################################
##
## [ PROJ ] Low-income enrollment across states with MRP
## [ FILE ] sens.R
## [ AUTH ] Benjamin Skinner (@btskinner)
## [ INIT ] 11 January 2022
##
################################################################################

## libraries
libs <- c("tidyverse", "survey")
sapply(libs, require, character.only = TRUE)

## directories
dat_dir <- file.path("..", "..", "data")
cln_dir <- file.path(dat_dir, "clean")
est_dir <- file.path("..", "..", "estimates")

## -----------------------------------------------------------------------------
## get data
## -----------------------------------------------------------------------------

df <- readRDS(file.path(cln_dir, "hsl_weights.RDS"))

## -----------------------------------------------------------------------------
## compute average enrollments by income and 10 states
## -----------------------------------------------------------------------------

## 10 representative states
keep_states <- c(6,12,13,26,37,39,42,47,48,53)

## set up survey data
svydat <- svydesign(~stuid, weights = ~w4w1stu, data = df,
                    repweights = "w4w1stu[0-9]+", type = "BRR")

## compute estimates w/ error for each state
est <- map(keep_states,
           ~ svyby(~ps_ontatt, ~lowinc,
                   subset(svydat, x1state == .x), svymean, na.rm = T) %>%
             as_tibble() %>%
             mutate(x1state = .x,
                    y = "ps_ontatt") %>%
             rename(est = ps_ontatt)) %>%
  bind_rows() %>%
  mutate(w = "w4w1stu")

## save
saveRDS(est, file.path(cln_dir, "state_estimates_w_error.RDS"))

## =============================================================================
## END SCRIPT
################################################################################
