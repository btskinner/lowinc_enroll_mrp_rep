################################################################################
##
## [ PROJ ] Low-income enrollment across states with MRP
## [ FILE ] analysis.R
## [ AUTH ] Benjamin Skinner (@btskinner)
## [ INIT ] 13 January 2021
##
################################################################################

## libraries
libs <- c("tidyverse", "purrr", "cmdstanr")
sapply(libs, require, character.only = TRUE)

## directories
dat_dir <- file.path("..", "..", "data")
cln_dir <- file.path(dat_dir, "clean")
est_dir <- file.path("..", "..", "estimates")
sta_dir <- file.path("..", "stan")

## cmdstan path
set_cmdstan_path("~/Desktop/cmdstan")

## stan sampler settings
stan_seed <- 19806
stan_adapt_delta <- .99
stan_max_depth <- 15L
stan_num_cores <- 4
stan_num_chains <- 4
stan_num_threads <- 1
stan_num_warmup <- 1000L
stan_num_samples <- 1000L

## -----------------------------------------------------------------------------
## get data
## -----------------------------------------------------------------------------

## first level data
hsl <- readRDS(file.path(cln_dir, "hsl.RDS"))

## second level data
hsl_sl <- readRDS(file.path(cln_dir, "hsl_sl.RDS"))

## -----------------------------------------------------------------------------
## analysis
## -----------------------------------------------------------------------------

## -------------------------------------
## compile model
## -------------------------------------

## eg: mrp_sim.stan
mod_file <- "mrp.stan"
mod <- cmdstan_model(stan_file = file.path(sta_dir, mod_file),
                     cpp_options = list(stan_threads = TRUE))

## -------------------------------------
## set up null data lists for storage
## -------------------------------------

## first level
fl <- hsl |>
  group_by(rg, st, lowinc, gender, raceeth) |>
  summarise(total = n(),
            college = sum(!!sym(k)),
            .groups = "drop") |>
  arrange(st)

## second level
sl <- hsl_sl |>
  distinct(st, .keep_all = TRUE) |>
  select(-stfips) |>
  arrange(st)

## data as list for Stan
stan_dat <- list(
  M = nrow(sl),
  N = nrow(fl),
  J = fl |> distinct(raceeth) |> nrow(),
  K = ncol(sl) - 1,
  R = fl |> distinct(rg) |> nrow(),
  st = fl |> pull(st),
  rg = fl |> pull(rg),
  college = fl |> pull(college),
  total = fl |> pull(total),
  ge = fl |> pull(gender),
  lo = fl |> pull(lowinc),
  ra = fl |> pull(raceeth),
  z = sl |> select(-st) |> as.matrix()
)

## -------------------------------------
## fit
## -------------------------------------

fit <- mod$sample(stan_dat,
                  seed = stan_seed,
                  adapt_delta = stan_adapt_delta,
                  max_treedepth = stan_max_depth,
                  iter_warmup = stan_num_warmup,
                  iter_sampling = stan_num_samples,
                  parallel_chains = stan_num_cores,
                  chains = stan_num_chains,
                  threads_per_chain = stan_num_threads
                  )

## -------------------------------------
## extract and save
## -------------------------------------

fit$save_output_files(dir = est_dir,
                      basename = "hsl_ps_ontatt",
                      timestamp = FALSE,
                      random = FALSE
                      )

## =============================================================================
## END SCRIPT
################################################################################
