################################################################################
##
## [ PROJ ] Tests of various Stan codings
## [ FILE ] simulation_analysis_models.R
## [ AUTH ] Benjamin Skinner and Will Doyle
## [ INIT ] 23 July 2021
##
################################################################################

## libraries
libs <- c("tidyverse", "cmdstanr", "rstan")
sapply(libs, require, character.only = TRUE)

## directories
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path("..", ".."), args)
dat_dir <- file.path(root, "data")
sim_dir <- file.path(dat_dir, "simulation")
sta_dir <- file.path(root, "scripts", "stan")

# stan sampler settings
stan_seed <- 1919
stan_adapt_delta <- .99
stan_max_depth <- 15L
stan_num_cores <- parallel::detectCores()
stan_num_chains <- 4
stan_num_threads <- 1
stan_num_warmup <- 1000L
stan_num_samples <- 1000L

## samples
sams <- c("srsl", "srss", "wrsl", "wrss")

## helper functions
inv_logit <- function(x) { exp(-log(1 + exp(-x))) }
ps_weight <- function(x, n) {
  if (is.null(n) | sum(n) == 0) {
    colMeans(x)
  } else if (is.null(dim(x)) & length(x) > 0) {
    ## if there's no dim(), but length(),
    ## then there's no need to reweight,
    ## b/c n = sum(n)
    x
  } else {
   colSums(x * n / sum(n))
  }
}

## -----------------------------------------------------------------------------
## get data
## -----------------------------------------------------------------------------

## population
pop <- readRDS(file.path(sim_dir, "sim_population.RDS"))

## simple random samples
df <- sams |>
  set_names() |>
  map(~ readRDS(file.path(sim_dir, paste0("sim_", .x, ".RDS"))))

## crosswalks for stan index
cw <- sams |>
  set_names() |>
  map(~ df[[.x]] |>
        distinct(stnum) |>
        arrange(stnum) |>
        mutate(stani = row_number()))

## join crosswalks back in
df <- sams |>
  set_names() |>
  map(~ df[[.x]] |>
        left_join(cw[[.x]], by = "stnum"))

## -----------------------------------------------------------------------------
## compile model
## -----------------------------------------------------------------------------

mod_file <- "simulation_mrp.stan"
mod <- cmdstan_model(stan_file = file.path(sta_dir, mod_file),
                     cpp_options = list(stan_threads = TRUE))

## -----------------------------------------------------------------------------
## loop thru sams: srsl, srss, wrsl, wrss
## -----------------------------------------------------------------------------

for (i in sams) {

  ## first level
  fl <- df[[i]] |>
    group_by(stani, t) |>
    summarise(total = n(),
              y = sum(y),
              .groups = "drop") |>
    arrange(stani)

  ## second level
  sl <- df[[i]] |>
    distinct(stani, .keep_all = TRUE) |>
    select(stani, starts_with("z")) |>
    arrange(stani)

  ## store for stan
  stan_data <- list(
    M = fl |> distinct(stani) |> nrow(),
    N = nrow(fl),
    K = ncol(sl) - 1,
    y = fl |> pull(y),
    t = fl |> pull(t),
    total = fl |> pull(total),
    state = fl |> pull(stani),
    z = sl |> select(-stani) |> as.matrix()
  )

  ## -------------------------------------
  ## fit
  ## -------------------------------------

  fit <- mod$sample(stan_data,
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
  ## predictions
  ## -------------------------------------

  ## design matrix
  dmat <- expand.grid(stnum = 1:51,
                      t = 0:1) |>
    as_tibble() |>
    left_join(pop |> distinct(stnum, state), by = "stnum")

  ## get parameters
  pars <- extract(read_stan_csv(fit$output_files()))

  ## fix dmat for this sample
  pc <- dmat |>
    select(-state) |>
    left_join(cw[[i]],
              by = "stnum") |>
    arrange(stnum, t) |>
    as.matrix()

  ## second level data
  z <- pop |>
    distinct(stnum, .keep_all = TRUE) |>
    select(stnum, starts_with("z")) |>
    left_join(cw[[i]], by = "stnum") |>
    arrange(stani) |>
    as.matrix()

  ## prespecify output matrix (much faster)
  out_mat <- matrix(NA_real_, nrow(pc), length(pars[["alpha"]]))

  ## get linear combination for each design matrix row
  for (j in 1:nrow(out_mat)) {
    out_mat[j,] <- pars[["alpha"]] +
      pars[["beta"]] * pc[j,"t"] +
      {
        ## adjustment for missing state
        if (is.na(pc[j,"stani"])) {
          c(pars[["gamma"]] %*% z[z[,"stnum"] == as.numeric(pc[j,"stnum"]),2:4])
        } else {
          pars[["alpha_state"]][,pc[j,"stani"]] +
            pars[["beta_state"]][,pc[j,"stani"]] * pc[j,"t"] +
            c(pars[["gamma"]] %*% z[pc[j,"stani"],2:4])
        }
      }
  }

  ## inverse logit function to get predicted probabilities
  preds <- inv_logit(out_mat)

  ## -------------------------------------
  ## poststratify
  ## -------------------------------------

  popc <- dmat |>
    select(-state) |>
    left_join(pop |>
              group_by(stnum, t) |>
              summarise(count = n(),
                        .groups = "drop") |>
              arrange(stnum),
              by = c("stnum", "t")) |>
    mutate(count = ifelse(is.na(count), 0, count)) |>
    arrange(stnum, t) |>
    as.matrix()

  ## ---------------------------
  ## overall
  ## ---------------------------

  ## limit by t
  lo_mask <- (popc[,"t"] == 1)
  hi_mask <- (popc[,"t"] == 0)
  ## poststratify within group
  lo <- ps_weight(preds[lo_mask,], popc[lo_mask,"count"])
  hi <- ps_weight(preds[hi_mask,], popc[hi_mask,"count"])
  ## get quantiles
  qlo <- quantile(lo, c(0.025, .25, .5, .75, .975))
  qhi <- quantile(hi, c(0.025, .25, .5, .75, .975))
  ## make tibble for output
  ps_overall <- tibble(
      sam = i,
      t = c(rep(1, times = length(qlo)),
            rep(0, times = length(qhi))),
      q = c(qlo, qhi) |> names(),
      v = c(qlo, qhi)
    ) |>
    ## bind together
    bind_rows() |>
    ## arrange columns
    select(sam, t, q, v) |>
    ## remove % from quantile names
    mutate(q = str_replace(q, "%", ""))

  ## ---------------------------
  ## by state
  ## ---------------------------

  ## temporary list to hold state estimates
  tmp <- list()
  for (s in popc[,"stnum"] |> unique()) {
    ## limit by t and state
    lo_mask <- (popc[,"t"] == 1 & popc[,"stnum"] == s)
    hi_mask <- (popc[,"t"] == 0 & popc[,"stnum"] == s)
    ## poststratify within group
    lo <- ps_weight(preds[lo_mask,], popc[lo_mask,"count"])
    hi <- ps_weight(preds[hi_mask,], popc[hi_mask,"count"])
    ## get quantiles
    qlo <- quantile(lo, c(0.025, .25, .5, .75, .975))
    qhi <- quantile(hi, c(0.025, .25, .5, .75, .975))
    ## make tibble for output
    tmp[[s]] <- tibble(
        sam = i,
        stnum = s,
        t = c(rep(1, times = length(qlo)),
              rep(0, times = length(qhi))),
        q = c(qlo, qhi) |> names(),
        v = c(qlo, qhi)
    )
  }

  ## bind
  ps_state <- tmp |>
    ## bind together
    bind_rows() |>
    ## join in state names
    left_join(pop |> distinct(stnum, state),
              by = "stnum") |>
    ## arrange columns
    select(sam, stnum, state, t, q, v) |>
    ## remove % from quantile names
    mutate(q = str_replace(q, "%", ""))

  ## ---------------------------
  ## save
  ## ---------------------------

  saveRDS(ps_overall, file.path(sim_dir, paste0("sim_ps_overall_",i,".RDS")))
  saveRDS(ps_state, file.path(sim_dir, paste0("sim_ps_state_",i,".RDS")))
}

## =============================================================================
## END SCRIPT
################################################################################
