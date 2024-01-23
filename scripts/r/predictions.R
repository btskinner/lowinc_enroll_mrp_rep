################################################################################
##
## [ PROJ ] Low-income enrollment across states with MRP
## [ FILE ] predictions.R
## [ AUTH ] Benjamin Skinner (@btskinner)
## [ INIT ] 21 January 2021
##
################################################################################

## libraries
libs <- c("tidyverse", "rstan", "crosswalkr")
sapply(libs, require, character.only = TRUE)

## directories
dat_dir <- file.path("..", "..", "data")
cln_dir <- file.path(dat_dir, "clean")
raw_dir <- file.path(dat_dir, "raw")
crw_dir <- file.path("..", "..", "crosswalks")
est_dir <- file.path("..", "..", "estimates")

## inverse logit function
inv_logit <- function(x) { exp(-log(1 + exp(-x))) }

## -----------------------------------------------------------------------------
## get data
## -----------------------------------------------------------------------------

## ---------------------------
## crosswalks
## ---------------------------

cw <- read_csv(file.path(crw_dir, "hsl_fips_cw.csv"),
               show_col_types = FALSE)

## ---------------------------
## second-level data
## ---------------------------

sl <- read_csv(file.path(raw_dir, "state_level.csv"),
               show_col_types = FALSE) |>
  select(fips = stfips, ends_with("2013")) |>
  left_join(cw, by = "fips") |>
  mutate(across(ends_with("2013"), ~ scale(.x)))

## ---------------------------
## design matrix
## ---------------------------

dmat <- expand.grid(fips = crosswalkr::stcrosswalk |> pull(stfips),
                    fe = 0:1,
                    ra = 1:6,
                    lo = 0:1) |>
  as_tibble() |>
  left_join(crosswalkr::stcrosswalk |> select(fips = stfips, rg = cenreg)) |>
  arrange(fips, fe, ra, lo)

## save for poststratify later
saveRDS(dmat, file.path(cln_dir, "design_matrix_lo.RDS"))

## -----------------------------------------------------------------------------
## poststratify
## -----------------------------------------------------------------------------

## get estimates
est <- read_stan_csv(list.files(est_dir, "hsl_ps_ontatt", full.names = TRUE))

## get parameters
pars <- extract(est)

## get second-level data for this sample
sls <- sl |>
  arrange(st) |>
  select(-fips) |>
  ## for missing states: 11
  mutate(st = ifelse(is.na(st), -999, st)) |>
  as.matrix()

## set up population counts
pc <- dmat |>
  left_join(cw, by = "fips") |>
  arrange(fips, fe, ra, lo) |>
  as.matrix()

## prespecify output matrix (much faster)
out_mat <- matrix(NA_real_, nrow(pc), length(pars[["a"]]))

## get predictions for each cell: row by row
for (k in 1:nrow(out_mat)) {
  out_mat[k,] <- pars[["a"]] +
    pars[["a_rg"]][,pc[k,"rg"]] +
    pars[["b_lo"]] * pc[k,"lo"] +
    pars[["b_fe"]] * pc[k,"fe"] +
    pars[["a_ra"]][,pc[k,"ra"]] +
    {
      ## adjustment for missing state
      if (is.na(pc[k,"st"])) {
        c(pars[["g"]] %*% sls[sls[,"st"] == -999,1:5])
      } else {
        pars[["a_st"]][,pc[k,"st"]] +
          pars[["a_st_lo"]][,pc[k,"st"]] * pc[k,"lo"] +
          c(pars[["g"]] %*% sls[sls[,"st"] == pc[k,"st"],1:5])
      }
    }
}

## inverse logit function to get predicted probabilities
preds <- inv_logit(out_mat)

## save
saveRDS(preds, file.path(cln_dir, "preds.RDS"))

## =============================================================================
## END SCRIPT
################################################################################
