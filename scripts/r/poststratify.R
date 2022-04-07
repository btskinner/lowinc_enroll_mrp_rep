################################################################################
##
## [ PROJ ] Low-income enrollment across states with MRP
## [ FILE ] poststratify.R
## [ AUTH ] Benjamin Skinner (@btskinner)
## [ INIT ] 15 January 2021
##
################################################################################

## libraries
libs <- c("tidyverse", "crosswalkr")
sapply(libs, require, character.only = TRUE)

## directories
dat_dir <- file.path("..", "..", "data")
cln_dir <- file.path(dat_dir, "clean")
crw_dir <- file.path("..", "..", "crosswalks")

## helper function
ps_weight <- function(x, n = NULL) {
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

## predictions
preds <- readRDS(file.path(cln_dir, "preds.RDS"))

## get population counts
pop_lo <- readRDS(file.path(cln_dir, "pop_lo_counts.RDS"))

## get design matrix
dmat <- readRDS(file.path(cln_dir, "design_matrix_lo.RDS"))

## get crosswalk
cw <- read_csv(file.path(crw_dir, "hsl_fips_cw.csv"),
               show_col_types = FALSE)

## -----------------------------------------------------------------------------
## poststratify
## -----------------------------------------------------------------------------

## get population counts for design matrix
pc <- dmat |>
  left_join(pop_lo |>
              select(fips, ge, ra, lo, count),
            by = c("fips", "ge", "ra", "lo")) |>
  left_join(cw, by = "fips") |>
  mutate(st = ifelse(is.na(st), 51, st)) |>
  mutate(count = ifelse(is.na(count), 0, count)) |>
  arrange(fips, ge, ra, lo) |>
  as.matrix()

## ---------------------------
## national
## ---------------------------

## limit by lowinc
lo_mask <- (pc[,"lo"] == 1)
hi_mask <- (pc[,"lo"] == 0)

## poststratify within group
ps_lo <- ps_weight(preds[lo_mask,], pc[lo_mask,"count"])
ps_hi <- ps_weight(preds[hi_mask,], pc[hi_mask,"count"])

## get difference between hi/lo groups
ps_diff <- ps_hi - ps_lo

## get quantiles
q_lo <- quantile(ps_lo, c(0.025, .25, .5, .75, .975))
q_hi <- quantile(ps_hi, c(0.025, .25, .5, .75, .975))

## make tibble for output
ps_national <- tibble(
  lowinc = c(rep(0, times = length(q_hi)),
             rep(1, times = length(q_lo))),
  q = c(q_hi, q_lo) |> names(),
  v = c(q_hi, q_lo)
) |>
  ## remove % from quantile names
  mutate(q = str_replace(q, "%", ""))

## make tibble for difference
ps_national_diff <- tibble(
  v = ps_diff
)

## ---------------------------
## state
## ---------------------------

## temporary lists
tmp <- list()
tmp_lo <- list()
tmp_diff <- list()

## loop thru states
for (s in 1:51) {

  message("  - ", pc[pc[,"st"] == s, "fips"] |> unique())

  ## limit by lowinc and state
  lo_mask <- (pc[,"lo"] == 1 & pc[,"st"] == s)
  hi_mask <- (pc[,"lo"] == 0 & pc[,"st"] == s)

  ## poststratify within group
  ps_lo <- ps_weight(preds[lo_mask,], pc[lo_mask,"count"])
  ps_hi <- ps_weight(preds[hi_mask,], pc[hi_mask,"count"])

  ## get difference between hi/lo groups
  ps_diff <- ps_hi - ps_lo

  ## get quantiles
  q_lo <- quantile(ps_lo, c(0.025, .25, .5, .75, .975))
  q_hi <- quantile(ps_hi, c(0.025, .25, .5, .75, .975))

  ## make tibble for full posterior for lowinc
  tmp_lo[[s]] <- tibble(
    st = s,
    lowinc = 1,
    v = ps_lo
  )

  ## make tibble for quantile output
  tmp[[s]] <- tibble(
    st = s,
    lowinc = c(rep(0, times = length(q_hi)),
               rep(1, times = length(q_lo))),
    q = c(q_hi, q_lo) |> names(),
    v = c(q_hi, q_lo)
  )

  ## make tibble for difference
  tmp_diff[[s]] <- tibble(
    st = s,
    v = ps_diff
  )
}

## ---------------------------
## munge
## ---------------------------

ps_state <- tmp |>
  ## bind together
  bind_rows() |>
  ## remove % from quantile names
  mutate(q = str_replace(q, "%", "")) |>
  ## join in state names
  left_join(pop_lo |> distinct(st, fips) |>
            mutate(st = ifelse(is.na(st), 51, st)),
            by = "st") |>
  ## join in state names from crosswalk
  left_join(stcrosswalk |> select(fips = stfips, stname),
            by = "fips")

ps_state_lo <- tmp_lo |>
  ## bind together
  bind_rows() |>
  ## join in state names
  left_join(pop_lo |> distinct(st, fips) |>
            mutate(st = ifelse(is.na(st), 51, st)),
            by = "st") |>
  ## join in state names from crosswalk
  left_join(stcrosswalk |> select(fips = stfips, stname),
            by = "fips")

ps_state_diff <- tmp_diff |>
  ## bind together
  bind_rows() |>
  ## join in state names
  left_join(pop_lo |> distinct(st, fips),
            by = "st") |>
  ## join in state names from crosswalk
  left_join(stcrosswalk |> select(fips = stfips, stname),
            by = "fips")

## ---------------------------
## save
## ---------------------------

saveRDS(ps_national, file.path(cln_dir, "ps_national.RDS"))
saveRDS(ps_national_diff, file.path(cln_dir, "ps_national_diff.RDS"))
saveRDS(ps_state, file.path(cln_dir, "ps_state.RDS"))
saveRDS(ps_state_lo, file.path(cln_dir, "ps_state_lo.RDS"))
saveRDS(ps_state_diff, file.path(cln_dir, "ps_state_diff.RDS"))

## =============================================================================
## END SCRIPT
################################################################################

