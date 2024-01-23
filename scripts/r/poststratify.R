################################################################################
##
## [ PROJ ] Low-income enrollment across states with MRP
## [ FILE ] poststratify.R
## [ AUTH ] Benjamin Skinner (@btskinner)
## [ INIT ] 15 January 2021
##
################################################################################

## libraries
libs <- c("tidyverse", "crosswalkr", "haven")
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

## get design matrix
dmat <- readRDS(file.path(cln_dir, "design_matrix_lo.RDS"))

## get crosswalk
cw <- read_csv(file.path(crw_dir, "hsl_fips_cw.csv"),
               show_col_types = FALSE)

## population counts
pop <- readRDS(file.path(cln_dir, "acs_lo_poststrat.RDS")) |>
  left_join(cw, by = "fips") |>
  left_join(stcrosswalk |> select(stfips, rg = cenreg),
            by = c("fips" = "stfips")) |>
  arrange(sample, fips, fe, ra, lo)

## -----------------------------------------------------------------------------
## poststratify
## -----------------------------------------------------------------------------

## looping through three ACS poststratification samples

## 1) 2009:      1 year
## 2) 2008-2010: 3 year
## 3) 2007-2011: 5 year

ps_national <- list()
ps_national_diff <- list()
ps_national_post <- list()
ps_state <- list()
ps_state_diff <- list()
ps_state_post <- list()

for (samp in c(1,3,5)) {

  ## store sample name as character to use in list index
  samp_name <- as.character(samp)

  ## get population counts for design matrix
  pc <- dmat |>
    left_join(pop |>
                filter(sample == samp) |>
                mutate(fips = zap_labels(fips)) |>
                select(fips, fe, ra, lo, count),
              by = c("fips", "fe", "ra", "lo")) |>
    left_join(cw, by = "fips") |>
    mutate(st = ifelse(is.na(st), 51, st)) |>
    mutate(count = ifelse(is.na(count), 0, count)) |>
    arrange(fips, fe, ra, lo) |>
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
  ps_national[[samp_name]] <- tibble(
    lowinc = c(rep(0, times = length(q_hi)),
               rep(1, times = length(q_lo))),
    q = c(q_hi, q_lo) |> names(),
    v = c(q_hi, q_lo)
  ) |>
    ## remove % from quantile names
    mutate(q = str_replace(q, "%", ""))

  ## make tibble for difference
  ps_national_diff[[samp_name]] <- tibble(
    v = ps_diff
  )

  ## make tibble for full posterior for lowinc
  ps_national_post[[samp_name]] <- bind_rows(
    tibble(
      lowinc = 1,
      v = ps_lo
    ),
    tibble(
      lowinc = 0,
      v = ps_hi
    )
  )

  ## ---------------------------
  ## state
  ## ---------------------------

  ## temporary lists
  tmp <- list()
  tmp_lo <- list()
  tmp_hi <- list()
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

    ## make tibble for full posterior for hi/med income
    tmp_hi[[s]] <- tibble(
      st = s,
      lowinc = 0,
      v = ps_hi
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

  ps_state[[samp_name]] <- tmp |>
    ## bind together
    bind_rows() |>
    ## remove % from quantile names
    mutate(q = str_replace(q, "%", "")) |>
    ## join in state names
    left_join(pop |>
                filter(sample == samp) |>
                distinct(st, fips) |>
                mutate(st = ifelse(is.na(st), 51, st)),
              by = "st") |>
    ## join in state names from crosswalk
    left_join(stcrosswalk |> select(fips = stfips, stname),
              by = "fips") |>
    ## remove labels from fips
    mutate(fips = zap_labels(fips))

  ps_state_post[[samp_name]] <- bind_rows(
    tmp_lo |>
      ## bind together
      bind_rows() |>
      ## join in state names
      left_join(pop |>
                  filter(sample == samp) |>
                  distinct(st, fips) |>
                  mutate(st = ifelse(is.na(st), 51, st)),
                by = "st") |>
      ## join in state names from crosswalk
      left_join(stcrosswalk |> select(fips = stfips, stname),
                by = "fips"),
    tmp_hi |>
      ## bind together
      bind_rows() |>
      ## join in state names
      left_join(pop |>
                  filter(sample == samp) |>
                  distinct(st, fips) |>
                  mutate(st = ifelse(is.na(st), 51, st)),
                by = "st") |>
      ## join in state names from crosswalk
      left_join(stcrosswalk |> select(fips = stfips, stname),
                by = "fips")
  ) |>
    ## remove labels from fips
    mutate(fips = zap_labels(fips))

  ps_state_diff[[samp_name]] <- tmp_diff |>
    ## bind together
    bind_rows() |>
    ## join in state names
    left_join(pop |>
                filter(sample == samp) |>
                distinct(st, fips),
              by = "st") |>
    ## join in state names from crosswalk
    left_join(stcrosswalk |> select(fips = stfips, stname),
              by = "fips") |>
    ## remove labels from fips
    mutate(fips = zap_labels(fips))
}

## ---------------------------
## bind everything together
## ---------------------------

ps_national <- bind_rows(ps_national, .id = "sample") |>
  mutate(sample = sample |> as.integer())
ps_national_diff <- bind_rows(ps_national_diff, .id = "sample") |>
  mutate(sample = sample |> as.integer())
ps_national_post <- bind_rows(ps_national_post, .id = "sample") |>
  mutate(sample = sample |> as.integer())
ps_state <- bind_rows(ps_state, .id = "sample") |>
  mutate(sample = sample |> as.integer())
ps_state_diff <- bind_rows(ps_state_diff, .id = "sample") |>
  mutate(sample = sample |> as.integer())
ps_state_post <- bind_rows(ps_state_post, .id = "sample") |>
  mutate(sample = sample |> as.integer())

## ---------------------------
## save
## ---------------------------

saveRDS(ps_national, file.path(cln_dir, "ps_national.RDS"))
saveRDS(ps_national_diff, file.path(cln_dir, "ps_national_diff.RDS"))
saveRDS(ps_national_post, file.path(cln_dir, "ps_national_post.RDS"))
saveRDS(ps_state, file.path(cln_dir, "ps_state.RDS"))
saveRDS(ps_state_diff, file.path(cln_dir, "ps_state_diff.RDS"))
saveRDS(ps_state_post, file.path(cln_dir, "ps_state_post.RDS"))

## =============================================================================
## END SCRIPT
################################################################################

