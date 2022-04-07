################################################################################
##
## [ PROJ ] Low-income enrollment across states with MRP
## [ FILE ] make_hsls.R
## [ AUTH ] Benjamin Skinner (@btskinner)
## [ INIT ] 13 January 2021
##
################################################################################

## libraries
libs <- c("tidyverse", "vroom", "haven")
sapply(libs, require, character.only = TRUE)

## file paths
dat_dir <- file.path("..", "..", "data")
raw_dir <- file.path(dat_dir, "PGPH9", "ASCII")
cln_dir <- file.path(dat_dir, "clean")
crw_dir <- file.path("..", "..", "crosswalks")

## -----------------------------------------------------------------------------
## read in data
## -----------------------------------------------------------------------------

## NOTES:
##
## Need to read in .dat (fixed width files); here are locations
##
## stu_id:              1-5
## x1sex:           342-343
## x1race:          344-345
## x1stdob:         366-371
## x1famincome:     531-532
## x1poverty:       533-534
## x1poverty130:    535-536
## x1poverty185:    537-538
## x1region:        751-752
## x1state:         757-758
## x4fb16enrstat: 2326-2327

## set up column start, stop, and names
col_sta <- c(1,342,344,366,531,533,535,537,751,757,2326)
col_end <- c(5,343,345,371,532,534,536,538,752,758,2327)
col_nms <- c("stuid","x1sex","x1race","x1stdob",
             "x1famincome","x1poverty","x1poverty130","x1poverty185",
             "x1region","x1state","x4fb16enrstat")

## read in
df <- vroom_fwf(file.path(raw_dir, "f2student.dat"),
                col_positions = fwf_positions(start = col_sta,
                                              end = col_end,
                                              col_names = col_nms),
                col_types = cols(.default = "i"))

## x1famincome
##  1: $15,000 or less
##  2: $15,001 - $35,000
##  3: $35,001 - $55,000
##  4: $55,001 - $75,000
##  5: $75,001 - $95,000
##  6: $95,001 - $115,000
##  7: $115,001 - $135,000
##  8: $135,001 - $155,000
##  9: $155,001 - $175,000
## 10: $175,001 - $195,000
## 11: $195,001 - $215,000
## 12: $215,001 - $235,000
## 13: $235,001 or more
## -8: Unit non-response
## -9: Missing

## -----------------------------------------------------------------------------
## munge
## -----------------------------------------------------------------------------

## set up state number crosswalk for stan, which needs proper index
stan_state <- df |>
    distinct(x1state) |>
    arrange(x1state) |>
    mutate(st = row_number())

## save crosswalk
write_csv(x = stan_state |> rename(fips = x1state),
          file = file.path(crw_dir, "hsl_fips_cw.csv"))

## join back in
df <- left_join(df, stan_state, by = "x1state")

## general munging
df <- df |>
    ## drop if missing income
    filter(x1famincome > 0) |>
    ## add low income indicator
    mutate(lowinc = as.integer(x1famincome <= 2)) |>
    ## drop if missing PS attendance
    filter(x4fb16enrstat > 0) |>
    ## set enrollments
    mutate(ps_evratt = as.integer(x4fb16enrstat < 5),      # ever enrolled
           ps_ontatt = as.integer(x4fb16enrstat %in% c(1,3))) |> # ontime enrollment
    ## combine race/ethnicity cats
    mutate(raceeth = case_when(
               x1race == 1 ~ 1,         # Amer.Ind.
               x1race %in% c(2,7) ~ 2,  # AsianPI
               x1race == 3 ~ 3,         # Black
               x1race %in% c(4,5) ~ 4,  # Hispanic
               x1race == 6 ~ 5,         # Multiracial,
               x1race == 8 ~ 6,         # White,
               x1race < 0 ~ 7)          # <missing>
           ) |>
    ## adjust gender cats
    mutate(gender = case_when(
               x1sex == 1 ~ 0,          # male
               x1sex == 2 ~ 1,          # female
               x1sex < 0 ~ 2)           # <missing>
           ) |>
    ## filter out missing gender (only 1)
    filter(gender != 2) |>
    ## rename x1region to cenreg for conformity
    rename(rg = x1region)

## need to add state X raceeth categorical variable
stan_state_raceeth <- df |>
    distinct(st, raceeth) |>
    arrange(st, raceeth) |>
    mutate(stra = row_number())

## save crosswalk
write_csv(x = stan_state_raceeth,
          file = file.path(crw_dir, "hsl_stra_cw.csv"))

## join back in
df <- left_join(df, stan_state_raceeth, by = c("st", "raceeth"))

## need to add state X income categorical variable
stan_state_inc <- df |>
    distinct(st, x1famincome) |>
    arrange(st, x1famincome) |>
    mutate(stfi = row_number())

## save crosswalk
write_csv(x = stan_state_inc,
          file = file.path(crw_dir, "hsl_stfi_cw.csv"))

## join back in
df <- left_join(df, stan_state_inc, by = c("st", "x1famincome")) |>
    rename(inc_cat = x1famincome)

## -----------------------------------------------------------------------------
## save main file
## -----------------------------------------------------------------------------

saveRDS(df, file.path(cln_dir, "hsl.RDS"))

## -----------------------------------------------------------------------------
## read in second-level data
## -----------------------------------------------------------------------------

## second level data
sl <- read_csv(file.path(dat_dir, "state_level.csv")) |>
    left_join(crosswalkr::stcrosswalk,
              by = c("state" = "stname")) |>
    select(stfips, ends_with("2013")) |>
    left_join(stan_state, by = c("stfips" = "x1state")) |>
    mutate(across(ends_with("2013"), ~ scale(.x))) |>
    select(stfips, st, everything()) |>
    filter(!is.na(st))

## -----------------------------------------------------------------------------
## save second level file
## -----------------------------------------------------------------------------

saveRDS(sl, file.path(cln_dir, "hsl_sl.RDS"))

## -----------------------------------------------------------------------------
## read in replicate weights for check
## -----------------------------------------------------------------------------

df_brr <- read_dta(file.path(dat_dir, "hsls_weights.dta")) |>
    rename(stuid = stu_id) |>
    mutate(stuid = as.integer(stuid)) |>
    left_join(df, by = "stuid")

## -----------------------------------------------------------------------------
## save replicate weight file
## -----------------------------------------------------------------------------

saveRDS(df_brr, file.path(cln_dir, "hsl_weights.RDS"))

## -----------------------------------------------------------------------------
## end script
################################################################################


