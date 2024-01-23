################################################################################
##
## [ PROJ ] Low-income enrollment across states with MRP
## [ FILE ] make_public_use.R
## [ AUTH ] Benjamin Skinner (@btskinner)
## [ INIT ] 20 October 2021
##
################################################################################

## libraries
libs <- c("tidyverse", "crosswalkr", "haven", "survey")
sapply(libs, require, character.only = TRUE)

## file paths
dat_dir <- file.path("..", "..", "data")
raw_dir <- file.path(dat_dir, "raw")
cln_dir <- file.path(dat_dir, "clean")

## -----------------------------------------------------------------------------
## HSLS
## -----------------------------------------------------------------------------

col_to_select <- c("STU_ID", "X1SEX", "X1RACE", "X1FAMINCOME", "X4FB16ENRSTAT",
                   "W4W1STU")

df_h <- read_dta(file.path(raw_dir, "hsls_17_student_pets_sr_v1_0.dta"),
                 col_select = c(all_of(col_to_select),
                                starts_with("w1student"),
                                starts_with("w4w1stu"))) |>
  ## lower names
  rename_all(tolower) |>
  ## drop if missing income
  filter(x1famincome > 0) |>
  ## add low income indicator
  mutate(lowinc = as.integer(x1famincome <= 2)) |>
  ## drop if missing PS attendance
  filter(x4fb16enrstat > 0) |>
  ## set on-time enrollment
  mutate(ps_ontatt = as.integer(x4fb16enrstat %in% c(1,3))) |>
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
  mutate(female = case_when(
           x1sex == 1 ~ 0,          # male
           x1sex == 2 ~ 1,          # female
           x1sex < 0 ~ 2)           # <missing>
         ) |>
  ## filter out missing gender (only 1)
  filter(female != 2,
         raceeth != 7)

## -----------------------------------------------------------------------------
## National weighted estimates
## -----------------------------------------------------------------------------

## set up survey data: base
svydat <- svrepdesign(weights = ~w1student, data = df_h,
                      repweights = "w1student[0-9]+", type = "BRR")

## compute weighted mean
df_e_base <- svyby(~ps_ontatt, ~lowinc, svydat, svymean, na.rm = T) |>
  as_tibble() |>
  mutate(weight = "base")

## set up survey data: longitudinal
svydat <- svrepdesign(weights = ~w4w1stu, data = df_h,
                      repweights = "w4w1stu[0-9]+", type = "BRR")

## computed weighted mean
df_e_long <- svyby(~ps_ontatt, ~lowinc, svydat, svymean, na.rm = T) |>
  as_tibble() |>
  mutate(weight = "long")

## combine
df_e <- bind_rows(
  df_e_base,
  df_e_long
)

## -----------------------------------------------------------------------------
## IPEDS: enrollment for 2013-2014 by income
## -----------------------------------------------------------------------------

## read in and join various IPEDS files that align with first year in college
## for HSLS (on-time)
df_i <- read_csv(file.path(raw_dir, "ipeds", "sfa1314_rv.csv"),
                 show_col_types = FALSE) |>
  rename_all(tolower) |>
  select(unitid, scugffn, grn4n12, upgrntp, scfa2) |>
  left_join(read_csv(file.path(raw_dir, "ipeds", "hd2014.csv"),
                     show_col_types = FALSE) |>
              rename_all(tolower) |>
              select(unitid, stabbr),
            by = "unitid") |>
  left_join(read_csv(file.path(raw_dir, "ipeds", "efia2014_rv.csv"),
                     show_col_types = FALSE) |>
              rename_all(tolower) |>
              select(unitid, efteug),
            by = "unitid")

## state average Pell percentage, weighted by FTE
df_i_pell <- df_i |>
  filter(!is.na(efteug)) |>
  group_by(stabbr) |>
  summarise(pct_pell = weighted.mean(upgrntp, efteug, na.rm = TRUE),
            .groups = "drop")

## state average pct of student in finaid cohort with 0-$30k family income,
## weighted by FTE
df_i_lo30 <- df_i |>
  filter(!is.na(grn4n12), !is.na(efteug)) |>
  mutate(pct_lo = grn4n12 / scugffn * 100) |>
  group_by(stabbr) |>
  summarise(pct_lo_mean = weighted.mean(pct_lo, efteug, na.rm = TRUE),
            .groups = "drop")

## join both state-level measures
df_i <- left_join(df_i_pell, df_i_lo30, by = "stabbr") |>
  filter(stabbr %in% c(stcrosswalk |> pull(stabbr)))

## -----------------------------------------------------------------------------
## save
## -----------------------------------------------------------------------------

saveRDS(df_h, file.path(cln_dir, "pu_hsl.RDS"))
saveRDS(df_e, file.path(cln_dir, "national_estimates_w_error.RDS"))
saveRDS(df_i, file.path(cln_dir, "ipeds_1314_lowinc.RDS"))

## =============================================================================
## END SCRIPT
################################################################################
