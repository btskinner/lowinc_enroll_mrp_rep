################################################################################
##
## [ PROJ ] Low-income enrollment across states with MRP
## [ FILE ] make_acs.R
## [ AUTH ] Benjamin Skinner (@btskinner)
## [ INIT ] 15 January 2021
##
################################################################################

## libraries
libs <- c("tidyverse", "haven")
sapply(libs, require, character.only = TRUE)

## file paths
dat_dir <- file.path("..", "..", "data")
raw_dir <- file.path(dat_dir, "raw")
cln_dir <- file.path(dat_dir, "clean")

## function
convert_to_NA <- function(x, miss_vals) { ifelse(x %in% miss_vals, NA, x) }

## inflation rate from Sept 2009 to Sept 2013
## using: https://www.bls.gov/data/inflation_calculator.htm
inf_adj <- 1.08

## -----------------------------------------------------------------------------
## poststratification data: munge
## -----------------------------------------------------------------------------

df <- read_dta(file.path(raw_dir, "acs.dta")) |>
  ## filter to 14/15 in 2009
  filter(year == 2009 & age %in% c(14,15)) |>
  ## convert 9999999 to NA
  mutate(across(c("inctot", "ftotinc"), ~ convert_to_NA(.x, 9999999))) |>
  ## use personal income if family income is missing
  mutate(fincp = ifelse(is.na(ftotinc), inctot, ftotinc)) |>
  ## filter to those with income
  filter(!is.na(fincp)) |>
  ## munge data
  mutate(gender = case_when(
           sex == 1 ~ 0,
           sex == 2 ~ 1),
         raceeth = case_when(
           race == 3 & hispan == 0 ~ 1,          # AmerInd
           race %in% c(4,5,6) & hispan == 0 ~ 2, # AsianPI
           race == 2 & hispan == 0 ~ 3,          # Black
           hispan > 0 ~ 4,                       # Hispanic
           race %in% c(7,8,9) & hispan == 0 ~ 5, # Multiracial
           race == 1 & hispan == 0 ~ 6),         # White
         lowinc = as.integer(fincp < 35000)) |>
  ## select, shortening names
  select(fips = statefip,
         ge = gender,
         ra = raceeth,
         lo = lowinc,
         perwt)

## -----------------------------------------------------------------------------
## poststratification data: summarise
## -----------------------------------------------------------------------------

## low income binary
df_lo <- df |>
  ## group
  group_by(fips, ge, ra, lo) |>
  ## get counts using person weights
  summarise(count = sum(perwt), .groups = "drop") |>
  ## ungroup
  ungroup() |>
  ## arrange
  arrange(fips, ge, ra, lo)

## save
saveRDS(df_lo, file = file.path(cln_dir, "acs_lo_poststrat.RDS"))

## -----------------------------------------------------------------------------
## comparison data: munge
## -----------------------------------------------------------------------------

cols <- c("statefip", "age", "school",
          "educd", "gradeattd", "inctot", "ftotinc", "perwt")

df <- read_dta(file.path(raw_dir, "usa_00016.dta"),
               col_select = all_of(cols)) |>
  ## HS diploma/GED, < AA deg, not in grad school, 18/19 in 2013
  filter(educd >= 62 & educd < 81 & gradeattd < 70 & age %in% c(18:19)) |>
  ## convert 9999999 to NA
  mutate(across(c("inctot", "ftotinc"), ~ convert_to_NA(.x, 9999999))) |>
  ## use personal income if family income is missing
  mutate(fincp = ifelse(is.na(ftotinc), inctot, ftotinc)) |>
  ## low income indicator to match 2009 with inflation adjustment
  mutate(lowinc = as.integer(fincp < 35000 * inf_adj)) |>
  ## enrollment indicator == 1 if college UG
  mutate(enroll = as.integer(school == 2 & gradeattd == 60)) |>
  ## select, shortening names
  select(fips = statefip,
         lo = lowinc,
         enroll,
         perwt) |>
  ## group by state and low income status
  group_by(fips, lo) |>
  ## get weighted mean enrollment by state
  summarise(enroll_pct = weighted.mean(enroll, perwt) * 100,
            .groups = "drop")

## save
saveRDS(df, file = file.path(cln_dir, "acs_2013_enroll.RDS"))

## =============================================================================
## END
################################################################################
