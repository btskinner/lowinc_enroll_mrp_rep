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

## inflation adjustment
cpi_adj <- read_csv(file.path(raw_dir, "USACPIALLAINMEI.csv"),
                    show_col_types = FALSE) |>
  rename_all(tolower) |>
  mutate(year = year(date),
         adj = usacpiallainmei / usacpiallainmei[year == 2009]) |>
  filter(year >= 2007 & year <= 2018) |>
  select(year, adj)

## -----------------------------------------------------------------------------
## poststratification data: munge and summarize
## -----------------------------------------------------------------------------

cols_to_select <- c("sample", "inctot", "ftotinc", "perwt", "sex", "age",
                    "race", "hispan", "statefip", "year", "multiyear")
## read in data
df <- read_dta(file.path(raw_dir, "acs.dta")) |>
  mutate(join_year = ifelse(!is.na(multyear), multyear, 2009)) |>
  left_join(cpi_adj, by = c("join_year" = "year")) |>
  select(-join_year)

## walk through all three samples: 1, 3, and 5 year
df_lo <- map(c(1,3,5),
             ~ df |>
               ## filter to 14/15 year-olds and sample
               filter(sample == case_when(
                 .x == 1 ~ 200901,
                 .x == 3 ~ 201003,
                 .x == 5 ~ 201105),
                 age %in% c(14,15)) |>
               ## convert 9999999 to NA
               mutate(across(c("inctot", "ftotinc"),
                             ~ convert_to_NA(.x, 9999999))) |>
               ## use personal income if family income is missing
               mutate(fincp = ifelse(is.na(ftotinc), inctot, ftotinc)) |>
               ## filter to those with income
               filter(!is.na(fincp)) |>
               ## munge data
               mutate(female = case_when(
                 sex == 1 ~ 0,
                 sex == 2 ~ 1),
                 raceeth = case_when(
                   race == 3 & hispan == 0 ~ 1,          # AmerInd
                   race %in% c(4,5,6) & hispan == 0 ~ 2, # AsianPI
                   race == 2 & hispan == 0 ~ 3,          # Black
                   hispan > 0 ~ 4,                       # Hispanic
                   race %in% c(7,8,9) & hispan == 0 ~ 5, # Multiracial
                   race == 1 & hispan == 0 ~ 6),         # White
                 lowinc = as.integer(fincp < 35000 * adj)) |>
               ## select, shortening names
               select(fips = statefip,
                      fe = female,
                      ra = raceeth,
                      lo = lowinc,
                      perwt) |>
               ## group
               group_by(fips, fe, ra, lo) |>
               ## get counts using person weights
               summarise(count = sum(perwt), .groups = "drop") |>
               ## ungroup
               ungroup() |>
               ## arrange
               arrange(fips, fe, ra, lo) |>
               ## add column for which sample
               mutate(sample = .x) |>
               ## put sample column first
               select(sample, everything())) |>
  ## bind together
  bind_rows()

## save
saveRDS(df_lo, file = file.path(cln_dir, "acs_lo_poststrat.RDS"))

## -----------------------------------------------------------------------------
## comparison data: munge
## -----------------------------------------------------------------------------

cols <- c("statefip", "age", "school", "year",
          "educd", "gradeattd", "inctot", "ftotinc", "perwt")

df <- read_dta(file.path(raw_dir, "usa_00016.dta"),
               col_select = all_of(cols)) |>
  ## HS diploma/GED, < AA deg, not in grad school, 18/19 in 2013
  filter(educd >= 62 & educd < 81 & gradeattd < 70 & age %in% c(18:19)) |>
  ## convert 9999999 to NA
  mutate(across(c("inctot", "ftotinc"), ~ convert_to_NA(.x, 9999999))) |>
  ## use personal income if family income is missing
  mutate(fincp = ifelse(is.na(ftotinc), inctot, ftotinc)) |>
  ## join cpi
  left_join(cpi_adj, by = "year") |>
  ## low income indicator to match 2009 with inflation adjustment
  mutate(lowinc = as.integer(fincp < 35000 * adj)) |>
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
