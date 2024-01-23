################################################################################
##
## [ PROJ ] Low-income enrollment across states with MRP
## [ FILE ] make_aux_data.R
## [ AUTH ] Benjamin Skinner (@btskinner)
## [ INIT ] 10 May 2023
##
################################################################################

## libraries
libs <- c("tidyverse", "tidycensus", "haven", "rscorecard", "crosswalkr",
          "httr", "readxl", "distRcpp")
sapply(libs, require, character.only = TRUE)

## file paths
dat_dir <- file.path("..", "..", "data")
cln_dir <- file.path(dat_dir, "clean")
raw_dir <- file.path(dat_dir, "raw")

## macros
m2miles <- 0.000621371

## -----------------------------------------------------------------------------
## check for data files and download if not there
## -----------------------------------------------------------------------------

## split base part of url from file name so we can use the file name to save
urls <- list(c("https://www.bls.gov/lau/", "laucnty13.xlsx"),
             c("https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/",
               "CenPop2010_Mean_BG.txt"))

## walk through url list
walk(urls,
     ~ {
       if (!file.exists(file.path(raw_dir, .x[[2]]))) {
         ## NB: using HTTR b/c BLS wants user agent
         GET(url = paste(.x, collapse = ""),
             user_agent("Mozilla/5.0"),
             write_disk(file.path(raw_dir, .x[[2]]), overwrite = TRUE))
       }
     }
     )

## -----------------------------------------------------------------------------
## make HSLS weight data
## -----------------------------------------------------------------------------

df <- read_dta(file.path(raw_dir, "hsls_17_student_pets_sr_v1_0.dta"),
               col_select = c("STU_ID",
                              starts_with("W1STUDENT"),
                              starts_with("W4W1STU"))) |>
  rename_all(tolower)

## save
saveRDS(df, file.path(cln_dir, "hsls_weights.RDS"))

## -----------------------------------------------------------------------------
## make second level state data
## -----------------------------------------------------------------------------

## ---------------------------
## college scorecard pull
## ---------------------------

df_cs <- sc_init() |>
  sc_year(2014) |>
  sc_filter(control == 1) |>
  sc_select(unitid, instnm, control, ccbasic, st_fips, tuitionfee_in,
            longitude, latitude) |>
  sc_get() |>
  rename(stfips = st_fips) |>
  filter(ccbasic %in% c(1:9,14:23),
         stfips <= 56) |>
  mutate(twoyr = ifelse(ccbasic %in% c(1:9,14,23), 1, 0)) |>
  left_join(stcrosswalk |> select(stfips, stname),
            by = "stfips")

## ---------------------------
## 2yr percentage
## ---------------------------

df_twoyr_pct <- df_cs |>
  group_by(stfips) |>
  summarise(twoyr_pct_2013 = round(mean(twoyr) * 100, 2))

## ---------------------------
## 4yr in-state tuition/fees
## ---------------------------

df_fourtuit <- df_cs |>
  filter(twoyr == 0) |>
  group_by(stfips) |>
  summarise(tuition_2013 = round(mean(tuitionfee_in, na.rm = TRUE)))

## ---------------------------
## distances
## ---------------------------

## read in census block groups
df_cb <- read_csv(file.path(raw_dir, "CenPop2010_Mean_BG.txt"),
                  show_col_types = FALSE) |>
  rename_all(tolower) |>
  mutate(stfips = as.integer(statefp)) |>
  filter(stfips <= 56) |>
  select(stfips, pop = population, lon = longitude, lat = latitude)

## get two-year school locations
df_ty <- df_cs |>
  filter(twoyr == 1) |>
  select(stfips, lon = longitude, lat = latitude)

## (1) compute in-state distances between census block groups and 2yrs
## (2) compute sum distances^1 for each CBG
## (3) compute population weighted mean for state
df_dist <- map(df_cb |> distinct(stfips) |> pull(),
               ~ {
                 x <- df_cb |> filter(stfips == .x)
                 if (.x == 11) {
                   ## DC
                   y <- df_ty |> filter(stfips %in% c(24,51))
                 } else {
                   y <- df_ty |> filter(stfips == .x)
                 }
                 ## compute distances
                 distmat <- dist_mtom(xlon = x[["lon"]],
                                      xlat = x[["lat"]],
                                      ylon = y[["lon"]],
                                      ylat = y[["lat"]]) * m2miles
                 ## compute SUM 1/dist^2
                 ilsum <- rowSums(exp(-log(distmat)))
                 ## compute population weighted average
                 stdist <- weighted.mean(ilsum, x[["pop"]])
                 ## return tibble
                 tibble(stfips = .x,
                        stdist_2013 = stdist |> round(3))
               }
               ) |>
  bind_rows()

## ---------------------------
## tidycensus: BA+
## ---------------------------

df_tc <- get_acs(geography = "state",
                 year = 2013,
                 variables = "DP02_0067P",
                 survey = "acs1") |>
  rename_all(tolower) |>
  mutate(stfips = as.integer(geoid)) |>
  filter(stfips <= 56) |>
  select(stfips, ba_pct_2013 = estimate)

## ---------------------------
## bls: unemployment
## ---------------------------

df_ue <- read_xlsx(file.path(raw_dir, "laucnty13.xlsx"),
                   skip = 5,
                   col_names = c("lauscode","stfips","countyfips",
                                 "countynm", "year","x","lf","emp",
                                 "unemp_n","unemp_pct")) |>
  filter(!is.na(stfips), stfips <= 56) |>
  mutate(stfips = as.integer(stfips)) |>
  group_by(stfips) |>
  summarise(unemp_pct_2013 = round(weighted.mean(unemp_pct, lf),2)) |>
  select(stfips, unemp_pct_2013)

## ---------------------------
## combine and save
## ---------------------------

df_sl <- list(df_tc, df_twoyr_pct, df_fourtuit, df_ue, df_dist) |>
  reduce(left_join, by = "stfips")

write_csv(df_sl, file.path(raw_dir, "state_level.csv"))

## =============================================================================
## END SCRIPT
################################################################################
