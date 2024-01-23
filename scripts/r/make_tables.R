################################################################################
##
## [ PROJ ] Low-income enrollment across states with MRP
## [ FILE ] make_tables.R
## [ AUTH ] Benjamin Skinner (@btskinner)
## [ INIT ] 22 June 2023
##
################################################################################

## libraries
libs <- c("tidyverse", "crosswalkr", "xtable", "tidybayes", "rstan")
sapply(libs, require, character.only = TRUE)

## file paths
dat_dir <- file.path("..", "..", "data")
cln_dir <- file.path(dat_dir, "clean")
est_dir <- file.path("..", "..", "estimates")
tab_dir <- file.path("..", "..", "tables")

## read in macros
source("macros.R")

## -----------------------------------------------------------------------------
## read in data
## -----------------------------------------------------------------------------

## regression estimates
fit <- read_stan_csv(list.files(est_dir, "hsl_ps", full.names = TRUE)) |>
  tidy_draws()

## MRP estimates:
## b := bayes
## s := state
## q := quantile
## d := difference
## p := poststrat full distribution
df_sq <- readRDS(file.path(cln_dir, "ps_state.RDS"))
df_sd <- readRDS(file.path(cln_dir, "ps_state_diff.RDS"))
df_sp <- readRDS(file.path(cln_dir, "ps_state_post.RDS"))
df_sb <- readRDS(file.path(cln_dir, "state_estimates_bayes_weights.RDS"))

## -----------------------------------------------------------------------------
## estimates: regression terms
## -----------------------------------------------------------------------------

## ---------------------------
## first/second level
## ---------------------------

## get data together: first level stats
fl_tab <- fit |>
  select(starts_with("b_"), starts_with("a_ra")) |>
  pivot_longer(cols = everything(),
               names_to = "coef",
               values_to = "est") |>
  mutate(coef = add_param_labels(coef),
         coef = fl_params_to_factor(coef, table = TRUE)) |>
  group_by(coef) |>
  summarise(med = round(median(est), 2),
            lo95 = round(quantile(est, .025), 3),
            hi95 = round(quantile(est, .975), 3),
            .groups = "drop") |>
  mutate(med = sprintf("%#.02f", med),
         lo95 = sprintf("%#.02f", lo95),
         hi95 = sprintf("%#.02f", hi95),
         ci = add_sqbr(paste0(lo95, ",", hi95))) |>
  select(-lo95,-hi95)

## get data together: second level parameters
sl_tab <- fit |>
  select(starts_with("g")) |>
  pivot_longer(cols = everything(),
               names_to = "coef",
               values_to = "est") |>
  mutate(coef = add_param_labels(coef),
         coef = sl_params_to_factor(coef, table = TRUE)) |>
  group_by(coef) |>
  summarise(med = round(median(est), 2),
            lo95 = round(quantile(est, .025), 3),
            hi95 = round(quantile(est, .975), 3),
            .groups = "drop") |>
  mutate(med = sprintf("%#.02f", med),
         lo95 = sprintf("%#.02f", lo95),
         hi95 = sprintf("%#.02f", hi95),
         ci = add_sqbr(paste0(lo95, ",", hi95))) |>
  select(-lo95,-hi95)

## build core content
content <- rbind(cbind("First level","",""),
                 fl_tab |> as.matrix(),
                 cbind("Second level","",""),
                 sl_tab |> as.matrix())

content[c(1,10),1] <- add_bold(content[c(1,10),1])
content[c(2:9,11:15),1] <- add_hspace(content[c(2:9,11:15),1])

notes <- c("95\\% credible intervals ({\\itshape C.I.}) are computed at ",
           "$\\hat{\\theta}_{q2.5}$ and $\\hat{\\theta}_{q97.5}$.")

head <- c(
  "\\begin{table}",
  "\\centering",
  "\\caption{Summary of first and second level regression posterior distributions}",
  "\\label{tbl:regtabflsl}",
  "\\begin{tabularx}{\\linewidth}{Xcc}",
  "\\toprule",
  "&$\\hat{\\theta}_{q50}$&95\\% C.I.\\\\"
)

contents <- print(
  xtable(content),
  booktabs = TRUE,
  sanitize.text.function = function(x){x},
  include.colnames = FALSE,
  include.rownames = FALSE,
  only.contents = TRUE,
  print.results = FALSE,
  comment = FALSE)

foot <- c("\\multicolumn{3}{p{.98\\linewidth}}{\\footnotesize ",
          "{\\bfseries Notes.} ",
          notes,
          "}",
          "\\end{tabularx}",
          "\\end{table}")

writeLines(c(head, contents, foot), con = file.path(tab_dir, "regtabflsl.tex"))

## ---------------------------
## state level
## ---------------------------

## get data together
st_tab <- fit |>
  select("a", starts_with("a_rg"), starts_with("a_st")) |>
  pivot_longer(cols = everything(),
               names_to = "coef",
               values_to = "est") |>
  mutate(coef = add_param_labels(coef),
         coef = st_params_to_factor(coef)) |>
  group_by(coef) |>
  summarise(med = round(median(est), 2),
            lo95 = round(quantile(est, .025), 3),
            hi95 = round(quantile(est, .975), 3),
            .groups = "drop") |>
  mutate(med = sprintf("%#.02f", med),
         lo95 = sprintf("%#.02f", lo95),
         hi95 = sprintf("%#.02f", hi95),
         ci = add_sqbr(paste0(lo95, ",", hi95))) |>
  select(-lo95,-hi95) |>
  left_join(crosswalkr::stcrosswalk |> select(stabbr, stname),
            by = c("coef" = "stabbr")) |>
  mutate(coef = ifelse(!is.na(stname), stname, coef)) |>
  select(-stname)

## build core matrix
content <- cbind(
  rbind(st_tab |> filter(coef == "Constant") |> as.matrix(),
        cbind("Region","",""),
        st_tab |> filter(row_number() %in% 2:5) |> as.matrix(),
        cbind("State","",""),
        st_tab |> filter(coef %in% stname) |> as.matrix()),
  rbind(matrix("",6,3),
        cbind("State $\\times$ Low-income","",""),
        st_tab |> filter(grepl("_lo", coef)) |> as.matrix())
)

content[c(1,2,7),c(1,4)] <- add_bold(content[c(1,2,7),c(1,4)])
content[c(3:6,8:57),c(1,4)] <- add_hspace(content[c(3:6,8:57),c(1,4)])
content <- gsub(".*_lo", " $\\\\ldots\\\\times$ low income", content)

notes <- c("95\\% credible intervals ({\\itshape C.I.}) are computed at ",
           "$\\hat{\\theta}_{q2.5}$ and $\\hat{\\theta}_{q97.5}$.")

head <- c(
  "\\begin{table}",
  "\\scriptsize",
  "\\centering",
  "\\caption{Regional and state-specific regression posterior distributions}",
  "\\label{tbl:regtabst}",
  "\\begin{tabularx}{\\linewidth}{Xcc|Xcc}",
  "\\toprule",
  "&$\\hat{\\theta}_{q50}$&95\\% C.I.&&$\\hat{\\theta}_{q50}$&95\\% C.I.\\\\"
)

contents <- print(
  xtable(content),
  booktabs = TRUE,
  sanitize.text.function = function(x){x},
  include.colnames = FALSE,
  include.rownames = FALSE,
  only.contents = TRUE,
  print.results = FALSE,
  comment = FALSE)

foot <- c("\\multicolumn{6}{p{.98\\linewidth}}{\\scriptsize ",
          "{\\bfseries Notes.} ",
          notes,
          "}",
          "\\end{tabularx}",
          "\\end{table}")

writeLines(c(head, contents, foot), con = file.path(tab_dir, "regtabst.tex"))

## -----------------------------------------------------------------------------
## validation: 10 representative states
## -----------------------------------------------------------------------------

## get weighted estimates
df_stw <- df_sb |>
  ungroup() |>
  mutate(v = .epred * 100,
         est = case_when(
           w == "w1student_norm" ~ "Base (W1STUDENT)",
           w == "w4w1stu_norm" ~ "Longitudinal (W4W1STU)")
         ) |>
  left_join(stcrosswalk |> select(stfips, stname),
            by = c("x1state" = "stfips")) |>
  select(stname, lowinc, v, est) |>
  group_by(stname, lowinc, est) |>
  summarise(med = round(median(v), 2),
            lo95 = round(quantile(v, .025), 3),
            hi95 = round(quantile(v, .975), 3),
            .groups = "drop") |>
  mutate(med = sprintf("%#.02f", med),
         lo95 = sprintf("%#.02f", lo95),
         hi95 = sprintf("%#.02f", hi95),
         ci = add_sqbr(paste0(lo95, ",", hi95))) |>
  select(-lo95,-hi95)

## states to keep from MRP estimates
keep_states <- c(6,12,13,26,37,39,42,47,48,53)

## subset MRP estimates and set up
df_stmrp <- df_sp |>
  filter(sample == 1,
         fips %in% keep_states) |>
  mutate(v = v * 100,
         est = "MRP") |>
  select(stname, lowinc, v, est) |>
  group_by(stname, lowinc, est) |>
  summarise(med = round(median(v), 2),
            lo95 = round(quantile(v, .025), 3),
            hi95 = round(quantile(v, .975), 3),
            .groups = "drop") |>
  mutate(med = sprintf("%#.02f", med),
         lo95 = sprintf("%#.02f", lo95),
         hi95 = sprintf("%#.02f", hi95),
         ci = add_sqbr(paste0(lo95, ",", hi95))) |>
  select(-lo95,-hi95)

## bind
df_st <- bind_rows(df_stw, df_stmrp) |>
  mutate(lowinc = factor(lowinc,
                         levels = 1:0,
                         labels = c("Low income",
                                    "Mid-high income")),
         est = factor(est,
                      levels = c("MRP",
                                 "Base (W1STUDENT)",
                                 "Longitudinal (W4W1STU)"))) |>
  arrange(stname, lowinc, est)

## read in data
df_ov50 <- readRDS(file.path(cln_dir, "df_ov50_1.RDS")) |>
  mutate(est = ifelse(grepl("Base", est),
                      "Base (W1STUDENT)",
                      "Longitudinal (W4W1STU)"))

df_st <- df_st |>
  left_join(df_ov50, by = c("stname", "lowinc", "est")) |>
  mutate(ov = ifelse(is.na(ov), "-", ov),
         diff = ifelse(is.na(diff), "-", diff))

## drop redundant row values
df_st[c(2:6,8:12,14:18,20:24,26:30,32:36,38:42,44:48,50:54,56:60),1] <- ""
df_st[c(2,3,5,6,8,9,11,12,14,15,17,18,20,21,23,24,26,27,29,30,32,33,
        35,36,38,39,41,42,44,45,47,48,50,51,53,54,56,57,59,60),2] <- ""

notes <- c("Overlap values represent the proportion of the weighted posterior ",
           "distribution that overlaps with MRP estimates for the state. ",
           "$|\\Delta\\hat{\\theta}_{q50}$|: the absolute distance in percentage ",
           "points between the median of the weighted posterior distribution ",
           "and the median of the MRP posterior distribution.")

head <- c(
  "\\begin{table}",
  "\\scriptsize",
  "\\centering",
  "\\caption{Validation for 10 representative states}",
  "\\label{tbl:stval}",
  "\\begin{tabularx}{\\linewidth}{Xllcccc}",
  "\\toprule",
  "&&&$\\hat{\\theta}_{q50}$&95\\% C.I.&Overlap&$|\\Delta\\hat{\\theta}_{q50}$|\\\\"
)

contents <- print(
  xtable(df_st),
  booktabs = TRUE,
  sanitize.text.function = function(x){x},
  include.colnames = FALSE,
  include.rownames = FALSE,
  only.contents = TRUE,
  print.results = FALSE,
  comment = FALSE)

foot <- c("\\multicolumn{7}{p{.98\\linewidth}}{\\scriptsize ",
          "{\\bfseries Notes.} ",
          notes,
          "}",
          "\\end{tabularx}",
          "\\end{table}")

writeLines(c(head, contents, foot), con = file.path(tab_dir, "stval.tex"))

## -----------------------------------------------------------------------------
## coef lines states
## -----------------------------------------------------------------------------

df_sthilo <- df_sq |>
  filter(q %in% c(2.5, 50, 97.5), sample == 1) |>
  mutate(v = v * 100) |>
  select(stname, lowinc, q, v) |>
  pivot_wider(names_from = "q",
              values_from = "v") |>
  mutate(med = sprintf("%#.02f", `50`),
         lo95 = sprintf("%#.02f", `2.5`),
         hi95 = sprintf("%#.02f", `97.5`),
         ci = add_sqbr(paste0(lo95, ",", hi95))) |>
  select(stname, lowinc, med, ci) |>
  pivot_wider(names_from = "lowinc",
              values_from = c("med", "ci")) |>
  select(stname, med_1, ci_1, med_0, ci_0)

df_stdiff <- df_sd |>
  filter(sample == 1) |>
  mutate(v = v * 100,
         stname = ifelse(is.na(stname), "District of Columbia", stname)) |>
  select(-fips, -st) |>
  group_by(stname) |>
  summarise(med_diff = round(median(v), 2),
            lo95 = round(quantile(v, .025), 3),
            hi95 = round(quantile(v, .975), 3),
            .groups = "drop") |>
  mutate(med_diff = sprintf("%#.02f", med_diff),
         lo95 = sprintf("%#.02f", lo95),
         hi95 = sprintf("%#.02f", hi95),
         ci_diff = add_sqbr(paste0(lo95, ",", hi95))) |>
  select(-lo95,-hi95)

df_stcoef <- df_sthilo |>
  left_join(df_stdiff, by = "stname") |>
  arrange(stname)

notes <- c("95\\% credible intervals ({\\itshape C.I.}) are computed at ",
           "$\\hat{\\theta}_{q2.5}$ and $\\hat{\\theta}_{q97.5}$.")

head <- c(
  "\\begin{table}",
  "\\scriptsize",
  "\\centering",
  "\\caption{Poststratified estimates of college attendance by state}",
  "\\label{tbl:stcoef}",
  "\\begin{tabularx}{\\linewidth}{Xcccccc}",
  "\\toprule",
  paste0("&\\multicolumn{2}{c}{Low income}",
         "&\\multicolumn{2}{c}{Mid-high income}",
         "&\\multicolumn{2}{c}{$\\theta_{Mid/high} - \\theta_{Low}$} \\\\"),
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}",
  paste0("&",
         paste0(rep("$\\hat{\\theta}_{q50}$&95\\% C.I.", 3),
                collapse = "&"),
         "\\\\")
)

contents <- print(
  xtable(df_stcoef),
  booktabs = TRUE,
  sanitize.text.function = function(x){x},
  include.colnames = FALSE,
  include.rownames = FALSE,
  only.contents = TRUE,
  print.results = FALSE,
  comment = FALSE)

foot <- c("\\multicolumn{7}{p{.98\\linewidth}}{\\scriptsize ",
          "{\\bfseries Notes.} ",
          notes,
          "}",
          "\\end{tabularx}",
          "\\end{table}")

writeLines(c(head, contents, foot), con = file.path(tab_dir, "stcoef.tex"))

## =============================================================================
## END SCRIPT
################################################################################
