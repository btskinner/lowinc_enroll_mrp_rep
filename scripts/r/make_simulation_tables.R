################################################################################
##
## [ PROJ ] Low-income enrollment across states with MRP
## [ FILE ] make_simulation_tables.R
## [ AUTH ] Benjamin Skinner (@btskinner)
## [ INIT ] 13 September 2023
##
################################################################################

## libraries
libs <- c("tidyverse", "crosswalkr", "xtable")
sapply(libs, require, character.only = TRUE)

## file paths
dat_dir <- file.path("..", "..", "data")
sim_dir <- file.path(dat_dir, "simulation")
tab_dir <- file.path("..", "..", "tables")

## read in macros
source("macros.R")

## function
std_err_mean <- function(x) sd(x) / sqrt(length(x))

## -----------------------------------------------------------------------------
## macros for figures
## -----------------------------------------------------------------------------

## labels
samp_label <- c("srsl" = "Simple random sample (1%)",
                "srss" = "Simple random sample (0.1%)",
                "wrsl" = "Weighted random sample (1%)",
                "wrss" = "Weighted random sample (0.1%)")
tau_labeller <- as_labeller(c(n = "D == 0", y = "D == 1"),
                            default = label_parsed)

## -----------------------------------------------------------------------------
## read in data
## -----------------------------------------------------------------------------

## population
pop <- readRDS(file.path(sim_dir, "sim_population.RDS")) |>
  mutate(t = factor(t, levels = c(1,0)))

## sample data
df_sam <- grep("sim_w|sim_s",
               list.files(sim_dir, full.names = TRUE),
               value = TRUE) |>
  map(~ readRDS(.x) |>
        mutate(sam = gsub("^.+_([a-z]+)\\.RDS$", "\\1", .x),
               t = factor(t, levels = c(1,0)))) |>
  bind_rows()

## estimates
df_est <- grep("ps",
               list.files(sim_dir, full.names = TRUE),
               value = TRUE) |>
  map(~ readRDS(.x) |>
        mutate(sam = gsub("^.+_([a-z]+)\\.RDS$", "\\1", .x),
               lev = gsub("^.+sim_ps_([a-z]+)_.+\\.RDS$", "\\1", .x),
               t = factor(t, levels = c(1,0)))) |>
  bind_rows() |>
  filter(lev == "state")

## -----------------------------------------------------------------------------
## national population differences
## -----------------------------------------------------------------------------

## make table data
tab_df <- pop |>
  mutate(y = y * 100) |>
  group_by(t) |>
  summarise(se = std_err_mean(y),
            y = mean(y),
            .groups = "drop") |>
  mutate(sam = "pop",
         weight = "n") |>
  bind_rows(df_sam |>
              mutate(y = y * 100) |>
              group_by(sam, t) |>
              summarise(se = std_err_mean(y),
                        y = mean(y),
                        .groups = "drop") |>
              mutate(weight = "n")) |>
  bind_rows(df_sam |>
              mutate(y = y * 100) |>
              filter(grepl("wrss|wrsl", sam)) |>
              group_by(sam, t) |>
              summarise(se = sqrt(Hmisc::wtd.var(y, 1/w, normwt = TRUE)) / sqrt(n()),
                        y = Hmisc::wtd.mean(y, 1/w, normwt = TRUE),
                        .groups = "drop") |>
              mutate(weight = "y")) |>
  mutate(t = factor(t,
                    levels = c(0,1),
                    labels = c("n","y")),
         sam = paste(sam, weight, sep = "_"),
         lab = factor(sam,
                      levels = c("pop_n",
                                 "srsl_n",
                                 "srss_n",
                                 "wrsl_n",
                                 "wrss_n",
                                 "wrsl_y",
                                 "wrss_y"),
                      labels = c("Population",
                                 "Simple random sample (1\\%)",
                                 "Simple random sample (0.1\\%)",
                                 "Weighted random sample (1\\%) w/o weights",
                                 "Weighted random sample (0.1\\%) w/o weights",
                                 "Weighted random sample (1\\%) w/ weights",
                                 "Weighted random sample (0.1\\%) w/ weights"))) |>
  select(-weight, -sam) |>
  pivot_longer(cols = c("y", "se"),
               names_to = "stat",
               values_to = "vals") |>
  pivot_wider(names_from = "t",
              values_from = "vals") |>
  mutate(y = round(y, 2),
         n = round(n, 2)) |>
  select(lab, n, y)

content <- tab_df |>
  as.matrix()

content[seq(2,14,2), 1] <- ""
content[seq(2,14,2), 2:3] <- add_paren(content[seq(2,14,2), 2:3] |> trimws())

notes <- c("From simulated data, national values of $\\Theta$ compared to ",
           "observed values of $\\theta$ by $D \\in {0, 1}$. ",
           "Samples of 1\\% and 0.1\\% have $N = 10{,}000$ and $N = 1{,}000$ ",
           "observations, respectively. Weighted random samples oversample ",
           "some subpopulations and estimates are presented without sampling ",
           "weights. All estimates were computed as simple mean statistics.")

head <- c(
  "\\begin{table}",
  "\\centering",
  "\\caption{National values of $\\Theta$ from simulated data}",
  "\\label{tbl:simpop}",
  "\\begin{tabularx}{\\linewidth}{Xcc}",
  "\\toprule",
  "&$D=0$&$D=1$\\\\"
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

writeLines(c(head, contents, foot), con = file.path(tab_dir, "simpop.tex"))

## -----------------------------------------------------------------------------
## state population/sample differences
## -----------------------------------------------------------------------------

## make table data
tab_df <- bind_rows(pop |>
                      group_by(state, t) |>
                      summarise(y = mean(y) * 100,
                                .groups = "drop") |>
                      mutate(sam = "pop"),
                    df_sam |>
                      group_by(sam, state, t) |>
                      summarise(y = mean(y) * 100,
                                .groups = "drop")
                    ) |>
  mutate(y = round(y,2)) |>
  pivot_wider(names_from = "sam",
              values_from = "y") |>
  pivot_wider(names_from = "t",
              values_from = pop:wrss) |>
  select(state, pop_0, pop_1, srsl_0, srsl_1, srss_0, srss_1, wrsl_0, wrsl_1,
         wrss_0, wrss_1)

content <- cbind(tab_df[1:26,], rbind(tab_df[27:51,], ""))

notes <- c("{\\itshape SRS}: simple random sample; {\\itshape WRS}: ",
           "weighted random sample. ",
           "From simulated data, state-specific values of $\\Theta_{state}$ ",
           "(Population) compared to observed values of $\\theta_{state}$ ",
           "(Samples) by $D \\in {0,1}$. Samples of 1\\% and 0.1\\% have ",
           "$N = 10{,}000$ and $N = 1{,}000$ observations, respectively. ",
           "Weighted random samples are presented without weighting adjustment. ",
           "Values with hyphen represent samples in which no observations were ",
           "observed.")

head <- c(
  "\\begin{sidewaystable}",
  "\\tiny",
  "\\centering",
  "\\caption{State-specific samples from simulated population}",
  "\\label{tbl:simsampop}",
  "\\begin{tabularx}{\\linewidth}{Xcccccccccc|Xcccccccccc}",
  "\\toprule",
  paste0("&\\multicolumn{2}{c}{Pop}",
         "&\\multicolumn{2}{c}{SRS (1\\%)}&\\multicolumn{2}{c}{SRS (0.1\\%)}",
         "&\\multicolumn{2}{c}{WRS (1\\%)}&\\multicolumn{2}{c}{WRS (0.1\\%)}",
         "&&\\multicolumn{2}{c}{Pop}",
         "&\\multicolumn{2}{c}{SRS (1\\%)}&\\multicolumn{2}{c}{SRS (0.1\\%)}",
         "&\\multicolumn{2}{c}{WRS (1\\%)}&\\multicolumn{2}{c}{WRS (0.1\\%)}\\\\"),
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}\\cmidrule(lr){8-9}",
  "\\cmidrule(lr){10-11}\\cmidrule(lr){13-14}\\cmidrule(lr){16-16}",
  "\\cmidrule(lr){17-18}\\cmidrule(lr){19-20}\\cmidrule(lr){21-22}",
  paste(paste(c(rep("&$D=0$&$D=1$", 5), "&", rep("&$D=0$&$D=1$", 5)), collapse = ""),
        "\\\\", collapse = "")
)

contents <- print(
  xtable(content),
  booktabs = TRUE,
  sanitize.text.function = function(x){x},
  include.colnames = FALSE,
  include.rownames = FALSE,
  only.contents = TRUE,
  print.results = FALSE,
  comment = FALSE,
  NA.string = "-")

foot <- c("\\multicolumn{22}{p{.98\\linewidth}}{\\footnotesize ",
          "{\\bfseries Notes.} ",
          notes,
          "}",
          "\\end{tabularx}",
          "\\end{sidewaystable}")

writeLines(c(head, contents, foot), con = file.path(tab_dir, "simsampop.tex"))

## =============================================================================
## END SCRIPT
################################################################################
