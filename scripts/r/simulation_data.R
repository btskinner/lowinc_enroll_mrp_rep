################################################################################
##
## [ PROJ ] Low-income college enrollment prediction (MRP)
## [ FILE ] simulation_data.R
## [ AUTH ] Benjamin Skinner and Will Doyle
## [ INIT ] 16 December 2020
##
################################################################################

## libraries
libs <- c("tidyverse", "fabricatr")
sapply(libs, require, character.only = TRUE)

## directories
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path("..", ".."), args)
dat_dir <- file.path(root, "data")
sim_dir <- file.path(dat_dir, "simulation")

## seed
seed <- 20201216

## -----------------------------------------------------------------------------
## simulate population
## -----------------------------------------------------------------------------

## set seed
set.seed(seed)

## set up population proportions so that states are roughly proportional
pop_prop <- tibble(
    state = c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID",
              "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO",
              "MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA",
              "RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"),
    ## https://simple.wikipedia.org/wiki/List_of_U.S._states_by_population
    pprop = c(1.48,0.22,2.17,0.91,11.96,1.72,1.08,0.29,0.21,6.44,3.18,0.43,
              0.53,3.85,2.02,0.95,0.88,1.35,1.41,0.4,1.83,2.09,3.02,1.7,0.9,
              1.85,0.32,0.58,0.92,0.41,2.69,0.63,5.91,3.14,0.23,3.53,1.19,1.27,
              3.87,0.32,1.54,0.27,2.05,8.68,0.96,0.19,2.58,2.28,0.55,1.76,0.17)
) |>
    ## norming pop percentages to 1 (since we dropped non-states)
    mutate(pprop = pprop / sum(pprop)) |>
    ## setting c1 proportions, which should change by state
    ## (1) starting values, randomly adjusted
    ## (2) no negative values (pop proportion at lowest)
    mutate(p1 = pmax(0, .60 + rnorm(n())/10) + pprop,
           p2 = pmax(0, .12 + rnorm(n())/10) + pprop,
           p3 = pmax(0, .01 + rnorm(n())/10) + pprop,
           p4 = pmax(0, .06 + rnorm(n())/10) + pprop,
           p5 = pmax(0, .03 + rnorm(n())/10) + pprop,
           p6 = pmax(0, .18 + rnorm(n())/10) + pprop) |>
    ## random state noise for later differences in college attendance
    mutate(s1 = runif(n(), 0, .1) * sample(c(-1,1), size = n(), replace = TRUE))

## set up population from which we'll draw
pop <- crosswalkr::stcrosswalk |>
    ## get state abbreviations and regions from crosswalkr
    select(state = stabbr, region = cenregnm, rgnum = cenreg) |>
    ## set state numbers (since fips aren't contiguous)
    arrange(state) |>
    mutate(stnum = row_number()) |>
    ## join helper data from above
    left_join(pop_prop, by = "state") |>
    ## fabricate levels
    fabricate(
        ## 2nd-level states
        state = modify_level(
            ## average income within states (no cluster)
            ainc = runif(N, min = 10000, max = 50000),
            ## random noise for later income adjustment
            i1 = draw_normal_icc(clusters = region, ICC = 0.4),
            ## 2nd-level vars
            z1 = draw_normal_icc(clusters = region, ICC = 0.2),
            z2 = draw_normal_icc(clusters = region, ICC = 0.6),
            z3 = draw_binary_icc(clusters = region, ICC = 0.4)
        ),
        ## individual level
        id = add_level(
            ## each state pop is proportional using empirical proportions
            N = ceiling(1000000 * pprop),
            ## cat 1 should be roughly similar, but change across states
            c1 = draw_categorical(prob = cbind(p1, p2, p3, p4, p5, p6)),
            ## cat 2 should be roughly the same across all states
            c2 = draw_binary(prob = .51, N = N),
            ## income is bound at $0 and varies based on geo/cats
            x = pmax(0, rnorm(N,
                              mean = ainc +
                                  I(region == "Midwest") * -1000 +
                                  I(region == "Northeast") * 2000 +
                                  I(region == "West") * 1000 +
                                  I(region == "South") * -5000 +
                                  ## state-level adj, corr in region
                                  i1 * 1500 +
                                  ## group adj
                                  I(c1 == 1) * 5500 +
                                  I(c1 == 2) * -3000 +
                                  I(c1 == 3) * -1000 +
                                  I(c1 == 4) * 3000 +
                                  I(c1 == 5) * 1000 +
                                  I(c1 == 6) * -2000 +
                                  I(c2 == 1) * -1000,
                              sd = 10000)
                     ),
            ## low income indicator is determined by place in income dist
            t = ifelse(x < 22000, 1, 0),
            ## outcome has baseline adjusted by geo/cats/inc,
            ## with some random noise at the individual level
            y = draw_binary(
                ## linear combination latent parameter (mu)
                latent = 1 +
                    (-1.1 * t) +
                    ## lower/higher in South/Northeast...
                    (-.4 * I(region == "South")) +
                    (.4 * I(region == "Northeast")) +
                    ## ...less/more difference in South/Northeast
                    (.15 * t * I(region == "South")) +
                    (-.15 * t * I(region == "Northeast")) +
                    ## group-level changes
                    (.4 * I(c1 == 1)) +
                    (-.3 * I(c1 == 2)) +
                    (-.1 * I(c1 == 3)) +
                    (.2 * I(c1 == 4)) +
                    (.1 * I(c1 == 5)) +
                    (-.1 * I(c1 == 6)) +
                    (.5 * I(c2 == 1)) +
                    ## group-level interactions with t
                    (-.05 * t * I(c1 == 1)) +
                    (-.3 * t * I(c1 == 2)) +
                    (-.02 * t * I(c1 == 3)) +
                    (-.2 * t * I(c1 == 4)) +
                    (-.15 * t * I(c1 == 5)) +
                    (-.04 * t * I(c1 == 6)) +
                    (-.25 * t * I(c2 == 1)) +
                    ## 2nd level vars
                    -.1 * z1 +
                    .15 * z2 +
                    .1 * z3 +
                    ## state-level differences by t
                    -1 * abs(s1) * t +
                    ## individual-level noise
                    rnorm(N),
                N = N,
                ## logit link function
                link = "logit"
            )
        )
    ) |>
    ## convert to tibble
    tibble() |>
    ## drop temporary helper vars
    select(-starts_with("p"), s1) |>
    ## order variables
    select(id, stnum, state, c1, c2, x, t, y, region, rgnum, ainc, z1, z2, z3) |>
    ## create oversample weights: increasing means more likely to draw
    ## - oversample t overall
    ## - oversample small c1 groups (3, 4, and 5)
    ## - oversample the Northeast
    mutate(w = 1 +
               t * 10 +
               I(c1 %in% c(3:5)) * 10 +
               I(region == "Northeast") * 10,
           w = as.integer(w))

## -----------------------------------------------------------------------------
## draw samples
## -----------------------------------------------------------------------------

## set seed (again)
set.seed(seed)

## simple random sample
srsl <- sample_n(pop, 10000)
srss <- sample_n(pop, 1000)

## weighted sample
wrsl <- sample_n(pop, 10000, weight = w)
wrss <- sample_n(pop, 1000, weight = w)

## save
saveRDS(pop, file.path(sim_dir, "sim_population.RDS"))
saveRDS(srsl, file.path(sim_dir, "sim_srsl.RDS"))
saveRDS(wrsl, file.path(sim_dir, "sim_wrsl.RDS"))
saveRDS(srss, file.path(sim_dir, "sim_srss.RDS"))
saveRDS(wrss, file.path(sim_dir, "sim_wrss.RDS"))

## =============================================================================
## END SCRIPT
################################################################################
