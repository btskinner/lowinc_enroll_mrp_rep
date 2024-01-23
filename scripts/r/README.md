# R

This directory contains all R scripts. These include those used in the
primary analyses and in simulations.

**Primary**

Scripts to replicate the analyses should be called in the following
order:

1. `make_acs.R`
1. `make_aux_data.R`
1. `make_hsls.R`
1. `make_public_use.R`
1. `sens.R`
1. `analysis.R`
1. `predictions.R`
1. `poststratify.R`
1. `make_figures.R`
1. `make_tables.R`

All should be called using `scripts/r` as the working directory.

**Simulation**

Scripts to create simulation data and run simulations should be called
in the following order:

1. `simulation_data.R`
1. `simulation_analysis_models.R`
1. `make_simulation_figures.R`
1. `make_simulation_tables.R`

All should be called using `scripts/r` as the working directory.
