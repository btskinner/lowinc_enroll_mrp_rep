This repository contains the replication files for  

> Skinner, Benjamin T. and Doyle, William R. (forthcoming) Predicting postsecondary
> attendance by family income in the United States using multilevel
> regression with poststratification. _Economics of Educatoin Review_ Available at
> SSRN: http://dx.doi.org/10.2139/ssrn.3054231 


## Data

Follow the instructions in `./data/raw/README.md` to download all
publicly-available data and place it in the correct locations. 

You will also need access to an NCES restricted-use data set, which
must be obtained through the [National Center for Education
Statistics](https://nces.ed.gov/statprog/instruct.asp).

## To run

Clone the project repository and `cd` into the `scripts/r` project
subdirectory.

```bash
git clone https://github.com/btskinner/lowinc_enroll_mrp_rep.git
cd ./lowinc_enroll_mrp_rep/scripts/r
```

Follow the instructions in `./scripts/r/README.md` to replicate the
primary and simulated analyses.
