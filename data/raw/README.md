# Raw data

Data for this folder include:

**Top level**

1. `acs.dta`
1. `PGPH9/ASCII/f2student.dat`
1. `hsls_17_student_pets_sr_v1_0.dta`
1. `state_level.csv`
1. `CenPop2010_Mean_BG.txt`
1. `laucnty13.xlsx`
1. `USACPIALLAINMEI.csv`

File (1) must be created using the [IPUMS USA data
portal](https://usa.ipums.org/usa-action/variables/group) to select
the following variables:

- `YEAR`
- `MULTYEAR`
- `SAMPLE`
- `SERIAL`
- `CBSERIAL`
- `HHWT`
- `CLUSTER`
- `REGION`
- `STATEFIP`
- `STRATA`
- `GQ`
- `HHINCOME`
- `PERNUM`
- `PERWT`
- `SEX`
- `AGE`
- `RACE`
- `RACED`
- `HISPAN`
- `HISPAND`
- `INCTOT`
- `FTOTINC`

Cho0se three sample:

- `2009 ACS 1.0%`
- `2010 ACS 3yr 3.0%`
- `2011 ACS 5yr 5.0%`

Choose the Stata version (`*.dta`) version of the file and rename
`acs.dta`.

File (2) must be accessed and used in accordance with [NCES restricted-use data
product guidelines](https://nces.ed.gov/statprog/instruct.asp). File (3) can be
downloaded directly from the [NCES data
portal](https://nces.ed.gov/surveys/hsls09/). File (4) is included in the
repository, but can be built with `scripts/r/make_aux_data.R`. Files (5) and (6)
can be downloaded directly using `scripts/r/make_aux_data.R` or from the US
Census and Bureau of Labor Statistics, respectively:

5. https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/
6. https://www.bls.gov/lau/

File (7) can be downloaded from the FRED online data system.
