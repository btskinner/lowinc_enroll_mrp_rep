# Raw data

Data for this folder include:

**Top level**

1. `acs.dta`
1. `PGPH9/ASCII/f2student.dat`
1. `hsls_17_student_pets_sr_v1_0.dta`
1. `state_level.csv`

File (1) must be created using the [IPUMS USA data
portal](https://usa.ipums.org/usa-action/variables/group) to select
the following variables from the 2009 single year ACS sample:

- `year`
- `sample`
- `serial`
- `cbserial`
- `hhwt`
- `cluster`
- `region`
- `statefip`
- `strata`
- `gq`
- `hhincome`
- `pernum`
- `perwt`
- `sex`
- `age`
- `race`
- `raced`
- `hispan`
- `hispand`
- `inctot`
- `ftotinc`

Choose the Stata version (`*.dta`) version of the file and rename
`acs.dta`.

File (2) must be accessed and used in accordance with [NCES
restricted-use data product
guidelines](https://nces.ed.gov/statprog/instruct.asp). File (3) can
be downloaded directly from the [NCES data
portal](https://nces.ed.gov/surveys/hsls09/). File (4) is included in
the repository.
