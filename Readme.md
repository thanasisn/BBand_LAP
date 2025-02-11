
# Broad Band LAP

Developed in the Laboratory of Atmospheric Physics of Thessaloniki, Greece.

To process the data from broadband instruments of LAP.

Some plots and reports should be found in my personal site [here](https://thanasisn.github.io/)
under the [Data display](https://thanasisn.github.io/data_display.html) section.


## Table of Contents

<!--ts-->


<!-- Created by https://github.com/ekalinin/github-markdown-toc -->
<!-- Added by: athan, at: Mon Feb 10 05:40:23 UTC 2025 -->

<!--te-->


## Data status overview




| Name                  |     Rows |    Size |    Values | Vars |   Fill | Bytes/Value |
|:----------------------|---------:|--------:|----------:|-----:|-------:|------------:|
| Broad Band LAP duckdb | 16875786 | 4.3 GiB | 871142232 |  140 | 60.75% |        5.34 |
| Raw files hashes      |   927095 | 4.9 MiB |   3708380 |    4 |   100% |        1.39 |
| **Total**             | 17802881 | 4.3 GiB | 874850612 |  144 |    NA% |        5.32 |

Table: Datasets sizes on 2025-02-10



## What it does

### For CHP-1

- Digest raw data
  - Signal from CHP-1
  - Tracker "async" 
  - CHP-1 internal temperature from thermistor
- Bad data ranges flagging
  - From manual set execution ranges
  - From acquisition signal physical limits
- Converts signal to radiation
  - Computes temperature correction when possible
- Plots
  - Overview of Clean/Dirty signal
  - Daily signal with and without dark
  - Overview of Direct radiation measurements
  - Daily Direct radiation measurements


### For CM-21

- Digest raw data
  - Signal from CHP-1
- Bad data ranges flagging
  - From manual set execution ranges
  - From acquisition signal physical limits
- Converts signal to radiation
- Plots
  - Overview of Clean/Dirty signal
  - Daily signal with and without dark


### For CHP-1 tracker

 - Digest async and step files for later analysis 


### Other processes

- Quality Check of radiation data (QCRad)   
  - Flags data using mainly the algorithm of C. N. Long and Y. Shi (2006)
- Clear sky identification (CSid)
  - Flags data as affected by clouds or not with the algorithm of M. J. Reno and C.
    W. Hansen (2016)
- Creates TSI data used in analysis
- Imports atmospheric pressure data from proxies
- Keeps an `md5sum` of all input files to check for bit rot and other data corruption.


### Tools

- `inspect_days_duckdb.R`  interactive plot of some data in the duckdb
- `inspect_days_DB.R`  interactive plot of some data in the DB
- `inspect_days_Lap.R` interactive plot of some data from source files 
- `inspect_days_Lap_sirena.R` interactive plot of some data from source files 


## TODO

- Process more instruments
- Import libRadtran data
- Improve CSid algorithm
- Import other references

----------------------

# Details

## Development and Design

Some aspects on the implementation of this project.

- We use a `duckdb` database for all measurements and additional data.
- There are some files with extra meta data for the data in the database and the
  analysis performed.
- We use features of the `duckdb` and `arrow` library, and also `dplyr` and
  `data.table` for data manipulations.
- The analysis should be able to be performed with under 8Gb of RAM, but this is not
  assured.
- There is a trade-of with the disk usage/wearing, especially when starting from
  scratch.
- New data should be easy to be added on daily base on all levels.
- New process and analysis should be easy to added for all data.
- This is intended as a framework for all broadband instruments data analysis and
  processing.

## Documentation and usage

There is no centralized documentation for the project. Although you can refer to:

- `Readme.md` or other markdown files for a relevant overview
- Summary notes on the start of each script
- Comments inside each script
- Compiled reports from each script


## Reproducibility

- We use the `renv` R package to keep track of the projects dependencies.
- Maybe will use a `nix-shell` environment as a more robust and portable method.

