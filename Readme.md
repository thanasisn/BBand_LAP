
# Broad Band LAP

Developed in the Laboratory of Atmospheric Physics of Thessaloniki, Greece.

To process the data from broadband instruments of LAP.

Some plots and reports should be found [here](https://thanasisn.github.io/).

This is partial used in operational procedures ([github.com/thanasisn/CS_id](https://github.com/thanasisn/CS_id) is still in use).


## Table of Contents

<!--ts-->


<!-- Created by https://github.com/ekalinin/github-markdown-toc -->
<!-- Added by: athan, at: Fri Jan  3 03:22:55 UTC 2025 -->

<!--te-->


## Data status overview




| Name             |     Rows | Vars |    Values |      Size |   Fill | Bytes/Value |
|:-----------------|---------:|-----:|----------:|----------:|-------:|------------:|
| BBDB             | 16833108 |   81 | 533599706 |   2.6 GiB | 39.14% |        5.33 |
| BBDB meta        |    11693 |   82 |    504804 |   2.2 MiB | 52.65% |        4.52 |
| TrackerDB        |  8751788 |   23 |  78692271 | 168.9 MiB | 39.09% |        2.25 |
| TrackerDB meta   |     3198 |    9 |     28030 | 368.0 KiB | 97.39% |       13.44 |
| Raw files hashes |   812387 |    4 |   3249548 |   4.1 MiB |   100% |        1.32 |
| **Total**        | 26412174 |  199 | 616074359 |   2.8 GiB |    NA% |        4.92 |

Table: Datasets sizes on 2025-01-03



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


### Other processes

- Quality Check of radiation data (QCRad)   
  - Flags data using mainly the algorithm of C. N. Long and Y. Shi (2006)
- Imports data from [github.com/thanasisn/TSI](https://github.com/thanasisn/TSI)
  - `Sun_Dist_Astropy` Sun - LAP distance
  - `TSI_TOA`          TSI at TOA at LAP
  - `TSI_1au`          TSI 
  - `TSI_source`       TSI data source
- Imports atmospheric pressure data from proxies
  - `Pressure`         Atmospheric pressure at LAP
  - `Pressure_source`  Data source
- Keeps an `md5sum` of all input files to check for bit rot and other data corruption.


### Tools

- `inspect_days_DB.R`  interactive plot of some data in the DB
- `inspect_days_Lap.R` interactive plot of some data from source files 
- `inspect_days_Lap_sirena.R` interactive plot of some data from source files 


## TODO

- Fully port all to duckdb
- Replace and compare processes from "CM_21_GLB"
  - All the major stages have been replaced
  - Secondary processes are to be ported
- Process more instruments
- Import libRadtran data
- May import CSid
- Import other references

----------------------

# Details

## Development and Design

Some aspects on the implementation of this project.

- We use a dataset of `parquet` files as a database for all measurements and additional data.
- We are migrating the original parquet dataset scheme to `Duckdb` to improve overall
  efficiency.
- The `parquet` dataset use one file for each month, this facilitates:
  - Syncing of the data between different computers.
  - Partial processing when needed without using the dataset function.
- It should be easy to migrate to a pure database like `duckdb` or `sqlite`.
- There are some files with extra meta data for the data in the database and the analysis performed.
- We use features of the `arrow` library, and also `data.table` when it is more suitable or clear to code.
- The analysis should be able to be performed with under 8Gb of RAM, but is not assured.
- There is a trade-of with the disk usage/wearing, especially when starting from scratch.
- New data should be easy to be added on daily base on all levels.
- New process and analysis should be easy to added for all data.
- Goal to become a framework for all broadband instruments data analysis and manipulation.

## Documentation and usage

There is no centralized documentation for the project. Although you can refer to:

- `Readme.md` or other markdown files for a relevant overview   
- Summary notes on the start of each script
- Comments inside each script
- Compiled reports from each script


