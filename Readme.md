
# Broad Band LAP

Developed in the Laboratory of Atmospheric Physics of Thessaloniki, Greece.

To process the data from broadband instruments of LAP.

This is partial used as operational procedures.

Plots and reports can be found here: [thanasisn.netlify.app/3-data_display](https://thanasisn.netlify.app/3-data_display)


Table of Contents
=================

* [Broad Band LAP](#broad-band-lap)
   * [What it does](#what-it-does)
      * [For CHP-1](#for-chp-1)
      * [For CM-21](#for-cm-21)
      * [Other processes](#other-processes)
   * [TODO](#todo)
* [Details](#details)

<!-- Created by https://github.com/ekalinin/github-markdown-toc -->


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
  - Overview or Direct radiation measurements
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

- Import data from [github.com/thanasisn/TSI](https://github.com/thanasisn/TSI)
  - `Sun_Dist_Astropy` Sun - LAP distance
  - `TSI_TOA`          TSI at TOA at LAP
  - `TSI_1au`          TSI 
  - `TSI_source`       TSI data source
- Keeps an `md5sum` of all input files to check for bit rot and other data corruption.



## TODO

- Process more instruments
- Interactive plot of db variables
- Import libRadtran data
- Import pressure
- May import QCrad and CSid

----------------------

# Details

## Development and Design

Some aspects on the implementation of this project.

- We use a dataset of `parquet` files as a database for all measurements and additional data.
- There are some files with extra meta data for the data in the database and the analysis performed.
- It should be easy to migrate to a pure database like `duckdb` or `sqlite`.
- We use features of the `arrow` library, and also `data.table` when it is more suitable or clear to code.
- The analysis should be able to be performed with under 8Gb of RAM.
- There is a trade-of with the disk usage/wearing.
- New data should be easy to be added on daily base on all levels.
- New process and analysis should be easy to added for all data.


