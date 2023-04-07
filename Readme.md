
# Broad Band LAP

Developed in the Laboratory of Atmospheric Physics of Thessaloniki, Greece.

To process the data from broadband instruments of LAP.

This is partial used as operational procedures.


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

We use a dataset of parquet files as a database for all measurements and additional data.
And some files with extra meta data for the database.
It should be easy to migrate to a pure database like `duckdb` or `sqlite`.


