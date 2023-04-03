
# Broad Band LAP

Developed in the Laboratory of Atmospheric Physics of Thessaloniki, Greece.

To process the data from broadband instruments of LAP.


## What it does

### CHP-1

- Digest raw data
  - Signal from CHP-1
  - Tracker "async" 
  - CHP-1 internal temperature from thermistor
- Bad data ranges flagging
  - From manual set execution ranges
  - From acquisition signal physical limits
- Converts signal to radiation

### CM-21

- Digest raw data
  - Signal from CHP-1
- Bad data ranges flagging
  - From manual set execution ranges
  - From acquisition signal physical limits
- Converts signal to radiation


## TODO

- Process more instruments
- Interactive plot of db variables
- Import Astropy
- Import TSI
- Import libRadtran data
- Import pressure
- May import QCrad and CSid




