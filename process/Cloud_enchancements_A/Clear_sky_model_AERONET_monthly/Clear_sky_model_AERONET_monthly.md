---
tags:      [ AERONET DNI GHI Libradtran AOD ]
scope:     work
created:   2024-01-26 15:17:55 +02:00
cunixtime: 1706275075
UTC:       2024-01-26 13:17:55 +00:00
---

## Clear sky model AERONET monthly

Run for monthly climatology from AERONET.

### Intention

Create a reference value of radiation for clear sky condition.  To be used as a base
for a better approximation or Clearness Index.  This should only create the
irradiance components, all other process will be elsewhere.

This should be done afterwards:

 - scale Kurudz to TSI.
 - adjust for day of year / sun distance
 - select summer/winter atmosphere file

### Perturbations of:

 - $\tau_{500}$
 - $\tau_{500} +/- 1\sigma$
 - $\tau_{500} +/- 2\sigma$ (this gives zero AOD on most months, are set to zero)
 - Precipitate water
 - Precipitate water +/- 1\sigma$
 - Precipitate water +/- 2\sigma$ (this gives zero AOD on most months, are set to zero)
 - Wavelength 280 - 2500 nm
 - SZA: 15 - 80

### Used

Paper for Cloud enhancement, 30 years climatological study.

