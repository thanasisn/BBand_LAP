# -*- coding: utf-8 -*-
"""
Wrapper for Pysolar sun vector calculation.
Use version 0.10 for python 3

Copyright 2022 Athanasios Natsis

@author:  Athanasios Natsis
@contact: natsisthanasis@gmail.com
@license: GPLv3
"""

import pysolar
import datetime
import sys

# load parameters from file
sys.path.append('/home/athan/Aerosols/source_Python')
from param_location import Thessaloniki
# set the default location to be used
Location = Thessaloniki

def sun_vector(date,
               lat         = Location.latitude,
               lon         = Location.longitude,
               height      = Location.elevation,
               pressure    = 1.013,
               temperature = Location.month_temp,
               rel_humid   = Location.month_humi):
    """Sun_vector function

    Calculates sun vector (azimuth, elevation) relevant to a
    given Location. If no coordinates for a location are given on call,
    the Location is set from a configuration file as default.

    Args:
        lat (float):
            Latitude in degrees (WCS).

        lon (float):
            Longitude in degrees (WCS).

        height (float):
            Location height in meters (WCS).

        date (datetime obj.):
            Date and time in UTC for the calculation

        pressure (float):
            Atmospheric pressure in bars at the location. This is necessary
            for performing refraction corrections.

        temperature (float):
            The ground-level temperature in deg Celsius. This is
            necessary for performing refraction corrections.

        rel_humid (float):
            This parameter is not available for Pysolar module.
            So it is not used here.

    Returns:
        (sun_azimuth, sun_elevation) A tuple with the sun azimuth angle
        in decimal degrees from North, the sun elevation angle in decimal
        degrees from the horizon.
    """

    ## pysolar needs timezone info
    date = date.replace(tzinfo = datetime.timezone.utc)
    ## pysolar needs kelvin
    temperature = temperature + 273.15
    ## pressure in Pascal (1 bar = 100000 pa)
    pressure = pressure * 100000.

    sun_alt = pysolar.solar.get_altitude(
                    latitude_deg  = lat,
                    longitude_deg = lon,
                    when          = date,
                    elevation     = height,
                    temperature   = temperature,
                    pressure      = pressure     )


    azz = pysolar.solar.get_azimuth(
                    latitude_deg  = lat,
                    longitude_deg = lon,
                    when          = date,
                    elevation     = height  )

    #sun_azi = 360 - (azz + 180) % 360
    sun_azi = azz

    return (sun_azi, sun_alt, None)


# def test_plot_pysolar():
#     """Generate plot of sun azimuth and elevation for today
#     """
# 
#     import matplotlib.pyplot as plt
#     import datetime
# 
#     today = datetime.datetime.utcnow().date()
#     today_start = datetime.datetime.combine(today, datetime.time())
# 
#     steps = 1500
# 
#     datelist = []
#     altlist  = []
#     azlist   = []
# 
#     timest = today_start
#     for mm in range(0, steps, 5):
#         timest = timest + datetime.timedelta(minutes=5)
# 
#         azi, zen, dist = sun_vector(date=timest)
# 
#         datelist.append(timest)
#         altlist.append(zen)
#         azlist.append(azi)
# 
# 
#     plt.plot(datelist,azlist)
#     plt.suptitle('Pysolar Azimuth ' + str(today) , fontsize=12, fontweight='bold')
#     plt.grid(True)
#     plt.tight_layout()
#     plt.show()
# 
#     plt.plot(datelist,altlist)
#     plt.suptitle('Pysolar Elevation ' + str(today) , fontsize=12, fontweight='bold')
#     plt.grid(True)
#     plt.tight_layout()
#     plt.show()
# 
#     plt.plot(azlist,altlist)
#     plt.show()




# test_plot_pysolar()

# import time
#import datetime
# while True:
    # a,b,c = sun_vector(datetime.datetime.utcnow())
    # print(a,b,c)
    # time.sleep(1)
