#!/usr/bin/python3
# -*- coding: utf-8 -*-
"""
Wrapper for astropy sun vector calculation.

Copyright 2016 Athanasios Natsis <natsisthanasis@gmail.com>

@author:  Athanasios Natsis
@contact: natsisthanasis@gmail.com
@license: GPLv3
"""

# load necessary modules
import datetime
import warnings
import sys


# load parameters from file
sys.path.append('/home/athan/Aerosols/source_Python')
from param_location import Thessaloniki
# set the default location to be used
Location = Thessaloniki

# load astropy
from astropy             import units as u
from astropy.time        import Time
from astropy.coordinates import EarthLocation, AltAz, get_sun

# improves astropy accuracy by downloading resent data tables
from astropy.utils.data import download_file
from astropy.utils      import iers

try:
    iers.IERS.iers_table = iers.IERS_A.open(
                                download_file(iers.IERS_A_URL, cache=True))
    print("Astropy updated IERS tables from net")
except:
    print("Astropy fail to update IERS tables")


def sun_vector(date,
               lat         = Location.latitude,
               lon         = Location.longitude,
               height      = Location.elevation,
               pressure    = 1.013,
               temperature = Location.month_temp,
               rel_humid   = Location.month_humi):
    """Sun_vector function

    Calculates sun vector (azimuth, elevation, distance) relevant to a
    given Location. Input units are parsed by astropy. If no
    coordinates for a location are given on call, the Location is set
    from a configuration file as default.

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
            for performing refraction corrections. Setting this to 0
            (the default) will disable refraction calculations when
            transforming to/from this frame.

        temperature (float):
            The ground-level temperature as an Quantity in deg Celsius. This is
            necessary for performing refraction corrections.

        rel_humid (float):
            The relative humidity as a number from 0 to 1. This is necessary
            for performing refraction corrections.

    Returns:
        (sun_azimuth, sun_elevation, sun_distance) A tuple with
        the sun azimuth angle in decimal degrees from North,
        the sun elevation angle in decimal degrees from horizon
        sun distance in AU.


#    Example:
#        >>> sun_vector(date=datetime.datetime(2015, 12, 20, 7, 11, 56, 458476),
#                 lat=40.63,
#                 lon=22.95,
#                 height=63.23)
#        (134.6786085386902, 11.229746367471188, 0.9838363004498384)
    Note:
        Results may vary slightly due to updated tables

    Note:
        Currently there is no protection for wrong data types or invalid
        values.
    """

    ## time coordinate
    observer_time = Time(date)

    ## Create observer for Location coordinates
    observer = EarthLocation(lat    = lat * u.deg,
                             lon    = lon * u.deg,
                             height = height * u.m)

    ## atmospheric conditions
    pressure = pressure * u.bar
    #temp=
    #humid=

    ## calculate astronomy
    altazframe = AltAz( obstime           = observer_time,
                        location          = observer,
                        pressure          = pressure,
                        temperature       = temperature * u.deg_C,
                        relative_humidity = rel_humid)

    ## calculate sun position
    sunaltazs = get_sun(observer_time).transform_to(altazframe)

    #### FIXME use simpler format at production
    # float() not necessary is used to ensure compatibility between versions
    return (float(sunaltazs.az.degree),
            float(sunaltazs.alt.degree),
            float(sunaltazs.distance.au))



def sun_vector_2(date,
               lat         = Location.latitude,
               lon         = Location.longitude,
               height      = Location.elevation,
               pressure    = 1.013,
               temperature = Location.month_temp,
               rel_humid   = Location.month_humi):
    """Sun_vector function

    Calculates sun vector (azimuth, elevation, distance) relevant to a
    given Location. Input units are parsed by astropy. If no
    coordinates for a location are given on call, the Location is set
    from a configuration file as default.

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
            for performing refraction corrections. Setting this to 0
            (the default) will disable refraction calculations when
            transforming to/from this frame.

        temperature (float):
            The ground-level temperature as an Quantity in deg Celsius. This is
            necessary for performing refraction corrections.

        rel_humid (float):
            The relative humidity as a number from 0 to 1. This is necessary
            for performing refraction corrections.

    Returns:
        (sun_azimuth, sun_elevation, sun_distance) A tuple with
        the sun azimuth angle in decimal degrees from North,
        the sun elevation angle in decimal degrees from horizon
        sun distance in AU.


#    Example:
#        >>> sun_vector(date=datetime.datetime(2015, 12, 20, 7, 11, 56, 458476),
#                 lat=40.63,
#                 lon=22.95,
#                 height=63.23)
#        (134.6786085386902, 11.229746367471188, 0.9838363004498384)
    Note:
        Results may vary slightly due to updated tables

    Note:
        Currently there is no protection for wrong data types or invalid
        values.
    """

    ## time coordinate
    observer_time = Time(datetime.datetime.fromtimestamp(int(date)))

    ## Create observer for Location coordinates
    observer = EarthLocation(lat    = lat * u.deg,
                             lon    = lon * u.deg,
                             height = height * u.m)

    ## atmospheric conditions
    pressure = pressure * u.bar
    #temp=
    #humid=

    ## calculate astronomy
    altazframe = AltAz( obstime           = observer_time,
                        location          = observer,
                        pressure          = pressure,
                        temperature       = temperature * u.deg_C,
                        relative_humidity = rel_humid)

    ## calculate sun position
    sunaltazs = get_sun(observer_time).transform_to(altazframe)

    #### FIXME use simpler format at production
    # float() not necessary is used to ensure compatibility between versions
    return (float(sunaltazs.az.degree),
            float(sunaltazs.alt.degree),
            float(sunaltazs.distance.au))




# ## Test sun_vector function by comparing known results
# totest = sun_vector(date=datetime.datetime(2015, 12, 20, 7, 11, 56, 458476),
#                 lat=40.63,
#                 lon=22.95,
#                 height=63.23,
#                 pressure=1.013,
#                 temperature=15,
#                 rel_humid=0)
# testvalues = (134.6786085386902, 11.225400326740665, 0.9838363004498384)
# if totest == testvalues :
#     print("Astropy sun_vector seems working good.")
# else:
#     warnings.warn("sun_vector astropy function unexpected results")
#     print("Prediction: ", totest)
#     print("Test values:",testvalues)

# print("Now: ", sun_vector(datetime.datetime.utcnow()))

## run astropy self check
# import astropy
# print(astropy.test())
