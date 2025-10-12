
import math
from   datetime import datetime, timezone
import ephem

def moon_sky_parameters(date, lat, lon, height = 0):
    """
    Calculate elevation, azimuth, and lunar phase for given date and location.

    Parameters:
    - date:   datetime object (UTC timezone)
    - lat:    float, degrees North (+), South (-)
    - lon:    float, degrees East (+), West (-)
    - height: float, meters above sea level (default: 0)

    Returns:
    - dict: Contains sun/moon elevation, azimuth, and lunar phase
    """

    # Create observer
    observer = ephem.Observer()
    observer.lat = str(lat)
    observer.lon = str(lon)
    observer.elevation = height
    observer.date = date

    # Calculate Sun position
    sun = ephem.Sun()
    sun.compute(observer)

    # Calculate Moon position
    moon = ephem.Moon()
    moon.compute(observer)

    # Convert from radians to degrees
    sun_alt  = math.degrees(sun.alt)
    sun_az   = math.degrees(sun.az)
    moon_alt = math.degrees(moon.alt)
    moon_az  = math.degrees(moon.az)

    # Calculate lunar phase (0=new moon, 0.5=full moon, 1.0=new moon)
    lunar_phase = moon.phase / 100.0  # ephem gives phase as percentage

    # Get moon phase name
    if lunar_phase < 0.03 or lunar_phase > 0.97:
        phase_name = "New Moon"
    elif lunar_phase < 0.22:
        phase_name = "Waxing Crescent"
    elif lunar_phase < 0.28:
        phase_name = "First Quarter"
    elif lunar_phase < 0.47:
        phase_name = "Waxing Gibbous"
    elif lunar_phase < 0.53:
        phase_name = "Full Moon"
    elif lunar_phase < 0.72:
        phase_name = "Waning Gibbous"
    elif lunar_phase < 0.78:
        phase_name = "Last Quarter"
    else:
        phase_name = "Waning Crescent"

    return {
        'sun': {
            'elevation': sun_alt,
            'azimuth':   sun_az
        },
        'moon': {
            'elevation':  moon_alt,
            'azimuth':    moon_az,
            'phase':      lunar_phase,
            'phase_name': phase_name
        },
        'observer': {
            'latitude':  lat,
            'longitude': lon,
            'altitude':  height,
            'date_utc':  date.strftime('%Y-%m-%d %H:%M:%S UTC')
        }
    }

# Example usage
if __name__ == "__main__":
    date_utc  = datetime.now(timezone.utc)
    latitude  = 40.5954
    longitude = 22.9863
    altitude  = 10  # meters

    result = moon_sky_parameters(date_utc, latitude, longitude, altitude)

    print("")
    print("Sky Parameters:")
    print(f"Date:     {result['observer']['date_utc']}")
    print(f"Location: {result['observer']['latitude']:.4f}°, {result['observer']['longitude']:.4f}°")
    print(f"Altitude: {result['observer']['altitude']} m")
    print("\nSun:")
    print(f"  Elevation: {result['sun']['elevation']:.2f}°")
    print(f"  Azimuth:   {result['sun']['azimuth']:.2f}°")
    print("\nMoon:")
    print(f"  Elevation: {result['moon']['elevation']:.2f}°")
    print(f"  Azimuth:   {result['moon']['azimuth']:.2f}°")
    print(f"  Phase:     {result['moon']['phase']:.3f} ({result['moon']['phase_name']})")
    print("")
