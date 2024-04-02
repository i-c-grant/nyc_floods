import pyart
# import cartopy.crs as ccrs
import matplotlib.pyplot as plt
import tempfile as tmp
import os
import numpy as np
import wradlib as wrl
import xarray as xr
import xradar as xd
import datatree as xt
import matplotlib.pyplot as plt
import datetime as dt
import boto3
import botocore
import metpy
import metpy.io
import re
path = "s3://noaa-nexrad-level2/2022/03/22/KHGX/KHGX20220322_120125_V06"

# set PROJ_LIB environment variable to point to my own version
# should fix in conda env by downgrading proj
# conda has proj 9.3.1
# my machine has 8.2.1
os.environ["PROJ_LIB"] = "/usr/share/proj/"

def get_nexrad_filenames(datetime, station):
    """
    Get a list of NEXRAD filenames on a certain day for a certain station.

    Parameters
    ----------
    datetime : datetime
        The datetime indicating the day of the NEXRAD files to list.
    station : str
        The four-letter NEXRAD station code.

    Returns
    -------
    list
        A list of NEXRAD filenames.
    """
    s3 = boto3.client("s3", config=botocore.client.Config(signature_version=botocore.UNSIGNED))
    year = datetime.strftime("%Y")
    month = datetime.strftime("%m")
    day = datetime.strftime("%d")
    prefix = f"{year}/{month}/{day}/{station}"
    response = s3.list_objects_v2(Bucket="noaa-nexrad-level2", Prefix=prefix)
    return [obj["Key"] for obj in response["Contents"]]

def build_nexrad_path(filename):
    base = "s3://noaa-nexrad-level2"
    return f"{base}/{filename}"

def load_radar_data(path):
    # Load the radar data into memory
    radar = pyart.io.read_nexrad_archive(path)

    # Write the radar object to a temporary file as cfradial-1
    temp = tmp.NamedTemporaryFile()
    pyart.io.write_cfradial(temp.name, radar)

    # Read the radar object back into memory using xradar
    data = xd.io.open_cfradial1_datatree(temp.name)
    return data

def extract_sweeps(nexrad_datatree):
    ans = []
    for key in nexrad_datatree.keys():
        if re.match(r'^sweep_\d+$', key):
                ans.append(nexrad_datatree[key])
    return ans
    
def extract_reflectivity(sweeps):
    ans = []
    # Extract the reflectivity data
    for sweep in sweeps:
        if sweep.sweep_mode == "azimuth_surveillance":
            ans.append(sweep.data_vars["reflectivity"])
    return ans

query_time = dt.datetime(2024, 3, 22)

filenames = get_nexrad_filenames(query_time, "KDIX")
filenames = [f for f in filenames if not f.endswith("MDM")]

path = build_nexrad_path(filenames[-1])

test_data = load_radar_data(path)

# swp = test_data['sweep_0'].ds
# swp = swp.assign_coords(sweep_mode = swp.sweep_mode)
# swp = swp.wrl.georef.georeference()

# my_plot = swp.reflectivity.sortby("time").plot(x="range", y="time")
# plt.show()
# read the radar object back into memory, now with wradlib

# fig = plt.figure(figsize=(12, 4))
# display = pyart.graph.RadarMapDisplay(radar)
 
# ax = plt.subplot(121, projection=ccrs.PlateCarree())


# Assuming your DataArray is named 'da'
sweeps = extract_sweeps(test_data)
da = extract_reflectivity(sweeps)[0]
fig, ax = plt.subplots(subplot_kw={'projection': 'polar'})

# Convert azimuth to radians
azimuth_rad = np.deg2rad(da.azimuth)

# Create a meshgrid for the plot
r, theta = np.meshgrid(da.range, azimuth_rad)

# Plot the data
c = ax.pcolormesh(theta, r, da.values, cmap='viridis')

# Add a color bar
fig.colorbar(c, ax=ax, label='Reflectivity (dBZ)')

# Set the title and labels
ax.set_title('Radar Reflectivity')
ax.set_theta_zero_location("N")
ax.set_theta_direction(-1)

plt.show()