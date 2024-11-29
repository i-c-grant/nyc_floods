import requests
import pandas as pd
import folium 
from folium import plugins
import numpy as np
import geopandas as gpd

API_BASE = "https://api.dev.floodlabs.nyc/api/rest/"

import folium
from folium import plugins

def draw_markers(map, df):
    '''Draw sensor markers on a map.
    '''
    # Add markers for each deployment location
    for _, row in df.iterrows():
        # show different icon styles based on sensor data
        icon_kw = {}
        if row.deploy_type == 'coastal':
            icon_kw.update(dict(color='blue', icon='water'))
        else:
            icon_kw.update(dict(color='darkblue', icon='cloud-rain'))
        if not pd.isna(row.date_down):
            icon_kw['color'] = 'red'

        # draw marker
        folium.Marker(
            row.coordinates,
            icon=folium.Icon(prefix='fa', **icon_kw),
            tooltip=f"<b>{row['name']}</b>",
            popup=folium.Popup(
                f"<b style=\"font-size: 1.3em\">{row['name']}</b><br>"
                f"<b>Deploy Type</b>: {row.deploy_type}<br>"
                f"<b>Date Deployed</b>: {row.date_deployed.strftime('%b %d, %Y') if row.date_deployed else '--'}<br>"
                f"<b>Date Down</b>: {row.date_down.strftime('%b %d, %Y') if not pd.isna(row.date_down) else '--'}<br>"
                f"<b>Location</b>: {row.coordinates.tolist()}<br>"
            , max_width=200)
        ).add_to(map)

def create_point_map(gdf, popup_column=None, tooltip_column=None, 
                    cluster=True, zoom_start=10):
    """
    Create an interactive Folium map from a GeoDataFrame of points.
    
    Parameters:
    -----------
    gdf : GeoDataFrame
        Point geometry GeoDataFrame to plot
    popup_column : str, optional
        Column name to use for popups
    tooltip_column : str, optional
        Column name to use for tooltips
    cluster : bool, default True
        Whether to cluster nearby points
    zoom_start : int, default 10
        Initial zoom level
        
    Returns:
    --------
    folium.Map
        Interactive map object
    """
    # Ensure the GeoDataFrame is in WGS84 (standard lat/long)
    if gdf.crs != 'EPSG:4326':
        gdf = gdf.to_crs('EPSG:4326')
    
    # Calculate center of points
    center_lat = gdf.geometry.y.mean()
    center_lon = gdf.geometry.x.mean()
    
    # Create base map
    m = folium.Map(location=[center_lat, center_lon], 
                  zoom_start=zoom_start,
                  tiles='OpenStreetMap')
    
    if cluster:
        # Create a marker cluster layer
        marker_cluster = plugins.MarkerCluster().add_to(m)
    
    # Add points to map
    for idx, row in gdf.iterrows():
        # Prepare popup and tooltip content
        popup = str(row[popup_column]) if popup_column else None
        tooltip = str(row[tooltip_column]) if tooltip_column else None
        
        # Create marker
        folium.CircleMarker(
            location=[row.geometry.y, row.geometry.x],
            radius=8,
            popup=popup,
            tooltip=tooltip,
            color='red',
            fill=True,
            fill_color='red'
        ).add_to(marker_cluster if cluster else m)
    
    # Add layer control
    folium.LayerControl().add_to(m)
    
    return m

def floodnet_request(endpoint: str, **kwargs) -> dict:
    '''Make a request to the FloodNet API.

    Args:
        endpoint: The API endpoint to query.
        **kwargs: Additional parameters to pass to the API.

    Returns:
        A dictionary containing the API response.
    '''
    response = requests.get(API_BASE + endpoint, params=kwargs)
    if response.status_code != 200:
        raise RuntimeError(f"API request failed with status code {response.status_code}")
    return response.json()

def to_datetime_safe(df: pd.DataFrame, col: str) -> pd.Series:
    '''Convert a column of strings to datetime objects, but drop decimal seconds. '''
    dt_col: pd.Series = (
        (df[col]                
         # Drop fractional component ofseconds
         .str.replace(r'\.\d+(?=\s|$)', '', regex=True)
         .pipe(pd.to_datetime)
         .dt.tz_localize(tz='America/New_York'))
    )

    return dt_col

def get_deployments() -> gpd.GeoDataFrame:
    '''Retrieve a table of all sensor deployment locations.

    '''
    # query the API
    deployments = requests.get(API_BASE + "deployments/flood").json()
    if 'error' in deployments:
        raise RuntimeError(str(deployments))

    # create a pandas table
    df = pd.DataFrame(deployments['deployments'])

    # drop any sensors without a location (TODO: do this within the query)
    df = df.dropna(subset=['location'])

    # convert date strings to datetime objects
    # Truncate decimal seconds and convert to datetime with timezone
    df['date_deployed'] = to_datetime_safe(df, 'date_deployed')
    df['date_down'] = to_datetime_safe(df, 'date_down')

    # extract latitude, longitude pair
    df['coordinates'] = [np.array(x['coordinates'][::-1]) for x in df.location]

    # convert to geodataframe
    sensor_geom: gpd.GeoSeries = gpd.points_from_xy(df.coordinates.str[1],
                                                    df.coordinates.str[0])

    gdf = (gpd.GeoDataFrame(df,
                          geometry=sensor_geom,
                          crs='EPSG:4326')
           .to_crs('EPSG:2263'))

    return gdf

def draw_markers(map, df):
    '''Draw sensor markers on a map.
    '''
    # Add markers for each deployment location
    for _, row in df.iterrows():
        # show different icon styles based on sensor data
        icon_kw = {}
        if row.deploy_type == 'coastal':
            icon_kw.update(dict(color='blue', icon='water'))
        else:
            icon_kw.update(dict(color='darkblue', icon='cloud-rain'))
        if not pd.isna(row.date_down):
            icon_kw['color'] = 'red'

        # draw marker
        folium.Marker(
            row.coordinates,
            icon=folium.Icon(prefix='fa', **icon_kw),
            tooltip=f"<b>{row['name']}</b>",
            popup=folium.Popup(
                f"<b style=\"font-size: 1.3em\">{row['name']}</b><br>"
                f"<b>Deploy Type</b>: {row.deploy_type}<br>"
                f"<b>Date Deployed</b>: {row.date_deployed.strftime('%b %d, %Y')}<br>"
                f"<b>Date Down</b>: {row.date_down.strftime('%b %d, %Y') if not pd.isna(row.date_down) else '--'}<br>"
                f"<b>Location</b>: {row.coordinates.tolist()}<br>"
            , max_width=200)
        ).add_to(map)


# get the center of the map using the average of sensor coordinates
df_deployments = get_deployments()
map_center = df_deployments.coordinates.dropna().mean(0)

# let's create the map
map = folium.Map(location=map_center, zoom_start=11, tiles="CartoDB Positron", attr="CartoDB")
folium.plugins.ScrollZoomToggler().add_to(map)  # disables scroll-zoom by default

# draw the markers
draw_markers(map, df_deployments)

# Display the map
map

map.save('floodnet_map.html')
