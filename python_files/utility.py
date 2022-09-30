# General Data  Tools
import os
import pandas as pd
import json

# For API Calls
import requests
import httplib2 as http

# String Processing Tools
import re

# Geospatial
import geopandas as gpd
import geojson
from fiona.drvsupport import supported_drivers
from shapely.geometry import Polygon, MultiPolygon, shape

# --------------------------- Data.gov Functions ---------------------------

def load_kml_file_as_df(path: str) -> pd.DataFrame:
    """
    Function to read KML Files downloaded from Data.gov.sg as DataFrame

    Parameters
    ----------
    path: str
        File Path of the KML File
    """
    supported_drivers['KML'] = 'rw'
    df = gpd.read_file(path, driver='KML')
    return df


def process_raw_df_from_kml(df: pd.DataFrame, *keys) -> pd.DataFrame:
    """
    Function to clean and process Raw DataFrames obtained from Data.gov's KML files

    Parameters
    ----------
    df: pd.DataFrame
        DataFrame derived from Data.gov's KML File

    keys: str
        Column Names (Raw Capitalise form) to be extracted from df's "Description" columns

    """
    master_dct = {}
    for key in keys:
        master_dct[key] = {}

    for idx, row in df.iterrows():
        text = row["Description"]

        re_tags_to_remove = [
            "<[/]*center>",
            "<[/]*table>",
            "<[/]*em>",
            "<[/]*th\s*[\w,\d,\#,\=,\",\',\s]*>",
            "<tr>Attributes</tr>",
            ","
        ]
        for regex in re_tags_to_remove:
            text = re.sub(regex, "", text)

        text = text.strip()
        text = re.sub(r"<tr\s*[\w,\d,\#,\=,\",\']*>", "<tr>", text)

        re_tags_to_replace = {
            "\s*<[/]*tr>\s*" : "",
            "\s*<td>\s*" : ":",
            "\s*</td>\s*" : ", ",
        }
        for old_regex, new_regex in re_tags_to_replace.items():
            text = re.sub(old_regex, new_regex, text)

        lst_of_attributes = text.split(", ")
        dct_of_attributes = {i.split(":")[0]:i.split(":")[1] for i in lst_of_attributes if i != ""}

        for key in dct_of_attributes.keys():
            master_dct[key][row["Name"]] = dct_of_attributes[key]
    
    for key in master_dct.keys():
        df[key] = df["Name"].map(master_dct[key])

    return df

# --------------------------- OneMap API Functions ---------------------------

def retrieve_from_onemap(token: str, path: str, method: str = 'GET', payload: dict = {}) -> pd.DataFrame:
    """
    Function to fetch data from OneMap API

    Parameters
    ----------
    token: str
        API Token

    path: str
        The end-point of the API request to be sent (e.g. privateapi/popapi/getPlanningareaNames)

    method: str, optional
        The type of API requests to be made (default to GET)

    payload: dict, optional
        Dictionary of the API Parameters

    """
    headers = {
        'accept': 'application/json'
    }

    # Put API token into param body
    if "token" not in payload:
        payload["token"] = token

    # Standardise Path
    if len(path) <= 1:
        raise Exception("No path is provided")
    if path[0] == "/":
        path = path[1:]
    
    if method == "GET":
        response = requests.get(
            url=f"https://developers.onemap.sg/{path}",
            params=payload,
            headers=headers
        )
    elif method == "POST":
        response = requests.post(
            url=f"https://developers.onemap.sg/{path}",
            data=payload,
            headers=headers
        )

    if response.status_code == 200:
        return pd.DataFrame(json.loads(response.content))

    else:
        raise Exception(f"API Request Error {response.status_code}: {requests.status_codes._codes[response.status_code][0].replace('_', ' ').title()}")


def convert_geojson_to_geometry(df: pd.DataFrame) -> pd.DataFrame:
    '''
    Function to convert Geojson column (containing raw Geojson string) to Geometry column (containing Shape object) in a DataFrame
    
    Parameters
    ----------
    df: pd.DataFrame
        DataFrame consisting of a 'geojson' column
    ----------

    '''
    df["geometry"] = df["geojson"].apply(
        lambda x: shape(geojson.loads(x)) if not pd.isna(x) else None
    )

    df.drop(columns=["geojson"], inplace=True)
    return df


def export_df_to_shapefile(df: pd.DataFrame, filename: str, crs: str="EPSG:4326") -> gpd.GeoDataFrame:
    '''
    Export DataFrame (containing a 'geometry' column) into the data folder in this project directory
    and return GeoDataFrame of the exported data
    
    Parameters
    ----------
    df: pd.DataFrame
        DataFrame consisting of a 'geometry' column

    filename: str
        Filename of output Shapefile (and its folder)

    crs: str, optional
        Coordinate Reference System of Shapefile (Default to EPSG:4326)
    ----------

    '''
    gdf = gpd.GeoDataFrame(df, geometry="geometry", crs=crs)

    # Create Folder in Data to store Shapefile
    shpfile_directory = os.path.join(os.path.dirname(os.getcwd()), "data", filename)
    if not os.path.isdir(shpfile_directory):
        os.makedirs(shpfile_directory)

    # Export as Shapefile
    gdf.to_file(f"../data/{filename}/{filename}.shp")

    return gdf


def retrieve_onemap_population_data(lst_of_area_names: list, token: str, path: str, year: int = 2020) -> pd.DataFrame:
    """""
    Function to fetch the full population related data from OneMap API

    Parameters
    ----------
    lst_of_area_names: list
        A Python List of the planning areas to be queried
    
    token: str
        API Token

    path: str
        The end-point of the API request to be sent (e.g. privateapi/popapi/getPlanningareaNames)

    year: int, optional
        Year of data that is queried (default to 2020)

    """
    df_output = pd.DataFrame()
    for region_name in lst_of_area_names:
        temp_df = retrieve_from_onemap(token, path, payload={"year":year,"planningArea":region_name})
        df_output = pd.concat([df_output, temp_df])

    df_output.reset_index(inplace=True, drop=True)
    df_output["planning_area"] = df_output["planning_area"].apply(lambda x: x.upper())
    return df_output



# --------------------------- LTA Datamall API Functions [ARCHIVED] ---------------------------

def retrieve_from_datamall(token: str, path: str, method: str = 'GET', payload: dict = {}) -> pd.DataFrame:
    """
    Function to fetch data from LTA Datamall

    Parameters
    ----------
    token: str
        API Token

    path: str
        The end-point of the API request to be sent

    method: str, optional
        The type of API requests to be made (default to GET)

    payload: dict, optional
        Dictionary of the API Parameters

    """
    headers = {
        'AccountKey': token,
        'accept': 'application/json'
    }

    #Get handle to http
    h = http.Http()

    # Convert payload to string
    body = str(payload)
    
    # Standardise Path
    if len(path) <= 1:
        raise Exception("No path is provided")
    if path[0] == "/":
        path = path[1:]

    # Full request uri
    uri = f'http://datamall2.mytransport.sg/ltaodataservice/{path}'

    #Obtain results
    response, content = h.request(uri, method, body, headers)

    if response['status'] == '200':
        jsonObj = json.loads(content)
        df = pd.DataFrame(jsonObj['value'])
        final_df = df
        skip = 0
        while len(df) == 500:
            skip += 500
            new_uri = f'{uri}?$skip={skip}'            
            new_response, new_content = h.request(new_uri, method, body, headers)
            if new_response['status'] != '200':
                raise Exception(f"API Request Error {new_response['status']}: {requests.status_codes._codes[int(new_response['status'])][0].replace('_', ' ').title()}")

            new_jsonObj = json.loads(new_content)
            df = pd.DataFrame(new_jsonObj['value'])
            final_df = pd.concat([final_df,df])

        return final_df
    else:
        raise Exception(f"API Request Error {response['status']}: {requests.status_codes._codes[int(response['status'])][0].replace('_', ' ').title()}")
