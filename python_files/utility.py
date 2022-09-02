# General Data  Tools
import pandas as pd

# For API Calls
import json
import requests
import httplib2 as http


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
        return pd.DataFrame(jsonObj['value'])

    else:
        raise Exception(f"API Request Error {response['status']}: {requests.status_codes._codes[int(response['status'])][0].replace('_', ' ').title()}")