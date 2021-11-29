# This script gets nba shot data and creates a raw data set

# Importing required modules

import pandas as pd
import json
import time
import urllib
from nba_api.stats.endpoints import shotchartdetail
from nba_api.stats.static import players

# Directory info for where data will be stored

username = 'macary'
filepath = 'C:/Users/' + username + '/Documents/Data/NBA/'

# Get a list of all players with relevant data in dictionary format

player_dict = players.get_players()

# Create a list of playerIDs

pids = [player_dict[i]['id'] for i in range(len(player_dict))]

# Create a list of seasons

seasons = ['2002-03', '2003-04', '2004-05', '2005-06']

# Initializing a dataframe in which all data is stored

df = pd.DataFrame()

# Loop for getting shooting data for players for 2018-2019

for p in pids:
    
    for s in seasons:
            
        for t in ['Regular Season']:#['Regular Season', 'Playoffs', 'All Star']:
            
            print('Retrieving data for the ' + s + ' ' + t + ' for player ' + str(pids.index(p)+1) + ' of ' + str(len(pids)) + '.......') # Visualizing progress
            response = shotchartdetail.ShotChartDetail(team_id = 0, player_id = p, context_measure_simple = 'FGA', season_nullable = s, season_type_all_star = t) # Query the API
            content = json.loads(response.get_json()) # Get json
            results = content['resultSets'][0] # Get resultsSets entry containing desired data
            rows = results['rowSet'] # Get the actual data
            headers = results['headers'] # Grab the variable names
            
            try: # Not all player-season-season-types return data
                
                tmp = pd.DataFrame(rows) # Create a dataframe for the results
                tmp.columns = headers # Add variable names to dateframe as column headings
                s2 = pd.Series([s]*len(tmp), name = 'Season') # A series of season
                t2 = pd.Series([t]*len(tmp), name = 'Season_Type') # A series of season-type
                tmp = pd.concat([tmp, s2, t2], axis = 1) # Add s2 and t2 to tmp
                df = pd.concat([df, tmp], axis = 0) # Append season data to the main df
                
            except:
                
                continue
                
            time.sleep(0.5) # Wait 2 seconds for the sake of the API

# Save data as csv

df.to_csv(filepath + 'shooting_data.csv', index = False)

