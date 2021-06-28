# This script scrapes NBA player injury data from pro sports transactions

# Importing required modules

import pandas as pd
import urllib
from bs4 import BeautifulSoup as bs

# Defining username + filepath for sacing output :: update accordingly!

username = ''
filepath = 'C:/Users/' + username + '/Documents/Data/NBA/injuries.csv'

# Defining the url components

base = 'http://www.prosportstransactions.com/basketball/Search/SearchResults.php?Player=&Team=&BeginDate=1970-01-01&EndDate=2021-01-01&InjuriesChkBx=yes&Submit=Search&start='
ends = [str(25*i) for i in range(1137)]

# Defining some helper functions

def h_date(inp): # helper fxn for parsing dates
    
    a = str(inp).find('>')
    b = str(inp)[a+1:].find('<')
    out = str(inp)[a+1:a+1+b]
    
    return out

def h_teams(inp): # helper fxn for parsing teams
    
    a = str(inp).find('>')
    b = str(inp)[a+2:].find('<')
    out = str(inp)[a+2:a+2+b]
    
    return out

def h_players(inp): # helper fxn for parsing players
    
    a = str(inp).find('•')
    b = str(inp)[a+2:].find('<')
    out = str(inp)[a+2:a+2+b]
    
    return out

def h_notes(inp): # helper fxn for parsing notes
    
    a = str(inp).find('>')
    b = str(inp)[a+2:].find('<')
    out = str(inp)[a+2:a+2+b]
    
    return out

# Main loop

dates = []
teams = []
players = []
notes = []

for e in ends:
    
    print(str(ends.index(e) + 1) + ' of ' + str(len(ends))) # Visual progress check
    url = base + e # Define the url
    page = urllib.request.Request(url, headers = {'User-Agent': 'Mozilla/5.0'}) # Define page
    response = urllib.request.urlopen(page) # Open page
    soup = bs(response, 'html.parser') # Get data from the page
    data = soup.find_all('td') # Get all appropriate data type
    data = data[5:130] # Subset for desired data
    
    for i in range(25):
        
        if data[5*i+3] != '<td> </td>': # Making sure this gets injury data not return to play data
            
            dates.append(h_date(data[5*i]))
            teams.append(h_teams(data[5*i+1]))
            players.append(h_players(data[5*i+3]))
            notes.append(h_notes(data[5*i+4]))

# Providing only one player name

for p in players:
    
    if '/' in p:
        
        idx = p.find('/')
        players[players.index(p)] = p[:idx-1]
    
    elif '(' in p:
        
        idx = p.find('(')
        players[players.index(p)] = p[:idx-1]
        
    elif 'Sr.' in p:
        
        idx = p.find('Sr.')
        players[players.index(p)] = p[:idx-1]
        
    elif 'Jr.' in p:
        
        idx = p.find('Jr.')
        players[players.index(p)] = p[:idx-1]

# Converting data and saving to file

dates = pd.Series(dates, name = 'Date')
teams = pd.Series(teams, name = 'Team')
players = pd.Series(players, name = 'Player')
notes = pd.Series(notes, name = 'Notes')

df = pd.concat([dates, teams, players, notes], axis = 1) # Create df
df = df[df.Notes != 'returned to lineup'].reset_index(drop = True) # Drop some bad data
df = df[df.Player != 'td> '].reset_index(drop = True) # Drop some bad data
df.to_csv(filepath, index = False) # Write to file

