# This script preps + cleans the NBA data

# Importing required modules

import pandas as pd
from datetime import datetime
import numpy as np

# Declaring the username + filepath :: update accordingly!

username = ''
filepath = 'C:/Users/' + username + '/Documents/Data/NBA/'

# Reading in the data sets

booboos = pd.read_csv(filepath + 'injuries.csv')
booboo_dudes = pd.read_csv(filepath + 'players.csv')

# A helper function for determining which season

def seasoned(inp):
    
    inp = datetime.strptime(inp, '%Y-%m-%d')
    flag = False
    yr = 1970
    
    while flag == False:
        
        seas = str(yr) + '-01-01'
        seas = datetime.strptime(seas, '%Y-%m-%d')
        
        if inp < seas:
            
            out = str(yr-1) + '-' + str(yr)
            flag = True
            
        else:
            
            yr += 1
    
    return out

# A helper function for determining player age

def aged(inp,refyear):
    
    inp = datetime.strptime(inp, '%m/%d/%Y')
    refyear = '11/1/' + str(refyear)
    refyear = datetime.strptime(refyear, '%m/%d/%Y')
    age = abs(int((inp-refyear).days)/365)
    
    return age

# A helper function for determining player experience

def experienced(inp,refyear):
    
    inp = datetime.strptime(inp, '%m/%d/%Y')
    refyear = '11/1/' + str(refyear)
    refyear = datetime.strptime(refyear, '%m/%d/%Y')
    out = abs(int((inp-refyear).days)/365)
    
    return out

# Adding a variable for the season

seasons = [seasoned(x) for x in booboos.Date]
seasons = pd.Series(seasons, name = 'Season')
booboos = pd.concat([seasons, booboos], axis = 1)

# Create a database of player-season injury data

years = []
players = []
positions = []
heights = []
weights = []
college = []
country = []
guard = []
guard2 = []
ouchies = []
ages = []
exper = []
notes = []

for yr in range(1989,2019): #for yr in range(1994,2014):
    
    sea = str(yr) + '-' + str(yr+1)
    boos = booboos[booboos.Season == sea].reset_index(drop = True)
    
    for p in list(booboo_dudes.NAME):
        
        if (yr >= booboo_dudes.FROM[list(booboo_dudes.NAME).index(p)]) and (yr <= booboo_dudes.TO[list(booboo_dudes.NAME).index(p)]):
            
            years.append(sea)
            players.append(p)
            positions.append(booboo_dudes.POSITION[list(booboo_dudes.NAME).index(p)])
            heights.append(booboo_dudes.HEIGHT[list(booboo_dudes.NAME).index(p)])
            weights.append(booboo_dudes.WEIGHT[list(booboo_dudes.NAME).index(p)])
            college.append(booboo_dudes['LAST ATTENDED'][list(booboo_dudes.NAME).index(p)])
            country.append(booboo_dudes.COUNTRY[list(booboo_dudes.NAME).index(p)])
            guard.append(booboo_dudes.GUARD[list(booboo_dudes.NAME).index(p)])
            guard2.append(booboo_dudes.GUARD2[list(booboo_dudes.NAME).index(p)])
            ages.append(aged(booboo_dudes.BIRTHDATE[list(booboo_dudes.NAME).index(p)],yr))
            exper.append(int(yr - booboo_dudes.FROM[list(booboo_dudes.NAME).index(p)]))
            
            if p in list(boos.Player):
                
                ouchies.append(1)
                notes.append(boos.Notes[list(boos.Player).index(p)])
                
            else:
                
                ouchies.append(0)
                notes.append(None)
                                
        else:
            
            continue

# Creating a pre/post treatment variable with a helper fxn

def treated(inp):
    
    out = int(int(inp[0:4]) >= 2004)
    
    return out

treats = [treated(x) for x in years]

# Creating a previous injury count variable with a helper fxn

def workers_comp(p,y):
    
    out = 0
    y = int(y[:4])
    
    for yr in range(1971,y):
        
        sea = str(yr) + '-' + str(yr+1)
        boos = booboos[booboos.Season == sea].reset_index(drop = True)
        boos = boos[boos.Player == p]
        
        if len(boos) > 0:
            
            out += 1
    
    return out

preves = [workers_comp(players[i],years[i]) for i in range(len(players))]

# The following list was created from looking at keepers_100 in the
# nba_textual_analysis file and looking for injuries keywords associated
# with falls/collisions/extremities since these are the most likely
# to be associated with guards attacking the paint

falls = ['ACL', 'MCL', 'ankle', 'arthroscopic', 'back', 'calf', 'elbow', 'fascia',
         'finger', 'foot', 'groin', 'hamstring', 'hand', 'heel', 'hip', 'knee',
         'leg', 'meniscus', 'patella', 'plantar', 'quadricep', 'toe', 'wrist']

M = np.zeros((len(players),len(falls)))

for i in range(len(falls)):
       
    for j in range(len(players)):
        
        boo = booboos[booboos.Player == players[j]]
        boo = boo[boo.Season == years[j]].reset_index(drop = True)
        
        if (len(boo) > 0) and (str(falls[i]) in str(boo.Notes[0])):
            
            M[j][i] = 1

M = pd.DataFrame(M)
M.columns = falls

# Any hand-check-realted injuries

hc = [max(M.iloc[i]) for i in range(len(players))]

# Checking if DTD is listed in injuries

dtd = [1 if 'DTD' in str(n) else 0 for n in notes]

# Creating the final dataframe and saving to file

years = pd.Series(years, name = 'Season')
players = pd.Series(players, name = 'Player')
positions = pd.Series(positions, name = 'Position')
heights = pd.Series(heights, name = 'Height')
weights = pd.Series(weights, name = 'Weight')
college = pd.Series(college, name = 'College')
country = pd.Series(country, name = 'Country')
guard = pd.Series(guard, name = 'Guard')
guard2 = pd.Series(guard2, name = 'Guard2')
ouchies = pd.Series(ouchies, name = 'Injured')
ages = pd.Series(ages, name = 'Age')
treats = pd.Series(treats, name = 'Post')
exper = pd.Series(exper, name = 'Experience')
preves = pd.Series(preves, name = 'Priors')
hc = pd.Series(hc, name = 'Injured2')
dtd = pd.Series(dtd, name = 'DTD')

df = pd.concat([years, players, positions, heights, weights, college, country, guard, guard2, ages, treats, exper, preves, ouchies, hc, dtd], axis = 1)
df = pd.concat([df, M], axis = 1)
df.to_csv(filepath + 'NBA.csv', index = False)

