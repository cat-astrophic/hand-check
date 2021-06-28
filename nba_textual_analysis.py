# Textual analysis of injuries for nba players

# Importing required modules

import pandas as pd
import numpy as np
from nltk.corpus import stopwords
from nltk.tokenize import word_tokenize
from wordcloud import WordCloud
from matplotlib import pyplot as plt

# Data location params :: update accordinly

username = ''
filepath = 'C:/Users/' + username + '/Documents/Data/NBA/'

# Reading in the data

wordsnstuff = pd.read_csv(filepath + 'injuries.csv')

# Isolating the notes

raw = list(wordsnstuff.Notes)

# Cleaning + standardizing the notes

for r in raw:
    
    x = r.find('(')
    r2 = r[:x-1]
    r2 = r2.replace('/', ' ')
    raw[raw.index(r)] = r2

uniqueys = list(sorted(list(pd.Series(raw, name = 'raw').unique())))

# Tokenize and remove stopwords

stop_words = set(list(set(stopwords.words('english'))) + ['(', ')', ',', "'s", '!', '.'])
cleaniqueys = []

for key in uniqueys:
    
    word_tokens = word_tokenize(key)
    filtered_sentence = [w for w in word_tokens if not w in stop_words]
   
    for w in filtered_sentence:
       
        cleaniqueys.append(w)

cleaniqueys = list(sorted(list(pd.Series(cleaniqueys, name = 'Unique_Keys').unique())))

# Write to keys to file

C = pd.Series(cleaniqueys, name = 'Key')
C.to_csv(filepath + 'keys.csv', index = False)

##############################################################################
##::::AT THIS STAGE A MANUAL ANALYSIS + UPDATE OF keys.csv WAS PERFORMED::::##
##############################################################################

# Read in the manually updated file

keys = pd.read_csv(filepath + 'keys.csv')
a = list(keys.Key)
b = list(keys.Drop)
c = list(keys.Convert)

# Complete the standardization of the keys

for x in range(len(c)):
    
    if pd.isnull(c[x]) == False:
        
        a[x] = c[x]

# Drop the elements selected for removal

drops = []

for x in range(len(b)):
    
    if pd.isnull(b[x]) == False:
        
        drops.append(x)

keylist = [a[x] for x in range(len(a)) if x not in drops]

# Remove duplicates

keylist = list(pd.Series(keylist).unique())

# Restricting terms to only those which appear frequently enough in the data

X = np.zeros((len(wordsnstuff),len(keylist)))

for i in range(len(keylist)):
   
    for j in range(len(wordsnstuff)):
       
        if str(keylist[i]) in str(wordsnstuff.Notes[j]):
           
            X[j][i] = 1

sums = sum(X)
keepers_10 = [i for i in range(len(sums)) if sums[i] >= 10]
keepers_20 = [i for i in range(len(sums)) if sums[i] >= 20]
keepers_50 = [i for i in range(len(sums)) if sums[i] >= 50]
keepers_100 = [i for i in range(len(sums)) if sums[i] >= 100]
keepers_200 = [i for i in range(len(sums)) if sums[i] >= 200]

# The following list was created from looking at keepers_100 and looking for
# injuries keywords associated with falls/collisions/extremities since these
# are the most likely to be associated with guards attacking the paint

falls = ['ACL', 'MCL', 'ankle', 'arthroscopic', 'back', 'calf', 'elbow', 'fascia',
         'finger', 'foot', 'groin', 'hamstring', 'hand', 'heel', 'hip', 'knee',
         'leg', 'meniscus', 'patella', 'plantar', 'quadricep', 'toe', 'wrist']

# Get list of all keywords and look at the frequency by year of the keys in falls

# Because the scripts are now intertwined, read in NBA.csv

nba = pd.read_csv(filepath + 'NBA.csv')

# A helper function for these frequency data

def freq(arg):
    
    out = []
    
    for i in range(1989,2019):
        
        season = str(i) + '-' + str(i+1)
        aba = nba[nba.Season == season]
        out.append(sum(aba[arg]))
    
    return out

# Get annual data for knee injuries and make plot

knees = freq('knee') # Knee Injury data
colors = ['blue']*15 + ['red']*15 # Colors for bar chart
plt.figure()
plt.bar(nba.Season.unique(),knees,color = colors)
plt.xticks(rotation = 90)
plt.xlabel('Season')
plt.ylabel('Number of Players with Knee Injuries')
plt.title('Knee Injuries by NBA Season')
plt.show()

# Creating a word cloud from the injury data via keepers_200

textvars = [keylist[i] for i in keepers_200]
thewords = ' '.join(textvars)
wordcloud = WordCloud(max_font_size = 50, max_words = 100, background_color = 'white', colormap = 'magma').generate(thewords)
plt.figure()
plt.imshow(wordcloud, interpolation = 'bilinear')
plt.axis('off')
plt.show()

# Creating a word cloud from the injury data via falls

thewords = ' '.join(falls)
wordcloud = WordCloud(max_font_size = 50, max_words = 100, background_color = 'white', colormap = 'magma').generate(thewords)
plt.figure()
plt.imshow(wordcloud, interpolation = 'bilinear')
plt.axis('off')
plt.show()

