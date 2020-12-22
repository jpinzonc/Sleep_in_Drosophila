#!/usr/bin/python

# This script calculates the sleep data on flies for a single sample. 

import pandas as pd
from pandas import Series
import numpy as np

#print "Enter the file name with extension please (Test_data.txt):",
#file = raw_input()
# Load the data
a = pd.read_csv('Test_data.txt', '\t', header =None)
# drop as many columns as you want... change the "0" for the column number of
# the column to be remove. Use commas to separate different columns (Check how to do ranges)
#a = a.drop(a.columns[[0]],axis=1)

# Give the names to the columns. This may or may not be necessary when running various columns at a time.
a.columns = ['time', 'raw']

# Average Sleep per hour of the day!
bg = a.groupby('time')['raw'].mean()[a['time']][0:48:].reset_index(level=['time'])

# THIS DOES THE SAME
#b = a
#b = b.set_index('time')#sets time column as the index 
#b = b.groupby(lambda x: x)
#b = b.aggregate(np.mean) # Can change mean for other operation such as sum


# Summary
# This dictionary is used to change the values of the time column and then group them by the new value
# day or night
day_nite={"8:00:00":"night","7:30:00":"night","7:00:00":"night","6:30:00":"night","6:00:00":"night","5:30:00":"night","5:00:00":"night","4:30:00": "night","4:00:00":"night","3:30:00":"night","3:00:00":"night","2:30:00":"night"
,"2:00:00":"night","1:30:00":"night","1:00:00": "night","0:30:00":"night","0:00:00":"night","23:30:00": "night", "23:00:00": "night","22:30:00": "night","22:00:00":"night","21:30:00": "night","21:00:00": "night","20:30:00": "day",
"20:00:00": "day","19:30:00": "day","19:00:00": "day","18:30:00": "day","18:00:00": "day","17:30:00": "day", "17:00:00": "day","16:30:00": "day","16:00:00": "day","15:30:00": "day","15:00:00": "day","14:30:00": "day","14:00:00": "day","13:30:00": "day","13:00:00": "day","12:30:00": "day","12:00:00": "day","11:30:00": "day","11:00:00": "day","10:30:00": "day","10:00:00": "day","9:30:00": "day","9:00:00": "day","8:30:00": "day"}

replaced = bg.replace(day_nite)
activity_summary = replaced.groupby('time')['raw'].sum().round(decimals=2)#.reset_index()

# THIS DOES THE SAME
#b = replaced
#b = b.set_index('time')#sets time column as the index 
#b = b.groupby(lambda x: x)
#b = b.aggregate(np.sum).round(decimals=2) # Can change mean for other operation such as sum

# Convert to data frame
activity_df = Series.to_frame(activity_summary).reset_index()

# Activity per day and night
# Create a new data empty data frame
activity = pd.DataFrame()
activity[""]=("day","night")
activity['col1'] = activity_df[['raw']]

print activity