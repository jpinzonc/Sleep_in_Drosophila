#!/usr/bin/python

# This script calculates the sleep data on flies for a single sample. 

import pandas as pd
import numpy as np

#print "Enter the file name with extension please (Test_data.txt):",
#file = raw_input()
# Load the data
a = pd.read_csv('Test_data.txt', '\t', header =None)
# drop as many columns as you want... change the "0" for the column number of
# the column to be remove. Use commas to separate different columns (Check how to do ranges)
a = a.drop(a.columns[[0]],axis=1)

# Give the names to the columns. This may or may not be necessary when running various columns at a time.
a.columns = ['time', 'raw']

# Get the inactive data
a.loc[a.raw == 0, "Inactive"] = 2

# get the slc data
a.loc[a['Inactive'] + a['Inactive'].shift(-1)+ a['Inactive'].shift(+1) == 6, "slc"] =2

# Replace NaNA with "0". This is necessary for the following calculations.
a = a.fillna(0)

# Get the sleep data. 
a.loc[a['slc'] + a['slc'].shift(-1)+ a['slc'].shift(+1) > 1, "sleep"]=2

# Sleep data per hour
ag = a.groupby(np.arange(len(a))//30).sum()
ag = ag.fillna(0)
ag.index = a.loc[0::30,"time"]
# make the index a column in the new data.frame
ag = ag.reset_index(level=['time'])
ag = ag.fillna(0)
# Average Sleep per hour of the day!
bg = ag.groupby('time')['sleep'].mean()[ag['time']][0:24:].reset_index(level=['time'])


# Total sleep day and night
# Create a new data frame (empty)
result = pd.DataFrame()
result["cat"]=("day","night")

result['col1'] = bg.groupby(np.arange(len(bg))//12).sum()

# Transpose the dataframe
res=result.transpose()


print(res)


