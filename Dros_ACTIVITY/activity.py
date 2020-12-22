#!/usr/bin/python
# This script calculates ACTIVITY in flies in the DAM system. 
import pandas as pd
import numpy as np
from pandas import Series
import sys
# usage python activity.py filename
print "This script calculates the ACTIVITY of Drosophila flies in the DAM system"
print "Data is analyzed per DAM monitor. The input file should have 42 columns with the time stamps on column No. 2"
print " and the data in columns 10 to 42, with a bin of 30 minutes. Columns should be tab delimited."
print "The output will be printed to the screen.\n VALUES OF 0 INDICATE THE FLY DIED!! CHECK FOR SMALL VALUES"
# Get the file name:
file = str(sys.argv[1])
try:
  # open file stream
  files = open(file)
except IOError:
  print "There was an error writing to", files
  sys.exit()
# Load the data
monitor_act = pd.read_csv(file, '\t', header =None)
# removes all columns except for those with the time stamp and the data. 
# should have 32 columns per monitor. 
monitor_act = monitor_act.drop(monitor_act.columns[[0,1,3,4,5,6,7,8,9]],axis=1)
# Replace the names of the columns with numbers 0 to the max number of columns. 
name_columns=range(0,len(monitor_act.columns))
monitor_act.columns=name_columns
# Function to calculate the activity of the flies. 
def flies_activity(data):
	column = 1
	# Create a empty data fram 
	activity = pd.DataFrame()
	#add the first column
	activity[""]=("day","night")
	# This dictionary is necessary later to replace the names of the rows. 
	day_nite={"08:00:00":"night","07:30:00":"night","07:00:00":"night","06:30:00":"night","06:00:00":"night","05:30:00":"night","05:00:00":"night","04:30:00": "night","04:00:00":"night","03:30:00":"night","03:00:00":"night","02:30:00":"night",
	"02:00:00":"night","01:30:00":"night","01:00:00": "night","00:30:00":"night","00:00:00":"night","23:30:00": "night", "23:00:00": "night","22:30:00": "night","22:00:00":"night","21:30:00": "night","21:00:00": "night","20:30:00": "day",
	"20:00:00": "day","19:30:00": "day","19:00:00": "day","18:30:00": "day","18:00:00": "day","17:30:00": "day", "17:00:00": "day","16:30:00": "day","16:00:00": "day","15:30:00": "day","15:00:00": "day","14:30:00": "day","14:00:00": "day","13:30:00": "day",
	"13:00:00": "day","12:30:00": "day","12:00:00": "day","11:30:00": "day","11:00:00": "day","10:30:00": "day","10:00:00": "day","09:30:00": "day","09:00:00": "day","08:30:00": "day"}
	while (column < len(data.columns)):
		sample = "sample_"+ str(column)
		# Analyzing each column at a time
		# Isolate one column
		a = data[[0,column]]
		a.columns = ['time', 'raw'] # Naming the columns - Just to make it easier to understand.
		bg = a.groupby('time')['raw'].mean()[a['time']][0:48:].reset_index(level=['time'])
		replaced = bg.replace(day_nite)
		activity_summary = replaced.groupby('time')['raw'].sum().round(decimals=2)#.reset_index()
		# Converting the series to data frame
		activity_df = Series.to_frame(activity_summary).reset_index()
		# Activity per day and night
		activity[sample] = activity_df[['raw']]
		column = column+1
	return activity.transpose()

print(flies_activity(monitor_act))

