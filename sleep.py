#!/usr/bin/python
# This script calculates SLEEP of flies in the DAM system. 
import pandas as pd
import numpy as np
import sys

#usage: pyhton sleep.py filename [f/s/a] 
# f = "final" result, a three column data frame with sample number, and the day and night sleep numbers
# s = "summary" a summary of the sleep data per hour 
# a = "all", both f and s together
# default is f

#print "This script calculates SLEEP of Drosophila flies in the DAM system"
#print "Data is analyzed per DAM monitor. The input file should have 42 columns with the time stamps in column 2"
#print "and the data in columns 10 to 42. The measuing bin is 2 minutes. Columns should be tab delimited."
#print "The output will be printed to the screen.\n IF THERE ARE VALUES CLOSE OR HIGHER  THAN 700, CHECK THE ORIGINAL DATA. THIS MAY INDICATE THE FLY DIED!!"

# Get the file name:
file = str(sys.argv[1])
try:
  # open file stream
  files = open(file)
except IOError:
  print ("There was an error writing to", files)
  sys.exit()

# Load the data
monitor = pd.read_csv(file, '\t', header = None)
# removes all columns except for those with the time stamp and the data. 
# should have 32 columns per monitor. 
monitor = monitor.drop(monitor.columns[[0,1,3,4,5,6,7,8,9]] , axis = 1)
# Replace the names of the columns with numbers 0 to the max number of columns. 
name_columns = range(0, len(monitor.columns))
monitor.columns = name_columns

## Defining which arguments to use for the calculations:
## This function determines if there is an argument after "-o", and its value (a/f/s, or anything else, or empty)
def arg(n): 
	try: 
		res = str(sys.argv[n])
	except IndexError: 
		res = "" 
	return res 
option = str(arg(2))

# Function to calculate sleep. 
def sleep(data, opt):
    column = 1
    # Create a empty DataFrame
    result = pd.DataFrame()
    # add the first column - This will become the column names at the end
    result.loc[:,""] = ("Day","Night")
    all_sleep = pd.DataFrame()
    while (column < len(data.columns)):
        sample = "Sample_" + str(column)
        # Analyzing each column separately
        # isolate  the data from each column
        a = data[[0, column]]
        # Naming the columns - Just to make it easier to understand.
        a.columns = ['time', 'raw'] 
        # Get the inactive data.
        a.loc[a.raw == 0, "Inactive"] = 2 
        # Get the slc data
        a.loc[a['Inactive'] + a['Inactive'].shift(-1) + a['Inactive'].shift(+1) == 6, "slc"] = 2 
        # Replace NaN's with "0" >> necessary for the following calculations.
        a = a.fillna(0) 
        # Get the sleep data. 
        a.loc[a['slc'] + a['slc'].shift(-1)+ a['slc'].shift(+1) > 1, "sleep"] = 2 
        # Sleep data per hour
        # Group the data by hours.
        ag = a.groupby(np.arange(len(a))//30).sum() 
        ag.index = a.loc[0::30, "time"]
        ag = ag.reset_index(level = ['time'])
        # Replace NaN's with "0" >> necessary for the following calculations.
        ag = ag.fillna(0) 
        # DataFrame with the summary for all hours
        all_sleep.loc[:,str(column)] = ag["sleep"]
        # Average Sleep per hour of the day!
        # Calculating the average for each hour.
        bg = ag.groupby('time').sleep.mean()[ag['time']][0:24:].reset_index(level=['time'])
        # Adding the data for day and night (light/dark)
        result[sample] = round(bg.groupby(np.arange(len(bg))//12).sum(), 2) 

        column += 1 
    result = result.set_index('')
    # Organizing the DataFrame depending on the selected option
    if (opt == "a"):
        a_d = print('********** FINAL RESULT: **********\n', result.transpose(), '\n********** SUMMARY: **********\n', all_sleep)
    elif (opt == "s"):
        a_d = print('********** SUMMARY: **********\n', all_sleep)
    elif (opt == "f"):
        a_d = print('********** FINAL RESULT: **********\n', result.transpose())
    return a_d

### Running the scrippt

if (option == "a"):
	f_r = sleep(monitor, "a")
elif (option == "s"):
	f_r = sleep(monitor, "s")
else:
	f_r = sleep(monitor, "f")
f_r