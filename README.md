## DROSOPHILA SLEEP 
 Python script designed to estimate the sleep time in Drosophila flies during
an experiment using the DAM system. 

Usage:
pyhton sleep.py filename [f/s/a]

 filename is the file produced during the experiment by the DAM system. 
 
 f = "final" result, a three column data frame with sample number, and the day and night sleep numbers
 s = "summary" a summary of the sleep data per hour 
 a = "all", both f and s together
 default is f

You can tested the script in using the Example_sleep.txt data.

execute:

python sleep.py Example_sleep.txt f

Script uses Python 3.6 and pandas. 

