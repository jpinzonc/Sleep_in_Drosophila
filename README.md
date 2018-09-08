## DROSOPHILA SLEEP 

Python script designed to estimate the sleep time in Drosophila flies during
an experiment using the Drosophila Activity Monitor (DAM) system. 

The script calculates the sleep numbers on a single monitor file. 

Usage:

> pyhton sleep.py filename [f/s/a]

Teo parameters are paseed:

 1. - filename is the file produced during the experiment by the DAM system. 
 
 2. one of three analysis options: 
  - f = "final" result, a three column data frame with sample number, and the day and night sleep numbers
 
  - s = "summary" a summary of the sleep data per hour 
 
  - a = "all", both f and s together
 
     The default argunment is f

Example_sleep.txt is a test file, run this command to test

> python sleep.py Example_sleep.txt f

The script was writen in Python 3.6, using pandas. 

