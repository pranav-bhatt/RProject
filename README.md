# RProject

Data Cleaning:-

1. in the quality columns, we are replacing NA with the previous valid occuring value(because the values are super stable)
2. in value columns we can initialize the value of previous two fields to zero, then replace NA with the average of the two values.
3. take the lat long value of each unique station, and then just replace those where ever the values dont match

Visualizations:-

1. the polution distribution with lat long, station-wise in 1 day(hourly breakdown), 
   and within 1 week intervals(at the median of the 1 day dist)
2. pollution trends in a month
3.  
