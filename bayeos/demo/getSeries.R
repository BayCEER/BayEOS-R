## A sample session using the bayeos package

# Load library and connect to bayeos database
library(bayeos)
bayeos.connect('http://bayeos.bayceer.uni-bayreuth.de/BayEOS-Server/XMLServlet','gast','gast')
# Get a series by id without date information 
bayeos.getSeries(2718)

# Get a series by id and time interval 
bayeos.getSeries(2718,from='2006-07-01 00:00:00',until='2006-07-01 01:00:00')

# Get a series of aggregated values by id and interval  
bayeos.getSeries(2718,from='2006-01-01 00:00:00',until='2006-02-01 01:00:00', aggfunc='Avg',aggint='day')

# Close the connection 
bayeos.close()




