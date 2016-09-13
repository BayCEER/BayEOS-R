## A sample session using the bayeos getMatrix Function

# Load library and connect to bayeos database
library(bayeos)
bayeos.connect('http://bayeos.bayceer.uni-bayreuth.de/BayEOS-Server/XMLServlet','gast','gast')

# Get a matrix by ids without date filter 
bayeos.getSeries(ids=c(2717,2718))

# Get a matrix by ids and time interval 
bayeos.getSeries(ids=c(2717,2718),from='2006-07-01 00:00:00',until='2006-07-01 01:00:00')
 
# Close the connection 
bayeos.close()




