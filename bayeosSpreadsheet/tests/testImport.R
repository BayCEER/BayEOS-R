# TEST for bayeos.readCSV, bayeos.readODS, bayeos.readXLSX, bayeos.import
# 
# Author: holzheu
###############################################################################


.setUp <- function(){
	library(bayeos)
	library(bayeosSpreadsheet)
	library(zoo)
}


test.readODS<-function(){
	# read data from ODS
	f = system.file("sampleData", "datetime.ods", package = "bayeosSpreadsheet")
	m1=bayeos.readODS(f,na.string='<NA')
	f = system.file("sampleData", "date_time.ods", package = "bayeosSpreadsheet")
	m2=bayeos.readODS(f,datetype='date+time',tz='Etc/GMT-1',na.string='<NA')
	checkTrue(all(time(m1) == time(m2)))
}

test.readXLSX<-function(){
	# Read data from xlsx
	f = system.file("sampleData", "datetime.xlsx", package = "bayeosSpreadsheet")
	m1=bayeos.readXLSX(f,na.string='<NA')
	f = system.file("sampleData", "date_time.xlsx", package = "bayeosSpreadsheet")
	m2=bayeos.readXLSX(f,datetype='date+time',tz='Etc/GMT-1',na.string='<NA')
	checkTrue(all(time(m1) == time(m2)))
}


.tearDown<-function(){
}