# TEST for bayeos.readCSV, bayeos.readODS, bayeos.readXLSX, bayeos.import
# 
# Author: holzheu
###############################################################################


.setUp <- function(){
	library(bayeos)
	library(zoo)
	bayeos.connect('bti3x3')
}

test.readCSV<-function(){				
	# read data from CSV
	f = system.file("sampleData", "datetime.csv", package = "bayeos")
	m1=bayeos.readCSV(f,dec=',',sep=';',na.string='<NA',dateformat='%d.%m.%y %H:%M')
	f = system.file("sampleData", "date_time.csv", package = "bayeos")
	m2=bayeos.readCSV(f,dec=',',sep=';',dateformat=c('%d.%m.%y','%H:%M'),na.string='<NA',tz='Etc/GMT-1')
	checkTrue(all(time(m1) == time(m2)))
}


test.import<-function(){
	# Note: the user needs the appropriate rights to run this test	
	f = bayeos.createFolder("Test Import")		
	bayeos.cd(f)
	m1=bayeos.readCSV(system.file("sampleData", "datetime.csv", package = "bayeos")
			,dec=',',sep=';',na.string='<NA',dateformat='%d.%m.%y %H:%M')
	# Import frame values into database
	checkTrue(bayeos.import(m1))
	
	ids=sapply(bayeos.getConnection()$childNodes,function(x) x$id)
	# Read values back
	m3 = bayeos.getSeries(ids,from='2010-01-01 10:00:00',until='2010-01-01 18:00:00')		
	checkTrue(all(time(m1) == time(m3)))
	# 	checkTrue(all(coredata(m1) == coredata(m3),na.rm=TRUE)) schlägt fehl da
	# 1. NA != NaN
	# double vs. float
	m1[is.nan(m1)]=NA
	checkTrue(all(round(m1,5) == round(m3,5),na.rm=TRUE))

	# Set Status of new Rows
	checkTrue(all(bayeos.setStatus(ids,time(m1),1)))
	checkTrue(all(bayeos.setStatus(ids,time(m1),'valid')))
	checkTrue(all(bayeos.setStatus(ids,time(m1)[1:4],'noisy')))
	
	# Delete Series 
	checkTrue(all(sapply(ids, function(x) bayeos.deleteNode(x,confirm=FALSE,removeRows=TRUE))))
	
	# Run same import with other tz
	m1=bayeos.readCSV(system.file("sampleData", "datetime.csv", package = "bayeos")
			,dec=',',sep=';',na.string='<NA',dateformat='%d.%m.%y %H:%M',tz='Etc/GMT-8')
	# Import frame values into database
	checkTrue(bayeos.import(m1))
	
	ids=sapply(bayeos.getConnection()$childNodes,function(x) x$id)
	# Read values back
	m4 = bayeos.getSeries(ids,from='2010-01-01 10:00:00',until='2010-01-01 18:00:00')		
	checkTrue(all(time(m1) == time(m4)))
	m1[is.na(m1)]=NaN
	checkTrue(all(round(m1,5) == round(m4,5),na.rm=TRUE))
	
	
	# Delete Series 
	checkTrue(all(sapply(ids, function(x) bayeos.deleteNode(x,confirm=FALSE,removeRows=TRUE))))
	
	
	# Delete folder
	bayeos.cd('..')
	checkTrue(bayeos.deleteNode(f,confirm=FALSE))
	
}

test.importDF<-function(){
	# Note: the user needs the appropriate rights to run this test	
	f = bayeos.createFolder("Test Import")		
	bayeos.cd(f)
	m1=bayeos.readCSV(system.file("sampleData", "datetime.csv", package = "bayeos")
			,dec=',',sep=';',na.string='<NA',dateformat='%d.%m.%y %H:%M')
	# Import frame values into database
	m1=bayeos.zoo2df(m1)
	checkTrue(bayeos.import(m1))
	
	ids=sapply(bayeos.getConnection()$childNodes,function(x) x$id)
	# Read values back
	m3 = bayeos.getSeries(ids,from='2010-01-01 10:00:00',until='2010-01-01 18:00:00')		
	checkTrue(all(m1$datetime == time(m3)))
	
	# Delete Series 
	checkTrue(all(sapply(ids, function(x) bayeos.deleteNode(x,confirm=FALSE,removeRows=TRUE))))
	
	# Delete folder
	bayeos.cd('..')
	checkTrue(bayeos.deleteNode(f,confirm=FALSE))
	
}

.tearDown<-function(){
	bayeos.close()	
}