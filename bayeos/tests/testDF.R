# TODO: Add comment
# 
# Author: holzheu
###############################################################################


.setUp <- function(){
	library(bayeos)
	bayeos.connect('bti3x3')
}

test.DF<-function(){				
	# Find Celsius in Unit-Tree	
	id=bayeos.find('testDF')$id
	if(is.null(id))
		id=bayeos.createDF('testDF')
	
	checkTrue(bayeos.updateDF(id,description='Ein kleiner Test öäü'))
	checkTrue(bayeos.getNode(id)$description=='Ein kleiner Test öäü')
	data=data.frame(sp1=logical(20),
			sp2=1:20,
			sp3=rnorm(20),
			sp4=as.POSIXct(1:20,origin='2013-01-01'),
			sp5=as.character(101:120)
	)
	checkTrue(all(bayeos.importDF(id,data)))
	data_ret=bayeos.getDF(id)
	checkTrue(all(data[[1]]==data_ret[[1]]))
	checkTrue(all(data[[2]]==data_ret[[2]]))
	checkTrue(all(data[[4]]==data_ret[[4]]))
	checkTrue(all(data[[5]]==data_ret[[5]]))
	checkTrue(all(round(data[[3]],3)==round(data_ret[[3]],3))) # note resolution is not the same in Database and R
	checkTrue(all(bayeos.getDF(id,rowNr=5:10)[[2]]==5:10))
	dir=bayeos.getChilds(id)
	checkTrue(all(bayeos.getDF(dir$id[2],rowNr=5:10)[[1]]==5:10))
	checkTrue(length(bayeos.getDF(dir$id[2])[[1]])==20)
	
	checkTrue(all(bayeos.importDF(id,data,append=TRUE)))
	
	checkTrue(bayeos.updateDFColumn(dir$id[1],description='Ein kleiner Test',colNr=7))
	checkTrue(bayeos.getNode(dir$id[1])$description=='Ein kleiner Test')
	
	
	checkTrue(bayeos.deleteNode(id,confirm=FALSE))

	# 
	
	
}

.tearDown<-function(){
	bayeos.close()	
}

