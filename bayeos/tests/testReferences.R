# TODO: Add comment
# 
# Author: holzheu
###############################################################################

.setUp <- function(){
	library(bayeos)
	bayeos.connect('bti3x3')
}

test.References<-function(){				
	# Note: the user needs the appropriate rights to run this test	
	f = bayeos.createFolder("Test Folder")		
	bayeos.cd(f)
	id = bayeos.createSeries('Series A',warningLevel=0)	
	# Add reference
	bayeos.createReferences(id,'Celsius','Unit',create=TRUE)
	# get reference
	ref=bayeos.getReferences(id)
	checkTrue(length(ref[,1]) == 1)	
	checkTrue(length(ref) == 3)	
	ref=bayeos.deleteReferences(id,'Unit')
	ref=bayeos.getReferences(id)
	checkTrue(length(ref[,1]) == 0)	
	checkTrue(length(ref) == 3)	
	# Delete Series 
	bayeos.deleteNode(id,confirm=FALSE,removeRows=TRUE)
	
	# Delete folder
	bayeos.cd('..')
	bayeos.deleteNode(f,confirm=FALSE)
	
}

.tearDown<-function(){
	bayeos.close()	
}

