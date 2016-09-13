# TODO: Add comment
# 
# Author: holzheu
###############################################################################


.setUp <- function(){
	library(bayeos)
	bayeos.connect('bti3x3')
}

test.Find<-function(){				
	# Find Celsius in Unit-Tree	
  id=bayeos.createFolder('test-Find')
  f = bayeos.find('test-Find')  	
  bayeos.deleteNode(id,confirm=FALSE)
	checkTrue(length(f) == 4)	
	checkTrue(all(f$name == 'test-Find'))	

	# 
	
	
}

.tearDown<-function(){
	bayeos.close()	
}

