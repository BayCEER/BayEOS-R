test.InstallPackage<- function() {
		
	 
	 ## # Remove packages from library 
	 ## remove.packages(c("bayeos","XML","RCurl"))
	 ## 
	 ## checkTrue(is.na(match("bayeos",library()$results[,1])))
	 ## checkTrue(is.na(match("XML",library()$results[,1])))
	 ## checkTrue(is.na(match("RCurl",library()$results[,1])))
	 ## 
	 ## 
	 ## # Install packages from repository
	 ## 
	 ## install.packages("RCurl", repos = "http://www.omegahat.org/R")		
	 ## checkTrue(match("RCurl",library()$results[,1]) == TRUE)	
	 ## 
	 ## install.packages("XML", repos = "http://www.omegahat.org/R")	
	 ## checkTrue(match("XML",library()$results[,1]) == TRUE)	
	 ## 
	 ## install.packages("bayeos", repos = "http://www.bayceer.uni-bayreuth.de/R-repository")	
	 ## checkTrue(match("bayeos",library()$results[,1]) == TRUE)
	 ## 
	
}