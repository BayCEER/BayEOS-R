

#wrapper für read.ods
bayeos.readODS <- function(file,tz='Etc/GMT-1',sheet=1,datetype='datetime',na.string=NULL,skipCols=0){
	data=read.ods(file,tz=tz,stringsAsFactors=FALSE)
	if(skipCols>0)
		sapply(1:skipCols,function(x) data[,x]<-NULL)
	if(class(data)=='list') data<-data[[sheet]]
	data[,1]=switch(datetype,
			'datetime'=data[,1],
			'date+time'=as.POSIXct(data[,1])+as.numeric(data[,2]),
			'time+date'=as.POSIXct(data[,2])+as.numeric(data[,1]))
	if(datetype=='date+time' | datetype=='time+date')
		data[,2]<-NULL
	
	bayeos:::cleanupImportedData(data,tz,na.string)
}

bayeos.readXLSX <- function(file,tz='Etc/GMT-1',sheet=1,datetype='datetime',na.string='NaN',header=TRUE,skipCols=0){
	data=read.xlsx(file)[[sheet]]
	if(skipCols>0)
		sapply(1:skipCols,function(x) data[,x]<-NULL)
	
	if(header){
		header_names=as.character(as.vector(as.matrix(data[1,])))
		data=data[2:length(data[,1]),]
		names(data)=header_names
	}
	for(i in 1:length(data)){
		data[,i]=as.numeric(sub(na.string,'NaN',as.character(data[,i])))
	}
	# Um Numerische Fehler zu vermeiden wird hier eine Rundung auf Sekundengenauigkeit durchgeführt...
	data[,1]=switch(datetype,
			'datetime'=as.POSIXct(round((data[,1]-2)*3600*24+0.001),origin='1900-01-01 00:00',tz=tz),
			as.POSIXct(round((data[,1]+data[,2]-2)*3600*24),origin='1900-01-01 00:00',tz=tz)
	)
	if(datetype=='date+time' | datetype=='time+date')
		data[,2]<-NULL
	bayeos:::cleanupImportedData(data,tz,na.string)
	
}

 





