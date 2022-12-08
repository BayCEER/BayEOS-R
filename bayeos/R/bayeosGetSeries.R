



bayeos.deleteRows <- function(ids,datetimes=NULL,from=NULL,until=NULL,con=1){
	if(is.character(ids)) ids=sapply(ids,function(x) bayeos.path2id(x,con))
	ids=as.integer(ids)
	if(is.null(datetimes) && is.null(from) && is.null(until)){
		cat(" you have to give datetimes or from and until")
		return(FALSE)
	}
	for(id in ids){
		cat("Deleting",id)
		if(! is.null(datetimes))
			res=bayeos.call(con,'MassenTableHandler.removeRows',id,datetimes)
		if(! is.null(from) && ! is.null(until)){
			timefilter=getTimeFilter(from,until,NA,con)
			res=bayeos.call(con,'MassenTableHandler.removeRowsByInterval',id,timefilter[1],timefilter[2])
		}
			
		if(res) cat(" ok\n")
		else {
			cat(" failed\n")
			return(FALSE)
		}
		
	}
	return(TRUE)
}

# setStatus
#
bayeos.setStatus <- function(ids,datetimes,status,con=1){
  if(is.character(ids)) ids=sapply(ids,function(x) bayeos.path2id(x,con))
  ids=as.integer(ids)

  
  if(is.character(status)){
		status_txt=status
		status=.localenv$c[[con]]$status[status]
		
	}
	
	if(is.na(status)){
		warning(paste('"',status_txt,'" not found'))
		return(FALSE)
	} 
	sapply(ids,function(x) bayeos.call(con,'MassenTableHandler.updateRows',as.integer(x),datetimes,as.integer(status)))
}

# getSeries
# returns a data.frame with "datetime","value","status"
# 
bayeos.getSeries <- function(ids,con=1,from='yesterday',until='today',interval=NA,aggfunc=NA,aggint=NA,
		maxrows=10000,csFlag=FALSE,statusFilter=c(0,1,2,3,4,8,9),localTime=F){
	ids=bayeos.id2id(ids,con)
	if(! localTime){
	  tz=bayeos.call(con,'MessungHandler.getTimeZone',as.integer(ids[1]))
	  setBayeosConTZ(con,tz)
	} else setBayeosConTZ(con,Sys.timezone())
	
	interval=tolower(interval)
	aggfunc=tolower(aggfunc)
	aggint=tolower(aggint)
	
	timefilter <- getTimeFilter(from,until,interval,con)
	if(is.character(ids)) ids=sapply(ids,function(x) bayeos.path2id(x,con))
	ids=as.integer(ids)
	
	
	# Checking for valid Aggregation:
	if(!is.na(aggfunc) & is.na(.localenv$c[[con]]$aggfunc[aggfunc])){
		warning(paste('"',aggfunc,'" not found in aggregation functions'))
		return(NA)
	}
	
	if(!is.na(aggint) & is.na(.localenv$c[[con]]$aggint[aggint])){
		warning(paste(aggint,' not found in aggregation intervals'))
		return(NA)
	}
	
	if(length(ids)>1){
		return(bayeos.getMatrix(ids,con,timefilter,maxrows,aggfunc,aggint,csFlag,statusFilter))
	}
	
	if(is.na(.localenv$c[[con]]$aggfunc[aggfunc]) | is.na(.localenv$c[[con]]$aggint[aggint]))
		res=bayeos.call(con,'MessungHandler.getResolution',ids)
	else
		res=.localenv$c[[con]]$aggint_sec[aggint]
	
	# Check Row Count 
	if (!isValidRowCount(res,timefilter,maxrows)) return(NA)
	
	if(is.na(.localenv$c[[con]]$aggfunc[aggfunc]) | is.na(.localenv$c[[con]]$aggint[aggint])){
		massendaten=bayeos.call(con,'MassenTableHandler.getRows',ids,timefilter,as.integer(statusFilter))[[2]]
		bytes=length(massendaten)						
		daten=bytes/9
		index=(0:(bytes-1))%%9		
		if (daten == 0) {
			datetime=vector()
			df=data.frame('value'=vector())
			if(csFlag) df$status=vector()
			
		} else {
			datetime=as.POSIXct(readBin(massendaten[index<4],integer(), n = daten, size = 4, endian = "big"),origin='1970-01-01',tz='GMT')
			attr(datetime,'tzone')=.localenv$c[[con]]$tz
			df=data.frame('value'=readBin(massendaten[index>3 & index<8],numeric(), n = daten, size = 4, endian = "big"))
			if(csFlag) df$status=readBin(massendaten[index>7],integer(), n = daten, size = 1, endian = "big")	
		}
	} else {
		aggregierte_daten=bayeos.call(con,'AggregationTableHandler.getRows',ids,timefilter,
				as.integer(c(.localenv$c[[con]]$aggfunc[aggfunc],.localenv$c[[con]]$aggint[aggint])))
		datetime=as.POSIXct(sapply(aggregierte_daten[[2]],function(x) x[[1]]),origin='1970-01-01',tz='GMT')
		attr(datetime,'tzone')=.localenv$c[[con]]$tz
		
		df=data.frame('value'=sapply(aggregierte_daten[[2]],function(x){
							if(is.null(x[[2]])) NA
							else x[[2]]}
		))
		
		if(csFlag) df$count=sapply(aggregierte_daten[[2]],function(x) x[[3]])
	}
	df=zoo(df,datetime)	
	attr(df,'ids')<-ids
	df
}

bayeos.zoo2df <- function(data){
	df=data.frame(time(data),coredata(data))
	names(df)=c('datetime',names(data))
	df
}

bayeos.df2zoo <- function(data){
	is_datetime=sapply(data,function(x) any(class(x)=='POSIXct'))
	if(sum(is_datetime)!=1)
		stop("No suitable datetime column found")	  
	datetime=data[is_datetime]
	data[is_datetime]=NULL
	zoo(data,datetime[,1])
}

bayeos.zoo2bin <- function(id,data){
	data=data[! is.na(data)]	#is.na includes NaN
	dt=as.numeric(time(data))*1000;
	dt4=as.integer(dt %% 2^16)
	dt=(dt-dt4)/2^16
	dt3=as.integer(dt %% 2^16)
	dt=(dt-dt3)/2^16
	dt2=as.integer(dt %% 2^16)
	dt=as.integer((dt-dt2)/2^16)	
	
	l=length(data)
	id=as.vector(matrix(id,1,l))
	as.vector(
			rbind(
					matrix(writeBin(as.integer(id),raw(),size=4,endian='big'),4,l),
					matrix(writeBin(as.vector(rbind(dt,dt2,dt3,dt4)),raw(),size=2,endian='big'),8,l),
					matrix(writeBin(as.numeric(coredata(data)),raw(),size=4,endian='big'),4,l)
			)
	)
}

bayeos.writeSeries <- function(ids,data,con=1,update=FALSE,status=NA,maxchunk=5000){
	ids=bayeos.id2id(ids,con)
	if(class(data)=='data.frame')
		data=bayeos.df2zoo(data)
	index=1:length(ids)
	
	if(update ){
	  checkServerVersion(con,1,8)
      xmlrpcfunc='MassenTableHandler.upsertByteRows'
	} else
	  xmlrpcfunc='MassenTableHandler.addByteRows'
	
	# Calculate number of data to import
  nrows=length(data[,1])
  ncols=length(ids)
  rowstep=ceiling(maxchunk/ncols)
	for(from in seq(1,nrows,rowstep)){
	  until=from+rowstep-1;
	  if(from>nrows) from=nrows;
	  if(until>nrows) until=nrows;
	  if(until>=from){
	    cat("Writing",from,"to",until,"...")
	    bin=as.vector(
			  unlist(
					sapply(index,function(i) bayeos.zoo2bin(ids[i],data[from:until,i]))
			  )
	    )
	    res=bayeos.call(con,xmlrpcfunc,bin)
      if(res) cat("ok\n")
      else {
        cat("failed\n")
        return(FALSE)
      }
	  }
	}
	if(! is.na(status))
		bayeos.setStatus(ids,time(data),status)
	res
}




#bayeos.findNode <- function(con,name){
#	bayeos.call(con,'TreeHandler.findNode',name)
#}


bayeos.getMatrix <- function(ids, con, timefilter, maxrows=10000,aggfunc='NONE',aggint='NONE',csFlag=FALSE,
		statusFilter=c(0,1,2,3,4,8,9)){	
	ids=bayeos.id2id(ids,con)
	
	if(is.na(.localenv$c[[con]]$aggfunc[aggfunc]) | is.na(.localenv$c[[con]]$aggint[aggint]))
		sec <- bayeos.call(con,'MessungHandler.getMinResolution',as.integer(ids))
	else 
		sec=.localenv$c[[con]]$aggint_sec[aggint]
	if (!isValidRowCount(sec,timefilter,maxrows)) return(NA)		
	if(is.na(.localenv$c[[con]]$aggfunc[aggfunc]) | is.na(.localenv$c[[con]]$aggint[aggint]))
		vec = bayeos.call(con,'MassenTableHandler.getMatrix',as.integer(ids),timefilter,as.integer(statusFilter),csFlag)
	else 
		vec = bayeos.call(con,'AggregationTableHandler.getMatrix',ids,timefilter,as.integer(c(.localenv$c[[con]]$aggfunc[aggfunc],.localenv$c[[con]]$aggint[aggint])),csFlag)
	
	colCount = length(vec[[1]])
	bytesPerRow = 4 + colCount * 4;		
	bytes=length(vec[[2]])
	if(bytes==0) vec[[2]]=as.raw(NULL) #avoid error for readBin
	rowCount=bytes/bytesPerRow	
	index=(0:(bytes-1))%%bytesPerRow
	datetime=as.POSIXct(readBin(vec[[2]][index<4],integer(), n = rowCount, size = 4, endian = "big"),origin='1970-01-01',tz='GMT')
	attr(datetime,'tzone')=.localenv$c[[con]]$tz
	
	if(csFlag){
		s_matrix=t(matrix(readBin(vec[[2]][index>3 & ((index-4)%%8)>3],integer(),n=rowCount*colCount/2,size=4,endian="big"),
						colCount/2,rowCount))
		s_matrix[s_matrix==2143289344]=NA
		m=cbind(
				t(matrix(readBin(vec[[2]][index>3 & ((index-4)%%8)<4],numeric(),n=rowCount*colCount/2,size=4,endian="big"),
								colCount/2,rowCount)),
				s_matrix
		)
	} else {
		m=t(matrix(readBin(vec[[2]][index>3],numeric(),n=rowCount*colCount,size=4,endian="big"),colCount,rowCount))
	}
	
	
	m[is.nan(m)]=NA #NA (missing value) is a better representation than NaN (not a number)!
	m=zoo(m,datetime)
	if(csFlag)
		names(m)=vec[[1]][sapply(1:colCount,function(x){if(x<=(colCount/2)) x*2-1 else (x-colCount/2)*2 })]	
	else
		names(m)=vec[[1]]
	attr(m,'ids')=ids
	m
}


bayeos.setNames <- function(data,names){
	data
}

# path: character - ./==current folder /=Root-Folder 
# column_selection
# data: bayeos dataframe
bayeos.import <- function (data,folder='./',columnSelection=NULL,columnDestination=NULL,
                           con=1,update=FALSE,status=NA,maxchunk=5000){
	# Transform to zoo
	if(class(data)=='data.frame')
		data=bayeos.df2zoo(data)
	folder=bayeos.id2id(folder,con)
	bayeos.cd(folder,con,ls=FALSE)
	
	if(tail(.localenv$c[[con]]$path,1)[[1]]$id!=folder){
		warning(paste('Could not change to target folder!'))
		return(FALSE)
	}
	child_class=sapply(.localenv$c[[con]]$childNodes,function(x) x$class)
	child_ids=sapply(.localenv$c[[con]]$childNodes,function(x) x$id)[child_class=='messung_massendaten']
	child_names=sapply(.localenv$c[[con]]$childNodes,function(x) x$name)[child_class=='messung_massendaten']
	names(child_ids)=child_names
	# Take names from data, if columnDestination is not given
	if(is.null(columnSelection))
		columnSelection=1:(length(names(data)))
	if(is.null(columnDestination)){
		if(is.numeric(columnSelection)){
			columnDestination=names(data)[columnSelection]
		} else columnDestination=columnSelection	
	}
	# create names if not present
	tz=attr(time(data),'tzone')
	datetime=time(data)
	if(length(datetime)>0){
		resolution=as.numeric(datetime[2])-as.numeric(datetime[1])
	} else resolution=NULL
	
	ids=sapply(columnDestination,function(x){
				if(is.na(child_ids[x]) | is.list(child_ids))
					bayeos.createSeries(x,con,FALSE,0,tz,'created by R bayeos.import',resolution)
				else
					child_ids[x]
				
			})
	bayeos.writeSeries(ids,data[,columnSelection],con,update,status,maxchunk)
	
}

# wrapper um read.table
bayeos.readCSV <- function(file,tz='Etc/GMT-1',dateformat=c('%d.%m.%Y %H:%M'),skip=0,sep=';',dec='.',
		na.strings='NA',header=TRUE,comment.char ="#",skipCols=0,...){
	num_datecols=length(dateformat)
	data=read.table(file,header=header,sep=sep,skip=skip,na.strings=na.strings,dec=dec,comment.char=comment.char,
			as.is=(1:num_datecols)+skipCols,fill=TRUE,...);
	num_data=length(data[,1])
	index=1:num_data
	dateformat=paste(dateformat,collapse=' ')
	
	if(skipCols>0)
		sapply(1:skipCols,function(x) data[,x]<-NULL)
	
	data[,1]=switch(num_datecols,
			'1'=data[,1],
			'2'=paste(data[,1],data[,2]),
			'3'=paste(data[,1],data[,2],data[,3]),
			'4'=paste(data[,1],data[,2],data[,3],data[,4]),
			'5'=paste(data[,1],data[,2],data[,3],data[,4],data[,5]),
			'6'=paste(data[,1],data[,2],data[,3],data[,4],data[,5],data[,6])
	)
	data[,1]=as.POSIXct(data[,1],format=dateformat,tz=tz)
	if(num_datecols>1){
		for(i in 2:num_datecols)
			data[2]<-NULL
	}
	cleanupImportedData(data,tz,NULL)
}








