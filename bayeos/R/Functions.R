# Connection information is stored in a local list valiable
# .localenv$c
# Every bayeos-connection has a separated sublist
# bayeos.connect returns an integer 
#
#

# Package Variables
classes=c('mess_geraet','mess_kompartiment','mess_ort','mess_ziel','messung_ordner','mess_einheit','web_ordner')
names(classes)=c('device','compartment','location','target','folder','unit','web')
.localenv <- new.env()


checkServerVersion<-function(con,major,minor){
	if((.localenv$c[[con]]$bayeosVersion[1]<major) ||
			(.localenv$c[[con]]$bayeosVersion[2]<minor && .localenv$c[[con]]$bayeosVersion[1]==major)){
		stop(paste('The server runs bayeos ',paste(.localenv$c[[con]]$bayeosVersion,collapse='.')
						,'. This function requires bayeos-server >= ',major,'.',minor,sep=''))
	}
}
	


xorEncode <- function(t,key){
	rt=charToRaw(t)
	if(length(key)<length(rt)) key=rep(key,ceiling(length(rt)/length(key))+1)
	base64Encode(xor(rt,key[1:length(rt)]))
}

xorDecode <- function(rt,key){
	rt=base64Decode(rt,'raw')
	if(length(key)<length(rt)) key=rep(key,ceiling(length(rt)/length(key))+1)
	rt=xor(rt,key[1:length(rt)])
	rawToChar(rt)
}

encode_pwfile <- function(pwFileName,key){
	warning("Converting old .bayeos.pwd file to new stye with encrypted passwords. 
					Decryption is possible with .bayeos.pwd_key file. 
					Make sure, that nobody else can read these two files!!!");
	pw_file=strsplit(readLines(pwFileName), ' ')
	pw_file=lapply(pw_file,function(x){if(x[4]>'') x[4]=xorEncode(x[4],key); x})
	cat(sapply(pw_file,function(x) paste(x[1],x[2],x[3],x[4])),file=pwFileName,sep="\n")
}

find_in_pw_file <- function(pw_file,search,index){
	i=1
	while(i<=length(pw_file)){
		if(pw_file[[i]][index]==search)
			return(i)
		i=i+1
	}
	return(NA)
}

# helper function get_interval_from_keyword
get_interval_from_keyword <- function(interval){
	today=strftime(Sys.Date(),format="%Y-%m-%d %H:%M");
	tomorrow=strftime(Sys.Date()+1,format="%Y-%m-%d %H:%M");
	yesterday=strftime(Sys.Date()-1,format="%Y-%m-%d %H:%M");
	monday=strftime(Sys.Date()-as.POSIXlt(Sys.Date())$wday+1,format="%Y-%m-%d %H:%M")
	monday_1=strftime(Sys.Date()-as.POSIXlt(Sys.Date())$wday-6,format="%Y-%m-%d %H:%M")
	first_of_month=strftime(Sys.Date()-as.POSIXlt(Sys.Date())$mday+1,format="%Y-%m-%d %H:%M")
	first_of_year=strftime(Sys.Date()-as.POSIXlt(Sys.Date())$yday,format="%Y-%m-%d %H:%M")
	first_of_month_1=strftime(as.Date(first_of_month)-as.POSIXlt(as.Date(first_of_month)-1)$mday,format="%Y-%m-%d %H:%M")
	first_of_year_1=strftime(as.Date(first_of_year)-as.POSIXlt(as.Date(first_of_year)-1)$yday-1,format="%Y-%m-%d %H:%M")
	
	switch(interval,
			'today'=c(today,tomorrow),
			'yesterday'=c(yesterday,today),
			'this week'=c(monday,tomorrow),
			'this month'=c(first_of_month,tomorrow),
			'this year'=c(first_of_year,tomorrow),
			'last week'=c(monday_1,monday),
			'last month'=c(first_of_month_1,first_of_month),
			'last year'=c(first_of_year_1,first_of_year))
	
}	

setBayeosConTZ <- function(con,tz){
	if(tz!=.localenv$c[[con]]$tz){
		message(paste('INFO: Setting connection time zone from',.localenv$c[[con]]$tz,'to',tz))
		.localenv$c[[con]]$tz<-tz
	}
}


isValidRowCount <- function (sec,timefilter,maxrows){
	if (sec == 0 | is.na(sec)) return (TRUE) 
	estimated_rows=(as.numeric(timefilter[2])-as.numeric(timefilter[1]))/sec		
	if(estimated_rows>maxrows){
		warning(paste('Estimated rows of',estimated_rows,'exceeds the maxrows',maxrows,'. Increase maxrows in the function call or use a different time filter.'))
		return(FALSE)
	} else {
		return(TRUE)
	}
	
}

getTimeFilter <- function (from,until,interval,con){
	from=as.character(from)
	until=as.character(until)
	
	timefilter=get_interval_from_keyword(interval);
	if(is.null(timefilter)){
		from=switch(from,	
				'yesterday'=strftime(Sys.Date()-1,format="%Y-%m-%d %H:%M"),
				from)
		until=switch(until,
				'today'=strftime(Sys.time()),
				until)
		timefilter=c(from,until)
	}
#	timefilter=as.POSIXct(timefilter,tz=.localenv$c[[con]]$tz) Macht z.T. Probleme z.B. '2010-01-01 8:00','2010-01-03'
	timefilter=as.POSIXct(c(as.POSIXct(timefilter[1],tz=.localenv$c[[con]]$tz),
					as.POSIXct(timefilter[2],tz=.localenv$c[[con]]$tz)-1),tz=.localenv$c[[con]]$tz)	
	return (timefilter)
	
}


checkReferences <- function(ref,level=1,print_warning=TRUE){
	types=c('unit','device','target','location','compartiment','employee','department')
	levels=c(1,2,2,2,3,3,3)
	i=1
	res=0
	while(i<length(types)){
		if(levels[i]<=level){
			if(sum(ref[,3]==types[i])==0){
				res=res+1
				if(print_warning)
					warning(paste('There is no reference for',types[i],'!'))
			}
		}
		i=i+1
	}
	return(res)
} 


cleanupImportedData<- function(data,tz,naString=NULL){
	del=sapply(2:length(data),function(x) !(class(data[[x]])=='numeric' | class(data[[x]])=='integer'))
	if(sum(del)>0){
		index=2
		for(i in 1:length(del)){
			if(del[i]){
				if(is.null(naString)){
					warning(paste('Column',i,'seem to be non numeric - ignoring!'))
					data[index]<-NULL
				} else {
					data[,index]<-as.numeric(sub(naString,'NaN',data[,index]))
					index=index+1
				} 
			} else 
				index=index+1
			
		}
	}
	
	if(sum(is.na(data[,1]))>0)
		warning('Some rows have NA as datetime - ignoring!')
	# 	data[,2:length(data)]=as.numeric(data[,2:length(data)])
	data=data[! is.na(data[,1]),]
	datetime=data[,1]
	attr(datetime,'tzone')=tz
	data[,1]=NULL
	zoo(data,datetime)
}


listlist2df<- function(ll,colnames=NULL,cols=NULL,row.names=FALSE){
	if(length(ll)==0) return(data.frame());
	cl=lapply(1:length(ll[[1]]),function(x){ i=x; sapply(ll,function(x) if(is.null(x[[i]]) ) NA else class(x[[i]])) })
	V1=logical(length(ll))
	df=as.data.frame(V1)
	
	if(is.null(cols)) cols=1:length(cl)
	index=1
	for(i in cols){
		c=sapply(ll,function(x) if(is.null(x[[i]])) NA else x[[i]])
		class=na.omit(cl[[i]])
		if(length(class)>0){
			if(class[1]=='POSIXct'){
				tz=attr(na.omit(cl[[i]]),'tzone')
				c=as.POSIXct(as.numeric(c),origin='1970-01-01',tz='GMT')
				attr(c,'tzone')=tz
			}
		}
		if(row.names){
			row.names(df)=c
			row.names=FALSE
		} else {
			df[[index]]=c
			index=index+1
		}
	}
	if(! is.null(colnames)) names(df)=colnames
	df
}

df2listlist<-function(df){
	ll=list();
	for(i in 1:length(df[[1]])){
		ll[[i]]=as.list(df[i,])
	}
	ll
}

