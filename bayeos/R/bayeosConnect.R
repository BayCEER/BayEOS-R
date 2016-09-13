
bayeos.getConnection <- function(con=1) .localenv$c[[con]]

bayeos.call <- function(con,method,...,.convert=TRUE)
	xml.rpc(.localenv$c[[con]]$url,method,...,
			.curl=.localenv$c[[con]]$curl,.opts=.localenv$c[[con]]$opts,.debugRPC=.localenv$c[[con]]$debug,
			serverTZ='Etc/GMT-1',clientTZ=.localenv$c[[con]]$tz,.convert=.convert)	


bayeos.close <- function(con=1){	
	r = bayeos.call(con,"LogOffHandler.terminateSession")	
	if (r == TRUE) 	cat(paste('Closed connection',con,'to',.localenv$c[[con]]$url,'\n'))		
	.localenv$c[[con]]<-NA
	return (r)
}



bayeos.connect <- function(url=NULL, user=NULL, password=NULL,save_as=NULL,
                           force_interactive=FALSE,con=1,.debugRPC=FALSE,list=FALSE){	
	# Default values for Treehandler 
	# 
	activeFilter=FALSE
	missingInterval='week'
	timeFilter=NULL
	newKeyFile=FALSE
	if(! exists("c",envir=.localenv)) .localenv$c<-list()
	curl = getCurlHandle()
	pwFileName=paste(Sys.getenv("HOME"),'/.bayeos.pwd',sep='')
	pwKeyFile=paste(Sys.getenv("HOME"),'/.bayeos.pwd_key',sep='')
	if(! file.exists(pwKeyFile)){
		warning("creating empty .bayeos.pwd_key file in home drive. The file is used to encrypt your passwords. Make sure, that nobody else can read the file!!!");
		writeBin(charToRaw(base64Encode(paste(rnorm(1),Sys.time()))),pwKeyFile)
		newKeyFile=TRUE;
	}
	pwKey=readBin(pwKeyFile,'raw',100)
	
	# Transformation auf old plaintext files
	if(file.exists(pwFileName) && newKeyFile){
		encode_pwfile(pwFileName,pwKey)
	}
	
	if(! file.exists(pwFileName)){
		warning("creating empty .bayeos.pwd file in home drive. The file may contain sensitive login data. Make sure, that nobody else can read the file!!!");
		file.create(pwFileName)
	}
	
	pw_file=strsplit(readLines(pwFileName), ' ')
  
  if(list){
    cat("Available connections\n")
    sapply(pw_file,function(x) cat("Alias:",x[1],"\n",
                                   "URL:",x[2],"\n",
                                   "User:",x[3],"\n\n"))
    return(0)
  }
# Take first entry in pw_file if no url was given
	if(is.null(url)){
		if(length(pw_file)==0){
			warning(".bayeos.pwd is empty. exiting!")
			return(0)
		} else url=pw_file[[1]][1]
	}
	
# Find line in pw_file
	pw_file_line=find_in_pw_file(pw_file,url,1)
	if(is.na(pw_file_line)) pw_file_line=find_in_pw_file(pw_file,url,2)
# set url when url was an alias
	if(! length(grep('^http',url,ignore.case=TRUE))>0){
		if(is.na(pw_file_line)){
			warning("Could not find valid url.")
			return(0)
		}
		url=pw_file[[pw_file_line]][2]
	}
# looking for user and password
	if(is.null(user) && is.null(password)){
		if(is.na(pw_file_line) || force_interactive){
			## interactiv input
			user=readline(prompt = 'Username:')
			password=readline(prompt = 'Password:')
		} else {
			user=pw_file[[pw_file_line]][3]
			password=xorDecode(pw_file[[pw_file_line]][4],pwKey)
		}
	}
# save the connection in the .bayeos.pwd file
	if(is.character(save_as)){
		pw_file_line=find_in_pw_file(pw_file,save_as,1)
		if(is.na(pw_file_line)) pw_file_line=length(pw_file)+1
		
		pw_file[[pw_file_line]]=c(save_as,url,user,xorEncode(password,pwKey))
		cat(sapply(pw_file,function(x) paste(x[1],x[2],x[3],x[4])),file=pwFileName,sep="\n")
	}
	cat(paste('Connecting to ',url,'with login',user),'\n')
	res <- xml.rpc(url,"LoginHandler.createSession",user,password,.curl=curl)
	key <- as.character(base64(paste(res,collapse=':')))
	opts <- list(httpheader = c("Authentication" = key))
	
# Getting Aggregation Functions
	res = xml.rpc(url,'LookUpTableHandler.getAgrFunktionen',.opts=opts,.curl=curl)
	aggfunc = sapply(res,function(x) x[[1]])
	names(aggfunc) = sapply(res,function(x) tolower(x[[2]]))
	
# Getting Aggregation Intervals	
	res = xml.rpc(url,'LookUpTableHandler.getAgrIntervalle',.opts=opts,.curl=curl)
	aggint = sapply(res,function(x) x[[1]])
	names(aggint) = sapply(res,function(x) tolower(x[[2]]))
	aggint_sec = sapply(res,function(x) x[[3]])
	names(aggint_sec) = sapply(res,function(x) x[[2]])
	
# Getting Supported TimeZones
	res = xml.rpc(url,'LookUpTableHandler.getTimeZones',.opts=opts,.curl=curl)
	timezones = sapply(res,function(x) x[[1]])
	names(timezones) = sapply(res,function(x) x[[2]])
	
# Getting Status flags
	res = xml.rpc(url,'LookUpTableHandler.getStatus',.opts=opts,.curl=curl)
	status = sapply(res,function(x) x[[1]])
	names(status) = sapply(res,function(x) x[[2]])
	
	# Getting Supported Interval Types
	res = xml.rpc(url,'LookUpTableHandler.getIntervalTypes',.opts=opts,.curl=curl)
	intervaltypes = sapply(res,function(x) x[[1]])
	names(intervaltypes) = sapply(res,function(x) x[[2]])
	
	# Getting Root-Nodes
	if(! is.null(timeFilter)) timeFilter=as.POSIXct(timeFilter)
#	classes=c('mess_geraet','mess_kompartiment','mess_ort','mess_ziel','messung_ordner','mess_einheit','web_pro','web_mit')
	classes=c('mess_geraet','mess_kompartiment','mess_ort','mess_ziel','messung_ordner','mess_einheit','web_ordner')
	root_ids=sapply(classes,
			function(x) xml.rpc(url,'TreeHandler.getRoot',x,activeFilter,missingInterval,timeFilter,.opts=opts,.curl=curl)[[3]])
	
#  BayEOS-Version
	version=xml.rpc(url,'LookUpTableHandler.getVersion',.opts=opts,.curl=curl)
	treeNames=c('check_write','check_exec','id','id_super','class','name','rec_start','rec_end','plan_start','plan_end','active','recordsMissing','hasChild')
	
# Setting .localenv$c:
	.localenv$c[[con]]<-list('url'=url,'opts'=opts,'curl'=curl,'aggfunc'=aggfunc,'aggint'=aggint,
			'aggint_sec'=aggint_sec,'debug'=.debugRPC,'timeFilter'=timeFilter,'activeFilter'=activeFilter,
			'missingInterval'=missingInterval,'path'=list(),'childNodes'=NA,tz='Etc/GMT-1',
			'root_ids'=root_ids,'timezones'=timezones,'intervaltypes'=intervaltypes,
			'status'=status,bayeosVersion=as.integer(unlist(strsplit(version,'\\.'))),'treeNames'=treeNames)
# Changing to Root-Folder:
	root=bayeos.call(con,'TreeHandler.getRoot','messung_ordner',.localenv$c[[con]]$activeFilter,
			.localenv$c[[con]]$missingInterval,.localenv$c[[con]]$timeFilter)
	names(root)=.localenv$c[[con]]$treeNames
	.localenv$c[[con]]$path[[1]]=root
	bayeos.readChilds(con)
	
	con	
}
