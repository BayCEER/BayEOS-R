library('RCurl')
library('XML')
library('zoo')

library('bayeos')

.localenv <- new.env()


## Helper Functions
find_in_pw_file <- function(pw_file,search,index){
  i=1
  while(i<=length(pw_file)){
    if(pw_file[[i]][index]==search)
      return(i)
    i=i+1
  }
  return(NA)
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



bayeosgateway.connect <- function(url=NULL, user=NULL, password=NULL,save_as=NULL,
                           force_interactive=FALSE,list=FALSE,con=1){  
  if(! exists("g",envir=.localenv)) .localenv$g<-list()
  curl = getCurlHandle()
  pwFileName=paste(Sys.getenv("HOME"),'/.bayeosgateway.pwd',sep='')
  pwKeyFile=paste(Sys.getenv("HOME"),'/.bayeosgateway.pwd_key',sep='')
  if(! file.exists(pwKeyFile)){
    warning("creating empty .bayeosgateway.pwd_key file in home drive. The file is used to encrypt your passwords. Make sure, that nobody else can read the file!!!");
    writeBin(charToRaw(base64Encode(paste(rnorm(1),Sys.time()))),pwKeyFile)
    newKeyFile=TRUE;
  }
  pwKey=readBin(pwKeyFile,'raw',100)
  
  if(! file.exists(pwFileName)){
    warning("creating empty .bayeosgateway.pwd file in home drive. The file may contain sensitive login data. Make sure, that nobody else can read the file!!!");
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
      warning(".bayeosgateway.pwd is empty. exiting!")
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

  
  .localenv$g[[con]]=list();
  .localenv$g[[con]]$url=url
  .localenv$g[[con]]$user=user
  .localenv$g[[con]]$password=password
  .localenv$g[[con]]$curlopts=list(
    verbose=FALSE,
    httpauth=1L,
    userpwd=paste(user,password,sep=":"))
  
  con
}

bayeosgateway.close<-function(con=1){
  .localenv$g[[con]]=NULL
  TRUE
}

bayeosgateway.post<-function(z,origin,matrix=FALSE,con=1){
  dt=as.numeric(time(z))*1000;
  dt4=as.integer(dt %% 2^16)
  dt=(dt-dt4)/2^16
  dt3=as.integer(dt %% 2^16)
  dt=(dt-dt3)/2^16
  dt2=as.integer(dt %% 2^16)
  dt=as.integer((dt-dt2)/2^16)  
  
  data=coredata(z)
  
  opts <- .localenv$g[[con]]$curlopts
  opts$httpheader<-c("Content-Type"="application/x-www-form-urlencoded",
                     "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
  opts$postfields<- paste("sender=",URLencode(origin),"&",
                       paste(sapply(1:length(dt),
                                    function(x){
                                      paste('bayeosframes[]=',
                                            gsub('\\+','%2B',gsub('=','%3D',base64Encode(
                                              c(
                                                as.raw(c(12)),
                                                writeBin(c(dt4[x],dt3[x],dt2[x],dt[x]),raw(),size=2,endian='little'),
                                                as.raw(c(1,1,0)),
                                                writeBin(as.numeric(data[x,]),raw(),size=4,endian='little')
                                              )
                                            ))),sep='')
                                      
                                    }),collapse='&')
                       ,sep='')
  #print(opts$postfields)
  if(matrix) type='Matrix'
  else type='Flat'
  res=postForm(paste(.localenv$g[[con]]$url,'/frame/save',type,sep=''), 
           .opts = opts, style = "POST")
  if(res[1]=='Frames saved.') return(TRUE)
  else return(FALSE)
}


bayeosgateway.get<-function(origin,from=Sys.Date(),until=Sys.Date()+1,tz='Etc/GMT-1',con=1){
  from=strftime(as.POSIXct(from,tz=tz),'%Y%m%d%H%M%S')
  until=strftime(as.POSIXct(until,tz=tz),'%Y%m%d%H%M%S')
  opts <- .localenv$g[[con]]$curlopts
#  opts$verbose=TRUE
  res=getForm(paste(.localenv$g[[con]]$url,'/board/rawData',sep=''),origin=origin,tz=tz,
              startDate=from,endDate=until,format='xls',header='true',
               .opts = opts,binary=FALSE)
  bayeos.readCSV(textConnection(res[1]),sep=';',skip=1,
                 header=TRUE,dateformat=c('%Y-%m-%d %H:%M:%OS'),tz=tz)
}
