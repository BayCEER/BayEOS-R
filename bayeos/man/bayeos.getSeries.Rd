\name{bayeos.timeseries}
\alias{bayeos.getSeries}
\alias{bayeos.writeSeries}
\alias{bayeos.setStatus}
\alias{bayeos.deleteRows}
\alias{bayeos.import}
\alias{bayeos.readCSV}
\alias{bayeos.zoo2df}
\title{Read and write time series data using a bayeos server connection}
\description{
 bayeos.getSeries creates a zoo object from a BayEOS connection. 
 bayeos.readCSV reads data from csv file into a zoo object.
 bayeos.import and bayeos.writeSeries write zoo objects or data frames to BayEOS database.
 bayeos.setStatus sets status for selected timestamps and ids
 bayeos.deleteRows behaves like bayeos.setStatus but removes the rows
}
\usage{
bayeos.getSeries(ids,con=1,from='yesterday',until='today',interval=NA,
		aggfunc=NA,aggint=NA, maxrows=10000,
		csFlag=FALSE,statusFilter=c(0,1,2,3,4,8,9))
bayeos.readCSV(file,tz='Etc/GMT-1',dateformat=c('\%d.\%m.\%Y \%H:\%M'),skip=0,sep=';',dec='.',
		na.strings='NA',header=TRUE,comment.char ="#",skipCols=0,...)
bayeos.import(data,folder='./',columnSelection=NULL,columnDestination=NULL,
    con=1,update=FALSE,status=NA,maxchunk=5000)
bayeos.writeSeries(ids,data,con=1,update=FALSE,status=NA,maxchunk=5000)
bayeos.setStatus(ids,datetimes,status,con=1)
bayeos.deleteRows(ids,datetimes=NULL,from=NULL,until=NULL,con=1)
bayeos.zoo2df(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ids}{time series ids (integer or path)}
  \item{con}{connection index returned by bayeos.connect}
  \item{data}{time series data as zoo object}
  \item{from}{datetime string or keyword 'yesterday'}
  \item{until}{datetime string or keyword 'today'}
  \item{interval}{string of the following keywords: today, this week, this month, this year, yesterday, last week, last month, last year. 
  Overwrites from and until settings.}
  \item{aggfunc}{aggregation function; valid values see bayeos.getConnection(con)$aggfunc}
  \item{aggint}{aggregation interval; valid values see bayeos.getConnection(con)$aggint}
  \item{maxrows}{maximum number of rows.}
  \item{csFlag}{dataframe will contain additional columns with status or counts}
  \item{statusFilter}{Only query values with status IDs within this vector}
  \item{update}{setting to TRUE will delete existing data from min(data$datetime) until max(data$datetime). 
  Otherwise data of existing timestamps is silently ignored. Use with care!}
  \item{columnDestination}{character vector of destination time series. Will overwrite column names from data}
  \item{columnSelection}{column numbers starting with 1 or character vector of column names to be imported}
  \item{datetimes}{Vector of POSIXct datetimes}
  \item{status}{Integer or character; valid values see bayeos.getConnection(con)$status}
  \item{maxchunk}{maximum number of data points passed to one post request}
  \item{...}{Further arguments to be passed to 'read.table'.}
} 


\value{
  bayeos.getSeries, bayeos.readCSV return a time series data as zoo object. 
  bayeos.import, bayeos.writeSeries and bayeos.setStatus return TRUE on success.
  bayeos.zoo2df transforms the zoo data object to data frame type used in the first releases of the package. 
}

\details{
  BayEOS-R package handles time series data as zoo data type. See package \link{zoo} for more information. 
  
  bayeos.getSeries will return a zoo object containing one or more time series. Search parameters from and until
  should be given as character strings. For POSIXct arguments as.character() will be called. 
  'from' and 'until' are interpreted in the local time zone of the series. The POSIXct time zone of the
  zoo object is set to the time zone of the the series. When there are time series with different time zones 
  in the zoo object the time zone of the first series is relevant.

  bayeos.readCSV is a wrapper for the read.table function of R. It returns a zoo object. 
  It support separated date and time columns. For dateformat description see \link{strftime}. Further arguments comform to
  \link{read.table}.
  
  bayeos.import and bayeos.writeSeries write data to the database. bayeos.import will operate in a folder creating the
  time series objects if not existing. bayeos.writeSeries writes data to a existing time series.
  
  Comment on using paths instead of ids: 
  In general from 1.4.5 one can use paths in the bayeos packages. However using long paths may slow down the package, 
  as there will be several lookups in the server. 
}


\author{S. Holzheu, O. Archner}

\seealso{
  \code{\link{bayeos.cd}}\cr
  \code{\link{bayeos.connect}}\cr
}
\examples{
\dontrun{
 bayeos.connect('http://www.bayceer.uni-bayreuth.de/servlet/XMLServlet','gast','gast')
 bayeos.getSeries(2718)
 bayeos.getSeries(c(2718,2717))
 bayeos.getSeries(2718,from='2006-07-01 00:00:00',until='2006-07-01 01:00:00')
 bayeos.getSeries(2718,from='2006-01-01 00:00:00',until='2006-02-01 01:00:00', aggfunc='Avg',aggint='day')
 bayeos.getSeries(2718,interval='Last Year', aggfunc='Avg',aggint='day')
 
 # Read data from csv
 f = system.file("sampleData", "datetime.csv", package = "bayeos")
 bayeos.readCSV(f,dec=',',sep=';',na.string='<NA')
 f = system.file("sampleData", "date_time.csv", package = "bayeos")
 bayeos.readCSV(f,dec=',',sep=';',dateformat=c('\%d.\%m.\%Y','\%H:\%M'),na.string='<NA',tz='Etc/GMT-8')

  
 # Write the data in timeseries of the current folder
 # Series names are taken from data (this will not run with the guest connection!) 
 f = system.file("sampleData", "datetime.csv", package = "bayeos")
 data=bayeos.readCSV(f,dec=',',sep=';',na.string='<NA')
 bayeos.import(data,status='valid')
 
 bayeos.close()
}
}
\keyword{data}
\keyword{timeseries}
