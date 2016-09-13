\name{bayeos.readXLSX}
\alias{bayeos.readODS}
\title{Import data from Excel XLSX and OpenOffice}
\description{
 bayeos.readODS and bayeos.readXLSX reads data from xlsx or ods file into a zoo object.
}
\usage{
bayeos.readODS(file,tz='Etc/GMT-1',sheet=1,datetype='datetime',na.string=NULL)
bayeos.readXLSX(file,tz='Etc/GMT-1',sheet=1,datetype='datetime',na.string='NaN',header=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{path to file or url}
  \item{tz}{timezone of data}
} 


\value{
  bayeos.readXLSX, bayeos.readODS return a time series data as zoo object. 
}

\details{
  
  bayeos.readODS is a wrapper based on a modified read.ods from www.omegahat.org. As like bayeos.readCSV date + time column(s)
  must precede data columns. The following datetypes are supported: "datetime" (default) - one column of type datetime, 
  "date+time" - first column date, second column time, "time+date" - like date+time but different order. Empty cells will 
  automatically converted to NaN. In addition an na.string may be set, to additionally import 
  columns with a special NaN string. Without na.string columns with characters will get deleted from the result.

  bayeos.readXLSX is a wrapper for the read.xlsx function from package RExcelXML. Works like bayeos.readODS.
 }


\author{S. Holzheu, O. Archner}

\seealso{
  \code{\link{bayeos.cd}}\cr
  \code{\link{bayeos.connect}}\cr
  \code{\link{bayeos.import}}\cr
}
\examples{
\dontrun{
 # Read data from ods
 f = system.file("sampleData", "datetime.ods", package = "bayeosSpreadsheet")
 bayeos.readODS(f)
 bayeos.readODS(f,sheet=2)
 bayeos.readODS(f,na.string='<NA')
 f = system.file("sampleData", "date_time.ods", package = "bayeosSpreadsheet")
 bayeos.readODS(f,datetype='date+time',tz='Etc/GMT-8',na.string='<NA')

 # Read data from xlsx
 f = system.file("sampleData", "datetime.xlsx", package = "bayeosSpreadsheet")
 bayeos.readXLSX(f)
 bayeos.readXLSX(f,sheet=2)
 f = system.file("sampleData", "date_time.xlsx", package = "bayeosSpreadsheet")
 bayeos.readXLSX(f,datetype='date+time',tz='Etc/GMT-8',na.string='<NA')
 
}
}
\keyword{data}
\keyword{timeseries}
