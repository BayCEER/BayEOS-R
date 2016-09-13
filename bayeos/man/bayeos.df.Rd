\name{bayeos.data.frame}
\alias{bayeos.getDF}
\alias{bayeos.getDFZoo}
\alias{bayeos.writeDF}
\alias{bayeos.importDF}
\title{Read and write data.frames using a bayeos server connection}
\description{
Functions to handle data.frames with a bayeos server. (BayEOS-Server Version >= 1.9)
}
\usage{
bayeos.getDF(ids,rowNr=NULL,con=1)
bayeos.getDFZoo(ids,zooIndexColumn=1,aggregateFun=NULL,con=1)
bayeos.writeDF(ids,data,rowNr=NULL,con=1)
bayeos.importDF(id,data,columnSelection=NULL,columnDestination=NULL,append=FALSE,con=1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{con}{connection identifier returned by bayeos.connect}  
	\item{ids}{data.frame  or data.frame.column ids} 
	\item{id}{data.frame id or name} 
	\item{data}{a data.frame to be imported} 
	\item{rowNr}{a integer vector of row numbers} 
	\item{columnDestination}{character vector of destination data.frame columns. Will overwrite column names from data}
    \item{columnSelection}{column numbers starting with 1 or character vector of column names to be imported} 
	\item{append}{logical, with append=FALSE allways whole column is written} 
	\item{zooIndexColumn}{Column number of data.frames to take as zoo index column.} 
	\item{aggregateFun}{aggregateion function name e.g. mean} 
}

\value{
  bayeos.getDF returns a data.frame.
  bayeos.getDFZoo returns a zoo object
  bayeos.writeDF and bayeos.importDF return a logical vector for every column with the write success
}

\author{S. Holzheu, O. Archner}


\examples{
 	
	\dontrun{
	# open connection 
	bayeos.connect('http://www.bayceer.uni-bayreuth.de/servlet/XMLServlet','gast','gast')

	# will not work with guest connection
	id=bayeos.createDF('testDF')
	bayeos.cd(id)
	data=data.frame(sp1=logical(10),
                sp2=1:10,
                sp3=rnorm(10),
                sp4=as.POSIXct(1:10,origin='2013-01-01'),
                sp5=as.character(101:110)
                )
	bayeos.importDF(data)
	bayeos.getDF(id)
	dir=bayeos.ls()
	bayeos.writeDF(dir$id[2],data.frame(1:20))
	bayeos.writeDF(dir$id[5],data.frame(as.character(1:5)))
	bayeos.getDF(id)
	bayeos.cd()
	bayeos.deleteNode(id,confirm=FALSE)
	
	# close connection
	bayeos.close()
	}

}
\keyword{data.frame}
