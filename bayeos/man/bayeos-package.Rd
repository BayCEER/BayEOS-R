\name{bayeos-package}
\alias{bayeos-package}
\alias{bayeos}
\docType{package}
\title{
Package to access BayEOS data from R via XMLRPC
}
\description{
The packages provides the user with an easy to use frontend to access a 
BayEOS database from R. The package is based on a modified XMLRPC package of
the omega project for statistical computing (www.omegahat.org).
}

\author{
    Stefan Holzheu \email{stefan.holzheu@uni-bayreuth.de}
	Oliver Archner \email{oliver.archner@uni-bayreuth.de} 	
}

\keyword{ package }
\keyword{ timeseries }
\seealso{
  \code{\link{bayeos.connect}}
  \code{\link{bayeos.getSeries}}
  \code{\link{bayeos.cd}}
  \code{\link{bayeos.getACL}}
  \code{\link{bayeos.getDF}}
}
\examples{

bayeos.connect('http://www.bayceer.uni-bayreuth.de/servlet/XMLServlet','gast','gast')
data = bayeos.getSeries(c(2717,2718))
plot(data)
bayeos.close()

}
