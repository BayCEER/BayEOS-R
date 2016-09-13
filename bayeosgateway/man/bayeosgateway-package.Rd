\name{bayeosgateway-package}
\alias{bayeosgateway-package}
\alias{bayeosgateway}
\docType{package}
\title{
BayEOS-Gateway-Client
}
\description{
Simple functions to post data to a BayEOS gateway or to get data from a BayEOS gateway
}
\details{
\tabular{ll}{
Package: \tab bayeosgateway\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2014-02-11\cr
License: \tab GPL\cr
}

}
\author{
Stefan Holzheu (holzheu@bayceer.uni-bayreuth.de)
}
\references{
}
\keyword{ package, timeseries, bayeos }
\seealso{
\code{\link[bayeos:bayeos-package]{bayeos}}
}
\examples{
# Store connection information
bayeosgateway.connect('http://gateway.bayceer.uni-bayreuth.de','admin','xbee')
# Create a time series as zoo object
z=zoo(...)
# Post the data as new board to the gateway
bayeosgateway.post(z,'myDevice')
# Get the data back from the gateway
data=bayeosgateway.get('myDevice')
# delete the connection information
bayeosgateway.close()

}
