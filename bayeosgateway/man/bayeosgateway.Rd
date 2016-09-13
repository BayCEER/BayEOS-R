\name{bayeosgateway}
\alias{bayeosgateway.connect}
\alias{bayeosgateway.close}
\alias{bayeosgateway.get}
\alias{bayeosgateway.post}
\title{
BayEOS Gateway functions
}
\description{
Simple BayEOS-Gateway functions
}
\usage{
bayeosgateway.connect(url=NULL, user=NULL, password=NULL,save_as=NULL,
                           force_interactive=FALSE,list=FALSE,con=1)
bayeosgateway.close(con=1)
bayeosgateway.get(origin,from=Sys.Date(),until=Sys.Date()+1,tz='Etc/GMT-1',con=1)
bayeosgateway.post(z, origin,matrix=FALSE,con=1)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{url}{BayEOS-gateway base url or alias of the .bayeosgateway.pwd-file}
  \item{user}{BayEOS-gateway login}
  \item{password}{BayEOS-gateway password}
  \item{force_interactive}{Setting TRUE will prompt for login and password}
  \item{save_as}{If set, will save the connection data in the .bayeosgateway.pwd-file}
  \item{con}{connection number}
  \item{list}{If set to TRUE, bayeosgateway.connect just prints out all available connections and exits}
  \item{z}{zoo object to post}
  \item{from}{Minimum timestamp of data - character, Date or POSIXct}
  \item{until}{Maximum timestamp of data - character, Date or POSIXct}
  \item{tz}{time zone string}
  \item{matrix}{Switch for data storage on gateway. For mass data e.g. eddy use matrix=TRUE}
}
\details{
bayeosgateway.connect just stores connection information in a local environment. bayeosgateway.close deletes this information.
bayeosgateway.post performs a POST request to put data on the gateway.
bayeosgateway.get reads data from the gateway into a zoo object.
}
\value{
bayeosgateway.get returns a zoo object on success.
bayeosgateway.post return TRUE on success
bayeosgateway.connect returns the connection number
}
\references{
}
\author{
Stefan Holzheu (holzheu@bayceer.uni-bayreuth.de)
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# 
library('bayeos')
library('zoo')
bayeosgateway.connect('http://bayeos-gateway/gateway','admin','xbee',save_as='gw1')
## next time one can just call 
#bayeosgateway.connect('gw1')
z=zoo(data.frame(a=rnorm(10),b=rnorm(10)),Sys.time()-seq(60,600,60))
bayeosgateway.post(z,'MyR-Device')
r=bayeosgateway.get('MyR-Device')
bayeosgateway.close()
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
}
\keyword{ bayeos }
\keyword{ gateway }
\keyword{ timeseries }
