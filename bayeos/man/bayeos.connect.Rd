\name{bayeos.connect}
\alias{bayeos.connect}
\alias{bayeos.getConnection}
\alias{bayeos.close}
\alias{bayeos.call}
\title{Create and close a BayEOS connection from R}
\description{
 This function creates an BayEOS connection via XMLRPC. It returns an integer of the connection index. 
 Normally all operations run per default on the first connection. It is possible to handle multiple BayEOS connections. 
 Use different connection numbers in this case.
}
\usage{
	bayeos.connect(url=NULL, user=NULL, password=NULL,save_as=NULL,
		force_interactive=FALSE,con=1,.debugRPC=FALSE,list=FALSE)
	bayeos.getConnection(con=1)
	bayeos.close(con=1)
	bayeos.call(con,method,...,.convert=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{url}{the URL to the BayEOS servlet or alias of the .bayeos.pwd-file}
  \item{user}{BayEOS login}
  \item{password}{BayEOS password}
  \item{force_interactive}{Setting TRUE will prompt for login and password}
  \item{save_as}{If set, will save the connection data in the .bayeos.pwd-file}
  \item{con}{connection number}
  \item{method}{XMLRPC method}
  \item{...}{XMLRPC arguments}
  \item{.debugRPC}{flag if TRUE each call shows the XMLRPC Strings}
  \item{.convert}{flag if TRUE XMLRPC response is converted to R objects}
  \item{list}{flag if TRUE bayeos.connect prints out available connections and exits}
}

\details{
Normally one does not want to handle clear text passwords in scripts. To avoid this, it is recommended to 
to use the save_as flag to create a simple text-file ".bayeos.pwd" in the home drive of the user. 
It will contain one line per connection in the format "alias url user encrypted_password". 
Together with the automatically created ".bayeos.pwd_key" it is possible to decrypt the passwords. 
So please make sure, that these two files are not readable to anyone else!
}

\value{
  If login is successful it returns an integer of the connection index. Otherwise it throws a XML-RPC exception. 
  bayeos.getConnection returns the connection list.
  bayeos.call runs a XMLRPC request on a connection an returns the result (developer and debugging function) 
}

\author{S. Holzheu, O. Archner}

\seealso{
  \code{\link[bayeos]{bayeos.getSeries}}\cr
  \code{\link[bayeos]{bayeos.cd}}
}
\examples{
 \dontrun{
# Open a connection to the given url with login and password 'gast'
con=bayeos.connect('http://www.bayceer.uni-bayreuth.de/servlet/XMLServlet','gast','gast')
# Open a connection to the given url with login and password 'gast' 
# and stores the connection data in the .bayeos.pwd-file in the
# HOME-directory of the user
con=bayeos.connect('http://www.bayceer.uni-bayreuth.de/servlet/XMLServlet','gast','gast',
	save_as='bayceer_gast')
# Open a connection using the .bayeos.pwd file
# will look for a configuration line 'bayceer_gast'
con=bayeos.connect('bayceer_gast')
# will take the first line of the .bayeos.pwd-file
con=bayeos.connect() 
# will take the first line of the .bayeos.pwd-file and 
# create a connection with number 2
con=bayeos.connect(con=2) 
# close connection
bayeos.close()
# close connection number 2
bayeos.close(2)
 }
}
\keyword{connection}
