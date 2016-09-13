\name{bayeos.acl}
\alias{bayeos.getACL}
\alias{bayeos.createACL}
\alias{bayeos.updateACL}
\alias{bayeos.deleteACL}
\alias{bayeos.getRoles}
\alias{bayeos.getRid}
\title{Manage ACL-Lists of BayEOS Tree}
\description{
Functions to manage Access Control Lists of BayEOS-Nodes.
}
\usage{
bayeos.getACL(id=NA,ls=FALSE,con=1)
bayeos.createACL(rid,id=NA,read=TRUE,write=FALSE,exec=FALSE,inherit=FALSE,ls=FALSE,con=1)
bayeos.deleteACL(rid,id=NA,ls=FALSE,con=1)
bayeos.updateACL(right,value,rid,id=NA,ls=FALSE,con=1)
bayeos.getRoles(con=1)
bayeos.getRid(name,con=1)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{con}{connection identifier returned by bayeos.connect}  
	\item{id}{numeric node id or NA for current folder}  
	\item{rid}{numeric role id or character role name}  
	\item{ls}{Print out ACL}  
	\item{right}{character one of read,write,exec,inherit} 
	\item{value}{boolean value of specified right}  
	\item{read,write,exec,inherit}{boolean value of specified right} 
	\item{name}{User or group name to search for}  
	 
}

\value{
  bayeos.getACL,bayeos.getRoles returns a data frame.
  bayeos.createACL, bayeos.deleteACL, bayeos.updateACL return success as boolean.
  bayeos.getRid return the numeric role id.
}
\details{
  BayEOS offers a flexible access control list. 
  
  bayeos.getRid will search the bayeos.getRoles data frame for the given name.
  It will first search the uname column. When there is no result it will continue
  with the fullname column.
  
}

\author{S. Holzheu, O. Archner}


\examples{
 	
	# open connection 
	bayeos.connect('http://www.bayceer.uni-bayreuth.de/servlet/XMLServlet','gast','gast')

	# show the current folder
	dir=bayeos.ls()
	
	bayeos.getACL()
	bayeos.getACL(dir$id[1])
	
	bayeos.close()
		

}

\keyword{acl}
