\name{bayeos.references}
\alias{bayeos.getReferences}
\alias{bayeos.createReferences}
\alias{bayeos.deleteReferences}
\alias{bayeos.createReferenceNode}
\alias{bayeos.deleteReferenceNode}
\title{Manage References in BayEOS Tree}
\description{
Functions to get, create or delete references.
}
\usage{
bayeos.getReferences(id,con=1)
bayeos.createReferences(id,to,tree,create=FALSE,id_parent=NULL,con=1)
bayeos.deleteReferences(id,type,con=1)
bayeos.createReferenceNode(name,tree,id_parent=NULL,con=1)
bayeos.deleteReferenceNode(id,con=1)


}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{con}{connection identifier returned by bayeos.connect}  
	\item{id}{numeric object id}  
	\item{tree}{Tree to search in. One of 'device','compartment','location','target','unit','web'}
	\item{to}{Object name or id to create a reference}
	\item{create}{When true, createReferences will call createReferenceNode when to is not found}
	\item{id_parent}{Parent Node. When Null the root node is taken}
	\item{name}{Name of the  references Node to create.}
	\item{type}{Type of the referenced object; use 'all' to delete all direct references.}
	 
}

\value{
  returns a data frame with the columns name,type,direct
}

\author{S. Holzheu, O. Archner}


\examples{
 	
	\dontrun{
	# open connection 
	bayeos.connect('http://www.bayceer.uni-bayreuth.de/servlet/XMLServlet','gast','gast')

	
	
	# create folder and series (only works, when rights are sufficient)
	newFolder=bayeos.createFolder('test folder')
	bayeos.createReferences(newFolder,'Somewhere','Location')
	newNode=bayeos.createSeries('test series')
	bayeos.createReferences(newNode,'Celsius','Unit')
	bayeos.deleteNode(newNode)
	# close connection
	bayeos.close()
	}

}
\keyword{references}
\keyword{meta data}

