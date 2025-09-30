\name{bayeos.tree}
\alias{bayeos.cd}
\alias{bayeos.pwd}
\alias{bayeos.getNode}
\alias{bayeos.createDF}
\alias{bayeos.createDFColumn}
\alias{bayeos.updateDF}
\alias{bayeos.ls}
\alias{bayeos.find}
\alias{bayeos.getChilds}
\alias{bayeos.createFolder}
\alias{bayeos.createSeries}
\alias{bayeos.updateSeries}
\alias{bayeos.updateDF}
\alias{bayeos.updateDFColumn}
\alias{bayeos.updateNode}
\alias{bayeos.deleteNode}
\title{Manage BayEOS Timeseries Tree}
\description{
Functions to list current folder, change folder and to create new folder or series
}
\usage{
bayeos.ls(con=1,full=FALSE)
bayeos.pwd(con=1)
bayeos.cd(id=NULL,con=1,ls=TRUE)
bayeos.createFolder(name,id_parent=NULL,con=1,ls=TRUE,force_create=FALSE)
bayeos.createSeries(name,con=1,ls=TRUE,warningLevel=1,tz='Etc/GMT-1',description='',
			resolution=600,planstart=NULL,planend=NULL,intervaltype='undefined',id_parent=NULL,
			lon=NULL,lat=NULL,elevation=NULL,force_create=FALSE)
bayeos.createDFColumn(name,colNr,class,description=NULL,con=1,id_parent=NULL,ls=TRUE,
		planstart=NULL,planend=NULL,recstart=NULL,recend=NULL,force_create=FALSE)
bayeos.createDF(name,description=NULL,tz=NULL,con=1,id_parent=NULL,ls=TRUE,
		planstart=NULL,planend=NULL,recstart=NULL,recend=NULL,force_create=FALSE)
bayeos.updateDF(id,name=NA,description=NA,tz=NA,con=1,id_parent=NA,ls=FALSE,
		planstart=NA,planend=NA,recstart=NA,recend=NA)
bayeos.updateDFColumn(id,name=NA,colNr=NA,class=NA,description=NA,con=1,id_parent=NA,ls=FALSE,
		planstart=NA,planend=NA,recstart=NA,recend=NA)
bayeos.updateSeries(id,name=NA,tz=NA,description=NA,resolution=NA,
		planstart=NA,planend=NA,intervaltype=NA,id_parent=NA,
		lon=NA,lat=NA,elevation=NA,con=1,ls=FALSE)
bayeos.updateNode(id,name=NA,id_parent=NA,ls=TRUE,con=1)
bayeos.deleteNode(id,con=1,confirm=TRUE,ls=TRUE,removeRows=FALSE)
bayeos.getChilds(id,isExtern=FALSE,maxDepth=-1,pathFilter='',classFilter='',con=1)
bayeos.find(search,tree='Folder',con=1)
bayeos.getNode(id,con=1)


}
%- maybe also 'usage' for other objects documented here.
\arguments{
	\item{con}{connection identifier returned by bayeos.connect}  
	\item{name}{name of the series or folder to create}
	\item{id}{numeric folder id or path character, NULL for root folder}  
	\item{id_parent}{numeric folder id or path character}
  \item{force_create}{Create node even if a node with the same name exists}
	\item{ls}{print out modified directory}
	\item{full}{flag, if FALSE ls() returns short attribute columns} 
	\item{maxDepth}{interger, max. depth for tree search, -1==unlimited} 
	\item{pathFilter}{Filter string for ANT-like tree search. See below for details.} 
	\item{search}{String to search nodes. For substring search use '*temp*'}
	\item{classFilter}{Object type filter string}
	\item{tree}{Tree to search in. One of 'folder','device','compartment','location','target','unit','web'}
	\item{planstart}{Planned start of the data. Date time string interpreted 
		in the local time zone of the series.}
	\item{planend}{Planned end of the data. Date time string interpreted 
		in the local time zone of the series.}
	\item{recstart}{Record start of the data. Date time string interpreted 
		in the local time zone of the data.}
	\item{recend}{Record end of the data. Date time string interpreted 
		in the local time zone of the data.}
	\item{description}{Node description}
	\item{resolution}{time series resolution in seconds}
	\item{tz}{time zone string e.g. Etc/GMT-1}
	\item{intervaltype}{one of undefined,start,end}
	\item{lon}{longitude in degrees}
	\item{lat}{latitude in degrees}
	\item{elevation}{elevation in meter}
	\item{colNr}{Data frame column number}
	\item{class}{Data frame column data type (STRING,BOOLEAN,INTEGER,DOUBLE,DATE)}
	\item{confirm}{Needs confirmation before deleting a node}
	\item{warningLevel}{Checks meta data of node. 0=suppress warnings }
	\item{removeRows}{if true, a removeAllRows is called on the node before deleting}
	\item{isExtern}{set to TRUE to search the web object tree}
	
	 
}

\value{
  bayeos.ls prints out the current directory and returns an invisible data.frame with columns id,class,name,rec_start,rec_end.
  bayeos.createXXX return ID of new node.
  bayeos.updateXXX return true on success
  bayeos.getChilds and bayeos.find returns a data frame with the columns id,class,name,path
}

\details{
  With bayeos 1.4.3 and newer bayeos.createXXX will not create a node, if a 
  node with the same name exists in the folder. Use force_create=TRUE to have 
  the old behaviour.
 
}
\author{S. Holzheu, O. Archner}


\examples{
 	
	\dontrun{
	# open connection 
	bayeos.connect('http://www.bayceer.uni-bayreuth.de/servlet/XMLServlet','gast','gast')

	# show the current folder
	bayeos.ls()
	
	# change to a subfolder
	bayeos.cd(14333)
	# change to a folder 
	bayeos.cd(36076)
	# change to the parent folder
	bayeos.cd('..')
	# change to root folder
	bayeos.cd()
	# change to folder via path
	bayeos.cd('Micrometeorology Dept/AWS Ecological Botanical Garden')

	## Childs
	# get all childs of a specified node
	bayeos.getChilds(83)
	# get all childs of a specified node using filter
	# ** is a wildcard for any character int the path including the folder separater
	# * is a wildcard for all characters except the folder separator
	# To find all Nodes beginning with "temparature" anywhere in the tree  
	bayeos.getChilds(83,pathFilter='**/temperature*')

	## Searching the trees
	# Seraching all nodes having temperatur somewhere in the name
	bayeos.find('*temperatur*')
	# Finding Unit 'g/l'
	bayeos.find('m/s',tree='Unit')
	# getting all units 
	bayeos.find('*',tree='Unit')
	
	
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

\keyword{tree}
