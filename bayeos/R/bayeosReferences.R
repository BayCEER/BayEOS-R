bayeos.getReferences <- function(id,con=1){
	id=bayeos.id2id(id,con)
	res=append(bayeos.call(con,'ObjektHandler.getDirectReferences',as.integer(id)),
			bayeos.call(con,'ObjektHandler.getInheritedReferences',as.integer(id)))
#	id_ref=sapply(res,function(x) x[[5]])
	type=sapply(res,function(x) {if(is.null(x[[1]])) '?' else x[[1]]}) #Null for Department - Fix in goat.server?
	name=sapply(res,function(x) x[[2]])
	direct=sapply(res,function(x) (x[[8]]==id))
	df=data.frame(name,type,direct)
	if(length(df)==0) df=data.frame(name=vector(),type=vector(),direct=vector())
	df
}

bayeos.deleteReferences <- function(id,type,con=1){
	id=bayeos.id2id(id,con)
	type=tolower(type);
	res=bayeos.call(con,'ObjektHandler.getDirectReferences',as.integer(id))
	deleted=sapply(res,function(x){
				if(tolower(x[[1]])==type | type=='all')
					bayeos.call(con,'ObjektHandler.deleteReference',as.integer(x[[5]]),x[[7]])
				else FALSE  	
			})
	bayeos.getReferences(id,con)
}

bayeos.createReferences <- function(id,to,tree,create=FALSE,id_parent=NULL,con=1){
	id=bayeos.id2id(id,con)
	tree=tolower(tree);
	if(is.na(classes[tree])){
		warning(paste('Unknown tree',tree))
		return(FALSE)
	}	
	if(! is.numeric(to)){
		res=bayeos.find(to,tree)
		if(length(res)==0 & ! create){
			warning(paste(to,'not found in',tree))
			return(FALSE)
		}
		if(length(res)==0 & create){
			message('Creating new reference node')
			to=bayeos.createReferenceNode(to,tree,id_parent,con)
		}else{
			if(length(res[,1])>1){
				print('More than one match:')
				print(res)
				num=readline(prompt = 'Please enter line number:')
				to=res$id[as.integer(num)]
			}
			if(length(res[,1])==1)
				to=res$id[1]
		}
	}
	res=bayeos.call(con,'ObjektHandler.createReference',as.integer(to),as.integer(id),as.character(classes[tree]))
	bayeos.getReferences(id,con)
}


bayeos.createReferenceNode <- function(name,tree,id_parent=NULL,con=1){
	id_parent=bayeos.id2id(id_parent,con)
	tree=tolower(tree);
	if(is.na(classes[tree])){
		warning(paste('Unknown tree',tree))
		return(FALSE)
	}	
	if(is.null(id_parent)) id_parent= .localenv$c[[con]]$root_ids[classes[tree]]
	bayeos.call(con,'TreeHandler.newNode',as.vector(classes[tree]),name,as.integer(id_parent))[[3]]
}

bayeos.deleteReferenceNode <- function(id,con=1){
	id=bayeos.id2id(id,con)
	bayeos.call(con,'TreeHandler.deleteNode',as.integer(id))
}
