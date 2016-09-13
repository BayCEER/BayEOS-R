

bayeos.getACL <- function(id=NA,ls=FALSE,con=1){
	id=bayeos.id2id(id,con)
	if(is.na(id))
		id=tail(.localenv$c[[con]]$path,1)[[1]]$id
	rights=bayeos.call(con,'RightHandler.getRights',as.integer(id))
	df=listlist2df(rights[[1]],c('user','read','write','exec','inherit','editable','uname','rid'),1:8)
	if(ls){
	  print(df[c(1:6,8)])	
	  invisible(df)
    } else df
}

getRid <- function(rid){
	if(is.character(rid)) bayeos.getRid(rid)
	else rid
}

bayeos.createACL <- function(rid,id=NA,read=TRUE,write=FALSE,exec=FALSE,inherit=FALSE,ls=FALSE,con=1){
	id=bayeos.id2id(id,con)
	if(is.na(id))
		id=tail(.localenv$c[[con]]$path,1)[[1]]$id
	rid=getRid(rid)
	res=bayeos.call(con,'RightHandler.createRight',as.integer(id),as.integer(rid),read,write,exec,inherit)
	if(ls) bayeos.getACL(id,ls=TRUE)
	res
}

bayeos.deleteACL <- function(rid,id=NA,ls=FALSE,con=1){
	id=bayeos.id2id(id,con)
	if(is.na(id))
		id=tail(.localenv$c[[con]]$path,1)[[1]]$id
	rid=getRid(rid)
	res=bayeos.call(con,'RightHandler.deleteRight',as.integer(id),as.integer(rid))
	if(ls) bayeos.getACL(id,ls=TRUE)
	res
}

bayeos.updateACL <- function(right,value,rid,id=NA,ls=FALSE,con=1){
	id=bayeos.id2id(id,con)
	if(is.na(id))
		id=tail(.localenv$c[[con]]$path,1)[[1]]$id
	rid=getRid(rid)
	res=bayeos.call(con,'RightHandler.updateRight',as.integer(id),as.integer(rid),right,value)
	if(ls) bayeos.getACL(id,ls=TRUE)
	res
	
}

bayeos.getRoles <- function(con=1){
	if(is.null(.localenv$c[[con]]$roles)){
		res=c(bayeos.call(con,'LookUpTableHandler.getBenutzer'),
				bayeos.call(con,'LookUpTableHandler.getGruppen'))
		.localenv$c[[con]]$roles=listlist2df(res,c('fullname','uname','class','rid'),c(14,21,5,3))
	}
	.localenv$c[[con]]$roles
}



bayeos.getRid <- function(name,con=1){
	roles=bayeos.getRoles(con)
	id=roles$rid[roles$uname==name]
	if(length(id)==0) id=roles$rid[roles$fullname==name]
	if(length(id)==0) id=NA
	id
}


