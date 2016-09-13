.localenv <- new.env()

xml.rpc =
function(url, method, ..., .args = list(...),
          .opts = list(),
          .defaultOpts = list(httpheader = c('Content-Type' = "text/xml")),
          .convert = TRUE, .curl = getCurlHandle(),serverTZ='',clientTZ=NULL,.debugRPC=FALSE)
{
	# Setting a package variable 
	.localenv$ServerTZ<-serverTZ
	if(is.null(clientTZ)) clientTZ=serverTZ
	.localenv$ClientTZ<-clientTZ
    # Turn the method and arguments to an RPC body.
  body = createBody(method,  .args)

   if( .debugRPC)  
	  warning(saveXML(body));
   # merge the .defaultOpts and the .opts into one list.
  .defaultOpts[["postfields"]] = saveXML(body)
  if(length(.opts))
     .defaultOpts[names(.opts)] = .opts
  
  ans = postForm(url, .opts = .defaultOpts, style = "POST", curl = .curl)

   # Now either convert using the default converter fnction (convertToR)
   # or return as is or allow the caller to specify a function to use for conversion.
  if(is.logical(.convert)) {
    if(.convert)
      convertToR(ans)
    else
      ans
  } else if(is.function(.convert))
          .convert(ans)
  else
      ans
}

createBody =
function(method, args)
{
  top = newXMLNode("methodCall", newXMLNode("methodName", method))
  params = newXMLNode("params", parent = top)
  sapply(args, function(x) newXMLNode("param", rpc.serialize(x), parent = params))
  top
}

setGeneric("rpc.serialize", function(x, ...) standardGeneric("rpc.serialize"))

basicTypeMap =
  c("integer" = "i4",
    "double" = "double",
    "character" = "string",
	"factor" = "string",
	"logical" = "boolean",
    "raw" = "base64",
	"NULL" = "nil")

cast <- function(x) {
  if (is.logical(x))
    as.integer(x)
  else if(is.factor(x))
	as.character(x)
  else
    x
}
setMethod("rpc.serialize","NULL",
		function(x,...)
			newXMLNode("value", newXMLNode("nil"))
)

setMethod("rpc.serialize", "POSIXct",
		function(x, ...) {
			val = strftime(x,'%Y%m%dT%H:%M:%S',tz=.localenv$ServerTZ);
#			if(length(names(x))) {
#				warning("Skipping named vector!")
#			} else {
				if(length(x) == 1)
					newXMLNode("value", newXMLNode("dateTime.iso8601", val))
				else {
					
					newXMLNode("value",newXMLNode("array",newXMLNode("data",sapply(x, function(x) newXMLNode("value", newXMLNode("dateTime.iso8601", 
												strftime(x,'%Y%m%dT%H:%M:%S',tz=.localenv$ServerTZ)))))))
				}
#			}
		})

setMethod("rpc.serialize", "raw",
           function(x, ...) {
 #             x = gsub("\\n", "", x) ## Why should we replace newline??? For real binary data this will fail! 
              val = base64Encode(x)
              newXMLNode("value", newXMLNode("base64", val))
           })

setMethod("rpc.serialize", "vector",
           function(x, ...) {
              type = basicTypeMap[typeof(x)]
              x = cast(x)
              
#              if(length(names(x))) {
#warning("Skipping named vector!")
#              } else {
                if(length(x) == 1)
                  newXMLNode("value", newXMLNode(type, if(type == "string") newXMLCDataNode(enc2utf8(x)) else x))
                else {
				  newXMLNode("value",
					newXMLNode("array",
					  newXMLNode("data",
						sapply(x, function(x) newXMLNode("value", newXMLNode(type, if(type == "string") newXMLCDataNode(enc2utf8(x)) else x)))
				)))
                }
#              }
           })

setMethod("rpc.serialize", "list",
		   function(x, ...) {
			   if(length(names(x))) {
				   newXMLNode("value",
					 newXMLNode("struct",
					  sapply(names(x), function(id) {
							   type = basicTypeMap[typeof(x[[id]])]
							   newXMLNode("member", newXMLNode("name", id),
									   newXMLNode("value", rpc.serialize(x[[id]])))
						   })
		   		))
			   } else {
				  newXMLNode("value",
					newXMLNode("array",
					  newXMLNode("data", .children=lapply(x, function(x) rpc.serialize(x)))
			  		))
			   }
		   })
    
setGeneric('convertToR', function(node) standardGeneric('convertToR'))

setMethod('convertToR', 'XMLInternalDocument', function(node)
{
    fault = getNodeSet(node,path="//methodResponse/fault/value/struct")
    if (length(fault) > 0) {
          fault = xmlRPCToR(fault[[1]])
          stop("faultCode: ",  fault$faultCode, " faultString: ", fault$faultString)
    }
    a = xpathApply(node, "//param/value", xmlRPCToR)
    if(length(a) == 1)
      a[[1]]
    else
      a
})

setMethod('convertToR', 'XMLInternalNode',
function(node)
{
   if(length(getNodeSet(node, "./param/value"))) {
     ans = xpathApply(node, "./param/value", xmlRPCToR, simplify = FALSE)
   } else
      xmlToList(node)
})

setMethod('convertToR', 'character',
function(node)
{
  convertToR(xmlParse(node, asText = TRUE,encoding='UTF-8'))
})

xmlRPCToR =
function(node, ...)
{
  if(is.null(node))
    return(NULL)
  
  if(xmlName(node) == "value")
    node = node[[1]]

  if(is(node, "XMLInternalTextNode"))
    return(xmlValue(node,encoding='UTF-8'))
  
  type = xmlName(node)
  switch(type,
         'array' = xmlRPCToR.array(node, ...),
         'nil' = NULL,
         'struct' = xmlRPCToR.struct(node, ...),
         'i4' = as.integer(xmlValue(node)),
         'int' = as.integer(xmlValue(node)),
         'boolean' = if(xmlValue(node) == "1") TRUE else FALSE,
         'double' = as.numeric(xmlValue(node)),
		 'string' = xmlValue(node,encoding='UTF-8'),
         'dateTime.iso8601' = xmlRPCToR.dateTime.iso8601(node),
		  # The original version with mode=character does not work for real binary data!  
         'base64' = if(nchar(xmlValue(node)) == 0) NULL else base64(xmlValue(node), encode = FALSE, mode='raw'),		
         xmlValue(node,encoding='UTF-8')
        )
}

xmlRPCToR.dateTime.iso8601 =
function(node)
{
	ans = as.POSIXct(strptime(xmlValue(node), "%Y%m%dT%H:%M:%S",tz=.localenv$ServerTZ))
	attr(ans,'tzone')=.localenv$ClientTZ
	ans
}

xmlRPCToR.struct =
function(node, ...)
{
  ans = xmlApply(node, function(x) xmlRPCToR(x[["value"]][[1]], ...))
  names(ans) = xmlSApply(node, function(x) xmlValue(x[["name"]]))
  ans
}


xmlRPCToR.array =
function(node, ...)
{
  ans = xmlApply(node[["data"]], function(x) xmlRPCToR(x[[1]]))
# support for empty Arrays...
  if(length(ans)==0) return(NULL)
  # vector of vector....
#  if(!is.list(ans[[1]]) && all(sapply(ans, typeof) == typeof(ans[[1]])))
#    structure(unlist(ans), names = NULL)
#  else
    ans
}
