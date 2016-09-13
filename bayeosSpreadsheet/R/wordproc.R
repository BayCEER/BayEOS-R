OpenOfficeNamespaces = bayeosSpreadsheet:::OpenOfficeNamespaces

getTitle =
function(doc, subtitle = TRUE)
{
  q =  c("//text:p[@text:style-name='Title']", "//text:p[@text:style-name='Subtitle']")
  if(!subtitle)
     q = q[1]
  xpathSApply(doc, paste(q, collapse = " | "),
               xmlValue, namespaces = OpenOfficeNamespaces["text"])
}


getTables =
function(doc, which = NA)
{
  nodes = getNodeSet(doc, "//table:table", OpenOfficeNamespaces["table"])
  
}
