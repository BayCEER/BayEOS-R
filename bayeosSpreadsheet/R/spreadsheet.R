# Came from RExcelML
if(FALSE) {
setClass("OOWorkbook", contains = "ZipArchiveEntry")
setClass("OOWorksheet", representation(content = "XMLInternalDocument", name = "ZipArchiveEntry"))
setClass("OOWorksheetFile", contains = "ZipArchiveEntry")

setMethod("names", "OOWorkbook",
            function(x) {
              doc = xmlParse(x[["content.xml"]])
              unlist(getNodeSet(doc, "//table:table/@table:name", "table"))
            })
#setMethod("[[", "Workbook"
}

read.ods =
function(file, header = TRUE, simplify = TRUE,
          doc = xmlParse(zipArchive(file)[["content.xml"]]),
          stringsAsFactors = TRUE,
		  tz='')
{
  # defining package variable ROpenOfficeConversionTZ
  ROpenOfficeConversionTZ<<-tz
  tb = getNodeSet(doc, "//table:table", "table")
  ans = lapply(tb, read.odsSheet, header = header, stringsAsFactors = stringsAsFactors)

  n = sapply(ans, is.null)
  if(any(n)) {
     ans = ans[!n]
     tb = tb[!n]
  }
  
  if(simplify && length(ans) == 1)
    return(ans[[1]])

  names(ans) = sapply(tb, xmlGetAttr, "name")
  ans
}

read.odsSheet =
  #
  #
  # We need to deal with blank rows and where cells are repeated.
  #   In progress
  #
  # tb is the table:table node.
  #
function(tb, header = TRUE, stringsAsFactors = TRUE) # really a header?
{

    # Get num columns from the first row.
  numCols = xmlGetAttr(tb[[1]], "number-columns-repeated", 0, as.integer)

    # Get all rows which have a cell with a office:value entry. Otherwise
    # it is an empty row.
  rows = getNodeSet(tb, "./table:table-row[./table:table-cell[@office:value | @office:value-type]]",
                        OpenOfficeNamespaces[c("office", "table")])

  if(length(rows) == 0)
     return(NULL)

  rowNames = FALSE
  varNames = character()
  
  if(header) {
    varNames = xmlSApply(rows[[1]], getCellValue)
#   if(length(varNames) && is.na(varNames[1])) {
#      rowNames = TRUE
#      varNames = varNames[-1]
#   }
    rows = rows[-1]
  } 


     # This gets the types by row and this might be ragged, i.e. not by column
     # Now changed to expand the missing columns so won't be ragged.
  types = t(sapply(rows, function(x) unlist(xmlApply(x, getCellType))))
     # Now get all the cells, expanding the elements that are missing so all the
     # the result will be a matrix
  ans = t(sapply(rows,
                 function(x) {
                    unlist(xmlApply(x, getCellValue), recursive = FALSE)
                  }))

  realCols = apply(ans, 2, function(x) any(!is.na(x)))

  if(!realCols[1]) {
      rowNames = FALSE
  }

  if(header) {
    if(is.na(varNames[1]) && realCols[1]) {
       rowNames = TRUE
    }
  }  

  
  ans = ans[ , realCols ]
  types = types[, realCols]


  if(!is.matrix(ans)) {
    tp = getColType(types)
    return(if(is.function(tp))
             tp(ans)
           else
             as(ans, tp))
  }  
  
  if(length(varNames))
     varNames = varNames[realCols]



    # This seems to go along rows, not columns
  tmp = lapply(seq(length = ncol(ans)),
               function(i) {
                  if(all(is.na(types[,i])))
                    return(NULL)
                  tp = unique(unlist(types[,i]))
                  if(is.null(tp))
                    return(NULL)
                  colType = getColType(tp)
                  if(is.function(colType))
                    colType(ans[,i])
                  else
                    as(unlist(ans[,i]), colType)
               })
  tmp = tmp[!sapply(tmp, is.null)]
  ans = as.data.frame(tmp, stringsAsFactors = stringsAsFactors)

  
  if(rowNames) {
    rownames(ans) = ans[,1]
    ans = ans[,-1]
    varNames = varNames[-1]
  }
  
  structure(ans, names = if(length(varNames))
                            as.character(varNames)
                         else
                            paste("V", seq(length = length(tmp)), sep = ""))
}

getCellType =
function(node)
{

  n = xmlGetAttr(node, "number-columns-repeated", 0)
  txt = xmlGetAttr(node, "value-type", NA)
  if(n > 0)
     rep(txt, n)
  else
     txt
}
  


getCellValue =
  #
  # Get the cell value or a collection of NAs if this is a number-columns-repeated cell.
  #
function(node)
{
  n = xmlGetAttr(node, "number-columns-repeated", 0) 
  txt = xmlGetAttr(node, "value", NA)
  if(is.na(txt)) txt = xmlGetAttr(node, "date-value", NA)
  if(is.na(txt) ) txt = xmlGetAttr(node, "time-value", NA)
  if(is.na(txt)) txt=  { if(xmlSize(node)) xmlValue(node) else as.character(NA) }
  if(n > 0)
     rep(txt, n)
  else
     txt
}

# Added converion methods for time and datetime
# As there is no R time class time values will get
# converted to POSIXct of 1970-01-01 (S.Holzheu)

Rtypes = c("string" = "character",
           "float" = "numeric",
		   "time" = function(x) { as.POSIXct(paste('1970-01-01',x), format="%Y-%m-%d PT%HH%MM%SS",tz=ROpenOfficeConversionTZ) },
           "date" = function(x) { if(grepl('T',x[1])) as.POSIXct(x, format="%Y-%m-%dT%H:%M:%S",tz=ROpenOfficeConversionTZ) else as.Date(x,format="%Y-%m-%d") },
           "percentage" = "numeric")

getColType =
function(types)
{
  types = types[!is.na(types)]
  if(length(types) == 1)
    Rtypes[[types]]
  else {
    i = match(types, names(Rtypes))
    Rtypes[[min(i)]]
  }
}
