# TODO: Add comment
#
# Author: holzheu
###############################################################################


bayeos.getDF <- function(ids, rowNr = NULL, con = 1) {
  ids = bayeos.id2id(ids, con)
  checkServerVersion(con, 1, 9)
  if (!is.null(rowNr)) {
    rowNr = as.integer(rowNr)
    if (length(rowNr) == 1)
      rowNr = as.list(rowNr)
  }
  if (length(ids) == 1 && is.character(ids))
    ids = bayeos.getNode(ids)$id
  
  #calls like MassenTableHandler.getMatrix
  if (length(ids) == 1 && bayeos.getNode(ids)$class == 'data_frame')
    res = bayeos.call(con,
                      'DataFrameHandler.getFrameRows',
                      as.integer(ids),
                      rowNr)
  else{
    ids = as.integer(ids)
    if (length(ids) == 1)
      ids = as.list(ids) #force vector over list
    res = bayeos.call(con, 'DataFrameHandler.getColumnRows', ids, rowNr)
  }
  # list( colattr(), values())
  attr = listlist2df(res[[1]])
  listlist2df(res[[2]], attr[[3]], row.names = TRUE)
}

bayeos.getDFZoo <- function(ids,
                            zooIndexColumn = 1,
                            aggregateFun = NULL,
                            con = 1) {
  checkServerVersion(con, 1, 9)
  ids = bayeos.id2id(ids, con)
  cnr = 0
  for (id in ids) {
    df = bayeos.getDF(id, con)
    name = bayeos.getNode(id)$name
    index = df[[zooIndexColumn]]
    df[[zooIndexColumn]] = NULL
    if (!is.null(aggregateFun)) {
      index_new = aggregate(df[[1]] ~ index, FUN = aggregateFun)[, 1]
      df = as.data.frame(sapply(df, function(x)
        aggregate(x ~ index, FUN = aggregateFun)[, 2]))
      index = index_new
    }
    tmp = zoo(df, index)
    names(tmp) = paste(name, names(df))
    if (cnr > 0)
      res = cbind(res, tmp)
    else
      res = tmp
    cnr = cnr + 1
  }
  res
}

bayeos.writeDF <- function(ids,
                           data,
                           rowNr = NULL,
                           con = 1) {
  checkServerVersion(con, 1, 9)
  ids = bayeos.id2id(ids, con)
  if (is.null(rowNr)) {
    sapply(1:length(ids), function(x) {
      if (class(data[[x]])[1] == "factor")
        data[[x]] = as.character(data[[x]])
      bayeos.call(con,
                  'DataFrameHandler.deleteColValues',
                  as.integer(ids[x]))
      if (length(data[[x]]) == 1)
        #Force vector over list!!
        bayeos.call(con,
                    'DataFrameHandler.writeColValues',
                    as.integer(ids[x]),
                    as.list(data[[x]]))
      else
        bayeos.call(con,
                    'DataFrameHandler.writeColValues',
                    as.integer(ids[x]),
                    data[[x]])
    })
  } else {
    rowNr = as.integer(rowNr)
    if (length(rowNr) == 1)
      rowNr = as.list(rowNr)
    sapply(1:length(ids), function(x) {
      if (class(data[[x]])[1] == "factor")
        data[[x]] = as.character(data[[x]])
      if (length(data[[x]]) == 1)
        #Force vector over list!!
        bayeos.call(
          con,
          'DataFrameHandler.updateColValues',
          as.integer(ids[x]),
          rowNr,
          as.list(data[[x]])
        )
      else
        bayeos.call(con,
                    'DataFrameHandler.updateColValues',
                    as.integer(ids[x]),
                    rowNr,
                    data[[x]])
    })
    
  }
}

.DataType <- function(x) {
  #	STRING, DOUBLE, INTEGER, DATE, BOOLEAN;
  switch(
    class(x)[1],
    'numeric' = 'DOUBLE',
    'integer' = 'INTEGER',
    'POSIXct' = 'DATE',
    'Date' = 'DATE',
    'logical' = 'BOOLEAN',
    'STRING'
  )
}


bayeos.importDF <- function(id,
                            data,
                            columnSelection = NULL,
                            columnDestination = NULL,
                            append = FALSE,
                            con = 1) {
  checkServerVersion(con, 1, 9)
  if (class(data) != 'data.frame')
    return(FALSE)
  
  
  dfnode = bayeos.getNode(id)
  if (dfnode$class != 'data_frame') {
    warning(paste(bayeos.getNode(id)$name, 'is not a data_frame'))
    return(FALSE)
  }
  id = dfnode$id
  childs = bayeos.getChilds(id)
  if (!is.null(childs$class)) {
    selection = childs$class == 'data_column'
    child_class = childs$class[selection]
    child_ids = childs$id[selection]
    child_names = childs$name[selection]
  } else {
    child_class = character()
    child_ids = integer()
    child_names = character()
  }
  names(child_ids) = child_names
  # Take names from data, if columnDestination is not given
  if (is.null(columnSelection))
    columnSelection = 1:(length(names(data)))
  if (is.null(columnDestination)) {
    if (is.numeric(columnSelection)) {
      columnDestination = names(data)[columnSelection]
    } else
      columnDestination = columnSelection
  }
  # create names if not present
  ids = sapply(1:length(columnDestination), function(x) {
    if (is.na(child_ids[columnDestination[x]]) | is.list(child_ids))
      bayeos.createDFColumn(
        columnDestination[x],
        x,
        .DataType(data[[x]]),
        con = con,
        ls = FALSE,
        id_parent = id
      )
    else
      child_ids[columnDestination[x]]
    
  })
  if (append) {
    cids = ids
    if (length(cids) == 1)
      cids = as.list(cids)
    max = bayeos.call(con, 'DataFrameHandler.getMaxRowIndex', cids)
    rowNr = seq(max + 1, max + length(data[[1]]))
  } else
    rowNr = NULL
  bayeos.writeDF(ids, data[, columnSelection], rowNr, con)
}
