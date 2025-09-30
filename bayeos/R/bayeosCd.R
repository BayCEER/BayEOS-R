


bayeos.findInFolder <- function(names,
                                id_parent = NULL,
                                con = 1) {
  if (is.null(id_parent))
    childs = .localenv$c[[con]]$childNodes
  else
    childs = bayeos.getChildNodes(id_parent, con)
  dir_names = sapply(childs, function(x)
    x$name)
  dir_ids = sapply(childs, function(x)
    x$id)
  ids = integer()
  for (i in 1:length(names)) {
    id = dir_ids[dir_names == names[i]]
    if (length(id) == 1) {
      rnames = c(names(ids), names[i])
      ids = c(ids, id)
      names(ids) = rnames
    }
  }
  ids
}

bayeos.id2id <- function(id, con = 1) {
  if (is.null(id))
    return(NULL)
  if (is.character(id))
    id = sapply(id, function(x)
      bayeos.path2id(x, con))
  as.integer(id)
}

bayeos.path2id <- function(path, con = 1) {
  path_list = .localenv$c[[con]]$path
  cd_vector = bayeos.path2vector(path)
  for (i in 1:length(cd_vector)) {
    cur = length(path_list)
    if (cd_vector[i] == '/') {
      root = bayeos.call(
        con,
        'TreeHandler.getRoot',
        'messung_ordner',
        .localenv$c[[con]]$activeFilter,
        .localenv$c[[con]]$missingInterval,
        .localenv$c[[con]]$timeFilter
      )
      names(root) = .localenv$c[[con]]$treeNames
      path_list <- list()
      path_list[[1]] <- root
    } else if (cd_vector[i] == '.') {
      ## nothing to do
    } else if (cd_vector[i] == '..') {
      if (cur == 1) {
        stop('Tried to decende below top level')
      }
      path_list[[cur]] <- NULL
    } else {
      #avoid fetching data from server when looking in current directory!
      if (path_list[[cur]]$id == .localenv$c[[con]]$path[[length(.localenv$c[[con]]$path)]]$id)
        childs = .localenv$c[[con]]$childNodes
      else
        childs = bayeos.getChildNodes(path_list[[cur]]$id)
      found = sapply(childs, function(x)
        x$name == cd_vector[i])
      if (is.list(found))
        num_found = 0
      else
        num_found = sum(found)
      if (num_found == 0) {
        stop('Node "', cd_vector[i], '" not found')
      }
      if (num_found > 1) {
        stop('Node name "', cd_vector[i], '" is not unique. Please use ID!')
      }
      path_list[[cur + 1]] = childs[[(1:length(found))[found]]]
    }
  }
  return(path_list[[length(path_list)]]$id)
}

bayeos.path2vector <- function(path) {
  id = character()
  
  while (substr(path, 1, 1) == '/') {
    id = c('/')
    path = substr(path, 2, nchar(path))
  }
  if (nchar(path) == 0)
    path = '.'
  f = strsplit(path, "/")
  
  append = FALSE
  
  for (i in 1:length(f[[1]])) {
    l = nchar(f[[1]][i])
    
    if (!append) {
      if (l > 0)
        id = c(id, f[[1]][i])
    }
    else
      id[-1] = paste(id[-1], f[[1]][i], sep = '')
    if (substr(f[[1]][i], l, l) == '\\') {
      append = TRUE
      
      l = nchar(id[-1])
      substr(id[-1], l, l) <- ''
      
    } else
      append = FALSE
  }
  id
}

## Helper function to read all direct childs of a node
bayeos.getChildNodes <- function(id, con = 1) {
  childs = bayeos.call(
    con,
    'TreeHandler.getChildren',
    id,
    'messung_massendaten',
    c('mitarbeiter', 'projekte'),
    .localenv$c[[con]]$activeFilter,
    .localenv$c[[con]]$missingInterval,
    .localenv$c[[con]]$timeFilter
  )
  childs = lapply(childs, function(x) {
    names(x) = .localenv$c[[con]]$treeNames
    x
  })
  childs
}

bayeos.readChilds <- function(con = 1) {
  id = tail(.localenv$c[[con]]$path, 1)[[1]]$id
  .localenv$c[[con]]$childNodes <- bayeos.getChildNodes(id)
}


bayeos.getChilds <- function(id = NA,
                             isExtern = FALSE,
                             maxDepth = -1,
                             pathFilter = '',
                             classFilter = '',
                             con = 1) {
  checkServerVersion(con, 1, 8)
  id = bayeos.id2id(id)
  
  if (is.na(id))
    id = tail(.localenv$c[[con]]$path, 1)[[1]]$id
  pathFilter = sub('^\\.?/', '', pathFilter) # strip ./ or / at beginning of pathFilter
  if (.localenv$c[[con]]$gis_features) {
    childs = bayeos.call(
      con,
      'TreeHandler.getObjektTreeWithLocation',
      as.integer(id),
      pathFilter,
      classFilter,
      as.integer(maxDepth),
      .localenv$c[[con]]$activeFilter,
      .localenv$c[[con]]$missingInterval,
      .localenv$c[[con]]$timeFilter
    )
    listlist2df(
      childs,
      c('id', 'name', 'class', 'path', 'lon', 'lat', 'alt'),
      c(1, 8, 10, 9, 11, 12, 13)
    )
    
  }	else {
    childs = bayeos.call(
      con,
      'TreeHandler.getAllChildren',
      as.integer(id),
      isExtern,
      c('mitarbeiter', 'projekte'),
      pathFilter,
      classFilter,
      as.integer(maxDepth),
      .localenv$c[[con]]$activeFilter,
      .localenv$c[[con]]$missingInterval,
      .localenv$c[[con]]$timeFilter
    )
    listlist2df(childs, c('id', 'name', 'class', 'path'), c(1, 8, 10, 9))
  }
  
}


bayeos.find <- function(search, tree = 'Folder', con = 1) {
  tree = tolower(tree)
  
  if (is.na(classes[tree])) {
    warning(paste('Unknown tree', tree))
    return(NULL)
  }
  bayeos.getChilds(
    .localenv$c[[con]]$root_ids[classes[tree]],
    pathFilter = paste('**/', sub('/', '\\\\/', search), sep = ''),
    isExtern = (tree == 'web')
  )
}

bayeos.pwd <- function(con = 1) {
  cat(sapply(.localenv$c[[con]]$path, function(x)
    x$name), '\n', sep = ' / ')
  invisible(.localenv$c[[con]]$path)
}

bayeos.ls <- function(con = 1, full = FALSE) {
  cur = tail(.localenv$c[[con]]$path, 1)[[1]]
  cat(sapply(.localenv$c[[con]]$path, function(x)
    x$name), '\n', sep = ' / ')
  #cat('Current Folder:',.localenv$c[[con]]$path[[cur]]$id,':\t',.localenv$c[[con]]$path[[cur]]$name,'\n\n')
  if (!is.null(cur$id_super))
    cat(cur$id_super, '\tparent_folder\t..\n')
  if (full)
    sapply(.localenv$c[[con]]$childNodes, function(x)
      print(x))
  else
    sapply(.localenv$c[[con]]$childNodes, function(x)
      cat(x$id, '\t', x$class, '\t', x$name, '\n'))
  if (length(.localenv$c[[con]]$childNodes) == 0)
    df = data.frame(
      id = vector(),
      class = vector(),
      name = vector(),
      rec_start = vector(),
      rec_end = vector()
    )
  else
    df = data.frame(
      id = sapply(.localenv$c[[con]]$childNodes, function(x)
        x$id),
      class = sapply(.localenv$c[[con]]$childNodes, function(x)
        x$class),
      name = sapply(.localenv$c[[con]]$childNodes, function(x)
        x$name),
      rec_start = as.POSIXct(
        sapply(.localenv$c[[con]]$childNodes, function(x)
          if (is.null(x$rec_start))
            NA
          else
            x$rec_start),
        origin = '1970-01-01',
        tz = .localenv$c[[con]]$tz
      ),
      rec_end = as.POSIXct(
        sapply(.localenv$c[[con]]$childNodes, function(x)
          if (is.null(x$rec_end))
            NA
          else
            x$rec_start),
        origin = '1970-01-01',
        tz = .localenv$c[[con]]$tz
      )
    )
  invisible(df)
}

bayeos.cd <- function(id = NULL,
                      con = 1,
                      ls = TRUE) {
  id = bayeos.id2id(id)
  if (is.null(id)) {
    root = bayeos.call(
      con,
      'TreeHandler.getRoot',
      'messung_ordner',
      .localenv$c[[con]]$activeFilter,
      .localenv$c[[con]]$missingInterval,
      .localenv$c[[con]]$timeFilter
    )
    names(root) = .localenv$c[[con]]$treeNames
    .localenv$c[[con]]$path <- list()
    .localenv$c[[con]]$path[[1]] <- root
    
  } else {
    found = sapply(.localenv$c[[con]]$childNodes, function(x)
      x$id == id)
    if (is.list(found))
      num_found = 0
    else
      num_found = sum(found)
    if (num_found == 0) {
      p = bayeos.getParents(id, con)
      if (is.null(p))
        stop('ID', id, 'not found!')
      .localenv$c[[con]]$path <- p
    } else
      .localenv$c[[con]]$path[[length(.localenv$c[[con]]$path) + 1]] <- .localenv$c[[con]]$childNodes[[(1:length(found))[found]]]
  }
  
  bayeos.readChilds(con)
  if (ls)
    bayeos.ls(con)
}

bayeos.getParents <- function(id, con = 1) {
  parents = list()
  while (is.numeric(id)) {
    if (.localenv$c[[con]]$bayeosVersion[1] == 1 &&
        .localenv$c[[con]]$bayeosVersion[2] < 9) {
      o = bayeos.call(con,
                      'ObjektHandler.getObjekt',
                      as.integer(id),
                      'messung_ordner')
      if (length(o) == 0)
        o = bayeos.call(con,
                        'ObjektHandler.getObjekt',
                        as.integer(id),
                        'messung_massendaten')
      
      if (length(o) == 1)
        return(NULL)
      names(o) = c(
        'check_write',
        'check_exec',
        'id',
        'id_super',
        'class',
        'id_art',
        'id_crole',
        'crole',
        'ctime',
        'id_urole',
        'urole',
        'utime',
        'dtime',
        'de',
        'en',
        'planstart',
        'planend',
        'recstart',
        'recend',
        'inherit_perm',
        'name',
        'description',
        'lon',
        'lat',
        'elevation'
      )
      
    } else
      o = bayeos.getNode(id)
    id = o$id_super
    parents = append(parents, list(o), 0)
  }
  parents
}




bayeos.updateSeries <- function(id,
                                name = NA,
                                tz = NA,
                                description = NA,
                                resolution = NA,
                                planstart = NA,
                                planend = NA,
                                intervaltype = NA,
                                id_parent = NA,
                                lon = NA,
                                lat = NA,
                                elevation = NA,
                                con = 1,
                                ls = FALSE) {
  id = bayeos.id2id(id, con)
  class = bayeos.getNode(id)$class
  bayeos.updateNode(id, name, id_parent, ls, con)
  attr = bayeos.call(con, 'ObjektHandler.getObjekt', as.integer(id), class)
  index = c(
    'planstart' = 16,
    'planend' = 17,
    'name' = 21,
    'description' = 22,
    'resolution' = 23,
    'intervaltype' = 24,
    'tz' = 25
  )
  if (.localenv$c[[con]]$gis_features) {
    index = c(index, c(
      'lon' = 26,
      'lat' = 27,
      'elevation' = 28
    ))
  }
  func_env = environment()
  if (!is.na(tz))
    tz = .localenv$c[[con]]$timezones[tz]
  if (!is.na(intervaltype))
    intervaltype = .localenv$c[[con]]$intervaltypes[intervaltype]
  
  sapply(names(index), function(x) {
    if (!is.null(get(x))) {
      if (is.na(get(x)))
        assign(x, attr[[index[x]]], envir = func_env)
    }
  })
  tz_string = names(.localenv$c[[con]]$timezones[tz])
  setBayeosConTZ(con, tz_string)
  if (is.character(planstart)) {
    if (planstart == '')
      planstart = NULL
    else
      planstart = as.POSIXct(planstart, tz = tz_string)
  }
  if (is.character(planend)) {
    if (planend == '')
      planend = NULL
    else
      planend = as.POSIXct(planend, tz = tz_string)
  }
  #	return(list(name,description,as.integer(resolution),planstart,planend,intervaltype,tz))
  if (!is.na(name))
    bayeos.call(con, 'TreeHandler.renameNode', as.integer(id), class, name)
  
  attr_list = list(
    name,
    description,
    as.integer(resolution),
    planstart,
    planend,
    as.integer(intervaltype),
    as.integer(tz)
  )
  if (.localenv$c[[con]]$gis_features) {
    attr_list[8] <- list(lon)
    attr_list[9] <- list(lat)
    attr_list[10] <- list(elevation)
  }
  bayeos.call(con,
              'ObjektHandler.updateObjekt',
              as.integer(id),
              class,
              attr_list)
}

bayeos.getNode <- function(id, con = 1) {
  id = bayeos.id2id(id, con)
  res = bayeos.call(con, 'TreeHandler.getNode', as.integer(id))
  names(res) = .localenv$c[[con]]$treeNames
  
  if (res$class %in% c(
    'data_frame',
    'data_column',
    'messung_massendaten',
    'messung_ordner',
    'mess_einheit',
    'mess_geraet',
    'mess_ort',
    'mess_kompartiment'
  )) {
    r = bayeos.call(con, 'ObjektHandler.getObjekt', as.integer(id), res$class)
    res$description = r[[22]]
    if (res$class == 'data_frame')
      res$tz = .localenv$c[[con]]$timezones[r[[23]]]
    if (res$class == 'data_column') {
      res$col_index = r[[23]]
      res$data_type = r[[24]]
    }
    if (res$class == 'messung_massendaten') {
      res$resolution = r[[23]]
      res$tz = .localenv$c[[con]]$timezones[r[[25]]]
      
    }
  }
  res
}

bayeos.createNode <- function(name,
                              uname,
                              id_parent = NULL,
                              con = 1,
                              ls = TRUE,
                              force_create = FALSE) {
  id_parent = bayeos.id2id(id_parent, con)
  id = bayeos.findInFolder(name, id_parent, con)
  
  if (length(id) > 0) {
    print(paste("Node", name, "exists already"))
  }
  
  if (length(id) == 0 || force_create) {
    if (is.null(id_parent)) {
      id_parent = tail(.localenv$c[[con]]$path, 1)[[1]]$id
    }
    NewNode = bayeos.call(con,
                          'TreeHandler.newNode',
                          uname,
                          name,
                          as.integer(id_parent))
    names(NewNode) = .localenv$c[[con]]$treeNames
    bayeos.readChilds(con)
    id = NewNode$id
  }
  if (ls)
    bayeos.ls(con)
  id
}


bayeos.createDF <- function(name,
                            description = NULL,
                            tz = NULL,
                            con = 1,
                            id_parent = NULL,
                            ls = TRUE,
                            planstart = NULL,
                            planend = NULL,
                            recstart = NULL,
                            recend = NULL,
                            force_create = FALSE) {
  checkServerVersion(con, 1, 9)
  if (is.null(tz))
    tz = .localenv$c[[con]]$tz
  id = bayeos.createNode(name, 'data_frame', id_parent, con, ls, force_create)
  bayeos.call(
    con,
    'ObjektHandler.updateObjekt',
    as.integer(id),
    'data_frame',
    list(
      planstart,
      planend,
      recstart,
      recend,
      name,
      description,
      as.integer(.localenv$c[[con]]$timezones[tz])
    )
  )
  id
}

bayeos.updateNode <- function(id,
                              name = NA,
                              id_parent = NA,
                              ls = TRUE,
                              con = 1) {
  id_parent = bayeos.id2id(id_parent, con)
  id = bayeos.id2id(id, con)
  node = bayeos.getNode(id, con)
  res = TRUE
  if (!is.na(name))
    res = bayeos.call(con, 'TreeHandler.renameNode', node$id, node$class, name)
  if (res &&
      !is.na(id_parent))
    res = bayeos.call(con, 'TreeHandler.moveNode', node$id, as.integer(id_parent))
  if (ls)
    bayeos.cd(bayeos.getNode(node$id)$id_super)
  res
}

bayeos.updateDF <- function(id,
                            name = NA,
                            description = NA,
                            tz = NA,
                            con = 1,
                            id_parent = NA,
                            ls = FALSE,
                            planstart = NA,
                            planend = NA,
                            recstart = NA,
                            recend = NA) {
  id = bayeos.id2id(id, con)
  bayeos.updateNode(id, name, id_parent, ls, con)
  attr = bayeos.call(con,
                     'ObjektHandler.getObjekt',
                     as.integer(id),
                     'data_frame')
  index = c(
    'planstart' = 16,
    'planend' = 17,
    'recstart' = 18,
    'recend' = 19,
    'name' = 21,
    'description' = 22,
    'tz' = 23
  )
  func_env = environment()
  if (!is.na(tz))
    tz = .localenv$c[[con]]$timezones[tz]
  
  sapply(names(index), function(x) {
    if (!is.null(get(x))) {
      if (is.na(get(x)))
        assign(x, attr[[index[x]]], envir = func_env)
    }
  })
  tz_string = names(.localenv$c[[con]]$timezones[tz])
  setBayeosConTZ(con, tz_string)
  index = c('planstart', 'planend', 'recstart', 'recend')
  sapply(index, function(x) {
    if (is.character(get(x))) {
      if (get(x) == '')
        assign(x, NULL, envir = func_env)
      else
        assign(x, as.POSIXct(get(x), tz = tz_string, envir = func_env))
    }
  })
  
  bayeos.call(
    con,
    'ObjektHandler.updateObjekt',
    as.integer(id),
    'data_frame',
    list(
      planstart,
      planend,
      recstart,
      recend,
      name,
      description,
      as.integer(.localenv$c[[con]]$timezones[tz])
    )
  )
  
  
  
}

bayeos.updateDFColumn <- function(id,
                                  name = NA,
                                  colNr = NA,
                                  class = NA,
                                  description = NA,
                                  con = 1,
                                  id_parent = NA,
                                  ls = FALSE,
                                  planstart = NA,
                                  planend = NA,
                                  recstart = NA,
                                  recend = NA) {
  id = bayeos.id2id(id, con)
  bayeos.updateNode(id, name, id_parent, ls, con)
  attr = bayeos.call(con,
                     'ObjektHandler.getObjekt',
                     as.integer(id),
                     'data_column')
  fnames = c(
    'planstart' = 16,
    'planend' = 17,
    'recstart' = 18,
    'recend' = 19,
    'name' = 21,
    'description' = 22,
    'colNr' = 23,
    'class' = 24
  )
  func_env = environment()
  
  sapply(names(fnames), function(x) {
    if (!is.null(get(x))) {
      if (is.na(get(x)))
        assign(x, attr[[fnames[x]]], envir = func_env)
    }
  })
  
  
  fnames = c('planstart', 'planend', 'recstart', 'recend')
  sapply(fnames, function(x) {
    if (is.character(get(x))) {
      if (get(x) == '')
        assign(x, NULL, envir = func_env)
      else
        assign(x, as.POSIXct(get(x), tz = .localenv$c[[con]]$tz, envir = func_env))
    }
  })
  
  bayeos.call(
    con,
    'ObjektHandler.updateObjekt',
    as.integer(id),
    'data_column',
    list(
      planstart,
      planend,
      recstart,
      recend,
      name,
      description,
      as.integer(colNr),
      class
    )
  )
  
  
  
}


bayeos.createDFColumn <- function(name,
                                  colNr,
                                  class,
                                  description = NULL,
                                  con = 1,
                                  id_parent = NULL,
                                  ls = TRUE,
                                  planstart = NULL,
                                  planend = NULL,
                                  recstart = NULL,
                                  recend = NULL,
                                  force_create = FALSE) {
  checkServerVersion(con, 1, 9)
  id = bayeos.createNode(name, 'data_column', id_parent, con, ls, force_create)
  bayeos.call(
    con,
    'ObjektHandler.updateObjekt',
    as.integer(id),
    'data_column',
    list(
      planstart,
      planend,
      recstart,
      recend,
      name,
      description,
      as.integer(colNr),
      class
    )
  )
  id
}

bayeos.createFolder <- function(name,
                                id_parent = NULL,
                                con = 1,
                                ls = TRUE,
                                force_create = FALSE) {
  bayeos.createNode(name, 'messung_ordner', id_parent, con, ls, force_create)
}

bayeos.createSeries <- function(name,
                                con = 1,
                                ls = TRUE,
                                warningLevel = 1,
                                tz = 'Etc/GMT-1',
                                description = '',
                                resolution = 600,
                                planstart = NULL,
                                planend = NULL,
                                intervaltype = 'undefined',
                                id_parent = NULL,
                                lon = NULL,
                                lat = NULL,
                                elevation = NULL,
                                force_create = FALSE) {
  setBayeosConTZ(con, tz)
  
  id = bayeos.createNode(name, 'messung_massendaten', id_parent, con, ls, force_create)
  
  if (is.character(planstart))
    planstart = as.POSIXct(planstart, tz = tz)
  if (is.character(planend))
    planend = as.POSIXct(planend, tz = tz)
  attr_list = list(
    name,
    description,
    as.integer(resolution),
    planstart,
    planend,
    as.integer(.localenv$c[[con]]$intervaltypes[intervaltype]),
    as.integer(.localenv$c[[con]]$timezones[tz])
  )
  if (.localenv$c[[con]]$gis_features) {
    attr_list[8] <- list(lon)
    attr_list[9] <- list(lat)
    attr_list[10] <- list(elevation)
  }
  bayeos.call(
    con,
    'ObjektHandler.updateObjekt',
    as.integer(id),
    'messung_massendaten',
    attr_list
  )
  id
}



bayeos.deleteNode <- function(id,
                              con = 1,
                              confirm = TRUE,
                              ls = TRUE,
                              removeRows = FALSE) {
  id = bayeos.id2id(id, con)
  node = bayeos.getNode(id)
  if (confirm) {
    confirm = readline(prompt = paste(
      'Do you really want to delete node ',
      node$name,
      '(type yes to confirm)'
    ))
    if (confirm != 'yes')
      return(FALSE)
  }
  if (node$class == 'messung_massendaten' & removeRows)
    bayeos.call(con, 'MassenTableHandler.removeAllRows', as.integer(id))
  
  res = bayeos.call(con, 'TreeHandler.deleteNode', as.integer(id))
  if (res)
    bayeos.readChilds(con)
  if (ls)
    bayeos.ls(con)
  res
}
