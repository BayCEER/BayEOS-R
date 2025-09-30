.setUp <- function() {
  library(bayeos)
  bayeos.connect('dev')
  f = bayeos.createFolder("Test GetSeries")
  bayeos.cd(f)
  m1 = bayeos.readCSV(
    system.file("sampleData", "datetime.csv", package = "bayeos")
    ,
    dec = ',',
    sep = ';',
    na.string = '<NA',
    dateformat = '%d.%m.%y %H:%M'
  )
  # Import frame values into database
  bayeos.import(m1)
  
}

test.GetSeries <- function() {
  id = bayeos.createSeries('testSeries', warningLevel = 0)
  m = bayeos.getSeries(id)
  # empty zoo with 0 rows returned
  checkTrue(length(m) == 0)
  bayeos.deleteNode(id, confirm = FALSE)
  
  bayeos.cd()
  d = bayeos.getChilds(pathFilter = 'Test GetSeries/*')
  #interval
  m = bayeos.getSeries(d$id[1], from = '2010-01-01 15:00:00', until = '2010-01-01 16:00:00')
  checkTrue(length(m) == 6)
  
  bayeos.updateSeries(d$id[1], resolution = 0)
  # Series with null resolution settings
  m = bayeos.getSeries(d$id[1], from = '2010-01-01 00:00:00', until = '2010-02-01 10:00:00')
  checkTrue(length(m) > 0)
  
  # Matrix
  m = bayeos.getSeries(d$id, from = "2010-01-01", until = "2010-01-02")
  checkTrue(length(names(m)) == 2)
  checkTrue(length(m) == 2 * 43)
  
  #Duplicate IDs
  d = bayeos.getChilds(pathFilter = 'Test GetSeries/*')
  checkException(bayeos.getSeries(ids = c(d$id[1], d$id[1])))
  
  
  
  
}


.tearDown <- function() {
  bayeos.cd()
  f = bayeos.find('Test GetSeries')
  bayeos.deleteNode(f$id, confirm = FALSE)
  bayeos.close()
}