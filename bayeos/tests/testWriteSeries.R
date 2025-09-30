.setUp <- function() {
  library(bayeos)
  library(zoo)
  bayeos.connect('dev')
}

test.WriteSeries <- function() {
  # Note: the user needs the appropriate rights to run this test
  f = bayeos.createFolder("Test Folder")
  bayeos.cd(f)
  id = bayeos.createSeries('Series A')
  # Construct the data frame
  value = c(11.0, 12.0, 11.0, 12.0)
  datetime = c(
    as.POSIXct('2011-01-01 11:00', tz = 'Etc/GMT-1'),
    as.POSIXct('2011-01-01 12:00', tz = 'Etc/GMT-1'),
    as.POSIXct('2011-01-01 13:00', tz = 'Etc/GMT-1'),
    as.POSIXct('2011-01-01 14:00', tz = 'Etc/GMT-1')
  )
  data = zoo(value, datetime)
  # Import frame values into database
  checkTrue(bayeos.writeSeries(id, data))
  
  checkTrue(bayeos.writeSeries(id, data))
  data2 = zoo(value * 2, datetime)
  checkTrue(bayeos.writeSeries(id, data2))
  data_b = bayeos.getSeries(id, from = '2011-01-01 11:00', until = '2011-01-01 15:00')
  checkTrue(bayeos.writeSeries(id, data2, update = TRUE))
  data2_b = bayeos.getSeries(id, from = '2011-01-01 11:00', until = '2011-01-01 15:00')
  # Read values back
  checkTrue(length(data) == 4)
  checkTrue(all(coredata(data_b) == value))
  checkTrue(all(coredata(data2_b) == value * 2))
  checkTrue(all(time(data) == datetime))
  
  # Update values * 2
  data = data * 2
  # Write values back
  bayeos.writeSeries(id, data, update = TRUE)
  
  m = bayeos.getSeries(id, from = '2011-01-01 11:00', until = '2011-01-01 15:00')
  checkTrue(all(m == data))
  
  # Delete Series
  bayeos.deleteNode(id, confirm = FALSE, removeRows = TRUE)
  
  # Delete folder
  bayeos.cd('..')
  bayeos.deleteNode(f, confirm = FALSE)
  
}

.tearDown <- function() {
  bayeos.close()
}