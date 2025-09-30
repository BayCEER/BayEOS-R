# TODO: Add comment
#
# Author: holzheu
###############################################################################


.setUp <- function() {
  library(bayeos)
  bayeos.connect('dev')
}

test.Find <- function() {
  # Find Celsius in Unit-Tree
  id = bayeos.createFolder('test-Find')
  bayeos.updateSeries(
    id,
    tz = 'Etc/GMT-1',
    lon = 51.023,
    lat = 12.234,
    elevation = 370
  )
  id_series = bayeos.createSeries(
    'test',
    id_parent = id,
    lon = 49.99932,
    elevation = 324,
    tz = 'Etc/GMT-1'
  )
  f = bayeos.find('test-Find')
  checkTrue(all(f$name == 'test-Find'))
  f = bayeos.getChilds()
  bayeos.deleteNode(id_series, confirm = FALSE)
  bayeos.deleteNode(id, confirm = FALSE)
  checkTrue(length(f) == 7)
}

.tearDown <- function() {
  bayeos.close()
}
