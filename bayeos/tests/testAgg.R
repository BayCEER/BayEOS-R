.setUp <- function() {
  library(bayeos)
  library(zoo)
  bayeos.connect(
    'http://bayeos.bayceer.uni-bayreuth.de/BayEOS-Server/XMLServlet',
    'gast',
    'gast'
  )
}



test.TimeIntervalAggre <- function() {
  # Time interval + aggregation
  m = bayeos.getSeries(
    2718,
    from = '2012-01-01 00:00:00',
    until = '2012-02-01 01:00:00',
    aggfunc = 'Avg',
    aggint = 'day'
  )
  checkTrue(length(m) == 32)
}

test.MissingReadAccess <- function() {
  # Missing read access series
  checkException(bayeos.getSeries(124403))
}

.tearDown <- function() {
  bayeos.close()
}