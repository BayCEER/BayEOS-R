.setUp <- function() {
  library(bayeos)
  library(zoo)
  bayeos.connect('dev')
}

test.ACL <- function() {
  # Note: the user needs the appropriate rights to run this test
  id = bayeos.createFolder('test-ACL')
  acl = bayeos.getACL(id)
  checkTrue(bayeos.createACL('All Users', id, read = TRUE))
  checkTrue(bayeos.deleteACL('All Users', id)) # should not work...
  bayeos.deleteNode(id, confirm = FALSE)
}

.tearDown <- function() {
  bayeos.close()
}