library(RUnit)
projectPath = paste(dirname(rstudioapi::getActiveDocumentContext()$path),
                    "..",
                    sep = "/")
testSuite = defineTestSuite(
  "allTests",
  dirs = file.path(projectPath, "/tests"),
  testFileRegexp = "^test.+\\.[rR]$",
  testFuncRegexp = "^test+"
)
testSuiteImport = defineTestSuite(
  "ImportTests",
  dirs = file.path(projectPath, "/tests"),
  testFileRegexp = "^test.+\\.[rR]$",
  testFuncRegexp = "^test\\.import.*"
)
testResult = runTestSuite(testSuite)
printHTMLProtocol(testResult, fileName = file.path(projectPath, "tests/runTests.html"))
