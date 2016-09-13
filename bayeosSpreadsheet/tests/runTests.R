library(RUnit)
projectPath=if(Sys.getenv('USER')=='holzheu') paste(Sys.getenv('HOME'),"/workspace/BayEOS-R/bayeosSpreadsheet",sep='') else
 "P:/EclipseProjects/BayEOS-R/bayeosSpreadsheet"
testSuite = defineTestSuite("allTests", dirs=file.path(projectPath,"/tests"), 
		testFileRegexp = "^test.+\\.[rR]$", testFuncRegexp = "^test+")
testSuiteImport = defineTestSuite("ImportTests", dirs=file.path(projectPath,"/tests"), 
		testFileRegexp = "^test.+\\.[rR]$", testFuncRegexp = "^test\\.import.*")
testResult = runTestSuite(testSuite)
printHTMLProtocol(testResult, fileName = file.path(projectPath,"tests/runTests.html"))
		