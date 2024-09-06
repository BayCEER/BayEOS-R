# BayEOS-R

Simple [BayEOS-Server](https://github.com/BayCEER/bayeos-server) access from R

``` 
library(bayeos)
bayeos.connect('http://bayeos.bayceer.uni-bayreuth.de/BayEOS-Server/XMLServlet','gast','gast')
data=bayeos.getSeries(c(2717,2718))
plot(data)
bayeos.close()
```

## Installation
You first have to install the packages zoo, XML, RCurl and RUnit

Make sure that you selected the repositories CRAN, CRAN (extra) and Omegahat. Check with

``` 
setRepositories()
```

Installation of the packages zoo, XML, RCurl and RUnit

``` 
install.packages(c('zoo','XML','RCurl','RUnit'))
```

Installation of bayeos packages as source package
``` 
install.packages("bayeos", repos = "https://www.bayceer.uni-bayreuth.de/R-repository", type="source")
```

For automatic updates of the packages via update.packages() add the BayCEER repository your local
repository list.
``` 
# BayCEER Repository
local({r <- getOption("repos"); r["BayCEER"] <- "https://www.bayceer.uni-bayreuth.de/R-repository"; options(repos=r)})
```

#### Note on installation on ubuntu/debian

Since dependent packages (zoo, XML, RCurl) are source packages, they have to be complied on linux.

Please install a build environment and the necesarry libraries before installing the R packages:

``` 
apt-get install libxml2-dev libcurl4-openssl-dev
``` 

## Database access without password

It is not recommended to include your password in human readable form in each script. 
You should create a encrypted password file with the following commands:


``` 
library(bayeos)
bayeos.connect('https://bayeos.bayceer.uni-bayreuth.de/BayEOS-Server/XMLServlet', 
    'btxxxxx','PASSWORT',save_as='my_connection') 
```

After running the command you can access the bayeos database by 

``` 
library(bayeos)
bayeos.connect('my_connection')
```

You can configure also several database connections.

SECURITY NOTE: The encryption of the password file is not strong. R must be able to decrypt the file.
Make sure that no one else gets access to the file.


