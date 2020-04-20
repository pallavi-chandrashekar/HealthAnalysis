library(plumber)
library(jsonlite)

pr <- plumb("predictApi.R")

swaggerFile <- pr$swaggerFile()
swaggerFile$info$title <- "MentalHealthService"
swaggerFile$info$description <- "Returns the probablity that a person will seek treatment for their health condition"
swaggerFile$info$version <- "1.0.0"

pr$run(port = 8000)