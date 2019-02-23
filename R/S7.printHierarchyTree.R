
#' @export

S7.printHierarchyTree <- function(fileName='D6.treeStudy_2010_CHICAGO.rds'){

startTime <- Sys.time()

old <- options(stringsAsFactors = FALSE)
on.exit(options(old), add = TRUE)

# Load Data -------------------------------------------------------------------

path.local <- try(rprojroot::find_rstudio_root_file(), silent=TRUE)

if(class(path.local) == 'try-error'){
  path.local <- getwd()
} else{}

if(!file.exists(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/", fileName, sep=""))) {

  stop("file does not exist in project directory. Run Script 6
       (S6.hierarchyTree.R) to generate the file called:
       D6.treeStudy_2010_CHICAGO.rds")

} else{

  readRDS(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/", fileName, sep=""))

}











}


