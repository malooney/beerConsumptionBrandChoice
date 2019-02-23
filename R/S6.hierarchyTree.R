
#' @export

S6.hierarchyTree <- function(Market = 'CHICAGO',
                             saveMarketHierarchy = T,
                             saveAllBrandHierarchy = T,
                             out_algoRunTime = T ){

  startTime <- Sys.time()

  old <- options(stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)

# Load Data -------------------------------------------------------------------

  path.local <- try(rprojroot::find_rstudio_root_file(), silent=TRUE)

  if(class(path.local) == 'try-error'){
    path.local <- getwd()
  } else{}

  if(saveAllBrandHierarchy == T){

  if(!file.exists(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D1.main_beer_drug_and_groc_4_2010.feather", sep=""))) {

    stop("file does not exist in project directory. Run Script 2
         (S1.beer_2010DataCleaning.R) to generate the file called:
         D1.main_beer_drug_and_groc_4_2010.feather")

  } else{

  main_beer_drug_and_groc_4_2010 <- feather::read_feather(paste(path.local,"/data_beerEthnicityConsumptionBrandChoice/D1.main_beer_drug_and_groc_4_2010.feather", sep=""))

  }

    dtaTmp <- main_beer_drug_and_groc_4_2010[, 53:55]
    rm(main_beer_drug_and_groc_4_2010)

    colnames(dtaTmp)[3] <- "Brands"
    colnames(dtaTmp)[2] <- "Firms"
    colnames(dtaTmp)[1] <- "Conglomerates"

    dtaTmp1 <- dplyr::distinct(dtaTmp)


    dtaTmp$pathString <- paste("Conglomerate",
                               dtaTmp$Conglomerates,
                               dtaTmp$Firms,
                               dtaTmp$Brands, sep="/")

    treeAll <- data.tree::as.Node(dtaTmp[,])

  } else {}

  if(saveMarketHierarchy == T){

    if(!file.exists(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D4.allBrands_2010analysis_Markets.csv", sep=""))) {

      stop("file does not exist in project directory. Run Script 4
           (S4.explore_2010marketData.R) to generate the file called:
           D4.allBrands_2010analysis_Markets.csv")

    } else{

      D4.allBrands_2010analysis_Markets <- read.csv(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D4.allBrands_2010analysis_Markets.csv", sep=""))

    }

    if(!file.exists(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D4.allBrands_2010analysis_Brands.csv", sep=""))) {

      stop("file does not exist in project directory. Run Script 4
           (S4.explore_2010marketData.R) to generate the file called:
           D4.allBrands_2010analysis_Brands.csv")

    } else{

      D4.allBrands_2010analysis_Brands <- read.csv(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D4.allBrands_2010analysis_Brands.csv", sep=""))

    }

    if(!file.exists(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D2.marketData_2010.rds", sep=""))) {

      stop("file does not exist in project directory. Run Script 2
           (S2.construct_2010marketData.R) to generate the file called:
           D2.marketData_2010.rds")

    } else{

      D2.marketData_2010 <- readRDS(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D2.marketData_2010.rds", sep=""))

    }


  dta <- D2.marketData_2010[[Market]]

# Add volume measures ---------------------------------------------------------
  oz <- round(data.frame(oz=dta$VOL_EQ.x* 288))
  total_oz <- (oz* dta$UNITS); colnames(total_oz) <- "total_oz"

  total_gal <- (0.0078125* total_oz); colnames(total_gal) <- "total_gal"

  dollarPerGal <- dta$DOLLARS/ total_gal;
  colnames(dollarPerGal) <- "dollarPerGal"

  dta_manip <- cbind(dta, oz, total_oz, total_gal,
                     dollarPerGal)

  rm(oz, total_gal, total_oz, dollarPerGal, dta)

# Remove zero data ------------------------------------------------------------
  dta_manip <- dplyr::filter(dta_manip, L5 !="ALL BRAND")
  dta_manip <- dplyr::filter(dta_manip, dollarPerGal !="Inf")

  colnames(dta_manip)[20] <- "Brands"
  colnames(dta_manip)[19] <- "Firms"
  colnames(dta_manip)[18] <- "Conglomerates"

  dtaManip_unqBrands <- subset(dta_manip[,18:20], Brands %in%
                                 D4.allBrands_2010analysis_Brands[,1])

  dtaManip_unqBrands$pathString <- paste("Conglomerate",
                                         dtaManip_unqBrands$Conglomerates,
                                         dtaManip_unqBrands$Firms,
                                         dtaManip_unqBrands$Brands, sep="/")

  treeStudy <- data.tree::as.Node(dtaManip_unqBrands[,])

}


  if(saveAllBrandHierarchy == T){

    saveRDS(treeAll,
            paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D6.treeAll_2010", ".rds", sep = ""))

  } else {}

  if(saveMarketHierarchy == T){

  saveRDS(treeStudy,
          paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D6.treeStudy_2010_", gsub('[ ,]', '', Market), ".rds", sep = ""))

  } else{}

  endTime <- Sys.time()

  if(out_algoRunTime == T) {

    hora <- list(starttime=startTime, endTime=endTime)

  } else {}
}
