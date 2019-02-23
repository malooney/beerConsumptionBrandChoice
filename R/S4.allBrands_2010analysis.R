

#' @export

S4.allBrands_2010analysis <- function( city= c("CHICAGO",
                                                 "DALLAS, TX",
                                                 "LOS ANGELES",
                                                 "SPOKANE",
                                                 "SYRACUSE",
                                                 "HOUSTON"),
                                       out_algoRunTime = T
                                      ){

  startTime <- Sys.time()

  old <- options(stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)

  path.local <- try(rprojroot::find_rstudio_root_file(), silent=TRUE)

  if(class(path.local) == 'try-error'){
    path.local <- getwd()
  } else{}

  if(!file.exists(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D3.explore_2010marketData.rds", sep=""))) {

    stop("file does not exist in project directory. Run Script 3
         (S3.explore_2010marketData.R) to generate the file called:
         D3.explore_2010marketData.rds")

  } else{

    dta <- readRDS(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D3.explore_2010marketData.rds", sep=""))

  }

  allBrands_2010analysis <- data.frame()

  tmp <- dta[city]

  if(length(city) == 1){

    allBrands_2010analysis <- data.frame(unlist(tmp[[1]][1], use.names = F))
    colnames(allBrands_2010analysis) <- "Brand_Name"

  } else{

    for(i in 1:length(city)){

      tmp1 <- data.frame(unlist(tmp[[i]][1], use.names = F))
      allBrands_2010analysis <- rbind(allBrands_2010analysis, tmp1)
      i <- i+1
    }

    freqs <- data.frame(table(allBrands_2010analysis))

    allBrandsAllMarkets <- data.frame(brand=freqs$allBrands_2010analysis)

    allBrands_2010analysis <- dplyr::filter(freqs, Freq==length(city))
    allBrands_2010analysis <- data.frame(allBrands_2010analysis[,1])
    colnames(allBrands_2010analysis) <- "Brand_Name"

    allBrands_2010analysis_main <- list()
    allBrands_2010analysis_main[[3]] <- allBrandsAllMarkets
    allBrands_2010analysis_main[[2]] <- allBrands_2010analysis
    allBrands_2010analysis_main[[1]] <- data.frame("Markets"=city)
    names(allBrands_2010analysis_main) <- c("markets",
                                            "brand_intersection_across_markets",
                                            "allBrandsAllMarkets")

    allBrands_2010analysis <- allBrands_2010analysis_main

    saveRDS(allBrands_2010analysis,
            paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D4.allBrands_2010analysis.rds", sep = ""))

    write.csv(allBrands_2010analysis[[3]],
              paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D4.allBrandsAllMarkets.csv", sep = ""), row.names = F)

    write.csv(allBrands_2010analysis[[2]],
            paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D4.brand_intersection_across_markets.csv", sep = ""), row.names = F)

    write.csv(allBrands_2010analysis[[1]],
              paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D4.markets.csv", sep = ""), row.names = F)

  }

  endTime <- Sys.time()

  if(out_algoRunTime == T) {

    hora <- list(starttime=startTime, endTime=endTime)

  } else {}

}

