
#' @export

S2.construct_2010marketData <- function(input_fileType = 'feather',
                                        marketNames = c('all'),
                                        out_algoRunTime = T) {

  startTime <- Sys.time()

  old <- options(stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)

  marketNames <- data.frame(marketNames)

  path.local <- try(rprojroot::find_rstudio_root_file(), silent=TRUE)

  if(class(path.local) == 'try-error'){
    path.local <- getwd()
  } else{}

  if(!file.exists(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D1.main_beer_drug_and_groc_4_2010.", input_fileType, sep=""))) {

  stop("file does not exist in project directory. Run Script 1
             (S1.beer_2010DataCleaning) to generate the file called:
             D1.main_beer_drug_and_groc_4_2010.(feather, csv, rds)")

  } else{

    if(input_fileType == "csv"){

    main_beer_drug_and_groc_4_2010 <- suppressWarnings(read_csv(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D1.main_beer_drug_and_groc_4_2010.csv", sep=""), col_types = cols(X1 = col_skip())))

    } else if(input_fileType == "rds") {

      main_beer_drug_and_groc_4_2010 <- readRDS(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D1.main_beer_drug_and_groc_4_2010.rds", sep=""))

    } else if(input_fileType == "feather") {

      main_beer_drug_and_groc_4_2010 <- feather::read_feather(paste(path.local,"/data_beerEthnicityConsumptionBrandChoice/D1.main_beer_drug_and_groc_4_2010.feather", sep=""))

    }

    if(marketNames[1,1] == 'all'){

      marketNames <- readRDS(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D1.mrkNames_2010.rds", sep=""))

    } else{

    }

    marketData_2010 <- list()

    for(i in 1:nrow(marketNames)) {

      market <- marketNames[i,1]

      tmp <- dplyr::filter(main_beer_drug_and_groc_4_2010,
                           Market_Name == market)

      marketData_2010[[i]] <- tmp[, c(1, 2, 7:14, 19, 21, 25:27, 30, 36,
                                      53:56, 71:76)]
      i = i + 1
    }

    names(marketData_2010) <- marketNames[,1]

    saveRDS(marketData_2010,
            paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D2.marketData_2010.rds", sep = ""))

  }

  endTime <- Sys.time()

  if(out_algoRunTime == T) {

    hora <- list(starttime=startTime, endTime=endTime)

  } else {}

}
