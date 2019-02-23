
#' @export

# Load Data -------------------------------------------------------------------

S3.explore_2010marketData <- function(out_algoRunTime = T) {


  startTime <- Sys.time()

  old <- options(stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)

  path.local <- try(rprojroot::find_rstudio_root_file(), silent=TRUE)

  if(class(path.local) == 'try-error'){
    path.local <- getwd()
  } else{}

  if(!file.exists(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D2.marketData_2010.rds", sep=""))) {

    stop("file does not exist in project directory. Run Script 2
             (S2.construct_2010marketData.R) to generate the file called:
             D2.marketData_2010.rds")

  } else{

    dta <- readRDS(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D2.marketData_2010.rds", sep=""))

  }

  explore_2010marketData <- list()

  for(ii in 1:length(dta)){

# Add volume measures ---------------------------------------------------------

  oz <- round(data.frame(oz = dta[[ii]]$VOL_EQ.x * 288))

  total_oz <- (oz * dta[[ii]]$UNITS)

  colnames(total_oz) <- "total_oz"

  total_gal <- (0.0078125 * total_oz)

  colnames(total_gal) <- "total_gal"

  dollarPerGal <- dta[[ii]]$DOLLARS / total_gal

  colnames(dollarPerGal) <- "dollarPerGal"

  data_manip <- cbind(dta[[ii]], oz, total_oz, total_gal, dollarPerGal)

  rm(oz, total_gal, total_oz, dollarPerGal)

# Remove zero data ------------------------------------------------------------

  data_manip <- dplyr::filter(data_manip, L5 != "ALL BRAND")

  data_manip <- dplyr::filter(data_manip, dollarPerGal != "Inf")

  Brands <- unique(data_manip$L5)

  Weeks <- unique(data_manip$WEEK)

  Chains <- unique(data_manip$MskdName)

  num_Brands <- length(Brands)

  num_Weeks <- length(Weeks)

  num_Chains <- length(Chains)

  augWeeks <- num_Weeks + 1

  augChains <- num_Weeks + num_Chains

  explore_Data <- data.frame(matrix(NA, nrow = num_Weeks + num_Chains,
                                    ncol = num_Brands + 2))

  colnames(explore_Data) <- c("Week_Chain", "Week_Chain_Num", Brands)

  explore_Data[1:num_Weeks, 1] <- paste("Week", 1:num_Weeks, sep = "")

  explore_Data[augWeeks:augChains, 1] <- paste("Chain", 1:num_Chains, sep = "")

  explore_Data[1:num_Weeks, 2] <- unique(data_manip$WEEK)

  explore_Data[augWeeks:augChains, 2] <- unique(data_manip$MskdName)

  i <- 1

  j <- 3

  for (i in seq_along(Brands)) {

    tmp <- dplyr::filter(data_manip, L5 == Brands[i])

    tmp_week <- unique(tmp$WEEK)

    tmp_chain <- unique(tmp$MskdName)

    explore_Data[1:num_Weeks, j] <- ifelse(explore_Data[1:num_Weeks, 2]
                                           %in% tmp_week,
                                           explore_Data[1:52, 2], NA)

    explore_Data[augWeeks:augChains, j] <-
      ifelse(explore_Data[augWeeks:augChains, 2] %in%
               tmp_chain, explore_Data[augWeeks:augChains, 2], NA)
    j <- j + 1

    }

  rm(tmp, num_Brands, tmp_chain, tmp_week, augChains, augWeeks,
     num_Chains, num_Weeks)

  explore_Data_Complete <- explore_Data[, apply(explore_Data, 2,
                                                function(x) !any(is.na(x)))]

  Brands_CompleteData_wksANDChains <- data.frame(
    Brand_Name = colnames(explore_Data_Complete[-c(1, 2)]))

  Brands_CompleteData_wksANDChains <- dplyr::arrange(Brands_CompleteData_wksANDChains, Brand_Name)

  explore_Data_Complete_wks <- explore_Data[,
                                            apply(explore_Data[1:52,], 2,
                                                  function(x) !any(is.na(x)))]

  BrandsOnly_CompleteData_wks <- data.frame(
    Brand_Name = colnames(explore_Data_Complete_wks[-c(1, 2)]))

  BrandsOnly_CompleteData_wks <- dplyr::arrange(BrandsOnly_CompleteData_wks, Brand_Name)



  explore_2010marketData[[ii]] <- list(

    BrandsOnly_CompleteData_wks = BrandsOnly_CompleteData_wks,

    explore_Data_Complete_wks = explore_Data_Complete_wks,

    BrandsOnly_CompleteData_wksANDChains = Brands_CompleteData_wksANDChains,

    explore_Data_Complete_wksANDChains = explore_Data_Complete,

    explore_Data = explore_Data

    )

  ii <- ii+1

  }

  names(explore_2010marketData) <- names(dta)

  saveRDS(explore_2010marketData,
          paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D3.explore_2010marketData.rds", sep = ""))

  endTime <- Sys.time()

  if(out_algoRunTime == T) {

    hora <- list(starttime=startTime, endTime=endTime)

  } else {}

}
