
#' @export

S5.aggregate_allBrands_2010 <- function( Market = 'CHICAGO',
                                         screenPlot = T,
                                         figurePlot = T,
                                         saveData = T,
                                         out_algoRunTime = T ){

  startTime <- Sys.time()

  old <- options(stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)

# Load Data -------------------------------------------------------------------

  path.local <- try(rprojroot::find_rstudio_root_file(), silent=TRUE)

  if(class(path.local) == 'try-error'){
    path.local <- getwd()
  } else{}

if(!file.exists(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D4.markets.csv", sep=""))) {

  stop("file does not exist in project directory. Run Script 4
       (S4.explore_2010marketData.R) to generate the file called:
       D4.markets.csv")

} else{

  D4.markets <- read.csv(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D4.markets.csv", sep=""))

}

if(!file.exists(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D4.brand_intersection_across_markets.csv", sep=""))) {

  stop("file does not exist in project directory. Run Script 4
       (S4.explore_2010marketData.R) to generate the file called:
       D4.brand_intersection_across_markets.csv")

} else{

  D4.brand_intersection_across_markets <- read.csv(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D4.brand_intersection_across_markets.csv", sep=""))

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

# Explore Brands, Firms, and percenatges of Brands by Firms -------------------
uniqueBrands <- dta_manip
colnames(uniqueBrands)[20] <- "Brands"
colnames(uniqueBrands)[19] <- "Firms"
colnames(uniqueBrands)[18] <- "Conglomerates"
colnames(dta_manip)[20] <- "Brands"
colnames(dta_manip)[19] <- "Firms"
colnames(dta_manip)[18] <- "Conglomerates"

uniqueBrands_U <- aggregate(UNITS ~ Brands, uniqueBrands, sum)
uniqueFirms_U <- aggregate(UNITS ~ Firms, uniqueBrands, sum)
uniqueConglomerates_U <- aggregate(UNITS ~ Conglomerates, uniqueBrands, sum)

uniqueBrands_Dol <- aggregate(DOLLARS ~ Brands, uniqueBrands, sum)
uniqueFirms_Dol <- aggregate(DOLLARS ~ Firms, uniqueBrands, sum)
uniqueConglomerates_Dol <- aggregate(DOLLARS ~ Conglomerates, uniqueBrands, sum)

uniqueBrands_TotGal <- aggregate(total_gal ~ Brands, uniqueBrands, sum)
uniqueFirms_TotGal <- aggregate(total_gal ~ Firms, uniqueBrands, sum)
uniqueConglomerates_TotGal <- aggregate(total_gal ~ Conglomerates,
                                        uniqueBrands, sum)

uniqueBrands <- dplyr::full_join(uniqueBrands_U, uniqueBrands_Dol,
                                 by = "Brands")
uniqueFirms <- dplyr::full_join(uniqueFirms_U, uniqueFirms_Dol,
                                 by = "Firms")
uniqueConglomerates <- dplyr::full_join(uniqueConglomerates_U,
                                        uniqueConglomerates_Dol,
                                        by = "Conglomerates")

uniqueBrands <- dplyr::full_join(uniqueBrands, uniqueBrands_TotGal,
                                 by = "Brands")
uniqueFirms <- dplyr::full_join(uniqueFirms, uniqueFirms_TotGal,
                                 by = "Firms")
uniqueConglomerates <- dplyr::full_join(uniqueConglomerates,
                                        uniqueConglomerates_TotGal,
                                        by = "Conglomerates")

uniqueBrands <- dplyr::arrange(uniqueBrands, desc(UNITS))
uniqueFirms <- dplyr::arrange(uniqueFirms, desc(UNITS))
uniqueConglomerates <- dplyr::arrange(uniqueConglomerates, desc(UNITS))

#rm(uniqueBrands_Dol, uniqueBrands_U, uniqueBrands_TotGal)

selectBrands <- subset(uniqueBrands, Brands %in%
                         D4.brand_intersection_across_markets[,1])
selectFirms <- subset(dta_manip, Brands %in%
                        D4.brand_intersection_across_markets[,1])
selectFirms <- subset(uniqueFirms, Firms %in%
                        unique(selectFirms$Firms))
selectConglomerates <- subset(dta_manip, Brands %in%
                        D4.brand_intersection_across_markets[,1])
selectConglomerates <- subset(uniqueConglomerates, Conglomerates %in%
                        unique(selectConglomerates$Conglomerates))

prcntBrandRep_units <- (sum(selectBrands[, 2]) /
                          sum(uniqueBrands$UNITS)) * 100
prcntFirmRep_units <- (sum(selectFirms[, 2]) /
                          sum(uniqueFirms$UNITS)) * 100
prcntConglomerateRep_units <- (sum(selectConglomerates[, 2]) /
                         sum(uniqueConglomerates$UNITS)) * 100

prcntBrandRep_dol <- (sum(selectBrands[, 3]) /
                        sum(uniqueBrands$DOLLARS)) * 100
prcntFirmRep_dol <- (sum(selectFirms[, 3]) /
                        sum(uniqueFirms$DOLLARS)) * 100
prcntConglomerateRep_dol <- (sum(selectConglomerates[, 3]) /
                       sum(uniqueConglomerates$DOLLARS)) * 100

prcntBrandRep_gal <- (sum(selectBrands[, 4]) /
                        sum(uniqueBrands$total_gal)) * 100
prcntFirmRep_gal <- (sum(selectFirms[, 4]) /
                        sum(uniqueFirms$total_gal)) * 100
prcntConglomerateRep_gal <- (sum(selectConglomerates[, 4]) /
                       sum(uniqueConglomerates$total_gal)) * 100

if(figurePlot == T){

  dir.create(file.path(path.local,
                       "data_beerEthnicityConsumptionBrandChoice/plots"),
             showWarnings = FALSE)

  pdf(paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/plots/plot.aggregateData_", gsub('[ ,]', '', Market), ".pdf", sep=""), width=11, height = 8.5)

  par(mfrow = c(1, 2))
  x <- selectBrands[order(selectBrands$UNITS) ,]
  dotchart(x$UNITS, labels = x$Brands,
         cex=.7,
         main = paste("Market: ", Market, "\n Total Units by Brand", sep=""),
         xlab = "Units")
  x <- selectBrands[order(selectBrands$DOLLARS), ]
  dotchart(x$DOLLARS, labels = x$Brands,
         cex = 0.7,
         main =paste("Market: ", Market, "\n Total Dollars by Brand", sep=""),
         xlab = "Dollars")

  dev.off()

} else {}

  if(screenPlot == T){

  x <- selectBrands[order(selectBrands$UNITS) ,]

  dotchart(x$UNITS, labels = x$Brands,
           main = paste("Market: ", Market, "\n Total Units by Brand",
                        sep=""), xlab = "Units")

  x <- selectBrands[order(selectBrands$DOLLARS), ]

  dotchart(x$DOLLARS, labels = x$Brands,
           main =paste("Market: ", Market, " \n Total Dollars by Brand",
                       sep=""), xlab = "Dollars")

  } else{}

aggregateDataSummaryBrands <- selectBrands

aggregateDataSummaryFirms <- selectFirms

aggregateDataSummaryConglomerates <- selectConglomerates

if(saveData == T){

  dta <- list(aggregateBrands=NA, aggregateFirms=NA, aggregateConglomerates=NA)

  dta[[1]] <- list(prcntBrandRep_units=prcntBrandRep_units,
                   prcntBrandRep_dol=prcntBrandRep_dol,
                   prcntBrandRep_gal=prcntBrandRep_gal,
                   aggregateDataSummaryBrands=aggregateDataSummaryBrands,
                   aggregateDataSummaryBrandsAll=uniqueBrands)

  dta[[2]] <- list(prcntFirmRep_units=prcntFirmRep_units,
                   prcntFirmRep_dol=prcntFirmRep_dol,
                   prcntFirmRep_gal=prcntFirmRep_gal,
                   aggregateDataSummaryFirms=aggregateDataSummaryFirms,
                   aggregateDataSummaryFirmsAll=uniqueFirms)

  dta[[3]] <- list(prcntConglomerateRep_units=prcntConglomerateRep_units,
                   prcntFConglomerateRep_dol=prcntConglomerateRep_dol,
                   prcntConglomerateRep_gal=prcntConglomerateRep_gal,
                   aggregateDataSummaryConglomerates=
                     aggregateDataSummaryConglomerates,
                   aggregateDataSummaryConglomeratesAll=uniqueConglomerates)


  saveRDS(dta,
          paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D5.aggregate_allBrands_2010_", gsub('[ ,]', '', Market), ".rds", sep = ""))

} else{}

endTime <- Sys.time()

if(out_algoRunTime == T) {

  hora <- list(starttime=startTime, endTime=endTime)

} else {}

}
