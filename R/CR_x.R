
#' @export

CR_x <- function(outputFormat="text",
                 Conglom_Firm_Num=8){
  
  n= Conglom_Firm_Num
  
  old <- options(stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  
# Load Data -------------------------------------------------------------------
  
  path.local <- try(rprojroot::find_rstudio_root_file(), silent=TRUE)
  
  if(class(path.local) == 'try-error'){
    path.local <- getwd()
  } else{}
  
  if(!file.exists(paste(path.local, "/data_beerConsumptionBrandChoice/D5.aggregate_allBrands_2010_all.rds", sep=""))) {
    
    stop("file does not exist in project directory. Run Script 5
         (S5.aggregate_allBrands_2010.R) to generate the file called:
         D5.aggregate_allBrands_2010_all.rds")
    
  } else{
    
    D5.aggregate_allBrands_2010_all <- readRDS("~/Google Drive/digitalLibrary/*beerProject/data_beerConsumptionBrandChoice/D5.aggregate_allBrands_2010_all.rds")
    
  }
  
  topFourConglomerates <- data.frame(D5.aggregate_allBrands_2010_all$aggregateConglomerates$aggregateDataSummaryConglomeratesAll_prct[1:n,])
  
  tmpShares <- colSums(topFourConglomerates[,2:4])
  
  tot <- data.frame(matrix(nrow=1, ncol=4))
  tot[1,1] <- "Total_Share"
  tot[1,2:4] <- tmpShares
  names(tot) <- names(topFourConglomerates)
  
  topFourConglomerates <- rbind(topFourConglomerates, tot)
  
  topFourConglomerates[,1] <- gsub("&", "AND", topFourConglomerates$Conglomerates)
  
  stargazer(topFourConglomerates, summary=F, header=F, rownames = F, 
            notes=paste("Top", n, "Conglomerates/Firms across all markets in 2010."), 
            type=outputFormat)
  
  
  
}
