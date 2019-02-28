
#' @export

CR_x <- function(print_Table="TRUE",
                 print_Graph="TRUE",
                 outputFormat="text",
                 Conglom_Firm_Num=8){
  
  n= Conglom_Firm_Num
  
  old <- options(stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)
  
# Load Data -------------------------------------------------------------------
  
  path.local <- try(rprojroot::find_rstudio_root_file(), silent=TRUE)
  
  if(class(path.local) == 'try-error'){
    path.local <- getwd()
  }
  
  if(!file.exists(paste(path.local, "/data_beerConsumptionBrandChoice/D5.aggregate_allBrands_2010_all.rds", sep=""))) {
    
    stop("file does not exist in project directory. Run Script 5
         (S5.aggregate_allBrands_2010.R) to generate the file called:
         D5.aggregate_allBrands_2010_all.rds")
    
  } else{
    
    D5.aggregate_allBrands_2010_all <- readRDS("~/Google Drive/digitalLibrary/*beerProject/data_beerConsumptionBrandChoice/D5.aggregate_allBrands_2010_all.rds")
    
  }
  
  topFourConglomerates <- data.frame(D5.aggregate_allBrands_2010_all$aggregateConglomerates$aggregateDataSummaryConglomeratesAll_prct[1:n,])
  
  tmpShares <- colSums(topFourConglomerates[,2:4])
  

  tot <- data.frame(matrix(nrow=2, ncol=4))
  
  tot[1,1:4] <- rep(NA,4)
  tot[2,1] <- paste(n, "-Firm Concentration Ratio",  sep="")
  tot[2,2:4] <- tmpShares
  names(tot) <- names(topFourConglomerates)
  
  topFourConglomerates <- rbind(topFourConglomerates, tot)
  
  topFourConglomerates[,1] <- gsub("&", "AND", topFourConglomerates$Conglomerates)
  
  if(print_Table=="TRUE"){
    
  stargazer::stargazer(topFourConglomerates, summary=F, header=F, type=outputFormat, rownames = F, notes= paste("Top", n, "Conglomerates/Firms across all markets in 2010."))
    
  }
  
  if(print_Graph=="TRUE"){
    
    topFourConglomerates$Conglomerates <- factor(topFourConglomerates$Conglomerates, levels = topFourConglomerates$Conglomerates[order(topFourConglomerates$total_gal,decreasing = TRUE)]) 
    
    p <- ggplot2::ggplot(data=topFourConglomerates[c(1:n),], ggplot2::aes(x=Conglomerates,  y=total_gal)) + ggplot2::geom_bar(stat="identity") + ggthemes::theme_economist() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
    
    p
    
    }
  
  
}
