
#' @export

CR_x <- function(print_Table="TRUE",
                 print_Graph="TRUE",
                 outputFormat="text",
                 Conglom_Firm_Num=8,
                 report_CR_Num="TRUE"){
  
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
  
  topConglomerates <- data.frame(D5.aggregate_allBrands_2010_all$aggregateConglomerates$aggregateDataSummaryConglomeratesAll_prct[1:n,])
  
  topConglomerates_HHI <- data.frame(D5.aggregate_allBrands_2010_all$aggregateConglomerates$aggregateDataSummaryConglomeratesAll_prct[,])
  
  HHI_length <- length(topConglomerates_HHI$Conglomerates)
  
  tmpShares <- colSums(topConglomerates[,2:4])
  tmpShares_HHI <- colSums((topConglomerates_HHI[,2:4]^2))
  

  tot <- data.frame(matrix(nrow=3, ncol=4))
  
  tot[1,1:4] <- rep(NA,4)
  tot[2,1] <- paste(n, "-Firm Concentration Ratio",  sep="")
  tot[2,2:4] <- tmpShares
  
  tot[3,1] <- paste(HHI_length, "-Firm Herfindahl-Hirschman Index",  sep="")
  tot[3,2:4] <- tmpShares_HHI
  
  
  names(tot) <- names(topConglomerates)
  
  topConglomerates <- rbind(topConglomerates, tot)
  
  topConglomerates[,1] <- gsub("&", "AND", topConglomerates$Conglomerates)
  
  if(print_Table=="TRUE"){
    
  stargazer::stargazer(topConglomerates, summary=F, header=F, type=outputFormat, rownames = F, notes= paste("Top", n, "Conglomerates/Firms across all markets in 2010."))
    
  }
  
  if(print_Graph=="TRUE"){
    
    topConglomerates$Conglomerates <- factor(topConglomerates$Conglomerates, levels = topConglomerates$Conglomerates[order(topConglomerates$total_gal,decreasing = TRUE)]) 
    
    p <- ggplot2::ggplot(data=topConglomerates[c(1:n),], ggplot2::aes(x=Conglomerates,  y=total_gal)) + 
      ggplot2::geom_bar(stat="identity") + 
      ggthemes::theme_economist() + 
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, size=6)) + 
      ggplot2::theme(axis.text.y = ggplot2::element_text(angle = 90, hjust = 1, size=6), plot.title = ggplot2::element_text(hjust = 0.5)) + 
      ggplot2::labs(y = "% by Total Gallons") +
      ggplot2::ggtitle(paste(n, "-Firm Concentration Ratio",  sep=""))
    
    p
    
    return(p)
    
  }
  
  if(report_CR_Num=="TRUE"){
    
    CR_location <- n+2
    
    reportCR <- topConglomerates[CR_location,4]
    
    return(reportCR)
    
  }
  
  
}
