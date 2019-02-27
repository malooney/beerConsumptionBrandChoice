
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
  
  
  tmp <- as.numeric(strsplit(as.character(n), "")[[1]])
  
  subscriptsUTF8 <- c("\u2080", "\u2081", "\u2082", "\u2083", "\u2084", 
                      "\u2085", "\u2086", "\u2087", "\u2088", "\u2089")
  t <- c()
  
  for(i in 1:length(tmp)){ 
    
    index <- tmp[i]+1
    t[i] <- subscriptsUTF8[index]
    i <- i+1
    
  }

  tot <- data.frame(matrix(nrow=2, ncol=4))
  
  tot[1,1:4] <- rep(NA,4)
  tot[2,1] <- paste("CR", paste(t, collapse = ""), sep="")
  tot[2,2:4] <- tmpShares
  names(tot) <- names(topFourConglomerates)
  
  topFourConglomerates <- rbind(topFourConglomerates, tot)
  
  topFourConglomerates[,1] <- gsub("&", "AND", topFourConglomerates$Conglomerates)
  
  if(print_Table=="TRUE"){
    
  stargazer(topFourConglomerates, summary=F, header=F, type=outputFormat, rownames = F, notes= paste("Top", n, "Conglomerates/Firms across all markets in 2010."))
    
  } else{}
  
  if(print_Graph=="TRUE"){
    
    topFourConglomerates$Conglomerates <- factor(topFourConglomerates$Conglomerates, levels = topFourConglomerates$Conglomerates[order(topFourConglomerates$total_gal,decreasing = TRUE)]) 
    
    p <- ggplot(data=topFourConglomerates[c(1:n),], 
              aes(x=Conglomerates,  y=total_gal)) +
       geom_bar(stat="identity") + theme_economist() +
       theme(axis.text.x = element_text(angle = 90, hjust = 1))
    p
    
    } else{}
  
  
}
