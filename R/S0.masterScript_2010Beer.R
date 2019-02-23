

S0.masterScript <- function(S1 = TRUE,
                            S2 = TRUE,
                            S3 = TRUE,
                            S4 = TRUE) {

# Housekeeping ----------------------------------------------------------------

  startTime <- Sys.time()

  old <- options(stringsAsFactors = FALSE)
  on.exit(options(old), add = TRUE)

  totalCount <- 4
  pb <- txtProgressBar(min = 0, max = totalCount, style = 3)
  count <- 1
  setTxtProgressBar(pb, count)

# S1 ---------------------------------------------------------------------------

  if(S1 == TRUE){S1.beer_2010DataCleaning(save_2010_main_data = 'all_formats')
  } else{}

  count <- 2
  setTxtProgressBar(pb, count)

# S2 ---------------------------------------------------------------------------

  if(S2 == TRUE){
    S2.construct_2010marketData(input_fileType = 'feather',
                                marketNames = 'all')
  } else{}

    count <- 3
  setTxtProgressBar(pb, count)

# S3 ---------------------------------------------------------------------------

  if(S3 == TRUE){

      S3.explore_2010marketData()

    } else{}

  count <- 4
  setTxtProgressBar(pb, count)

# S4 ---------------------------------------------------------------------------

  if(S4 == TRUE){

    S4.allBrands_2010analysis(brands= c("CHICAGO",
                                        "DALLAS, TX",
                                        "LOS ANGELES",
                                        "SPOKANE",
                                        "SYRACUSE"))
  } else{}

  close(pb)

  endTime <- Sys.time()

  endTime - startTime

  }
