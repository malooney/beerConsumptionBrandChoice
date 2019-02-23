
#' @export

S8.1.studyBrandsSelect <- function(Brands= #"all"
                                     c("BUD LIGHT",
                                             "COORS LIGHT",
                                             "CORONA EXTRA",
                                             "BUDWEISER",
                                             "MILLER HIGH LIFE",
                                             "TECATE",
                                             "HEINEKEN",
                                             "MILLER LITE",
                                             "MODELO ESPECIAL",
                                             "MILLER GENUINE DRAFT",
                                             "NATURAL LIGHT",
                                             "FOSTERS LAGER",
                                             "NEWCASTLE BROWN ALE",
                                             "CORONA LIGHT",
                                             "BLUE MOON BELGIAN WHITE ALE",
                                             "BUD LIGHT LIME",
                                             "STELLA ARTOIS LAGER",
                                             "MICHELOB ULTRA",
                                             "KEYSTONE LIGHT",
                                             "COORS")
                              ){


old <- options(stringsAsFactors = FALSE)
on.exit(options(old), add = TRUE)

# Load Data -------------------------------------------------------------------

path.local <- try(rprojroot::find_rstudio_root_file(), silent=TRUE)

if(!file.exists(paste(path.local, "/data/beer_MS_Data.csv", sep=""))) {

  stop("file does not exist in project directory. Move file called: beer_MS_Data.csv into project directory called: /data. This file should contain the product characteristics of interest.")

} else{

  Beer_Characteristics_Master_List <- read.csv(paste(path.local, "/data/beer_MS_Data.csv", sep=""))

}

Beer_Characteristics_Master_List <- na.omit(Beer_Characteristics_Master_List)

if(Brands[1] == "all"){

  tmp_BCML <- Beer_Characteristics_Master_List

  } else {

    tmp_BCML <- dplyr::filter(Beer_Characteristics_Master_List, Brand_Name %in% Brands)

}

saveRDS(tmp_BCML, paste(path.local, "/data_beerEthnicityConsumptionBrandChoice/D8.1.Beer_Characteristics_Study_List.rds", sep = ""))

}
