

price_elasticity_matrix <- function(elas_list_name,
                                    se_elas_list_name,
                                    numMarkets,
                                    numBrands,
                                    statistic= median #mean/median
                                    ){

  price_elasticity <- matrix(NA, nrow=numBrands)
  se_price_elasticity <- matrix(NA, nrow=numBrands)

  temp= matrix(NA, nrow= numBrands, ncol=numBrands)
  tmp= matrix(NA, nrow= numMarkets, ncol=1)

  temp_se= matrix(NA, nrow= numBrands, ncol=numBrands)
  tmp_se= matrix(NA, nrow= numMarkets, ncol=1)

  numExperiments= length(elas_list_name[[1]][1])
  i=1; j=1; n=1

  #for(e in seq_len(numExperiments)){
    while(j < numBrands + 1){
      while(i < numBrands + 1){
        for(n in seq_len(numMarkets)){
          tmp[n] <- elas_list_name[[n]][[i,j]]
          temp[i,j] <- statistic(tmp)

          tmp_se[n] <- se_elas_list_name[[n]][[i,j]]
          temp_se[i,j] <- statistic(tmp_se)

          }
        i <- i + 1
        }
      i <- 1
      j <- j + 1
      #}
    price_elasticity <- temp
    se_price_elasticity <- temp_se

    }

  rownames(price_elasticity) <- rownames(elas_list_name[[1]])
  colnames(price_elasticity) <- colnames(elas_list_name[[1]])
  rownames(se_price_elasticity) <- rownames(elas_list_name[[1]])
  colnames(se_price_elasticity) <- colnames(elas_list_name[[1]])
  price_elasticity <- list(price_elasticity=as.data.frame(price_elasticity),
                           se_price_elasticity=as.data.frame(se_price_elasticity))

  return(price_elasticity)
  }
