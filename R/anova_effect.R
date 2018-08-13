
# Credit to J. Patrick Meyer
omega_sq <- function(aovm){
  sum_stats <- summary(aovm)[[1]]
  SSm <- sum_stats[["Sum Sq"]][1]
  SSr <- sum_stats[["Sum Sq"]][2]
  DFm <- sum_stats[["Df"]][1]
  MSr <- sum_stats[["Mean Sq"]][2]
  W2 <- (SSm-DFm*MSr)/(SSm+SSr+MSr)
  return(W2)
}



# Modified Meyer Code
eta_sq<-function(aovm){
  sum_stats <- summary(aovm)[[1]]
  SSm <- sum_stats[["Sum Sq"]][1]
  SSr <- sum_stats[["Sum Sq"]][2]
  e2 <- (SSm)/(SSm+SSr)
  return(e2)
}





anova_effect<-function(aovm, continuous, group){
# 1) the anova model that we would previously have made
# 2) the continuous variable
# 3) the grouping variable
  sum_stats <- summary(aovm)[[1]]                  
  SSm <- sum_stats[["Sum Sq"]][1]                  
  SSr <- sum_stats[["Sum Sq"]][2]                  
  DFm <- sum_stats[["Df"]][1]                      
  MSr <- sum_stats[["Mean Sq"]][2]                
  w2 <- (SSm-DFm*MSr)/(SSm+SSr+MSr)
  e2 <- (SSm)/(SSm+SSr)
  grandmean<-mean(continuous)                       
  groupmeans<-aggregate(continuous, list(group), mean)[,2]
  difmeans<-sum((groupmeans-grandmean)^2)
  d<-sqrt(1/(DFm)*(difmeans/MSr))      
  f<-sqrt(1/(DFm+1)*(difmeans/MSr))     
  mat<-cbind(d,f,e2,w2) 
  colnames(mat) <- c("d effect size","f effect size","eta squared","omega squared")
  return(mat)           
}
