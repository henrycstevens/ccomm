#' givemeSTRESS
#' 
#' This function takes a sites (rows) x species(columns) matrix, 
#' uses the package 'vegan' to perform distance calculations and non-metric 
#' dimensional scaling analysis, and returns a stress plot showing how well 
#' your data fit the analysis. Should be used with 'givemeNMDS' to confirm
#' that your data is appropriate. May return a warning that says stress is
#' nearly zero -- this is usually occurs with datasets that have lots of
#' 0 values, which is typically expected in large community datasets. 
#' 
#' @export givemeNMDS

givemeNMDS <- function(community) {
  
  library(vegan)
  
  comm.rel <- decostand(community, method = 'total')
  
  comm.distmat <- vegdist(comm.rel, method = 'bray')
  
  comm.distmat <- as.matrix(comm.distmat, labels = T)
  
  commNMDS <- metaMDS(comm.distmat, k=3, maxit = 999, trymax = 500)
  
  stressplot(commNMDS)
  
}