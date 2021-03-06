#' givemeNMDS
#' 
#' This function takes a sites (rows) x species(columns) matrix, 
#' uses the package 'vegan' to perform distance calculations and non-metric 
#' dimensional scaling analysis, and returns an NMDS plot with all sites. Use
#' vegan functions 'orditorp' and 'ordihull' to display sites and visualize
#' site relationships, respectively.
#' 
#' @export givemeNMDS

givemeNMDS <- function(community) {
  
  library(vegan)
  
  comm.rel <- decostand(community, method = 'total')
  
  comm.distmat <- vegdist(comm.rel, method = 'bray')
  
  comm.distmat <- as.matrix(comm.distmat, labels = T)
  
  commNMDS <- metaMDS(comm.distmat, k=3, maxit = 999, trymax = 500)
  
  plot(commNMDS)
  
}
