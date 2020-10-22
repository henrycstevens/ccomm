#' givemeNMDS
#' 
#' This function takes a sites (rows) x species(columns) matrix, 
#' uses the package 'vegan' to perform distance calculations and non-metric dimensional scaling analysis,
#' and returns (1) a stress plot showing how well your data fit the analysis, 
#' and (2) a NMDS plot with all sites.

givemeNMDS <- function(community) {
  
  library(vegan)
  
  comm.rel <- decostand(community, method = 'total')
  
  comm.distmat <- vegdist(comm.rel, method = 'bray')
  
  comm.distmat <- as.matrix(comm.distmat, labels = T)
  
  commNMDS <- metaMDS(comm.distmat, k=3, maxit = 999, trymax = 500)
  
  stressplot(commNMDS)
  
  plot(commNMDS)
  
  orditorp(commNMDS, display = "sites", cex = 1, air = .5)
}
