#' givemeTREE
#' 
#' This function takes a species (rows) by traits (columns) matrix,
#' performs intermediate distance and clustering calculations,
#' and plots a dendrogram relating species that share similar traits.
#' 
#' @export givemeTREE

givemeTREE <- function(traits) {
  
  library(vegan)
  
  fd_d <- vegdist(traits, method = "jaccard", binary = TRUE)
  
  clust <- hclust(fd_d, method = "average")
  
  plot(clust, xlab = "Species", ylab = "Branch height", hang = -1, cex = 0.6)
  
}


