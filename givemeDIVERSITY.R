#' givemeDIVERSITY
#' 
#' This function takes a sites (rows) x species (columns) matrix, 
#' uses the package 'BAT' to perform alpha and beta diversity calculations,
#' and returns (1) boxplots depicting alpha diversity across sites, 
#' and (2) boxplots depicting beta diversity across sites.

givemeDIVERSITY <- function(community) {
  
  library(BAT)
  
  td <- alpha(community, raref = 1)
  
  cols = colorRampPalette(c("white", "black"))
  boxplot(t(td), names = rownames(test) , col=cols(length(td)), main="Alpha diversity", 
          xlab = "Site", pars = list
          (boxwex = 0.8, staplewex = 0, outwex = 0.5), range=1.5,
          lwd=0.8, lty=1, ylab = expression(alpha))
}











