#' calculate genotype frequencies from allele frequencies
#' 
#' PROVISIONAL

#' @param rfreq resistance allele frequency
#' 
#' @examples 
#' a_genfreq <- genotype_freq(0.001)

#' @return fitness values
#' @export

genotype_freq <- function ( rfreq )
{
  
  
  a_genfreq<- array_named(genotype=c('SS','RS', 'RR'))

  a_genfreq['SS'] <- (1 - rfreq) ^ 2
  
  a_genfreq['RS'] <- 2 * rfreq * (1 - rfreq)
  
  a_genfreq['RR'] <- rfreq ^ 2
  
  
  return(a_genfreq)
}

