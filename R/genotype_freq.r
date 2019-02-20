#' calculate genotype frequencies from allele frequencies
#' 
#' PROVISIONAL

#' @param resist_freq resistance allele frequency
#' 
#' @examples 
#a_genfreq <- genotype_freq(0.001)

#' @return fitness values
#' @export

genotype_freq <- function ( resist_freq = 0.01)
{
  
  
  a_genfreq<- array_named(genotype=c('SS','RS', 'RR'))

  a_genfreq['SS'] <- (1 - resist_freq) ^ 2
  
  a_genfreq['RS'] <- 2 * resist_freq * (1 - resist_freq)
  
  a_genfreq['RR'] <- resist_freq ^ 2
  
  
  return(a_genfreq)
}

