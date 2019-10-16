#' calculate mortality from resistance
#' 
#' @param rfreq resistance allele frequency
#' @param eff effectiveness, for all insecticides or individually
#' @param dom_sel dominance of selection, for all insecticides or individually
# @param dom_cos dominance of cost, for all insecticides or individually
#' @param rr resistance restoration, for all insecticides or individually 
# @param cost fitness cost of RR in no insecticide, for all insecticides or individually
# @param fitSS fitness of SS if no insecticide, for all insecticides or individually
#' 
#' 
#' @examples 
#' mort_from_resist( rfreq=0.01, eff=0.9, dom_sel=0.5, rr=0.5 )
#' 
#' @return total mortality single value
#' @export

mort_from_resist <- function ( rfreq, # = 0.01, # no defaults to avoid code bugs with it being called with wrong values
                                   eff,# = 0.9,
                                   dom_sel,# = 0.5,
                                   rr )# = 0.5,
                                   #cost = 0,
                                   #fitSS = 1)
{
  
  a_genfreq <- genotype_freq(rfreq)
  
  #formals() passes all args from this function
  a_mort <- mort_by_genotype(eff=eff, dom_sel=dom_sel, rr=rr)
  
  #proportional mortality
  a_propmort <- a_mort * a_genfreq   
  
  mort <- sum(a_propmort)
  
  return(mort)
}

