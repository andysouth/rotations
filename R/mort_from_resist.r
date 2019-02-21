#' calculate mortality from resistance
#' 
#' PROVISIONAL

#' @param eff effectiveness, for all insecticides or individually
#' @param dom_sel dominance of selection, for all insecticides or individually
#' @param dom_cos dominance of cost, for all insecticides or individually
#' @param rr resistance restoration, for all insecticides or individually 
#' @param cost fitness cost of RR in no insecticide, for all insecticides or individually
#' @param fitSS fitness of SS if no insecticide, for all insecticides or individually
#' 
#' 
#' @examples 

#' @return total mortality
#' @export

mort_from_resist <- function ( resist_freq = 0.01,
                                   eff = 0.9,
                                   dom_sel = 0.5,
                                   dom_cos = 0.5,
                                   rr = 0.5,
                                   cost = 0,
                                   fitSS = 1)
{
  
  a_genfreq <- genotype_freq(resist_freq)
  
  #formals() passes all args from this function
  a_mort <- mort_by_genotype(formals())
  
  #proportional mortality
  a_propmort <- a_mort * a_genfreq   
  
  mort <- sum(a_propmort)
  
  return(mort)
}

