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

#' @return fitness values
#' @export

mort_from_resist <- function ( resist_freq = 0.01,
                                   eff = 0.9,
                                   dom_sel = 0.5,
                                   dom_cos = 0.5,
                                   rr = 0.5,
                                   cost = 0,
                                   fitSS = 1,
                                   plot = FALSE)
{
  
  a_genfreq <- genotype_freq(resist_freq)
  #just to set up array
  a_mort <- a_genfreq
  
  sel <- rr * eff #selection coeff is resistance restoration * effectiveness
  
  #TODO check this
  #TODO have an intermediate step that returns the mortality of each genotype
  a_mort['RR'] <- (eff-sel)
  a_mort['RS'] <- (eff-(sel*dom_sel))
  a_mort['SS'] <- eff
  
  # a_mort['RR'] <- a_genfreq['RR'] * (eff-sel)
  # a_mort['RS'] <- a_genfreq['RS'] * (eff-(sel*dom_sel))
  # a_mort['SS'] <- a_genfreq['SS'] * eff
  
  return(a_mort)

  #TODO implement the below
  #proportional mortality
  #a_propmort <- a_mort * a_genfreq   
  #mort <- sum(a_propmort)
  
  #return(mort)
}

