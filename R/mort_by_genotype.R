#' calculate mortality by genotype from resistance freq
#' 
#' PROVISIONAL

#' @param eff effectiveness, for all insecticides or individually
#' @param dom_sel dominance of selection, for all insecticides or individually
# @param dom_cos dominance of cost, for all insecticides or individually
#' @param rr resistance restoration, for all insecticides or individually 
# @param cost fitness cost of RR in no insecticide, for all insecticides or individually
# @param fitSS fitness of SS if no insecticide, for all insecticides or individually
#' 
#' 
#' @examples 

#' @return fitness values
#' @export

mort_by_genotype <- function ( eff,# = 0.9,
                               dom_sel,# = 0.5,
                               rr )# = 0.5)
                               #cost = 0,
                               #fitSS = 1,
                               #plot = FALSE)
{
  
  #just to set up array
  a_mort <- genotype_freq(0)
  
  sel <- rr * eff #selection coeff is resistance restoration * effectiveness
  
  a_mort['RR'] <- (eff-sel)
  a_mort['RS'] <- (eff-(sel*dom_sel))
  a_mort['SS'] <- eff
  
  
  return(a_mort)
  
}

