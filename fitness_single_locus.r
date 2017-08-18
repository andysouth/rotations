#' calculate single locus fitness for a flexible number of insecticides
#' 
#' can be used in 2 ways
#' 1) by passing arrays a_dom etc. as done from runModel2()
#' 2) pass a single value and num_ins to give all insecticides the same value
#' 2) pass vectors - new way for rotations

#' @param num_ins number of insecticides (not needed if vectors or arrays are passed)
#' @param eff effectiveness
#' @param dom dominance1
#' @param rr_ resistance restoration selection coefficient = resistance restoration * effectiveness
#' @param cost fitness cost of R in no insecticide
#' @param fitSS fitness of SS if no insecticide
#' 
# @param a_dom dominance array
# @param a_sel selection coefficient array
# @param a_effect effectiveness array
# @param a_cost cost array
#' @param a_fitloc array of single locus fitnesses to fill
#' @param plot whether to plot fitness
#' 
#' @examples 
#' #defaults
#' fitness_single_locus()
#' fitness_single_locus(eff1 = 0.8)
#' #2 different insecticides
#' fitness_single_locus(eff=c(0.5,1), dom=c(0.5,0.5), rr=c(0.5,0.5), cost=c(0,0.1), fitSS=c(1,1))
#' #4 same insecticides
#' fitness_single_locus(num_ins=4, eff=0.5, dom=0.5, rr=0.5, cost=0, fitSS=1)
#' #4 pairs of the same insecticides
#' fitness_single_locus(num_ins=8, eff=c(0.5,1), dom=c(0.5,0.5), rr=c(0.5,0.5), cost=c(0,0.1), fitSS=c(1,1))

#' @return fitness values
#' @export

fitness_single_locus <- function ( num_ins = NULL,
                                   eff = c(0.5, 0.7, 0.9),
                                   dom = c(0.5, 0.5, 0.5),
                                   rr = c(0.5, 0.5, 0.5),
                                   cost = c(0,0,0),
                                   fitSS = c(1,1,1),
                                   # a_dom = NULL,
                                   # a_sel = NULL,
                                   # a_effect = NULL,
                                   # a_cost = NULL,
                                   a_fitloc = NULL,
                                   plot = FALSE)
{
  
  #get num_ins if it is not specified
  #todo add checks, allow single
  if ( is.null(num_ins)) num_ins <- length(eff)
  
  if ( is.null(a_fitloc) )
  {
    #in resistance :
    #a_fitloc   <- array_named( loci=c('SS1','RS1','RR1','SS2','RS2','RR2'), exposure=c('no','lo','hi') )
    #ians    
    #fitness <- array_named(insecticide=1:no_insecticides, genotype=c('SS','SR', 'RR'), amount=c('no','lo', 'hi'))
    #compromise :
    #BEWARE we need to decide on whether to call SR or RS, I've gone for RS Ian had SR
    a_fitloc<- array_named(insecticide=1:num_ins, genotype=c('SS','RS', 'RR'), exposure=c('no','lo', 'hi'))
    
    #set from input file in resistance::runModel2
    a_fitloc[,'SS','no'] <- fitSS 
  }
  
  #now vectorised so all the calculations below are done for every insecticide
  
  #exposure 0 'no'
  a_fitloc[ ,'RS', 'no'] <- 1 - (dom * cost)
  a_fitloc[ ,'RR', 'no'] <- 1 - cost
    
  sel <- rr * eff #selection coeff is resistance restoration * effectiveness
  
  for( exposID in c('lo','hi') )
  {
    #? is effectiveness, dominance & selection the same in lo as hi
    a_fitloc[ ,'SS', exposID] <- 1 - eff
    
    a_fitloc[ ,'RS', exposID] <- 1 - eff + dom * sel
    
    a_fitloc[ ,'RR', exposID] <- 1 - eff + sel
  }
  
  #error check for fitnesses > 1 or < 0
  if ( any( a_fitloc > 1  ) ) 
    warning( sum(a_fitloc > 1 ), " locus fitness values (a_fitloc) are >1 : ", a_fitloc[a_fitloc>1])
  if ( any( a_fitloc < 0 ) ) 
    warning( sum( a_fitloc < 0 ), " locus fitness values (a_fitloc) are <0")     
  
  # if (plot)
  # {
  #   df_fit1 <- as.data.frame(a_fitloc)
  #   #temp adding an extra column for faceting
  #   df_fit1$locus <- paste('locus', c(1,1,1,2,2,2))
  #   
  #   plot_fit_rs(df_fit1, 'hi', column_facet = 'locus')
  # }
  
  return(a_fitloc)
}