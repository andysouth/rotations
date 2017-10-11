#' calculate single locus fitness for a flexible number of insecticides
#' 
#' can be used in 2 ways
#' 1) pass a single value and n_insecticides to give all insecticides the same value
#' 2) pass vectors - new way for rotations

#' @param n_insecticides number of insecticides (not needed if vectors or arrays are passed)
#' @param eff effectiveness, for all insecticides or individually
#' @param dom_sel dominance of selection, for all insecticides or individually
#' @param dom_cos dominance of cost, for all insecticides or individually
#' @param rr resistance restoration, for all insecticides or individually 
#' @param cost fitness cost of RR in no insecticide, for all insecticides or individually
#' @param fitSS fitness of SS if no insecticide, for all insecticides or individually
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
#' #2 different insecticides
#' fitness_single_locus(eff=c(0.5,1), dom_sel=c(0.5,0.5), dom_cost=c(0.5,0.5), rr=c(0.5,0.5), cost=c(0,0.1), fitSS=c(1,1))
#' #4 same insecticides
#' fitness_single_locus(n_insecticides=4, eff=0.5, dom_sel=0.5, dom_cost=0.5, rr=0.5, cost=0, fitSS=1)
#' #4 pairs of the same insecticides
#' fitness_single_locus(n_insecticides=8, eff=c(0.5,1), dom_sel=c(0.5,0.5), dom_cost=c(0.5,0.5), rr=c(0.5,0.5), cost=c(0,0.1), fitSS=c(1,1))

#' @return fitness values
#' @export

fitness_single_locus <- function ( n_insecticides = NULL,
                                   eff = c(0.5, 0.7, 0.9),
                                   dom_sel = c(0.5, 0.5, 0.5),
                                   dom_cos = c(0.5, 0.5, 0.5),
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
  
  #get n_insecticides if it is not specified
  #todo add checks, allow single
  if ( is.null(n_insecticides)) n_insecticides <- length(eff)
  
  if ( is.null(a_fitloc) )
  {
    #in resistance :
    #a_fitloc   <- array_named( loci=c('SS1','RS1','RR1','SS2','RS2','RR2'), exposure=c('no','lo','hi') )
    #ians    
    #fitness <- array_named(insecticide=1:no_insecticides, genotype=c('SS','SR', 'RR'), amount=c('no','lo', 'hi'))
    #compromise :
    #BEWARE we need to decide on whether to call SR or RS, I've gone for RS Ian had SR
    a_fitloc<- array_named(insecticide=1:n_insecticides, genotype=c('SS','RS', 'RR'), exposure=c('no','lo', 'hi'))
    
    #set from input file in resistance::runModel2
    a_fitloc[,'SS','no'] <- fitSS 
  }
  
  #now vectorised so all the calculations below are done for every insecticide
  
  #exposure 0 'no'
  a_fitloc[ ,'RS', 'no'] <- 1 - (dom_cos * cost)
  a_fitloc[ ,'RR', 'no'] <- 1 - cost
    
  sel <- rr * eff #selection coeff is resistance restoration * effectiveness
  
  for( exposID in c('lo','hi') )
  {
    #? is effectiveness, dominance & selection the same in lo as hi
    a_fitloc[ ,'SS', exposID] <- 1 - eff
    
    a_fitloc[ ,'RS', exposID] <- 1 - eff + dom_sel * sel
    
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

#' test fill a single locus fitness array
#' 
#' with hardcoded values

#' @param n_insecticides number of insecticides 
#' @param a_fitloc array of single locus fitnesses to fill
#' @param same_insecticides whether to just set fitnesses for all insecticides the same
#' @param plot whether to plot fitness
#' 
#' @examples 
#' #defaults
#' fitness_single_locus()

#' @return fitness values
#' @export

fitness_single_locus_test <- function ( n_insecticides = NULL,
                                   a_fitloc = NULL,
                                   same_insecticides = FALSE,
                                   plot = FALSE)
{
  
  #set n_insecticides if it is not specified
  if ( is.null(n_insecticides)) n_insecticides <- 4

  # create array
  a_fitloc <-  array_named(insecticide=1:n_insecticides, genotype=c('SS','RS', 'RR'), amount=c('no','lo', 'hi'))
  
  
  # andy looking to set fitnesses for all insecticides to be same
  # but it seemed to mess up simulation
  if (same_insecticides)
  {
    a_fitloc[, 'SS', 'no']=0.3; a_fitloc[, 'SS', 'lo']=0.3; a_fitloc[, 'SS', 'hi']=0.3;
    a_fitloc[, 'RS', 'no']=0.3; a_fitloc[, 'RS', 'lo']=0.7; a_fitloc[, 'RS', 'hi']=0.7;
    a_fitloc[, 'RR', 'no']=0.3; a_fitloc[, 'RR', 'lo']=0.9; a_fitloc[, 'RR', 'hi']=0.9;
  
  } else #ians original test data
  {
    #genetic data for locus 1
    a_fitloc[1, 'SS', 'no']=0.1; a_fitloc[1, 'SS', 'lo']=0.3; a_fitloc[1, 'SS', 'hi']=0.3;
    a_fitloc[1, 'RS', 'no']=0.1; a_fitloc[1, 'RS', 'lo']=0.7; a_fitloc[1, 'RS', 'hi']=0.7;
    a_fitloc[1, 'RR', 'no']=0.1; a_fitloc[1, 'RR', 'lo']=0.9; a_fitloc[1, 'RR', 'hi']=0.9;
    #genetic data for locus 2
    if(n_insecticides>=2){
      a_fitloc[2, 'SS', 'no']=0.3; a_fitloc[2, 'SS', 'lo']=0.3; a_fitloc[2, 'SS', 'hi']=0.3;
      a_fitloc[2, 'RS', 'no']=0.3; a_fitloc[2, 'RS', 'lo']=0.8; a_fitloc[2, 'RS', 'hi']=0.8;
      a_fitloc[2, 'RR', 'no']=0.3; a_fitloc[2, 'RR', 'lo']=0.9; a_fitloc[2, 'RR', 'hi']=0.9;
    }
    #genetic data for locus 3
    if(n_insecticides>=3){
      a_fitloc[3, 'SS', 'no']=0.3; a_fitloc[3, 'SS', 'lo']=0.3; a_fitloc[3, 'SS', 'hi']=0.3;
      a_fitloc[3, 'RS', 'no']=0.3; a_fitloc[3, 'RS', 'lo']=0.3; a_fitloc[3, 'RS', 'hi']=0.3;
      a_fitloc[3, 'RR', 'no']=0.3; a_fitloc[3, 'RR', 'lo']=0.3; a_fitloc[3, 'RR', 'hi']=0.3;
    }
    #genetic data for locus 4
    if(n_insecticides>=4){
      a_fitloc[4, 'SS', 'no']=0.3; a_fitloc[4, 'SS', 'lo']=0.3; a_fitloc[4, 'SS', 'hi']=0.3;
      a_fitloc[4, 'RS', 'no']=0.3; a_fitloc[4, 'RS', 'lo']=0.4; a_fitloc[4, 'RS', 'hi']=0.4;
      a_fitloc[4, 'RR', 'no']=0.3; a_fitloc[4, 'RR', 'lo']=0.5; a_fitloc[4, 'RR', 'hi']=0.5;
    }#genetic data for locus 5
    if(n_insecticides>=5){
      a_fitloc[5, 'SS', 'no']=0.3; a_fitloc[5, 'SS', 'lo']=0.3; a_fitloc[5, 'SS', 'hi']=0.3;
      a_fitloc[5, 'RS', 'no']=0.3; a_fitloc[5, 'RS', 'lo']=0.3; a_fitloc[5, 'RS', 'hi']=0.3;
      a_fitloc[5, 'RR', 'no']=0.3; a_fitloc[5, 'RR', 'lo']=0.3; a_fitloc[5, 'RR', 'hi']=0.3;
    }    
  }
    
  
  #error check for fitnesses > 1 or < 0
  if ( any( a_fitloc > 1  ) ) 
    warning( sum(a_fitloc > 1 ), " locus fitness values (a_fitloc) are >1 : ", a_fitloc[a_fitloc>1])
  if ( any( a_fitloc < 0 ) ) 
    warning( sum( a_fitloc < 0 ), " locus fitness values (a_fitloc) are <0")     
  
  
  return(a_fitloc)
}


