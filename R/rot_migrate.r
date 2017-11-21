#' migration between intervention and refugia
#' 
#' @param RAF array of resistance allele frequencies
#' @param migrate_intervention migration from intervention
#' @param migrate_refugia migration from refugia
#' 
#' @examples
#' #frequencies the same for all insecticides
#' RAF <- set_start_freqs(n=3, freqs=0.001)
#' RAF[,,,1] # to view generation 1
#' #frequencies different for each insecticide
#' RAF <- set_start_freqs(freqs=c(0.1,0.01,0.001))
#' RAF[,,,1] # to view generation 1
#' #testing mutliplying frequency arrays
#' RAF <- set_start_freqs(max_generations = 1, freqs=c(0.1,0.01,0.001))
#' RAF2 <- set_start_freqs(max_generations = 1, freqs=c(1,2,3))
#' RAF*RAF2
#' 
#' 
#' @return array of resistance freqs in 1 gen following migration
#' @export
#' 
rot_migrate <- function( RAF1gen,
                         migration = 0.8,
                         coverage = 0.6,
                         verbose = FALSE)
{
  
  # TODO I think I should get this to work
  # on RAF without the generation dimension
  
  # to allow migration to be on a scale from 0-1 (1-coverage is the max)
  # calculating this in rot_migrate function allows easier testing of function
  migrate_intervention <- migration*(1-coverage)
  migrate_refugia <- migrate_intervention*coverage/(1-coverage)  
  
  #RAF <- array_named(insecticide=1:n_insecticides, sex=c('m','f'), site=c('intervention','refugia'), gen=1:max_generations)
  
  # RAF[,, ensures calc is repeated for each insecticide and sex 
  # more concise version of original code
  
  mig_intervention <- (1-migrate_intervention)*RAF1gen[,, 'intervention']+
    migrate_intervention *RAF1gen[,, 'refugia']
  
  mig_refugia      <- (1-migrate_refugia)*RAF1gen[,, 'refugia']+
    migrate_refugia *RAF1gen[,, 'intervention']
  
  #to allow showing what has happened
  RAF1gen_old <- RAF1gen
  
  RAF1gen[,, 'intervention'] <- mig_intervention
  RAF1gen[,, 'refugia'] <- mig_refugia

  #RAF[ , , , 1] = freqs
  
  # optional display of before & after for testing
  # may remove later
  if (verbose)
  {
    print(RAF1gen_old)
    print(RAF1gen)    
  }
  
  return(RAF1gen)  
}