#' migration between intervention and refugia
#' 
#' @param RAF1gen single generation array of resistance allele frequencies
#' @param migration migration rate between treated & untreated areas 0-1. We assume that immigration=emigration.
#' @param coverage proportion of mosquitoes that are covered by the intervention (and 1-C is the proportion of the population in the untreated refugia).
#' @param verbose whether to output before and after migration
#' 
#' @examples
#' #frequencies the same for all insecticides
#' RAF <- set_start_freqs(n=3, freqs=0.001)
#' RAF[,,,1] # to view generation 1
#' #frequencies different for each insecticide
#' RAF <- set_start_freqs(freqs=c(0.1,0.01,0.001))
#' RAF[,,,1] # to view generation 1
#' #testing mutliplying frequency arrays
#' RAF <- set_start_freqs(max_gen = 1, freqs=c(0.1,0.01,0.001))
#' RAF2 <- set_start_freqs(max_gen = 1, freqs=c(1,2,3))
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
  
  # to allow migration to be on a scale from 0-1 (1-coverage is the max)
  # calculating this in rot_migrate function allows easier testing of function
  migrate_intervention <- migration*(1-coverage)
  migrate_refugia <- migrate_intervention*coverage/(1-coverage)  
  
  # not needed because now calculated
  # if(migrate_intervention>(1-coverage)){
  # message(sprintf("warning from calibration: migration rate in/out of intervenation exceed 1 minus coverage\n"))   
  # }
  
  #RAF <- array_named(insecticide=1:n_insecticides, sex=c('m','f'), site=c('intervention','refugia'), gen=1:max_gen)
  
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
    #print(RAF1gen_old)
    #print(RAF1gen) 
    print(raf_get(RAF1gen_old,asdf=TRUE))
    print(raf_get(RAF1gen,asdf=TRUE))
  }
  
  invisible(RAF1gen)  
}