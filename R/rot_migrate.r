#' migration between intervention and refugia
#' 
#' @param RAF1gen single generation array of resistance allele frequencies
#' @param migration migration rate between treated & untreated areas 0-1. We assume that immigration=emigration.
#' @param coverage proportion of mosquitoes that are covered by the intervention (and 1-C is the proportion of the population in the untreated refugia).
#' @param verbose whether to output before and after migration
#' 
#' @examples
#' 
#' #frequencies different for each insecticide
#' RAF <- set_start_freqs(freqs=c(0.1,0.01,0.001))
#' 
#' #simplest example, freqs 1 in intervention, 0 in refugia
#' RAF <- set_start_freqs(n=3, freqs=1)
#' RAF1gen <- RAF[,,,1] # to view generation 1
#' #set freqs in refugia to 0
#' RAF1gen[,,'refugia']  <- 0
#' #testing migration
#' RAF2 <- rot_migrate(RAF1gen, migration=0.5, coverage=0.5, verbose=TRUE)
#' 
#' #max migration leads to random mixing, irrespective of coverage, intervention and refugia end with same freq
#' RAF2 <- rot_migrate(RAF1gen, migration=1, coverage=0.5, verbose=TRUE)
#' RAF2 <- rot_migrate(RAF1gen, migration=1, coverage=0.2, verbose=TRUE)
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
  
  # Ian suggested we could allow migration to be greater than 1
  # which might be the case if insects killed in the treated area caused greater than 
  # random migration in from the untreated area
  
  # OLD WAY
  # to allow migration to be on a scale from 0-1 (1-coverage is the max)
  # calculating this in rot_migrate function allows easier testing of function
  # migrate_intervention <- migration*(1-coverage)
  # migrate_refugia <- migrate_intervention*coverage/(1-coverage)  
  # 
  # # RAF[,, ensures calc is repeated for each insecticide and sex 
  # # more concise version of original code
  # 
  # mig_intervention <- (1-migrate_intervention)*RAF1gen[,, 'intervention']+
  #   migrate_intervention *RAF1gen[,, 'refugia']
  # 
  # mig_refugia      <- (1-migrate_refugia)*RAF1gen[,, 'refugia']+
  #   migrate_refugia *RAF1gen[,, 'intervention']
  # 
  # RAF1gen[,, 'intervention'] <- mig_intervention
  # RAF1gen[,, 'refugia'] <- mig_refugia
  # 
  
  # 9/10/19 changing dispersal to make simpler and clearer
  leavers_intervention <- migration*(1-coverage)
  leavers_refugia <- migration * coverage
  stay_intervention <- 1-leavers_intervention
  stay_refugia <- 1-leavers_refugia
  
  # after dispersal
  # f_intervention <- f_int*stay_intervention + f_ref*leave_intervention
  # because those that have left intervention are replaced by dispersers from refuge
  # BE CAREFUL - it feels wrong to multiply f_refugia by leavers_intervention but it is right
  freq_intervention <- RAF1gen[,, 'intervention']*stay_intervention + RAF1gen[,, 'refugia']*leavers_intervention  
  
  freq_refugia <- RAF1gen[,, 'refugia']*stay_refugia + RAF1gen[,, 'intervention']*leavers_refugia  
    
  
  #to allow showing what has happened
  RAF1gen_old <- RAF1gen
  
  RAF1gen[,, 'intervention'] <- freq_intervention
  RAF1gen[,, 'refugia'] <- freq_refugia

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