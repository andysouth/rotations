#' migration_test to testimplementation of migration between an implementation area and refugia
#'
#' @param migration
#' @param coverage
#'
#'
# @import
# @importFrom
#' @return 
#' @export
#'
# @examples

#' 
migration_test <- function(migration = 0.5,
                           coverage = 0.6,
                           startfreqs = c(0.2,0.8),
                           max_generations = 10) {
  
#convert migration 0-1 to proportion leaving intervention
m_int <- migration*(1-coverage)  
m_ref <- m_int*coverage/(1-coverage)  
  
afreq <- array_named(site=c('intervention','refugia'), 
                      gen=1:max_generations)

# set frequencies in gen1 for intervention & refuge  
afreq[,1] <- startfreqs

for( gen in 2:max_generations)
{

  # sort of worked, kept sum(f) as 1, but went to 0.5 in first gen
  # afreq['refugia', gen] <-
  #   (1-migration)*afreq['refugia', gen-1] +
  #      migration *afreq['intervention', gen-1]
  # 
  # afreq['intervention', gen] <-
  #   (1-migration)*afreq['intervention', gen-1] +
  #      migration*afreq['refugia', gen-1]  
    
  #coverage/(1-coverage)
  
  #nearly, gen1 OK, but then declines over time
  # afreq['refugia', gen] <-
  #     (1-migration)*(1-coverage)*afreq['refugia', gen-1] +  #remainers
  #      migration *(coverage)  *afreq['intervention', gen-1] #arrivals
  # 
  # afreq['intervention', gen] <-
  #     (1-migration)*coverage*afreq['intervention', gen-1] + #remainers
  #      migration *(1-coverage)*afreq['refugia', gen-1]    #arrivals
    
  
  afreq['refugia', gen] <-
    (1-m_ref)*afreq['refugia', gen-1] +  #remainers
    m_ref*afreq['intervention', gen-1] #arrivals
  
  afreq['intervention', gen] <-
    (1-m_int)*afreq['intervention', gen-1] + #remainers
    m_int*afreq['refugia', gen-1]    #arrivals  
  
  #can I apply same eq to both intervention & refugia ? maybe not.
  #although perhaps with modulus
  
}

return(afreq)
}