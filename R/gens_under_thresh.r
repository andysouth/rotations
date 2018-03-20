#' gens_under_thresh calculate generations under thresholds from simulation results.
#' 
#' Note this is the sum across all insecticides for the number of generations below 
#' threshold while that insecticide is deployed.
#'
#' @param dfres dataframe of resistance results from run_rot()
#' @param rot_criterion resistant allele frequency threshold
# @import ggplot2
#' @return integer num generations
#' @export
#'
#' @examples
#' df_res2 <- run_rot()
#' gens_under_thresh(df_res2)
#' 

gens_under_thresh <- function(dfres,
                              rot_criterion = 0.5)
{


res <- dfres %>%
  # only assess in control areas not in refugia  
  dplyr::filter(active_or_refuge=='active') %>%
  group_by(resist_gene) %>%
  # for all insecticides in all generations  
  # summarise(gens_under50 = sum(resistance < 0.5, na.rm=TRUE)) %>%
  # summarise(mean_gens_under50 = mean(gens_under50)) %>%
  # just for deployed insecticides 
  summarise(gens_dep_under50 = sum(resistance < rot_criterion &
                                     #finds insecticide in use = this one
                                     resist_gene==paste0('insecticide',insecticide), na.rm=TRUE)) %>%
  summarise(tot_gens_dep_under50 = sum(gens_dep_under50)) %>%    
  unlist()

return(res)

}