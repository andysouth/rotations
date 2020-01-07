#' gens_under_thresh calculate generations under thresholds from simulation results.
#' 
#' Note this is the sum across all insecticides for the number of generations below 
#' threshold while that insecticide is deployed.
#'
#' @param dfres dataframe of resistance results from run_rot()
#' @param threshold trigger for change of insecticide, either resistance frequency or mortality dependent on mort_or_freq, also precludes switch to an insecticide.
#' @param mort_or_freq whether threshold for insecticide change is mortality 'mort' or resistance frequency 'freq'
#' 
# @import ggplot2
#' @importFrom rlang .data
#' @return integer num generations
#' @export
#'
#' @examples
#' df_res2 <- run_rot(plot=FALSE)
#' gens_under_thresh(df_res2, threshold=0.5, mort_or_freq = 'freq')
#' 

gens_under_thresh <- function(dfres,
                              threshold,
                              mort_or_freq = 'freq')
{


res <- dfres %>%
  # only assess in control areas not in refugia  
  dplyr::filter(.data$active_or_refuge=='active') %>%
  group_by(.data$resist_gene)
  # for all insecticides in all generations  
  # summarise(gens_under50 = sum(resistance < 0.5, na.rm=TRUE)) %>%
  # summarise(mean_gens_under50 = mean(gens_under50)) %>%
  # just for deployed insecticides 
if ( mort_or_freq == 'freq' )
  res <- summarise(res, gens_dep_under50 = sum(.data$resistance < threshold &
                                   #finds insecticide in use = this one
                                   .data$resist_gene==paste0('insecticide',.data$insecticide), na.rm=TRUE))

else if ( mort_or_freq == 'mort' )
{
  res <- summarise(res, gens_dep_under50 = sum(.data$mortality > threshold &
                                                 #finds insecticide in use = this one
                                                 .data$resist_gene==paste0('insecticide',.data$insecticide), na.rm=TRUE))
}
        
##14/6/19 corrected bug here by adding back res <-      
res <- summarise(res, tot_gens_dep_under50 = sum(.data$gens_dep_under50)) %>%    
unlist()

return(res) 

}