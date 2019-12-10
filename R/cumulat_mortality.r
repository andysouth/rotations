#' cumulat_mortality to calculate cumulative mortality across all generations for a simulation
#'  just for the currently active insecticide (and not in the refuge) 
#'
#' @param dfres dataframe of resistance results from run_rot()
#' 
#' @importFrom rlang .data
#' @return integer num generations
#' @export
#'
#' @examples
#' df_res2 <- run_rot(plot=FALSE)
#' cumulat_mortality(df_res2)
#' 

cumulat_mortality <- function(dfres)
{

  #just for the currently active insecticide (and not in the refuge) 
  
  res <- dfres %>%
    # only assess in control areas not in refugia  
    # just for deployed insecticides 
    dplyr::filter(.data$active_or_refuge=='active' &
                  .data$resist_gene==paste0('insecticide',.data$insecticide)) %>%
    #group_by(.data$resist_gene) %>%
    summarise(cum_mort = sum(.data$mortality, na.rm=TRUE)) 
    #%>%
    #unlist()
  
  return(res$cum_mort) 
  

}