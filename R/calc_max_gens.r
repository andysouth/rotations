#' calc_max_gens to get maximum generations reached in a single simulation from simulation results.
#' 
#' very simple
#' 
#'
#' @param dfres dataframe of resistance results from run_rot()
#' 
#' @importFrom rlang .data
#' @return integer num generations
#' @export
#'
#' @examples
#' df_res2 <- run_rot(plot=FALSE)
#' calc_max_gens(df_res2)
#' 

calc_max_gens <- function(dfres)
{


  df <- dfres %>%
    dplyr::filter(!is.na(.data$insecticide)) %>%
    summarise(max_gens = max(.data$generation))  

  return(df$max_gens)  
  

}