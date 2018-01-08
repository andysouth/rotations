#' insecticide_check check if insecticide needs to be changed
#'
#' based on a fixed interval no. generations, or rotate-when-resistant if interval is 0
#' 
#' @param RAF1gen single generation array of resistance allele frequencies
#' @param current_insecticide id number of current insecticide
#' @param rot_interval frequency of rotation (in generations) NB if set to zero mean RwR i.e. rotate when resistant
#' @param rot_criterion resistant allele frequency that triggers a RwR change or precludes a insecticide from being rotated in.
#' @param gens_this_insecticide num generations this insecticide has been used for
#' @param min_rwr_interval minimum rotate-when-resistant interval to stop short switches, only used when rot_interval==0. set to 0 to have no effect.
# @examples 
#' 
#' @return TRUE/FALSE whether to change 
#' @export


insecticide_check <- function( RAF1gen,
                               current_insecticide,
                               rot_interval, 
                               rot_criterion,
                               gens_this_insecticide,
                               min_rwr_interval
                              ) 
{
  change_insecticide <- FALSE
  
  # if rotate-when resistant
  if (rot_interval == 0)
  {  
    #TODO check with Ian that switch criterion is female only
    
    #TODO later alternative stop switching back to insecticide that has been used within 5 generations
    
    if ( RAF1gen[current_insecticide, 'f','intervention'] > rot_criterion &
         #to add a min interval to stop lots short switches
         gens_this_insecticide > min_rwr_interval )
    {
      change_insecticide <- TRUE        
    }
    #message(sprintf("confirm. RAF=%f, change_insecticide=%d \n", RAF1gen[current_insecticide, 'f','intervention'], change_insecticide))
    
  } else if (rot_interval != 0)
    # if periodic rotation  
  {
    if (gens_this_insecticide != rot_interval) 
    {
      # keeps the rotation going
      # not needed here because done in run_rot()
      gens_this_insecticide <- gens_this_insecticide+1  
      # to make responsive rotation add a check of resistance frequency here
      # and stop the rotation if threshold exceeded
      
    } else 
    {
      # time to rotate so need to identify the next insecticide in the rotation
      change_insecticide <- TRUE 
    }
  }        
  
return(change_insecticide)  
}