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
#' @param exit_rot whether to exit rotation interval if rot_criterion is reached

# @examples 
#' 
#' @return TRUE/FALSE whether to change 
#' @export


insecticide_check <- function( RAF1gen,
                               current_insecticide,
                               rot_interval, 
                               rot_criterion,
                               gens_this_insecticide,
                               min_rwr_interval,
                               exit_rot
                              ) 
{
  change_insecticide <- FALSE
  
  # if rotate-when resistant OR exit_rot==TRUE so rotation can be exited
  if (rot_interval == 0 | exit_rot == TRUE)
  {  
    #BEWARE that switch criterion is female only
    
    # because have drop=false to preserve site dimension the gen dimension is preserved too
    # so just has a single item in the final dimension : RAF1gen[,,,1]
    
    if ( RAF1gen[current_insecticide, 'f','intervention',1] > rot_criterion &
         #to add a min interval to stop lots short switches
         #BEWARE this min_rwr_interval can cause unexpected behaviour, 
         #e.g. can stop simulation ending when resistance threshold has been reached
         gens_this_insecticide > min_rwr_interval )
    {
      change_insecticide <- TRUE 
      
      #message(paste0("change from insecticide",current_insecticide, " freq=",RAF1gen[current_insecticide, 'f','intervention',1],"\n"))
    }
    #message(sprintf("confirm. RAF=%f, change_insecticide=%d \n", RAF1gen[current_insecticide, 'f','intervention'], change_insecticide))
  } 

  # even if in a rotation it my have assessed in previous loop that insecticide needs to be changed  
  if (rot_interval != 0)   # if a rotation 
  {
    # if (gens_this_insecticide != rot_interval) 
    # {
    #   # keeps the rotation going
    #   # not needed here because done in run_rot()
    #   gens_this_insecticide <- gens_this_insecticide+1  
    #   # to make responsive rotation add a check of resistance frequency here
    #   # and stop the rotation if threshold exceeded
    # } else 
      
    #end of rotation reached
    if (gens_this_insecticide >= rot_interval) 
      {
      # 31/7/18 I could add a check in here for case
      # where only this insecticide remains below resistance threshold
      # in which case don't want to change
      # if I reset gens-this-insecticide then it will be used for another rot_interval
      #I can use either of these to get the freqs for all other insecticides
      # RAF[-current_insecticide, 'f','intervention',1]
      other_ins_freqs <- raf_get(RAF1gen,insecticide=-current_insecticide,sex='f',gen=1,site='intervention')
      #TODO check whether I need to assess that current insecticide is below its frequency
      #& RAF1gen[current_insecticide, 'f','intervention',1] <= rot_criterion 
      
      # 31/7/18 new condition, only change of one to change to
      if ( min(other_ins_freqs) <= rot_criterion )
      {
        # time to rotate so need to identify the next insecticide in the rotation
        change_insecticide <- TRUE         
      }
      
      # message(paste0("insecticide",current_insecticide, 
      #                " freq=",RAF1gen[current_insecticide, 'f','intervention',1],
      #                " min other freqs=",min(other_ins_freqs),
      #                " change=",change_insecticide,"\n"))
      
      
    }
  }        
  
return(change_insecticide)  
}