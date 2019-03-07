#' insecticide_check check if insecticide needs to be changed
#'
#' based on a fixed interval no. generations, or rotate-when-resistant if interval is 0
#' 
#' @param RAF1gen single generation array of resistance allele frequencies
#' @param current_insecticide id number of current insecticide
#' @param rot_interval frequency of rotation (in generations) NB if set to zero mean RwR i.e. rotate when resistant
#' @param threshold trigger for change of insecticide, either resistance frequency or mortality dependent on mort_or_freq, also precludes switch to an insecticide.
#' @param mort_or_freq whether threshold for insecticide change is mortality 'mort' or resistance frequency 'freq'
#' @param gens_this_insecticide num generations this insecticide has been used for
#' @param min_rwr_interval minimum rotate-when-resistant interval to stop short switches, only used when rot_interval==0. set to 0 to have no effect.
#' @param exit_rot whether to exit rotation interval if threshold is reached
# BEWARE these can be vectors and this func prob won't cope
#' @param eff effectiveness propn. SS killed by insecticide, for all insecticides or individually
#' @param dom_sel dominance of selection, for all insecticides or individually
#' @param dom_cos dominance of cost, for all insecticides or individually
#' @param rr resistance restoration, for all insecticides or individually 

# @examples 
#' 
#' @return TRUE/FALSE whether to change 
#' @export


insecticide_check <- function( RAF1gen,
                               current_insecticide,
                               rot_interval, 
                               threshold,
                               mort_or_freq,
                               gens_this_insecticide,
                               min_rwr_interval,
                               exit_rot,
                               eff,
                               dom_sel,
                               rr
                              ) 
{
  #check that required value
  stopifnot( mort_or_freq == 'mort' | mort_or_freq == 'freq')
  
  change_insecticide <- FALSE
  
  # if rotate-when resistant OR exit_rot==TRUE so rotation can be exited
  if (rot_interval == 0 | exit_rot == TRUE)
  {  
    #BEWARE that switch criterion is female only
    
    # because have drop=false to preserve site dimension the gen dimension is preserved too
    # so just has a single item in the final dimension : RAF1gen[,,,1]
    
    # 26/2/19 allow mortality-based change criteria
    
    # set from resistance frequency first
    freq <- RAF1gen[current_insecticide, 'f','intervention',1]
    surv <- 1-mort_from_resist(rfreq=freq, eff=eff, dom_sel=dom_sel, rr=rr )
      
    check_value <- freq 
    
    # convert to survival if mortality option is selected
    # BEWARE mort-survival conversion
    # then check is if value is > survival same as for frequency
    if (mort_or_freq == 'mort') 
    {
      check_value <- surv  
      threshold <- 1-threshold
    }

    

    message(paste0("insecticide",current_insecticide, " check=",check_value, 
                   " freq=", freq,
                   " surv=", surv
    ))
        
    if ( check_value > threshold &
         #to add a min interval to stop lots short switches
         #BEWARE this min_rwr_interval can cause unexpected behaviour, 
         #e.g. can stop simulation ending when resistance threshold has been reached
         gens_this_insecticide > min_rwr_interval )
    {
      change_insecticide <- TRUE 
      
      message(paste0("change from insecticide",current_insecticide, " check=",check_value,"\n"))
    }
    

    
    
  } 

  # even if in a rotation it my have assessed in previous loop that insecticide needs to be changed  
  if (rot_interval != 0)   # if a rotation 
  {
      
    #end of rotation reached
    if (gens_this_insecticide >= rot_interval) 
      {
      # 31/7/18 I could add a check in here for case
      # where only this insecticide remains below resistance threshold
      # in which case don't want to change
      # if I reset gens-this-insecticide then it will be used for another rot_interval
      #I can use either of these to get the freqs for all other insecticides
      # RAF[-current_insecticide, 'f','intervention',1]
      other_ins_checks <- raf_get(RAF1gen,insecticide=-current_insecticide,sex='f',gen=1,site='intervention')
      #TODO check whether I need to assess that current insecticide is below its frequency
      #& RAF1gen[current_insecticide, 'f','intervention',1] <= threshold 
      
      # 26/2/19 allow this check to work on mortality instead of frequency
      # convert to survival if mortality option is selected
      # BEWARE mort-survival conversion
      # then check is if value is < survival same as for frequency
      # ARG can't do this because mort_from_resist is not vectorised
      # sapply fixes it
      #PROBLEM HERE giving arg 'rfreq' is missing error from UI
      if (mort_or_freq == 'mort' & length(other_ins_checks)>0 ) 
      {
        #other_ins_checks <- 1 - mort_from_resist(other_ins_checks)       
        other_ins_checks <- 1 - sapply(other_ins_checks, function(x) mort_from_resist(rfreq=x,eff=eff, dom_sel=dom_sel, rr=rr))
        
        # 31/7/18 new condition, only change if one to change to
        if ( min(other_ins_checks) <= threshold )
        {
          # time to rotate so need to identify the next insecticide in the rotation
          change_insecticide <- TRUE         
        }        
      }

      
      # message(paste0("insecticide",current_insecticide, 
      #                " freq=",RAF1gen[current_insecticide, 'f','intervention',1],
      #                " min other freqs=",min(other_ins_checks),
      #                " change=",change_insecticide,"\n"))
      
      
    }
  }        
  
return(change_insecticide)  
}