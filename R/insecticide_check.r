#' insecticide_check check if insecticide needs to be changed
#'
#' based on a fixed interval no. generations, or rotate-when-resistant if interval is 0.    
#' The actual switching of insecticides is done in insecticide_switch().
#' 
#' @param RAF1gen single generation array of resistance allele frequencies
#' @param current_insecticide id number of current insecticide
#' @param rot_interval frequency of rotation (in generations) NB if set to zero mean RwR i.e. rotate when resistant
#' @param threshold trigger for change of insecticide, either resistance frequency or mortality dependent on mort_or_freq, also precludes switch to an insecticide.
#' @param mort_or_freq whether threshold for insecticide change is mortality 'mort' or resistance frequency 'freq'
#' @param gens_this_insecticide num generations this insecticide has been used for
#' @param min_rwr_interval minimum rotate-when-resistant interval to stop short switches, only used when rot_interval==0. set to 0 to have no effect.
#' @param change_interval interval that insecticide can be changed on for both rotation and sequence, default 10 gens 1 year
#' @param exit_rot whether to exit rotation interval if threshold is reached
# BEWARE these can be vectors and this func prob won't cope
#' @param eff effectiveness propn. SS killed by insecticide, for all insecticides or individually
#' @param dom_sel dominance of selection, for all insecticides or individually
# dom_cos not needed for mortality calculation
# @param dom_cos dominance of cost, for all insecticides or individually
#' @param rr resistance restoration, for all insecticides or individually 
#' @param diagnostics whether to output running info

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
                               rr,
                               change_interval,
                               diagnostics = FALSE
                              ) 
{
  #check that required value
  stopifnot( mort_or_freq == 'mort' | mort_or_freq == 'freq')
  
  # convert threshold to survival if mortality option is selected
  # BEWARE mort-survival conversion
  if (mort_or_freq == 'mort') 
  {
    threshold <- 1-threshold
  }
  
  change_insecticide <- FALSE
  
  # only allow change at change_interval, e.g. yearly
  if (gens_this_insecticide %% change_interval == 0)
  {
  
    # if sequence OR exit_rot==TRUE so rotation can be exited
    if (rot_interval == 0 | exit_rot == TRUE)
    {  
      # because have drop=false to preserve site dimension the gen dimension is preserved too
      # so just has a single item in the final dimension : RAF1gen[,,,1]
      
      # set from resistance frequency first
      freq <- mean(c(RAF1gen[current_insecticide, 'f','intervention',1],
                     RAF1gen[current_insecticide, 'm','intervention',1]))
      
      surv <- 1-mort_from_resist(rfreq=freq, eff=eff, dom_sel=dom_sel, rr=rr )
        
      check_value <- freq 
      
      # then check is if value is > survival same as for frequency
      if (mort_or_freq == 'mort') 
      {
        check_value <- surv  
      }
  
      
      if (diagnostics)
        message(paste0("insecticide",current_insecticide, " check=",round(check_value,3), 
                     " freq=", round(freq,3),
                     " surv=", round(surv,3),
                     " gens_this_insecticide=", gens_this_insecticide
      ))
          
      if ( check_value > threshold &
           #to add a min interval to stop lots short switches
           #BEWARE this min_rwr_interval can cause unexpected behaviour, 
           #e.g. can stop simulation ending when resistance threshold has been reached
           #20/8/19 also this allows an insecticide to start being used even when outside threshold
           gens_this_insecticide > min_rwr_interval )
      {
        change_insecticide <- TRUE 
        
        if (diagnostics)
           message(paste0("change from insecticide",current_insecticide, " check=",round(check_value,4),"\n"))
      }
      
      
    } 
  
    # even if in a rotation it my have assessed in previous loop that insecticide needs to be changed  
    if (rot_interval != 0)   # if a rotation 
    {
        
      #end of rotation reached
      if (gens_this_insecticide >= rot_interval) 
        {
        # 31/7/18 if only this insecticide remains below resistance threshold don't change
        # resetting gens-this-insecticide means it will be used for another rot_interval
        
        # freqs for all other insecticides, mean of M&F
        #other_ins_checks <- raf_get(RAF1gen,insecticide=-current_insecticide,sex='f',gen=1,site='intervention')
        #this_ins_check <- raf_get(RAF1gen,insecticide=current_insecticide,sex='f',gen=1,site='intervention') 
        other_ins_checks <- rowMeans(cbind(raf_get(RAF1gen,insecticide=-current_insecticide,sex='f',gen=1,site='intervention'),
                                           raf_get(RAF1gen,insecticide=-current_insecticide,sex='m',gen=1,site='intervention')))
        this_ins_check <- mean(c(raf_get(RAF1gen,insecticide=current_insecticide,sex='f',gen=1,site='intervention'),
                                 raf_get(RAF1gen,insecticide=current_insecticide,sex='m',gen=1,site='intervention'))) 
        
              
        # 26/2/19 allow this check to work on mortality instead of frequency
        # convert to survival if mortality option is selected
        # BEWARE mort-survival conversion
        # then check is if value is < survival same as for frequency
        # sapply allows mort_from_resist to be applied to all
        if (mort_or_freq == 'mort' & length(other_ins_checks)>0 ) 
        {
          #16/10/19 bug that this_ins_check wasn't converted to mortality
          #also bug that mortality hadn't been converted to survival, now is above
          this_ins_check <- 1 - mort_from_resist(this_ins_check, eff=eff, dom_sel=dom_sel, rr=rr)       
          other_ins_checks <- 1 - sapply(other_ins_checks, function(x) mort_from_resist(rfreq=x,eff=eff, dom_sel=dom_sel, rr=rr))
        }
        
        #BEWARE 4/10/2019 used to be BUG here, this bracket was after next loop leading to no change for 'freq'
        
        # only change if one to change to
        # in rotation force stop if this insecticide above thresh, even if no other to change to 
        # also in rotation stick with this insecticide if it's the only one below threshold
        if ( this_ins_check > threshold | min(other_ins_checks) <= threshold)
        {
          # time to rotate so need to identify the next insecticide in the rotation
          change_insecticide <- TRUE         
        }        
  
  
        
        if (diagnostics) 
        {
          
          #put survival in message
          msgtext <- ifelse( mort_or_freq=='mort','survival','resist frequency' )
          
          message(paste0("insecticide",current_insecticide, 
                         " ", msgtext, "=", round(this_ins_check,3),
                         " min others=",round(min(other_ins_checks),3),
                         " thresh=", threshold,
                         " change=",change_insecticide,"\n"))
        }
      }
    }  
  } # end of if change_interval
  
return(change_insecticide)  
}