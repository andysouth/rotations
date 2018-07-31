#' insecticide_switch choose a new insecticide to switch to
#'
#' 
#' 
#' @param RAF array of resistance allele frequencies
#' @param current_insecticide id number of current insecticide
#' @param n_insecticides number of insecticides (and hence loci)
#' @param rot_criterion resistant allele frequency that triggers a RwR change or precludes a insecticide from being rotated in.
#' @param gen generation number
#' @param min_gens_switch_back minimum num gens before can switch back to an insecticide
#' @param df_ins number of generations since each insecticide used
#' @param df_results results of sim so far
#' @param diagnostics whether to output running info
# @param rot_interval frequency of rotation (in generations) NB if set to zero mean RwR i.e. rotate when resistant
# @param rot_criterion resistant allele frequency that triggers a RwR change or precludes a insecticide from being rotated in.
# @param rot_count num generations this insecticide has been used for
#' 
# @examples 
#' 
#' @return integer : insecticide number to switch to, if 0 none found 
#' @export


insecticide_switch <- function( RAF,
                                current_insecticide,
                                n_insecticides,
                                rot_criterion,
                                gen,
                                min_gens_switch_back,
                                df_ins,
                                df_results,
                                diagnostics)
{
  next_insecticide_found <- FALSE
  candidate <- current_insecticide 
  
  for(temp_int in 1:n_insecticides)
  {
    #search through insecticides and go back to start if reach end
    candidate <- ifelse(candidate==n_insecticides, yes=1, no=candidate+1)
    
    #11/5/18 adding condition of not going back to recently used insecticide
    if (RAF[candidate, 'f','intervention', gen] < rot_criterion & 
        df_ins$last_used[candidate] > min_gens_switch_back )
    {
      
      if (diagnostics) message(sprintf("generation %d, switch from insecticide %d to %d; frequencies = %f and %f",
                      gen, current_insecticide, candidate,
                      RAF[current_insecticide, 'f','intervention', gen], RAF[candidate, 'f','intervention', gen]))
      
      next_insecticide_found <- TRUE 
      current_insecticide <- candidate 
      #change_insecticide <- 0 
    }
    
    if (next_insecticide_found) break   
    
  } # end of loop checking each insecticide
 
# return 0 if no insecticide to use next found  
if (!next_insecticide_found) current_insecticide <- 0 
   
return(current_insecticide)  
}