#' set run inputs from an experiment file
#'
#' probably want it to create a single object containing one row of inputs per run.
#' Then after another function can select rows from it and maybe direct to multiple cores.
#' 
#' @param in_expt object containing the experiment input values and ranges
#' 
# @examples 
#' 
#' @return a list of inputs for each of a set of scenarios can be accessed by e.g. linputs$max_gen[2]  
#' @export


set_run_inputs <- function( in_expt = NULL )
{
  
  # have a default file
  if (is.null(in_expt)) in_expt <- read_in_expt()
  
  
  # in_runs or should it be called in_scenarios
  # currently a list that grows, slow, but not a large file  
  linputs <- list()
  
  
  # take code from sensi_an_rotations1
  for(i in 1:in_expt$n_scenarios)
  {
    if (i%%200 == 1) message("scenario ",i," of ",in_expt$n_scenarios," ",Sys.time())

    linputs$max_gen[i] <- in_expt$max_gen
        
    linputs$coverage[i] <- 1
    linputs$migration[i] <- 0


    
    
  }

linputs    
  
}
