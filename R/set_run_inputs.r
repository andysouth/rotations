#' set run inputs from an experiment file
#'
#' probably want it to create a single object containing one row of inputs per run.
#' Then after another function can select rows from it and maybe direct to multiple cores.
#' 
#' @param inex object containing the experiment input values and ranges
#' 
#' @examples 
#' linputs_multi <- set_run_inputs()
#' # to get all inputs for a single scenario
#' linputs <- purrr::map(linputs_multi, 2)
#' # run one scenario
#' dfres <- do.call(run_rot, linputs)
#' 
#' @return a list of inputs for each of a set of scenarios can be accessed by e.g. purrr::map(linputs_multi, 2) linputs$max_gen[2]  
#' @export


set_run_inputs <- function( inex = NULL )
{
  
  # have a default file
  if (is.null(inex)) inex <- read_in_expt()
  
  
  # in_runs or should it be called in_scenarios
  # currently a list that grows, slow, but not a large file  
  linputs <- list()
  
  
  # take code from sensi_an_rotations1
  for(i in 1:inex$n_scenarios)
  {
    if (i%%200 == 1) message("scenario ",i," of ",inex$n_scenarios," ",Sys.time())

    linputs$max_gen[i] <- inex$max_gen
 
    # n_insecticides different because choosing random integer
    linputs$n_insecticides[i] <- sample( inex$n_insecticides_min:inex$n_insecticides_max, 1 )  #beware set replace=T if more than 1
    
       
    # select random uniform numbers between the ranges
    # this copes if min & max are the same
    
    # some can be different for each insecticide if insecticides_different is set to 1
    n_ins <- linputs$n_insecticides[i]
    
    n_rands <- ifelse(inex$insecticides_different==1, n_ins, 1)
    
    #TODO this doesn't work yet
    # In linputs$coverage[i] <- runif(n_rands, min = inex$coverage_min,  ... :
    #                                   number of items to replace is not a multiple of replacement length
    
    linputs$coverage[i] <-       runif(n_rands, min=inex$coverage_min,    max=inex$coverage_max)
    linputs$migration[i] <-      runif(n_rands, min=inex$migration_min,   max=inex$migration_max)
    linputs$cost[i] <-           runif(n_rands, min=inex$cost_min,        max=inex$cost_max) 
    linputs$eff[i] <-            runif(n_rands, min=inex$eff_min,         max=inex$eff_max)
    linputs$rr[i]  <-            runif(n_rands, min=inex$rr_min,          max=inex$rr_max)
    linputs$dom_sel[i] <-        runif(n_rands, min=inex$dom_sel_min,     max=inex$dom_sel_max)
    linputs$dom_cos[i] <-        runif(n_rands, min=inex$dom_cos_min,     max=inex$dom_cos_max)
    
    # some inputs cannot be different for diff insecticides
    # TODO should exposure be different for diff insecticides ?
    linputs$expo_hi[i] <-        runif(1, min=inex$expo_hi_min,     max=inex$expo_hi_max)
    linputs$male_expo_prop[i] <- runif(1, min=inex$male_expo_prop_min,   max=inex$male_expo_prop_max)
    
    # rotation interval, 0 for sequence
    linputs$rot_interval[i]  <-  runif(1, min=inex$rot_interval_min,max=inex$rot_interval_max)

    #TODO make this lognormal to get lower frequencies too
    linputs$start_freqs[i] <-    runif(1, min=inex$start_freqs_min,   max=inex$start_freqs_max) 
    
    
    
  }

linputs    
  
}
