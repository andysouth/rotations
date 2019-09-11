#' set run inputs from an experiment file
#'
#' probably want it to create a single object containing one row of inputs per run.
#' Then after another function can select rows from it and maybe direct to multiple cores.
#' 
#' @param inex object containing the experiment input values and ranges
#' 
#' @examples 
#' # read an experiment file specifying ranges
#' inex <- read_in_expt() #specify filename
#' # create an object containing inputs for all scenarios
#' linmulti <- set_run_inputs(inex=inex)
#' # get inputs for a single scenario
#' linputs <- get_one_in(linmulti, scen_num=2)
#' # run one scenario
#' dfres <- do.call(run_rot, linputs)
#' 
#' @return a list of inputs for each of a set of scenarios can be accessed by e.g. purrr::map(linputs_multi, 2) linputs$max_gen[2]  
#' @export


set_run_inputs <- function( inex = NULL )
{
  
  # have a default file
  if (is.null(inex)) inex <- read_in_expt()
  
  
  # currently a list that grows, slow, but not a large file  
  linmulti <- list()
  
  
  # take code from sensi_an_rotations1
  for(i in 1:inex$n_scenarios)
  {
    if (i%%200 == 1) message("scenario ",i," of ",inex$n_scenarios," ",Sys.time())

    linmulti$max_gen[i] <- inex$max_gen
 
    # n_insecticides different because choosing random integer
    linmulti$n_insecticides[i] <- sample( inex$n_insecticides_min:inex$n_insecticides_max, 1 )  #beware set replace=T if more than 1
    
       
    # select random uniform numbers between the ranges
    # this copes if min & max are the same
    
    # some can be different for each insecticide if insecticides_different is set to 1
    n_ins <- linmulti$n_insecticides[i]
    
    n_rands <- ifelse(inex$insecticides_different==1, n_ins, 1)
    
    # select random numbers for each insecticide
    
    cove <-     runif(n_rands, min=inex$coverage_min,    max=inex$coverage_max)
    migr <-     runif(n_rands, min=inex$migration_min,   max=inex$migration_max)
    cost <-     runif(n_rands, min=inex$cost_min,        max=inex$cost_max) 
    eff  <-     runif(n_rands, min=inex$eff_min,         max=inex$eff_max)
    rr   <-     runif(n_rands, min=inex$rr_min,          max=inex$rr_max)
    dsel <-     runif(n_rands, min=inex$dom_sel_min,     max=inex$dom_sel_max)
    dcos <-     runif(n_rands, min=inex$dom_cos_min,     max=inex$dom_cos_max)    
    
    # convert vectors to strings if more than one insecticide
    if (n_rands > 1)
    {
      cove <- toString(cove)
      migr <- toString(migr)
      cost <- toString(cost) 
      eff  <- toString(eff)
      rr   <- toString(rr)
      dsel <- toString(dsel)
      dcos <- toString(dcos)       
    }

    linmulti$coverage[i] <-       cove
    linmulti$migration[i] <-      migr
    linmulti$cost[i] <-           cost 
    linmulti$eff[i] <-            eff
    linmulti$rr[i]  <-            rr
    linmulti$dom_sel[i] <-        dsel
    linmulti$dom_cos[i] <-        dcos    
        
    # linmulti$coverage[i] <-       runif(n_rands, min=inex$coverage_min,    max=inex$coverage_max)
    # linmulti$migration[i] <-      runif(n_rands, min=inex$migration_min,   max=inex$migration_max)
    # linmulti$cost[i] <-           runif(n_rands, min=inex$cost_min,        max=inex$cost_max) 
    # linmulti$eff[i] <-            runif(n_rands, min=inex$eff_min,         max=inex$eff_max)
    # linmulti$rr[i]  <-            runif(n_rands, min=inex$rr_min,          max=inex$rr_max)
    # linmulti$dom_sel[i] <-        runif(n_rands, min=inex$dom_sel_min,     max=inex$dom_sel_max)
    # linmulti$dom_cos[i] <-        runif(n_rands, min=inex$dom_cos_min,     max=inex$dom_cos_max)
    
    
    # some inputs cannot be different for diff insecticides
    # TODO should exposure be different for diff insecticides ?
    linmulti$expo_hi[i] <-        runif(1, min=inex$expo_hi_min,     max=inex$expo_hi_max)
    linmulti$male_expo_prop[i] <- runif(1, min=inex$male_expo_prop_min,   max=inex$male_expo_prop_max)
    
    # rotation interval, 0 for sequence
    linmulti$rot_interval[i]  <-  runif(1, min=inex$rot_interval_min,max=inex$rot_interval_max)

    #TODO make this lognormal to get lower frequencies too
    linmulti$start_freqs[i] <-    runif(1, min=inex$start_freqs_min,   max=inex$start_freqs_max) 
    
    
  }

linmulti    
  
}
