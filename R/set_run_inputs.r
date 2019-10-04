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
  
  # set random seed so that runs can be reproduced
  set.seed(inex$rand_seed)
  
  # currently a list that grows, slow, but not a large file  
  linmulti <- list()
  
  
  # take code from sensi_an_rotations1
  for(i in 1:inex$nscenarios)
  {
    #if (i%%200 == 1) message("scenario ",i," of ",inex$nscenarios," ",Sys.time())
    
    # constant inputs
    linmulti$max_gen[i] <- inex$max_gen
    linmulti$min_rwr_interval[i] <- inex$min_rwr_interval
    linmulti$min_gens_switch_back[i] <- inex$min_gens_switch_back
    linmulti$no_r_below_start[i] <- inex$no_r_below_start
    linmulti$no_r_below_mut[i] <- inex$no_r_below_mut
    linmulti$exit_rot[i] <- inex$exit_rot
    linmulti$mort_or_freq[i] <- inex$mort_or_freq
    linmulti$threshold[i] <- inex$threshold 
    linmulti$plot[i] <- inex$plot
    
 
    # random integers BEWARE that sample selects from 1:x when x is length 1
    # causes problem when want n_insecticides or rot_interval to be fixed
    # this resample function from the sample help file sorts it
    # so e.g when min & max are 4 sample is always 4
    resample <- function(x, ...) x[sample.int(length(x), ...)]
    
    linmulti$n_insecticides[i] <- resample( inex$n_insecticides_min:inex$n_insecticides_max, 1 )  #beware set replace=T if more than 1
    # rotation interval, 0 for sequence
    # TODO decide maybe only allow this in yearly intervals c10 generations 
    # divide min and max by 10 then multiply after
    mindiv10 <- inex$rot_interval_min/10
    maxdiv10 <- inex$rot_interval_max/10    
    linmulti$rot_interval[i]  <-  as.integer(10*resample( mindiv10:maxdiv10, 1))    
    # old version not restricted to yearly intervals
    #linmulti$rot_interval[i]  <-  resample( inex$rot_interval_min:inex$rot_interval_max, 1)    
       
    # select random uniform numbers between the ranges
    # this copes if min & max are the same
    
    # some inputs cannot be different for diff insecticides
    
    linmulti$expo_hi[i] <-        runif(1, min=inex$expo_hi_min,     max=inex$expo_hi_max)
    linmulti$male_expo_prop[i] <- runif(1, min=inex$male_expo_prop_min,   max=inex$male_expo_prop_max)
    linmulti$coverage[i] <-     runif(1, min=inex$coverage_min,    max=inex$coverage_max)
    linmulti$migration[i] <-     runif(1, min=inex$migration_min,   max=inex$migration_max)
    
    #log-uniform to get lower frequencies represented
    #select ‘x’ from uniform -1 to -4 and set to 10^x to give equal weight to each log interval.
    #log(0.01,10)
    #from resistance
    #P_1 <- 10^-(runif(1, min=1, max=4))
    minlog <- log(inex$start_freqs_min,10)
    maxlog <- log(inex$start_freqs_max,10)
    
    linmulti$start_freqs[i] <-  10^runif(1, min=minlog, max=maxlog) 
    
    # some can be different for each insecticide if insecticides_different is set to 1
    n_ins <- linmulti$n_insecticides[i]
    
    n_rands <- ifelse(inex$insecticides_different==1, n_ins, 1)
    
    # select random numbers for each insecticide
    
    cost <-     runif(n_rands, min=inex$cost_min,        max=inex$cost_max) 
    eff  <-     runif(n_rands, min=inex$eff_min,         max=inex$eff_max)
    rr   <-     runif(n_rands, min=inex$rr_min,          max=inex$rr_max)
    dsel <-     runif(n_rands, min=inex$dom_sel_min,     max=inex$dom_sel_max)
    dcos <-     runif(n_rands, min=inex$dom_cos_min,     max=inex$dom_cos_max)    
    
    # convert vectors to strings if more than one insecticide
    if (n_rands > 1)
    {
      cost <- toString(cost) 
      eff  <- toString(eff)
      rr   <- toString(rr)
      dsel <- toString(dsel)
      dcos <- toString(dcos)       
    }

    linmulti$cost[i] <-           cost 
    linmulti$eff[i] <-            eff
    linmulti$rr[i]  <-            rr
    linmulti$dom_sel[i] <-        dsel
    linmulti$dom_cos[i] <-        dcos    
        
    # linmulti$cost[i] <-           runif(n_rands, min=inex$cost_min,        max=inex$cost_max) 
    # linmulti$eff[i] <-            runif(n_rands, min=inex$eff_min,         max=inex$eff_max)
    # linmulti$rr[i]  <-            runif(n_rands, min=inex$rr_min,          max=inex$rr_max)
    # linmulti$dom_sel[i] <-        runif(n_rands, min=inex$dom_sel_min,     max=inex$dom_sel_max)
    # linmulti$dom_cos[i] <-        runif(n_rands, min=inex$dom_cos_min,     max=inex$dom_cos_max)
    
    
    
  }

linmulti    
  
}
