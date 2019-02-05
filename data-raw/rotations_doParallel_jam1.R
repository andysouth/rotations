## script to test if Andy South and Ian Hasting's insecticide
## rotations R model can be speed up by parallelizing some of the loops. All the
## code and original workings are from Andy South.

## The name of the original file from Andy is sensi_an_rotations_trimmed_4_jim.r 

## Minimal example of sensitivity analysis for rotations for Jim Maas to
## investigate parallelisation potential."  andy south December 2018

## First created on 7/12/2018 by JAM as LSTM
## comments added by ABS 10/12/18
## Last updated on 10/12/2018

# This runs parallel operations, on a single computer (node) and 
# will use as many cores as you have on that individual node, or however 
# many you give the script access to. So for example on your notebook you 
# would likely not give it more than three, and keep one free to run all 
# the other operations you need to function.  My desktop has twelve cores 
# so I allocate ten to run these simulations and it works well.  I just 
# ran this script for 5000 scenarios, on ten cores and it took a little 
# less than eight minutes to complete.  So for Simon's new server you 
# could run this on about 120 cores at a single time if they were free.


## remove any stuff hanging around in memory
#rm(list=ls())

## install necessary libraries
library(devtools)    
library(rotations)
library(doParallel)

## start the cluster
num_cores <- 3
cl <- makeCluster(num_cores)
registerDoParallel(cl)

getDoParWorkers()

## linputs       a list of inputs for one scenario
## run_rot()     runs a single scenario
## dfres         detailed results from one scenario
## df_res_all    summary results one row per scenario across all scenarios

## example for 1 scenario
## linputs <- list(max_gen = 50, n_insecticides = 2)
## do.call(run_rot, linputs)

## get start time to do profiling
ptm <- proc.time()

## set number of scenarios
n_scenarios <- 100

## set random seed so that results can be reproduced
set.seed(2)


#andy aha this causes (df_in_out) all results to be rbind together : .combine = rbind

para1results <- foreach (i = 1:n_scenarios,
                         .combine = rbind,
                         .packages = c("rotations")) %dopar% {
  
  ## blank list for inputs 
  linputs <- list()
  
  #### constant inputs ####
  linputs$coverage <- 1
  linputs$migration <- 0
  linputs$max_gen <- 300
  linputs$plot <-     FALSE

  #### variable inputs ####
  linputs$cost <-           runif(1, min=0,   max=0.1) 
  linputs$eff <-            runif(1, min=0.5, max=1) ## min=0.3, max=1
  linputs$n_insecticides <- sample( 2:5, 1 )  ##beware set replace=T if more than 1
  
  ##to compare sequence to rotation run all the scenarios above for both seq & rot 
    for(rot_or_not in 0:1)
  { 
    ##sequence:rot_interval=0 
    ##rotation:rot_interval>0
    ##to get rot_interval of 0 every other run
    if (rot_or_not == 0) linputs$rot_interval <- 0
    else   ##random rot interval integer
        linputs$rot_interval <-   sample( 5:50, 1 )  ##beware set replace=T if > 1
    
    ##################
    ## run one scenario
    dfres <- do.call(rotations::run_rot, linputs)
    
    ## summarise results per scenario 
    ## (number of generations under the resistance threshold)
    ## this is also calc at end of run_rot() here is probably better
    res <- rotations::gens_under_thresh(dfres)
    
    ## for saving of results later
    if ( linputs$rot_interval == 0 ) { gens_seq <- as.numeric(res)
    }  else{                           gens_rot <- as.numeric(res) } 

  ## later I may want to also concat all dfres into one object for more detailed analysis
    
  } ##end rot_or_not loop    
   
  ## make an output row for each scenario with inputs, gens_rot, gens_seq
  df_in_out <- as.data.frame(c(linputs, gens_rot=gens_rot, gens_seq=gens_seq, id=i))
  df_in_out$rot_minus_seq <- df_in_out$gens_rot - df_in_out$gens_seq
  df_in_out
  
  # all the df_in_out instances are bound together with rbind
  # as specified at the start of the foreach loop
  
} ##end scenario parallel loop  



## record end time and calculate time used
proc.time() - ptm

write.csv(para1results,'para1results.csv')
##    save(df_res_all, file='df_res_all_rot.rda') ##paste0(outFolder,'*.rda'))


## close down the cluster and mpi
stopCluster(cl)
