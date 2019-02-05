# rotations_doMPI_jam1.R
# Jim Maas and Andy South Dec 2018
#
# To run simultaneously across many computers that are connected into a cluster, 
# as big as you can imagine. Requires specific backend software called Message 
# Passing Interface (MPI) that must be loaded onto each node of the 
# cluster.  For testing and development purposes MPI can be loaded locally,
# most clusters have it loaded by default.   Within the R 
# script it uses the same frontend "foreach" loop that connects to the MPI 
# backend.  I've been running jobs on the UEA cluster using 404 cores, 
# usually comprised of several nodes that each comprise 16 cores per 
# node.  With this method the number of cores is almost infinite, up to 
# the maximum of your hardware, nowdays most clusters are a minimum of 
# several thousand cores.  It is also my motivation for learning how to do 
# this type of job on an Amazon Web Services cloud instance, where the 
# number of cores required can be requested at time of execution, and you 
# only pay for what you use.

## The name of the original file from Andy is sensi_an_rotations_trimmed_4_jim.r 

## Minimal example of sensitivity analysis for rotations for Jim Maas to
## investigate parallelisation potential."  andy south December 2018

## First created on 7/12/2018 by JAM as LSTM
## Last updated on 7/12/2018

## remove any stuff hanging around in memory
rm(list=ls())

## install necessary libraries
## library(devtools)    
library(rotations)
library(doMPI)

## start the cluster
cl <- startMPIcluster(count = 10)
registerDoMPI(cl)

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
n_scenarios <- 5000

## set random seed so that results can be reproduced
set.seed(2)

## create a blank dataframe to contain results of all scenarios 
## df_res_all <- data.frame()

## for(i in 1:n_scenarios)

para1results <- foreach (i = 1:n_scenarios,
                         .combine = rbind,
                         .packages = c("rotations")) %dopar% {
  ##message should output to the R markdown console
##  if (i%%200 == 1) message("scenario ",i," of ",n_scenarios," ",Sys.time())
  
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

write.csv(para1results,'MpiPara1Results.csv')
save(df_res_all, file='df_res_all_rot.rda') ##paste0(outFolder,'*.rda'))

## close down the cluster and mpi
## closeCluster(cl)
mpi.quit()
