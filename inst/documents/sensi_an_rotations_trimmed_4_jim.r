# sensi_an_rotations_trimmed_4_jim.r 

# Minimal example of sensitivity analysis for rotations for Jim Maas to investigat parallelisation potential."
# andy south December 2018


#install.packages("devtools")
library(devtools)    
install_github('AndySouth/rotations', upgrade = 'never')  
library(rotations)

# linputs       list of inputs for one scenario
# run_rot()     main function that runs a single scenario
# dfres         dataframe with detailed results from one scenario
# df_res_all    dataframe with summary results one row per scenario across all scenarios

# example for 1 scenario
# linputs <- list(max_gen = 50, n_insecticides = 2)
# do.call(run_rot, linputs)

# set number of scenarios
n_scenarios     <- 3 #000 

# set random seed so that results can be reproduced
set.seed(2)

# create a blank dataframe to contain results of all scenarios 
df_res_all <- data.frame()

for(i in 1:n_scenarios)
{
  #message
  if (i%%200 == 1) message("scenario ",i," of ",n_scenarios," ",Sys.time())
  
  # blank list for inputs 
  linputs <- list()
  
  ## constant inputs ##
  linputs$coverage <- 1
  linputs$migration <- 0
  linputs$max_gen <- 300
  linputs$plot <-     FALSE

  ## variable inputs ##
  linputs$cost <-           runif(1, min=0,   max=0.1) 
  linputs$eff <-            runif(1, min=0.5, max=1) # min=0.3, max=1
  linputs$n_insecticides <- sample( 2:5, 1 )  #beware set replace=T if more than 1
  
  #to compare sequence to rotation run all the scenarios above for both seq & rot 
  for(rot_or_not in 0:1)
  { 
    #sequence:rot_interval=0 
    #rotation:rot_interval>0
    #to get rot_interval of 0 every other run
    if (rot_or_not == 0) linputs$rot_interval <- 0
    else   #random rot interval integer
        linputs$rot_interval <-   sample( 5:50, 1 )  #beware set replace=T if > 1
    
    ##################
    # run one scenario
    dfres <- do.call(rotations::run_rot, linputs)
    
    # summarise results per scenario 
    # (number of generations under the resistance threshold)
    # this is also calc at end of run_rot() here is probably better
    res <- rotations::gens_under_thresh(dfres)
    
    # for saving of results later
    if ( linputs$rot_interval == 0 ) { gens_seq <- as.numeric(res)
    }  else{                           gens_rot <- as.numeric(res) } 

  # later I may want to also concat all dfres into one object for more detailed analysis
    
  } #end rot_or_not loop    
   
  # make an output row for each scenario with inputs, gens_rot, gens_seq
  df_in_out <- as.data.frame(c(linputs, gens_rot=gens_rot, gens_seq=gens_seq, id=i))
  df_in_out$rot_minus_seq <- df_in_out$gens_rot - df_in_out$gens_seq
  
  
  # set a dataframe for the outputs to the required size in scenario1
  if (i==1)
  {
    df_res_all <- data.frame(matrix(NA, ncol=length(names(df_in_out)), nrow=n_scenarios))
    names(df_res_all) <- names(df_in_out)
  }
  
  #putting results into one row of overall results dataframe
  df_res_all[i,] <- df_in_out 
  

  # save object containing inputs and results as rda for analysis
  save(df_res_all, file='df_res_all_rot.rda') #paste0(outFolder,'*.rda'))    
  
} #end scenario loop  
  


