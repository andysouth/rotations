# sensi_an_rotations_trimmed_4_jim.r 

# Minimal example of sensitivity analysis for rotations for Jim Maas to investigat parallelisation potential."
# andy south December 2018


#install.packages("devtools")
require(devtools)    
#install_github('AndySouth/rotations')  
require(rotations)


# linputs is a list of inputs for one scenario
# run_rot() runs a single scenario

# example for 1 scenario
# linputs <- list(max_gen = 50, n_insecticides = 2)
# do.call(run_rot, linputs)
# dfinputs <- as.data.frame(linputs)

# set number of scenarios
n_scenarios     <- 2 #000 

# set random seed so that results can be reproduced
set.seed(2)
max_gen         <- 500 #maximum generations in a scenario



# create a blank dataframe to contain results of all scenarios 
df_res_all <- data.frame()

for(i in 1:n_scenarios)
{
  #message should output to the R markdown console
  if (i%%200 == 1) message("scenario ",i," of ",n_scenarios," ",Sys.time())
  
  # blank list for inputs 
  linputs <- list()
  
  ## constant inputs ##
  linputs$coverage <- 1
  linputs$migration <- 0
  linputs$max_gen <- max_gen
  
  linputs$min_rwr_interval <- 10
  linputs$min_gens_switch_back <- 1
  linputs$no_r_below_start <- FALSE 
  linputs$no_r_below_mut <- FALSE 
  linputs$plot <-     FALSE

  ## variable inputs ##
  linputs$cost <-           runif(1, min=0,   max=0.1) 
  linputs$expo_hi <-        runif(1, min=0.4, max=0.9)  #min=0.1, max=0.9
  linputs$male_expo_prop <- runif(1, min=0,   max=1)
  linputs$eff <-            runif(1, min=0.5, max=1) # min=0.3, max=1
  linputs$rr  <-            runif(1, min=0.1, max=0.9)
  linputs$dom_sel <-        runif(1, min=0,   max=1)
  linputs$dom_cos <-        runif(1, min=0,   max=1) 
  #TODO make this lognormal to get lower frequencies too
  linputs$start_freqs <-    runif(1, min=0.005,   max=0.1) #note not log yet   
  linputs$n_insecticides <- sample( 2:5, 1 )  #beware set replace=T if more than 1
  
  #to compare sequence to rotation run all the scenarios above for both seq & rot 
  for(rot_or_not in 0:1)
  {  
    #to get rot_int of 0 every other run
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
    
    # to plot results for checking
    if ( linputs$rot_interval == 0 )
    {
      gens_seq <- as.numeric(res)
    } else
    {
      gens_rot <- as.numeric(res)
    } 


  # later I may want to concat all dfres into one object 
  # (add a column for scenario num) to save for later analysis
  # e.g. if I wanted to calculate average resistance frequencies over time
    
  } #end rot_or_not loop    
   
  # make an output row for each scenario with inputs, gens_rot, gens_seq
  # need to make sure I retain the rot_interval for rot

  # add scenario id column onto results
  df_in_out <- as.data.frame(c(linputs, gens_rot=gens_rot, gens_seq=gens_seq, id=i))
  df_in_out$rot_minus_seq <- df_in_out$gens_rot - df_in_out$gens_seq
  
  # previously I used rbind this scenario inputs & results onto previous
  # not a good speed or memory way of doing
  # df_res_all <- rbind(df_res_all, df_in_out)
  
  # now replaced with this better way
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
  


