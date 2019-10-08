# rotations

In progress work to investigate the implications for the evolution of insecticide resistance of different public health insecticide use strategies. Principally to compare rotations where insecticides are changed frequently to sequential use where an insecticide is only changed once it has reached a resistance threshold. 

A population genetic simulation in an R package.


## Installation

Install the development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("AndySouth/rotations")
```

## Quick start

The core simulation function is `run_rot()` which runs one scenario generation by generation.

``` r
#default inputs
dfr <- run_rot()

#modifying specified inputs
dfr <- run_rot(rot_interval=10, n_insecticides=2)

```

## Running experiments

You can run experiments made up of multiple scenarios by reading in a file specifying input ranges (`read_in_expt()`) and the number of randomly generated scenarios.

``` r

nscenarios <- 3

# read in input ranges from a file (here using a default)
inex <- read_in_expt(nscenarios = nscenarios)

# create an object containing inputs for all scenarios
linmulti <- set_run_inputs(inex=inex)

# run all scenarios
for(i in 1:nscenarios)
{
  # get inputs for this scenario
  linputs <- get_one_in(linmulti, scen_num=i)
    
  ##################
  # run one scenario
  dfres <- do.call(run_rot, linputs)
  
  # summarise results per scenario
  gens <- gens_under_thresh(dfres)
  
  # add scenario id column and inputs onto results
  # df_in_out is one row
  df_in_out <- as.data.frame(c(linputs, gens=gens, id=i))
  
  # if the first scenario create df to store results of all scenarios
  if (i==1)
  {
    df_res_all <- data.frame(matrix(NA, ncol=length(names(df_in_out)), nrow=nscenarios))
    names(df_res_all) <- names(df_in_out)
  }
  
  #putting results into one row of overall results dataframe
  df_res_all[i,] <- df_in_out
}


```
