#' run_rot to run simulation of the effect of rotations on the spread of resistance
#'
#' can run any number of insecticides/loci 
#' 
#' @param max_gen maximum number of mosquito generations to run the simulation
#' @param n_insecticides number of insecticides (and hence loci)
#' @param start_freqs starting frequencies of resistance either one per insecticide or same for all
#' @param rot_interval frequency of rotation (in generations) NB if set to zero mean RwR i.e. rotate when resistant
#' @param rot_criterion resistant allele frequency that triggers a RwR change or precludes a insecticide from being rotated in.
#' @param migration migration rate between treated & untreated areas 0-1. We assume that immigration=emigration.
#' @param coverage proportion of mosquitoes that are covered by the intervention (and 1-C is the proportion of the population in the untreated refugia).
#' @param start_insecticide which insecticide to start with
#' @param expo_hi exposure to insecticide in hi niche, either single or vector of 1 per insecticide
#' @param expo_lo exposure to insecticide in lo niche, either single or vector of 1 per insecticide
#' @param male_expo_prop proportion tht males are exposed relative to f, default 1, likely to be <1 (could possibly be a vector per insecticide)
#' @param eff effectiveness, for all insecticides or individually
#' @param dom_sel dominance of selection, for all insecticides or individually
#' @param dom_cos dominance of cost, for all insecticides or individually
#' @param rr resistance restoration, for all insecticides or individually 
#' @param cost fitness cost of RR in no insecticide, for all insecticides or individually
#' @param fitSS fitness of SS if no insecticide, for all insecticides or individually
#' @param min_rwr_interval minimum rotate-when-resistant interval to stop short switches, only used when rot_interval==0. set to 0 to have no effect.
#' @param no_r_below_start to stop resistance frequencies going below starting values TRUE or FALSE
#' @param no_r_below_mut to stop resistance frequencies going below mutation-selection balance TRUE or FALSE
#' @param exit_rot whether to exit rotation interval if rot_criterion is reached
#' @param min_gens_switch_back minimum num gens before can switch back to an insecticide
# @param df_ins number of generations since each insecticide used
#' 
#' @param plot whether to plot results
#' @param diagnostics whether to output running info
# @param hardcode_fitness whether to use hardcoded fitness, default FALSE
# @param same_insecticides only used with hardcode_fitness, whether to just set fitnesses for all insecticides the same
# @param hardcode_exposure whether to use hardcoded exposure, default FALSE
#' @param logy whether to use log scale for y axis
#' @param add_gens_under50 whether to add a label of num generations under 50 pcent resistance
#' 
#' @examples 
#' run_rot(rot_interval=100)
#' dfr <- run_rot(rot_interval=50, max_gen = 300)
#' dfr <- run_rot(rot_interval=0, max_gen = 300)
#' dfr <- run_rot(rot_interval=0, max_gen = 300, migration=0.01)
#' #running for insecticides with different inputs
#' dfr <- run_rot(n_insecticides=3, eff=c(1,0.6,0.4), rot_interval=0)
#' 
#' @import tidyverse 
#to try help with standard evaluation of dplyr couldn't get to work
# @importFrom rlang .data 
#' @return dataframe of results
#' @export


run_rot <- function(max_gen = 200, 
                    n_insecticides = 4, 
                    start_freqs = 0.01,
                    rot_interval = 10, 
                    rot_criterion = 0.5, 
                    migration = 0.01, 
                    #migrate_intervention = 0.01, 
                    coverage = 0.8, 
                    start_insecticide = 1,
                    expo_hi = 0.8,
                    expo_lo = 0,                              
                    male_expo_prop = 1,
                    eff = 0.8, #c(0.5, 0.7, 0.9),
                    dom_sel = 0.5, #c(0.5, 0.5, 0.5),
                    dom_cos = 0.5, #c(0.5, 0.5, 0.5),
                    rr = 0.5, #c(0.5, 0.5, 0.5),
                    cost = 0.1, #c(0,0,0),
                    fitSS = 1,
                    min_rwr_interval = 1,
                    no_r_below_start = TRUE,
                    no_r_below_mut = FALSE,
                    exit_rot = TRUE,
                    min_gens_switch_back = 10,
                    
                    #inputs below not needed to run model itself
                    plot = TRUE,
                    diagnostics = FALSE,
                    #hardcode_fitness = FALSE,
                    #same_insecticides = TRUE,
                    #hardcode_exposure = FALSE,
                    logy = TRUE,
                    add_gens_under50 = FALSE ) 
  {
  
  
  if ( coverage <= 0 ) stop("the model cannot represent 0 (or less) coverage\n")
  
  
  # setup dataframe to store results, tricky to cope with variable number insecticides
  l_gene_plus_activity <- rep(list(rep(NA,max_gen)), n_insecticides*2) #2 because active & refuge 
  
  names(l_gene_plus_activity) <- c( paste0('insecticide', 1:n_insecticides, "_active"),
                                    paste0('insecticide', 1:n_insecticides, "_refuge") )
  
  df_results <- do.call(data.frame, list(generation = 1:max_gen,                                         
                                         insecticide = NA,
                                         stringsAsFactors = FALSE,
                                         l_gene_plus_activity))  
  
  # 11/5/18 adding an df to store for each insecticide the last time it was used
  df_ins <- data.frame(last_used=rep(Inf, n_insecticides))
  # set value for this to 0 each time an insecticide is in use
  # each generation if the value is not Inf add 1 to it for all insecticides not in use
  # use this to assess whether can go back to an insecticide
  
  ### set starting allele frequencies 
  #todo add checks thats start_freqs is either length 1 or n_insecticides
  RAF <- set_start_freqs( n_insecticides=n_insecticides, 
                          max_gen=max_gen, 
                          freqs = start_freqs,
                          coverage = coverage )
  
  #old hardcoded test function
  #RAF <- set_start_freqs_test( n_insecticides=n_insecticides, max_gen=max_gen )    
    
  ### set exposures
  exposure <- set_exposure_rot( n_insecticides=n_insecticides,
                                expo_hi = expo_hi,
                                expo_lo = expo_lo,                              
                                male_expo_prop = male_expo_prop)
  #old hardcoded test function
  #exposure <- set_exposure_rot_test( n_insecticides=n_insecticides )
  
  ### set fitnesses 
  fitness <- fitness_single_locus(n_insecticides=n_insecticides, 
                                  eff=eff, 
                                  dom_sel=dom_sel, 
                                  dom_cos=dom_cos, 
                                  rr=rr, 
                                  cost=cost, 
                                  fitSS=fitSS)
  
  #old hardcoded test function
  #if (hardcode_fitness)
  #  fitness <- fitness_single_locus_test( n_insecticides=n_insecticides, same_insecticides = same_insecticides )

  # check that exposure(none) is not less than zero
  for(temp_int in 1:n_insecticides){
    if (exposure[temp_int, 'm', 'no']<0) message(sprintf("warning from calibration: m exposure to no insecticide %d is <0\n", temp_int)) 
    if (exposure[temp_int, 'f', 'no']<0) message(sprintf("warning from calibration: f exposure to no insecticide %d is <0\n", temp_int)) 
  }
 
  # calc mutation-selection balance if option selected
  if (no_r_below_mut)
  {
    if (no_r_below_start) stop("you can't have no_r_below_start and no_r_below_mut")
    
    mut_sel_freqs <- mutn_seln_bal( mutation=1e-9, cost=cost, dom_cos=dom_cos )
    # to cope with insecticides with different inputs
    if (length(mut_sel_freqs) < n_insecticides) mut_sel_freqs <- rep(mut_sel_freqs,n_insecticides)
  }
   
  
  # usually start the rotation sequence at #1 but can specify any one start
  current_insecticide <- start_insecticide 
  # store the insecticide for generation1 although its effects will only be seen in following gen
  # this is consistent with later generations.
  df_results$insecticide[1] <- current_insecticide
  
  gens_this_insecticide <- 1
  
  ## start at generation 2 because generation 1 holds the user-defined initial allele frequencies
  ## The conditions within a generation influence resistance which is stored in the following generation.
  ## in the plots there is gap between rotation that stops e.g. at gen 10 and next starts at gen 11
  for(gen in 2:max_gen)
    { 
    for(insecticide in 1:n_insecticides)
    {
      # extract resistance allele freqs (raf) for previous timestep
      raf_m <- RAF[insecticide, 'm', 'intervention', gen-1]
      raf_f <- RAF[insecticide, 'f', 'intervention', gen-1]  
      
      # later var names
      # raf_f_r_new resistance_allele_frequency_female_resistant, m_s male_susceptible
      
      ######
      # intervention site, with the insecticide in use  
      if(insecticide==current_insecticide){
       
        coeff_1 <- (raf_m * (1-raf_f) +
                   (1-raf_m) * raf_f) * 0.5
       
        coeff_2 <- sum( exposure[insecticide, 'm', ]*fitness[insecticide, 'RS', ] )
        coeff_3 <- sum( exposure[insecticide, 'f', ]*fitness[insecticide, 'RS', ] )
        
        # male RR (Eqn 3 rotations MS)
        temp_coeff <- sum( exposure[insecticide, 'm', ]*fitness[insecticide, 'RR', ] )
        raf_m_r_new <- raf_m * raf_f * temp_coeff + coeff_1*coeff_2
        
        # male SS (Eqn 4 rotation MS)
        temp_coeff <- sum( exposure[insecticide, 'm', ]*fitness[insecticide, 'SS', ] )
        raf_m_s_new <- (1-raf_m) * (1-raf_f) * temp_coeff + coeff_1*coeff_2
    
        #normalise the male gamete frequencies and store the results
        norm_coeff <- raf_m_r_new + raf_m_s_new
        RAF[insecticide, 'm', 'intervention', gen] <- raf_m_r_new / norm_coeff
    
        
        # female RR (Eqn 5 rotation MS)
        temp_coeff <- sum( exposure[insecticide, 'f', ]*fitness[insecticide, 'RR', ] )    
        raf_f_r_new <- raf_m * raf_f * temp_coeff + coeff_1*coeff_3
        
        #female SS (Eqn 6 rotation MS)
        temp_coeff <- sum( exposure[insecticide, 'f', ]*fitness[insecticide, 'SS', ] )  
        raf_f_s_new <- (1-raf_m) * (1-raf_f) * temp_coeff + coeff_1*coeff_3 
      
        #normalise female gamete frequencies and store the results
        norm_coeff <- raf_f_r_new + raf_f_s_new
        RAF[insecticide, 'f', 'intervention', gen] <- raf_f_r_new / norm_coeff
        
        #if (diagnostics) message(sprintf("generation %d: completed insecticide selection for locus/insecticide %d\n", gen, insecticide))
        
      } #end of loop that deals with this insecticide if it is being deployed
    
     ###### 
     # intervention site, insecticide not in use  
     else{ 
       
       #coefficient for RS common to equations 2 and 3
       rs_coeff <- (raf_m * (1-raf_f) +
                   raf_f * (1-raf_m)) *
                   0.5*fitness[insecticide, 'RS', 'no']
 
       # 23/11/18 talking to Ian
       # fitness of adults is calculated same for m&f when not exposed
       # because m & f can be exposed differently has to be done differently for mf above
       
       # male RR
       raf_m_r_new <- raf_m * raf_f *
                                fitness[insecticide, 'RR', 'no'] + rs_coeff
       # male SS
       raf_m_s_new <- (1-raf_m) * (1-raf_f) *
                                fitness[insecticide, 'SS', 'no'] + rs_coeff
       # normalise
       norm_coeff <- raf_m_r_new + raf_m_s_new
       RAF[insecticide, 'm', 'intervention', gen] <- raf_m_r_new / norm_coeff
       # no insecticides in use so same frequencies for both sexes
       RAF[insecticide, 'f', 'intervention', gen] <- raf_m_r_new / norm_coeff
       
       #if (diagnostics) message(sprintf("generation %d: completed selection against locus %d in intervention site\n", gen, insecticide))   
          
       } #end of code for insecticides that are not being deployed in the intervention site
      
      ######  
      # refugia, only needed if coverage < 1
      
      if ( coverage < 1 )
      {
        # extract resistance allele freqs (raf) for previous timestep
        raf_m <- RAF[insecticide, 'm', 'refugia', gen-1]
        raf_f <- RAF[insecticide, 'f', 'refugia', gen-1]  
        
        # RS coefficient common to equations 2 and 3
        rs_coeff <- (raf_m*(1-raf_f) +
                    raf_f*(1-raf_m)) *
                    0.5*fitness[insecticide, 'RS', 'no']
        
        # male RR   
        raf_m_r_new <- raf_m * raf_f * 
                                    fitness[insecticide, 'RR', 'no'] + rs_coeff
        # male SS
        raf_m_s_new  <- (1-raf_m) * (1-raf_f) *
                                    fitness[insecticide, 'SS', 'no'] + rs_coeff 
        
        #normalise and store results
        norm_coeff <- raf_m_r_new + raf_m_s_new
        # TODO check with Ian, raf_m_s_new not used except in normalisation
        
        RAF[insecticide, 'm', 'refugia', gen] <- raf_m_r_new / norm_coeff
        
        # no insecticides in use so same frequencies for both sexes
        RAF[insecticide, 'f', 'refugia', gen] <- RAF[insecticide, 'm', 'refugia', gen]
        
        #if (diagnostics) message(sprintf("generation %d: completed selection against locus %d in refugia\n", gen, insecticide))
      
      } # end if coverage < 1
     } #end of cycling insecticides
    
    
    ######  
    # migration between refugia and intervention site
    # andy adding a condition, refugia not needed if coverage=1
    if ( coverage < 1 )
    {
      #BEWARE if only one insecticide the insecticide dimension gets dropped and that causes an error
      #but I can't do drop=FALSE because I currently rely on the generation dimension being dropped
      RAF[,,,gen] <- rot_migrate(RAF[,,,gen], migration=migration, coverage=coverage)
    }
    
    ######
    # ensure that resistance stays above start or mutation-selection balance 
    # if options selected
    
    # to work with single start_freqs value too
    if (length(start_freqs) == 1) start_freqs <- rep(start_freqs, n_insecticides)
    
    
    #todo implement a no_r_below_mut option here
    if ( no_r_below_start | no_r_below_mut )
    {
      for(insecticide in 1:n_insecticides)
      {
        for(sex in dimnames(RAF)[[2]])
        {
          for(site in dimnames(RAF)[[3]])
          {
           if ( no_r_below_start & RAF[insecticide,sex,site,gen] < start_freqs[insecticide]) 
           {
             RAF[insecticide,sex,site,gen] <- start_freqs[insecticide]            
           }
           # if frequency has gone below the mutation-selection balance restore it if option selected  
           else if (no_r_below_mut)
           {
             if (RAF[insecticide,sex,site,gen] < mut_sel_freqs[insecticide] )
             {
               RAF[insecticide,sex,site,gen] <- mut_sel_freqs[insecticide]              
             }             
            }
          }         
        }
      }
    }
    
    
    ######
    # check if insecticide switch is needed
    # drop=FALSE imp to preserve site dimension when coverage=1
    change_insecticide <- insecticide_check( RAF1gen = RAF[,,,gen, drop=FALSE],
                                             current_insecticide, 
                                             rot_interval=rot_interval, 
                                             rot_criterion=rot_criterion, 
                                             gens_this_insecticide=gens_this_insecticide,
                                             min_rwr_interval=min_rwr_interval,
                                             exit_rot=exit_rot)

    
    if (! change_insecticide)
    {
      gens_this_insecticide <- gens_this_insecticide + 1
    }
    else if (change_insecticide)
    {
      gens_this_insecticide <- 1 
      
      current_insecticide <- insecticide_switch(RAF=RAF,
                                                current_insecticide=current_insecticide,
                                                n_insecticides=n_insecticides, 
                                                rot_criterion=rot_criterion,
                                                gen=gen,
                                                min_gens_switch_back=min_gens_switch_back,
                                                df_ins=df_ins,
                                                df_results=df_results,
                                                diagnostics=diagnostics)
    } 
  
    #message("gen", gen, " curr_ins=", current_insecticide, " change=", change_insecticide)
    
    #11/5/18 record how many generations since each insecticide has been used
    for (temp_int in 1:n_insecticides)
    {
      if ( temp_int == current_insecticide )
      {
        df_ins$last_used[temp_int] <- 0        
      }
      else #if ( !is.na(df_ins$last_used[temp_int]))
      {
        df_ins$last_used[temp_int] <- 1 + df_ins$last_used[temp_int]
      }
    }
    
    #temp testing
    #print(df_ins)
    
    # if no suitable insecticide left, break out of generations loop
    if (current_insecticide == 0) #(next_insecticide_found==0)
    {
      if (diagnostics) message(sprintf("\nsimulation terminating at generation %d because all RAFs above threshold of %f\n", gen,  rot_criterion))
      for (temp_int in 1:n_insecticides)
      {
        if (diagnostics) message(sprintf("frequency of resistance in females to insecticide %d is %f", temp_int, RAF[temp_int, 'f','intervention', gen]))  
      }
      break #breaks out of looping generations and terminates the simulation
    }    
 
    # recording the insecticide that's going to be used in next timestep
    df_results$insecticide[gen] <- current_insecticide    
  
   } #### end of max_gen loop

  # warning
  if ( diagnostics & gen == max_gen ) message("thresholds not reached before max generations, consider rerunning with higher max_gen")
      
  #####################################  
  # recording results of resistance frequency
  
  # saving results in wide data frame  
  for(i_num in 1:n_insecticides)
  {
    # does calculation for all generations (final dimension in RAF array)
    df_results[[paste0('insecticide',i_num,'_active')]] <- 0.5*(RAF[i_num, 'm','intervention', ]+
                                                                RAF[i_num, 'f','intervention', ])
    
    # andy refugia not done if coverage==1                                                                                                                RAF[i_num, 'f','intervention', ])
    if ( coverage < 1 )
    {
      df_results[[paste0('insecticide',i_num,'_refuge')]] <- 0.5*(RAF[i_num, 'm','refugia', ]+
                                                                  RAF[i_num, 'f','refugia', ])       
    }

    
    #df_res_active$region[[(i_num-1)*max_gen:(i_num)*max_gen]] <- paste0("insecticide",i_num)
    #df_res_active$resistance[[(i_num-1)*max_gen:(i_num)*max_gen]] <-  0.5*(RAF[i_num, 'm','intervention', ]+                                                                                            RAF[i_num, 'f','intervention', ])
  }
  
  # to enable facetting by intervention later
  df_res2 <- df_results %>%
    gather(names(l_gene_plus_activity),
           key=region, value=resistance)
  
  # use tidyr::separate() to get from r1_refuge to r1 & refuge in different columns.
  # to get active & refuge into the same subplot
  df_res2 <- tidyr::separate(df_res2, region, into=c("resist_gene","active_or_refuge"))
  
  # calculate number generations under 50% resistance to be used in plotting 
  # probably should be somewhere else ! ? just for active area
  # also calculated in sensi_an_rotations1.Rmd
  # this does give the answer, but only 1 per insecticide
  # NSE problem fixed by dplyr::filter 
  df_res2 <- df_res2 %>%
    dplyr::filter(active_or_refuge=='active') %>%
    group_by(resist_gene) %>%
    # for all insecticides in all generations  
    # summarise(gens_under50 = sum(resistance < 0.5, na.rm=TRUE)) %>%
    # just for deployed insecticides 
    summarise(gens_dep_under50 = sum(resistance < rot_criterion &
                                       #finds insecticide in use = this one
                                       resist_gene==paste0('insecticide',insecticide), na.rm=TRUE)) %>%
    #summarise(tot_dep_gens_under50 = sum(gens_dep_under50)) %>%    
    ungroup() %>%
    left_join(df_res2, by='resist_gene')
  
  # if migration is set to 0, or coverage==1 don't show refuge in plots
  plot_refuge <- ifelse(migration==0 | coverage==1,FALSE,TRUE)
  
  # do the plots
  if (plot) rot_plot_resistance(df_res2, plot_refuge=plot_refuge, 
                                logy=logy, add_gens_under50=add_gens_under50,
                                rot_criterion=rot_criterion)

  
  invisible(df_res2)
  
} # end of run_rot()



