#' run_rot to run simulation of the effect of rotations on the spread of resistance
#'
#' can run any number of insecticides/loci 
#' but at present, input will only allow a maximumum of 5
#' 
#' @param max_generations maximum number of mosquito generations to run the simulation
#' @param n_insecticides number of insecticides (and hence loci), current max is 5
#' @param start_freqs starting frequencies of resistance either one per insecticide or same for all
#' @param rotation_interval frequency of rotation (in generations) NB if set to zero mean RwR i.e. rotate when resistant
#' @param rotation_criterion resistant allele frequency that triggers a RwR change or precludes a insecticide from being rotated in.
#' @param migration migration rate between treated & untreated areas 0-1. We assume that immigration=emigration.
#' @param coverage proportion of mosquitoes that are covered by the intervention (and 1-C is the proportion of the population in the untreated refugia).
#' @param plot whether to plot results
#' @param start_insecticide which insecticide to start with
#' @param diagnostics whether to output running info
#' @param hardcode_fitness whether to use hardcoded fitness, default FALSE
#' @param same_insecticides only used with hardcode_fitness, whether to just set fitnesses for all insecticides the same
#' @param hardcode_exposure whether to use hardcoded exposure, default FALSE
#' @param expo_hi exposure to insecticide in hi niche, either single or vector of 1 per insecticide
#' @param expo_lo exposure to insecticide in lo niche, either single or vector of 1 per insecticide
#' @param male_expo_prop proportion tht males are exposed relative to f, default 1, likely to be <1 (could possibly be a vector per insecticide)
#' @param eff effectiveness, for all insecticides or individually
#' @param dom_sel dominance of selection, for all insecticides or individually
#' @param dom_cos dominance of cost, for all insecticides or individually
#' @param rr resistance restoration, for all insecticides or individually 
#' @param cost fitness cost of RR in no insecticide, for all insecticides or individually
#' @param fitSS fitness of SS if no insecticide, for all insecticides or individually
#' @param logy whether to use log scale for y axis
#' @param add_gens_under50 whether to add a label of num generations under 50 pcent resistance
#' 
#' @examples 
#' run_rot(rotation_interval=100)
#' dfr <- run_rot(rotation_interval=50, max_generations = 300)
#' dfr <- run_rot(rotation_interval=0, max_generations = 300)
#' dfr <- run_rot(rotation_interval=0, max_generations = 300, hardcode_fitness = TRUE, 
#'                same_insecticides =TRUE, migration=0.01)
#' 
#' @import tidyverse 
#' @return dataframe of results
#' @export


run_rot <- function( max_generations = 200, #the maximum number of mosquito generations to run the simulation
                     n_insecticides = 4, #MAX is 5<<<<the number of insecticides (and hence loci) in the simuation MAX IS 5<<<
                      start_freqs = 0.001,
                      rotation_interval = 10, #frequency of rotation (in generations) NB if set to zero mean RwR i.e. rotate when resistant
                      rotation_criterion = 0.5, #resistant allele frequency that triggers a RwR change or precludes a insecticide from being rotated in.
                      migration = 0.01, 
                      #migrate_intervention = 0.01, # migration rate into and out-of the treated area. It is the proportion of the treated population that migrates. We assume that immigration=emigration.
                      coverage = 0.8, # "coverage" of the intervention is defined as the proportion of mosquitoes that are covered by the intervention (and 1-C is the proportion of the population in the untreated refugia).
                      plot = TRUE,
                      start_insecticide = 1,
                      diagnostics = FALSE,
                      hardcode_fitness = FALSE,
                      same_insecticides = TRUE,
                      hardcode_exposure = FALSE,
                      expo_hi = 0.8,
                      expo_lo = 0,                              
                      male_expo_prop = 1,
                     eff = 0.8, #c(0.5, 0.7, 0.9),
                     dom_sel = 0.5, #c(0.5, 0.5, 0.5),
                     dom_cos = 0.5, #c(0.5, 0.5, 0.5),
                     rr = 0.5, #c(0.5, 0.5, 0.5),
                     cost = 0.1, #c(0,0,0),
                     fitSS = 1,
                     logy = FALSE,
                     add_gens_under50 = TRUE) 
  {
  
    
  exposure <- array_named(insecticide=1:n_insecticides, sex=c('m','f'), amount=c('no','lo', 'hi'))
  
  # df_results <- data.frame(generation=1:max_generations,
  #                     insecticide=NA,
  #                     r1_active=NA,
  #                     r1_refuge=NA,
  #                     r2_active=NA,
  #                     r2_refuge=NA,
  #                     r3_active=NA,
  #                     r3_refuge=NA,
  #                     r4_active=NA,
  #                     r4_refuge=NA, stringsAsFactors = FALSE)
  
  # setup dataframe to store results, tricky to cope with variable number insecticides
  l_gene_plus_activity <- rep(list(rep(NA,max_generations)), n_insecticides*2) #2 because active & refuge 
  
  names(l_gene_plus_activity) <- c( paste0('insecticide', 1:n_insecticides, "_active"),
                                    paste0('insecticide', 1:n_insecticides, "_refuge") )
  
  df_results <- do.call(data.frame, list(generation = 1:max_generations,
                                         insecticide = NA,
                                         stringsAsFactors = FALSE,
                                         l_gene_plus_activity))  
  
  
  #experimental
  # df_res_active <- data.frame(generation=rep(1:max_generations,n_insecticides),
  #                       insecticide=NA,
  #                       region=NA,
  #                       resistance=NA, stringsAsFactors = FALSE)
  # df_res_refuge <- df_res_active
  
  
  ### set starting allele frequencies from hardcoded test function or based on other inputs
  #RAF <- set_start_freqs_test( n_insecticides=n_insecticides, max_generations=max_generations )  
  #todo add checks thats start_freqs is either length 1 or n_insecticides
  RAF <- set_start_freqs( n_insecticides=n_insecticides, 
                          max_generations=max_generations, 
                          freqs = start_freqs )  
    
  ### set exposures from hardcoded test function or based on other inputs
  #exposure <- set_exposure_rot_test( n_insecticides=n_insecticides )
  exposure <- set_exposure_rot( n_insecticides=n_insecticides,
                                expo_hi = expo_hi,
                                expo_lo = expo_lo,                              
                                male_expo_prop = male_expo_prop)
  
  ### set fitnesses from hardcoded test function or based on other inputs
  if (hardcode_fitness)
  {
    fitness <- fitness_single_locus_test( n_insecticides=n_insecticides, same_insecticides = same_insecticides )
  } else
  {
    fitness <- fitness_single_locus(n_insecticides=n_insecticides, 
                                    eff=eff, 
                                    dom_sel=dom_sel, 
                                    dom_cos=dom_cos, 
                                    rr=rr, 
                                    cost=cost, 
                                    fitSS=fitSS)
    
  }
  
  # check that exposure(none) is not less than zero
  for(temp_int in 1:n_insecticides){
    if (exposure[temp_int, 'm', 'no']<0) message(sprintf("warning from calibration: m exposure to no insecticide %d is <0\n", temp_int)) 
    if (exposure[temp_int, 'f', 'no']<0) message(sprintf("warning from calibration: f exposure to no insecticide %d is <0\n", temp_int)) 
  }
  
  if(migrate_intervention>(1-coverage)){
  message(sprintf("warning from calibration: migration rate in/out of intervenation exceed 1 minus coverage\n"))   
  }
  
  
  current_insecticide=start_insecticide #usually start the rotation sequence at #1 but can specify any one start
  next_insecticide_found=1
  change_insecticide=0;
  rotation_count=1
  
  
  #start at generation 2 because generation 1 holds the user-defined initial allele frequencies
  for(gen in 2:max_generations)
  { 
    for(insecticide in 1:n_insecticides)
    {
      #first the intervention site, insecticide selection taking place  
      if(insecticide==current_insecticide){
       
        #a function to calc these wouldn't save much code and might make less transparent 
        
        coeff_1 <-
         (  RAF[insecticide, 'm', 'intervention',gen-1]*(1-RAF[insecticide, 'f', 'intervention',gen-1])+
         (1-RAF[insecticide, 'm', 'intervention',gen-1]) * RAF[insecticide, 'f', 'intervention',gen-1])*0.5
       
        coeff_2 <- sum( exposure[insecticide, 'm', ]*fitness[insecticide, 'RS', ] )
        coeff_3 <- sum( exposure[insecticide, 'f', ]*fitness[insecticide, 'RS', ] )
         
        #andy todo, almost identical code is repeated below for m & f
        #only difference is that coeff_2 used for m & 3 for f
        #i could create a mf loop to reduce code by half
        
        # male RR (Eqn 4)
        temp_coeff <- sum( exposure[insecticide, 'm', ]*fitness[insecticide, 'RR', ] )
          
        F_male_r_intervention <- 
          RAF[insecticide, 'm', 'intervention', gen-1]*
          RAF[insecticide, 'f', 'intervention', gen-1]* temp_coeff+
          coeff_1*coeff_2
      
        # male SS (Eqn 5)
        temp_coeff <- sum( exposure[insecticide, 'm', ]*fitness[insecticide, 'SS', ] )
       
        F_male_s_intervention=
          (1-RAF[insecticide, 'm', 'intervention', gen-1])*
          (1-RAF[insecticide, 'f', 'intervention', gen-1])*temp_coeff+
          coeff_1*coeff_2
    
        #normalise the male gamete frequencies and store the results
        norm_coeff <- F_male_r_intervention + F_male_s_intervention
        RAF[insecticide, 'm', 'intervention', gen] <- F_male_r_intervention/norm_coeff
    
        #female RR
        temp_coeff <- sum( exposure[insecticide, 'f', ]*fitness[insecticide, 'RR', ] )    
     
        F_female_r_intervention <-
        RAF[insecticide, 'm', 'intervention', gen-1]*RAF[insecticide, 'f', 'intervention',gen-1]*temp_coeff+
          coeff_1*coeff_3
        
        #female SS 
        temp_coeff <- sum( exposure[insecticide, 'f', ]*fitness[insecticide, 'SS', ] )  
        
        F_female_s_intervention <-
          (1-RAF[insecticide, 'm', 'intervention', gen-1])*
          (1-RAF[insecticide, 'f', 'intervention', gen-1])*temp_coeff+
          coeff_1*coeff_3 
      
        #normalise female gamete frequencies and store the results
        norm_coeff <- F_female_r_intervention + F_female_s_intervention
        RAF[insecticide, 'f', 'intervention', gen] <- F_female_r_intervention/norm_coeff
        
        if(diagnostics) message(sprintf("generation %d: completed insecticide selection for locus/insecticide %d\n", gen, insecticide))
        
        
      } #end of loop that deals with this insecticide if it is being deployed
      
     else{ #i.e no selection for this insecticide in the intervention site
       
       #coefficient for RS common to equations 2 and 3
       temp_coeff <- 
         (RAF[insecticide, 'm', 'intervention',gen-1]*(1-RAF[insecticide, 'f', 'intervention',gen-1])+
          RAF[insecticide, 'f', 'intervention',gen-1]*(1-RAF[insecticide, 'm', 'intervention',gen-1]))*
          0.5*fitness[insecticide, 'RS', 'no']
                    
       # male RR
       #now the resistant and sensitive frequencies in untreated areas  
       F_male_r_intervention <- RAF[insecticide, 'm', 'intervention', gen-1]*
                                RAF[insecticide, 'f', 'intervention', gen-1]*
                                fitness[insecticide, 'RR', 'no']+temp_coeff
       # male SS
       F_male_s_intervention <- (1-RAF[insecticide, 'm', 'intervention',gen-1])*
                                (1-RAF[insecticide, 'f', 'intervention',gen-1])*
                                fitness[insecticide, 'SS', 'no']+temp_coeff
       # normalise
       norm_coeff <- F_male_r_intervention + F_male_s_intervention
       RAF[insecticide, 'm', 'intervention', gen] <- F_male_r_intervention/norm_coeff
      
       #same allele frequencies in both sexes if no differential exposure so 
       RAF[insecticide, 'f', 'intervention', gen] <- RAF[insecticide, 'm', 'intervention', gen]
       
       if(diagnostics) message(sprintf("generation %d: completed selection against locus %d in intervention site\n", gen, insecticide))   
          
       } #end of code for insecticides that are not being deployed in the intervention site
        
      #now for the refugia
      
      # RS coefficient common to equations 2 and 3
      temp_coeff <- (RAF[insecticide, 'm', 'refugia',gen-1]*(1-RAF[insecticide, 'f', 'refugia',gen-1])+
                     RAF[insecticide, 'f', 'refugia',gen-1]*(1-RAF[insecticide, 'm', 'refugia',gen-1]))*
                     0.5*fitness[insecticide, 'RS', 'no']
      
       # male RR   
       F_male_r_refugia <- RAF[insecticide, 'm', 'refugia',gen-1]*
                           RAF[insecticide, 'f', 'refugia',gen-1]*
                           fitness[insecticide, 'RR', 'no']+temp_coeff
       # male SS
       F_male_s_refugia  <- (1-RAF[insecticide, 'm', 'refugia',gen-1])*
                            (1-RAF[insecticide, 'f', 'refugia',gen-1])*
                            fitness[insecticide, 'SS', 'no']+temp_coeff 
      
       #normalise and store results
       norm_coeff <- F_male_r_refugia + F_male_s_refugia
       RAF[insecticide, 'm', 'refugia', gen] <- F_male_r_refugia/norm_coeff
      
       #same allele frequencies in both sexes if no differential selection so
       RAF[insecticide, 'f', 'refugia', gen]=RAF[insecticide, 'm', 'refugia', gen]
      
       if(diagnostics) message(sprintf("generation %d: completed selection against locus %d in refugia\n", gen, insecticide))
      
     }   #end of cycling insecticides
    
    
    #################################################  
    # migration between refugia and intervention site




    
    
  #do we need to switch current insecticide ?
    if(rotation_interval==0){  #i.e.  its a RwR policy:
          if(RAF[current_insecticide, 'f','intervention', gen] > rotation_criterion) change_insecticide=1
          #message(sprintf("confirm. RAF=%f, change_insecticide=%d \n", RAF[current_insecticide, 'f','intervention', gen], change_insecticide))
          }  
    
  if(rotation_interval!=0){ #i.e. if freq_rotation>0 then its a policy of routine, periodic rotation
  #NOTE. At the moment if all but one of the insecticides are over the thereshold frequency, the one under the threshold
  #will be repeatedly deployed. For example, if there are 3 insecticides and rotation is every 10 generations. 
  #if #3 is being deployed and due to rotate out at generetion 100: if only #3 is under the threshold
  #it will not rotate out, but will continue to be used util the next scheduled rotation at which point
    #(a) it will continue to be use if it is the only one below the threshold 
    #(b) it will be replaced if one of the other insecticides is now below the threshold due to fitness effects or migration
    #(c) the simulation will terminate if all insecticides exceed the threshold
  
  # TODO this keeps the rotation going, if I want to add in responsive that stops the rotation if 
  # the resistance frequency is reached then I can add it in here with &
  if (rotation_count!=rotation_interval) rotation_count=rotation_count+1
  else change_insecticide=1 #i.e. its time to rotate so need to identify the next insecticide in the rotation
  }
    
  if(change_insecticide==1){
  rotation_count=1; next_insecticide_found=0; candidate=current_insecticide 
              
  for(temp_int in 1:n_insecticides){
    if(candidate==n_insecticides) candidate=1 else candidate=candidate+1 
    if(RAF[candidate, 'f','intervention', gen]<rotation_criterion){
      message(sprintf("generation %d, switch from insecticide %d to %d; frequencies = %f and %f\n",
                      gen, current_insecticide, candidate,
                      RAF[current_insecticide, 'f','intervention', gen], RAF[candidate, 'f','intervention', gen]))
      next_insecticide_found=1; current_insecticide=candidate; change_insecticide=0
        }
  if(next_insecticide_found==1) break
  } #end of temp_int loop
  } #end of loop to try and change insecticide i.e. the "if(change_insecticide==1)" lopp
  
  
  df_results$insecticide[gen] <- current_insecticide 
  
  if(next_insecticide_found==0){
  message(sprintf("simulation terminating at generation %d because all RAFs above threshold of %f\n", gen,  rotation_criterion))
    for(temp_int in 1:n_insecticides){
      message(sprintf("frequency of resistance in females to insecticide %d is %f\n", temp_int, RAF[temp_int, 'f','intervention', gen]))  
    }
  break #breaks out of looping generations and terminates the simulation
  }    
    
   } #end of max_generations loop
    
  #####################################  
  # recording results of resistance frequency
  
  #andy saving results in wide data frame  
  for(i_num in 1:n_insecticides)
  {
    # does calculation for all generations (final dimension in RAF array)
    df_results[[paste0('insecticide',i_num,'_active')]] <- 0.5*(RAF[i_num, 'm','intervention', ]+
                                                      RAF[i_num, 'f','intervention', ])
    df_results[[paste0('insecticide',i_num,'_refuge')]] <- 0.5*(RAF[i_num, 'm','refugia', ]+
                                                      RAF[i_num, 'f','refugia', ]) 
    
    #df_res_active$region[[(i_num-1)*max_generations:(i_num)*max_generations]] <- paste0("insecticide",i_num)
    #df_res_active$resistance[[(i_num-1)*max_generations:(i_num)*max_generations]] <-  0.5*(RAF[i_num, 'm','intervention', ]+                                                                                            RAF[i_num, 'f','intervention', ])
  }
  
  #but if I want to facet by intervention may want to structure differently
  #todo region is a bad name for r1_refuge etc.
  
  #generation, region, resistance
  # df_res2 <- df_results %>%
  #   gather('r1_refuge', 'r1_active',
  #          'r2_refuge', 'r2_active',
  #          'r3_refuge', 'r3_active',
  #          'r4_refuge', 'r4_active',
  #          key=region, value=resistance)
  
  df_res2 <- df_results %>%
    gather(names(l_gene_plus_activity),
           key=region, value=resistance)
  
  # use tidyr::separate() to get from r1_refuge to r1 & refuge in different columns.
  # to get active & refuge into the same subplot
  df_res2 <- separate(df_res2, region, into=c("resist_gene","active_or_refuge"))
  
  # calculate number generations under 50% resistance to be used in plotting 
  # probably should be somewhere else !
  # ? just for active area
  # this does give the answer, but only 1 per insecticide
  # so all the results per generation are lost
  df_res2 <- df_res2 %>%
    filter(active_or_refuge=='active') %>%
    group_by(resist_gene) %>%
    summarise(gens_under50 = sum(resistance < 0.5, na.rm=TRUE)) %>%
    ungroup() %>%
    left_join(df_res2, by='resist_gene')
  
  # if migration is set to 0 don't show refuge in plots
  plot_refuge <- ifelse(migration==0,FALSE,TRUE)
  
  # do the plots
  if (plot) rot_plot_resistance(df_res2, plot_refuge=plot_refuge, 
                                logy=logy, add_gens_under50=add_gens_under50)
  
  invisible(df_res2)
  
} # end of run_rot()



