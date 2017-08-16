#to simulate the effect of rotations on the spread of resistance
#Ian Hastings and Andy South

#can run any number of insecticides/loci 
#but at present, input will only allow a maximumum of 5


# <<<<<<<<<<<<<<<first up are user-defined parameters>>>>>>>>>>>
#I will eventually block this out and use a function to generate values from distributions for sensitivity analysis


max_no_generations=500 #the maximum number of mosquito generations to run the simulation
no_insecticides=4 #MAX is 5<<<<the number of insecticides (and hence loci) in the simuation MAX IS 5<<<

rotation_interval=0 #frequency of rotation (in generations) NB if set to zero mean RwR i.e. rotate when resistant
rotation_criterion=0.5 #resistant allele frequency that triggers a RwR change or precludes a insecticide from being rotated in.

migration_rate_intervention=0.01 # migration rate into and out-of the treated area. It is the proportion of the treated population that migrates. We assume that immigration=emigration.
coverage=0.8; # "coverage" of the intervention is defined as the proportion of mosquitoes that are covered by the intervention (and 1-C is the proportion of the population in the untreated refugia).
migration_rate_refugia=migration_rate_intervention*coverage/(1-coverage)

diagnostics=0

#now set up some arrays to hold data. 

array_named <- function(...)  {  
array(0, dim = lengths(list(...)), dimnames = list(...))  
 }  

#RAF stands for resistance allele frequency

RAF <- array_named(insecticide=1:no_insecticides, sex=c('male','female'), site=c('intervention','refugia'), gen=1:max_no_generations)
exposure <- array_named(insecticide=1:no_insecticides, sex=c('male','female'), amount=c('none','low', 'high'))
fitness <- array_named(insecticide=1:no_insecticides, genotype=c('SS','SR', 'RR'), amount=c('none','low', 'high'))
results<-array(0, dim=c(max_no_generations, 12))

#andy try to store results in data frame might make easier
df_results <- data.frame(generation=1:max_no_generations,
                    insecticide=NA,
                    r1_active=NA,
                    r1_refuge=NA,
                    r2_active=NA,
                    r2_refuge=NA,
                    r3_active=NA,
                    r3_refuge=NA,
                    r4_active=NA,
                    r4_refuge=NA,
                    r5_active=NA,
                    r5_refuge=NA, stringsAsFactors = FALSE)
#experimental
# df_res_active <- data.frame(generation=rep(1:max_no_generations,no_insecticides),
#                       insecticide=NA,
#                       region=NA,
#                       resistance=NA, stringsAsFactors = FALSE)
# df_res_refuge <- df_res_active


#inital resistance allele frequency (i.e. generation 1) in the intervention and refugia
#locus 1>>
RAF[1, 'male', 'intervention',1]=0.002;  RAF[1, 'female', 'intervention',1]=0.002;
RAF[1, 'male', 'refugia',1]=0.001;       RAF[1, 'female', 'refugia',1]=0.001
#locus 2>>
if(no_insecticides>=2){ #need to avoid exceeding size of the array
  RAF[2, 'male', 'intervention',1]=0.001;   RAF[2, 'female', 'intervention',1]=0.001;
  RAF[2, 'male', 'refugia',1]=0.001;        RAF[2, 'female', 'refugia',1]=0.001
}
#locus 3>>
if(no_insecticides>=3){
  RAF[3, 'male', 'intervention',1]=0.09;  RAF[3, 'female', 'intervention',1]=0.09;
  RAF[3, 'male', 'refugia',1]=0.001;       RAF[3, 'female', 'refugia',1]=0.001
}
#locus 4>>
if(no_insecticides>=4){
  RAF[4, 'male', 'intervention',1]=0.09;  RAF[4, 'female', 'intervention',1]=0.09;
  RAF[4, 'male', 'refugia',1]=0.001;       RAF[4, 'female', 'refugia',1]=0.001
}#locus 5>>
if(no_insecticides>=5){
  RAF[5, 'male', 'intervention',1]=0.09; RAF[5, 'female', 'intervention',1]=0.09;
  RAF[5, 'male', 'refugia',1]=0.001;      RAF[5, 'female', 'refugia',1]=0.001
}

#exposure patterns for insecticide 1
exposure[1, 'male', 'low'] =0.1; exposure[1, 'male', 'high'] =0.5;
exposure[1, 'male', 'none'] =1-exposure[1, 'male', 'low']-exposure[1, 'male', 'high']; 
exposure[1, 'female', 'low'] =0.1; exposure[1, 'female', 'high'] =0.6;
exposure[1, 'female', 'none'] =1-exposure[1, 'female', 'low']-exposure[1, 'female', 'high']; 
#exposure patterns for insecticide 2
if(no_insecticides>=2){ #need to avoid exceeding size of the array
  exposure[2, 'male', 'low'] =0.1; exposure[2, 'male', 'high'] =0.1;
  exposure[2, 'male', 'none'] =1-exposure[2, 'male', 'low']-exposure[2, 'male', 'high']; 
  exposure[2, 'female', 'low'] =0.1; exposure[2, 'female', 'high'] =0.1;
  exposure[2, 'female', 'none'] =1-exposure[2, 'female', 'low']-exposure[2, 'female', 'high']; 
}
#exposure patterns for insecticide 3
if(no_insecticides>=3){
  exposure[3, 'male', 'low'] =0.1; exposure[3, 'male', 'high'] =0.1;
  exposure[3, 'male', 'none'] =1-exposure[3, 'male', 'low']-exposure[3, 'male', 'high']; 
  exposure[3, 'female', 'low'] =0.1; exposure[3, 'female', 'high'] =0.1;
  exposure[3, 'female', 'none'] =1-exposure[3, 'female', 'low']-exposure[3, 'female', 'high'];
}
#exposure patterns for insecticide 4
if(no_insecticides>=4){
  exposure[4, 'male', 'low'] =0.1; exposure[4, 'male', 'high'] =0.1;
  exposure[4, 'male', 'none'] =1-exposure[4, 'male', 'low']-exposure[4, 'male', 'high']; 
  exposure[4, 'female', 'low'] =0.1; exposure[4, 'female', 'high'] =0.1;
  exposure[4, 'female', 'none'] =1-exposure[4, 'female', 'low']-exposure[4, 'female', 'high'];
}
#exposure patterns for insecticide 5
if(no_insecticides>=5){
  exposure[5, 'male', 'low'] =0.1; exposure[5, 'male', 'high'] =0.1;
  exposure[5, 'male', 'none'] =1-exposure[5, 'male', 'low']-exposure[5, 'male', 'high']; 
  exposure[5, 'female', 'low'] =0.1; exposure[5, 'female', 'high'] =0.1;
  exposure[5, 'female', 'none'] =1-exposure[5, 'female', 'low']-exposure[5, 'female', 'high'];
}



#genetic data for locus 1
fitness[1, 'SS', 'none']=0.1; fitness[1, 'SS', 'low']=0.3; fitness[1, 'SS', 'high']=0.3;
fitness[1, 'SR', 'none']=0.1; fitness[1, 'SR', 'low']=0.7; fitness[1, 'SR', 'high']=0.7;
fitness[1, 'RR', 'none']=0.1; fitness[1, 'RR', 'low']=0.9; fitness[1, 'RR', 'high']=0.9;
#genetic data for locus 2
if(no_insecticides>=2){
fitness[2, 'SS', 'none']=0.3; fitness[2, 'SS', 'low']=0.3; fitness[2, 'SS', 'high']=0.3;
fitness[2, 'SR', 'none']=0.3; fitness[2, 'SR', 'low']=0.8; fitness[2, 'SR', 'high']=0.8;
fitness[2, 'RR', 'none']=0.3; fitness[2, 'RR', 'low']=0.9; fitness[2, 'RR', 'high']=0.9;
}
#genetic data for locus 3
if(no_insecticides>=3){
fitness[3, 'SS', 'none']=0.3; fitness[3, 'SS', 'low']=0.3; fitness[3, 'SS', 'high']=0.3;
fitness[3, 'SR', 'none']=0.3; fitness[3, 'SR', 'low']=0.3; fitness[3, 'SR', 'high']=0.3;
fitness[3, 'RR', 'none']=0.3; fitness[3, 'RR', 'low']=0.3; fitness[3, 'RR', 'high']=0.3;
}
#genetic data for locus 4
if(no_insecticides>=4){
fitness[4, 'SS', 'none']=0.3; fitness[4, 'SS', 'low']=0.3; fitness[4, 'SS', 'high']=0.3;
fitness[4, 'SR', 'none']=0.3; fitness[4, 'SR', 'low']=0.4; fitness[4, 'SR', 'high']=0.4;
fitness[4, 'RR', 'none']=0.3; fitness[4, 'RR', 'low']=0.5; fitness[4, 'RR', 'high']=0.5;
}#genetic data for locus 5
if(no_insecticides>=5){
fitness[5, 'SS', 'none']=0.3; fitness[5, 'SS', 'low']=0.3; fitness[5, 'SS', 'high']=0.3;
fitness[5, 'SR', 'none']=0.3; fitness[5, 'SR', 'low']=0.3; fitness[5, 'SR', 'high']=0.3;
fitness[5, 'RR', 'none']=0.3; fitness[5, 'RR', 'low']=0.3; fitness[5, 'RR', 'high']=0.3;
}

# <<<<<<<<<<<<< end of user-defined variable >>>>>>>>>>>>>>>>>>>>>>>>

################### now to check input value are sensible, lie within their limits, etc #######################################

#>>>Now check that exposure(none) is not less than zero
for(temp_int in 1:no_insecticides){
  if (exposure[temp_int, 'male', 'none']<0) message(sprintf("warning from calibration: male exposure to no insecticide %d is <0\n", temp_int)) 
  if (exposure[temp_int, 'female', 'none']<0) message(sprintf("warning from calibration: female exposure to no insecticide %d is <0\n", temp_int)) 
}

if(migration_rate_intervention>(1-coverage)){
message(sprintf("warning from calibration: migration rate in/out of intervenation exceed 1 minus coverage\n"))   
}



#>>>>>>>> now to run the simulations <<<<<<<<<<<<<<<<<<<<<<<<<<<

current_insecticide=1 #usually start the rotation sequence at #1 but can specify any one start
next_insecticide_found=1
change_insecticide=0;
rotation_count=1

results[1,1]=1; results[1,2]=current_insecticide;

for(gen in 2:max_no_generations)
{ #start at generation 2 because generation 1 holds the user-defined initial allele frequenciess
  

for(insecticide in 1:no_insecticides){

#first the intervention site  
 if(insecticide==current_insecticide){ #i.e. insecticide selection taking place
   
   coeff_1=
     (RAF[insecticide, 'male', 'intervention',gen-1]*(1-RAF[insecticide, 'female', 'intervention',gen-1])+
     (1-RAF[insecticide, 'male','intervention',gen-1])*RAF[insecticide, 'female', 'intervention',gen-1])*0.5

   coeff_2=
     exposure[insecticide, 'male', 'none']*fitness[insecticide, 'SR', 'none']+
     exposure[insecticide, 'male', 'low']*fitness[insecticide, 'SR', 'low']+
     exposure[insecticide, 'male', 'high']*fitness[insecticide, 'SR', 'high']
    
   coeff_3=
     exposure[insecticide, 'female', 'none']*fitness[insecticide, 'SR', 'none']+
     exposure[insecticide, 'female', 'low']*fitness[insecticide, 'SR', 'low']+
     exposure[insecticide, 'female', 'high']*fitness[insecticide, 'SR', 'high']
  
#Eqn 4: first the male resistant alleles>>
  temp_coeff=
    exposure[insecticide, 'male', 'none']*fitness[insecticide, 'RR', 'none']+
    exposure[insecticide, 'male', 'low']*fitness[insecticide, 'RR', 'low']+
    exposure[insecticide, 'male', 'high']*fitness[insecticide, 'RR', 'high']
      
  F_male_r_intervention=
    RAF[insecticide, 'male', 'intervention', gen-1]*RAF[insecticide, 'female', 'intervention', gen-1]*temp_coeff+
    coeff_1*coeff_2
  
  #Eqn 5: now the male sensitive alleles>> 
  temp_coeff=
    exposure[insecticide, 'male', 'none']*fitness[insecticide, 'SS', 'none']+
    exposure[insecticide, 'male', 'low']*fitness[insecticide, 'SS', 'low']+
    exposure[insecticide, 'male', 'high']*fitness[insecticide, 'SS', 'high']
   
F_male_s_intervention=
(1-RAF[insecticide, 'male', 'intervention',gen-1])*(1-RAF[insecticide, 'female', 'intervention', gen-1])*temp_coeff+
coeff_1*coeff_2

#now normalise the male gamete frequencies and store the results
norm_coeff=F_male_r_intervention+F_male_s_intervention
RAF[insecticide, 'male', 'intervention', gen]=F_male_r_intervention/norm_coeff

    
  #now the female resistant alleles>>
  temp_coeff=
    exposure[insecticide, 'female', 'none']*fitness[insecticide, 'RR', 'none']+
    exposure[insecticide, 'female', 'low']*fitness[insecticide, 'RR', 'low']+
    exposure[insecticide, 'female', 'high']*fitness[insecticide, 'RR', 'high']
 
  F_female_r_intervention=
  RAF[insecticide, 'male', 'intervention', gen-1]*RAF[insecticide, 'female', 'intervention',gen-1]*temp_coeff+
    coeff_1*coeff_3
  
  #now the female sensitive alleles>> 
  temp_coeff=
    exposure[insecticide, 'female', 'none']*fitness[insecticide, 'SS', 'none']+
    exposure[insecticide, 'female', 'low']*fitness[insecticide, 'SS', 'low']+
    exposure[insecticide, 'female', 'high']*fitness[insecticide, 'SS', 'high']
      
 F_female_s_intervention=
(1-RAF[insecticide, 'male', 'intervention', gen-1])*(1-RAF[insecticide, 'female', 'intervention',gen-1])*temp_coeff+
coeff_1*coeff_3 

 
  #now normalise the female gamete frequencies and store the results
  norm_coeff=F_female_r_intervention+F_female_s_intervention
  RAF[insecticide, 'female', 'intervention', gen]=F_female_r_intervention/norm_coeff
  
  if(diagnostics==1) message(sprintf("generation %d: just completed insecticide selection for locus/insecticide %d\n", gen, insecticide))
  
  
} #end of loop that deals with this insecticide if it is being deployed
  
 else{ #i.e no selection for this insecticide in the intervention site
   #first the coefficient for heterozygotes common to equations 2 and 3
   temp_coeff=
     (RAF[insecticide, 'male', 'intervention',gen-1]*(1-RAF[insecticide, 'female', 'intervention',gen-1])+
     RAF[insecticide, 'female', 'intervention',gen-1]*(1-RAF[insecticide, 'male', 'intervention',gen-1]))*
     0.5*fitness[insecticide, 'SR', 'none']
                
   
#now the resistant and sensitive frequencies in untreated areas  
   F_male_r_intervention=RAF[insecticide, 'male', 'intervention',gen-1]*RAF[insecticide, 'female', 'intervention', gen-1]*fitness[insecticide, 'RR', 'none']+temp_coeff
   F_male_s_intervention=(1-RAF[insecticide, 'male', 'intervention',gen-1])*(1-RAF[insecticide, 'female', 'intervention',gen-1])*fitness[insecticide, 'SS', 'none']+temp_coeff
   #now to normalise them
   norm_coeff= F_male_r_intervention+F_male_s_intervention
   RAF[insecticide, 'male', 'intervention', gen]=F_male_r_intervention/norm_coeff
  

#same allele frequencies in both sexes if no differential exposure so 
   RAF[insecticide, 'female', 'intervention', gen]=RAF[insecticide, 'male', 'intervention', gen]
  if(diagnostics==1) message(sprintf("generation %d: just completed natural selection against locus %d in intervention site\n", gen, insecticide))   
      
 
   } #end of code for insecticides that are not being deployed  in the intervention site
    
#now for the refugia
  #first the coefficient for heterozygotes common to equations 2 and 3
  temp_coeff=(RAF[insecticide, 'male', 'refugia',gen-1]*(1-RAF[insecticide, 'female', 'refugia',gen-1])+
                RAF[insecticide, 'female', 'refugia',gen-1]*(1-RAF[insecticide, 'male','refugia',gen-1]))*
    0.5*fitness[insecticide, 'SR', 'none']
  
 
   #now the resistant and sensitive frequencies   
  F_male_r_refugia=RAF[insecticide, 'male', 'refugia',gen-1]*RAF[insecticide, 'female', 'refugia',gen-1]*
    fitness[insecticide, 'RR', 'none']+temp_coeff
  F_male_s_refugia=(1-RAF[insecticide, 'male', 'refugia',gen-1])*(1-RAF[insecticide, 'female', 'refugia', gen-1])*
    fitness[insecticide, 'SS', 'none']+temp_coeff 
  
  #now to normalise them and store the results
  norm_coeff= F_male_r_refugia+F_male_s_refugia
  RAF[insecticide, 'male', 'refugia', gen]=F_male_r_refugia/norm_coeff
  #same allele frequencies in both sexes if no differential selection so
  RAF[insecticide, 'female', 'refugia', gen]=RAF[insecticide, 'male', 'refugia', gen]
  
  if(diagnostics==1) message(sprintf("generation %d: just completed natural selection against locus %d in refugia\n", gen, insecticide))
  
}   #end of cycling insecticides

  
#now for migration between refugia and intervention site

for(temp_int in 1:no_insecticides){ 
    
 fem_intervention=
   (1-migration_rate_intervention)*RAF[temp_int, 'female', 'intervention', gen]+
   migration_rate_intervention*RAF[temp_int, 'female', 'refugia', gen]
  
 male_intervention=
   (1-migration_rate_intervention)*RAF[temp_int, 'male', 'intervention', gen]+
   migration_rate_intervention*RAF[temp_int, 'male', 'refugia', gen]
 
 fem_refugia=
   (1-migration_rate_refugia)*RAF[temp_int, 'female', 'refugia', gen]+
   migration_rate_refugia*RAF[temp_int, 'female', 'intervention', gen]
 
 male_refugia=
   (1-migration_rate_refugia)*RAF[temp_int, 'male', 'refugia', gen]+
   migration_rate_refugia*RAF[temp_int, 'male', 'intervention', gen]
 
RAF[temp_int, 'female', 'intervention', gen]=fem_intervention
RAF[temp_int, 'male', 'intervention', gen]=male_intervention 
RAF[temp_int, 'female', 'refugia', gen]=fem_refugia
RAF[temp_int, 'male', 'refugia', gen]=male_refugia

 } #end of cycling migration for each insecticide
  
  
#now to find if we need to switch current insecticide
  if(rotation_interval==0){  #i.e.  its a RwR policy:
        if(RAF[current_insecticide, 'female','intervention', gen] > rotation_criterion) change_insecticide=1
        #message(sprintf("confirm. RAF=%f, change_insecticide=%d \n", RAF[current_insecticide, 'female','intervention', gen], change_insecticide))
        }  
  
if(rotation_interval!=0){ #i.e. if freq_rotation>0 then its a policy of routine, periodic rotation
#NOTE. At the moment if all but one of the insecticides are over the thereshold frequency, the one under the threshold
#will be repeatedly deployed. For example, if there are 3 insecticides and rotation is every 10 generations. 
#if #3 is being deployed and due to rotate out at generetion 100: if only #3 is under the threshold
#it will not rotate out, but will continue to be used util the next scheduled rotation at which point
  #(a) it will continue to be use if it is the only one below the threshold 
  #(b) it will be replaced if one of the other insecticides is now below the threshold due to fitness effects or migration
  #(c) the simulation will terminate if all insecticides exceed the threshold
    
if (rotation_count!=rotation_interval) rotation_count=rotation_count+1
else change_insecticide=1 #i.e. its time to rotate so need to identify the next insecticide in the rotation
}
  
if(change_insecticide==1){
rotation_count=1; next_insecticide_found=0; candidate=current_insecticide 
            
for(temp_int in 1:no_insecticides){
  if(candidate==no_insecticides) candidate=1 else candidate=candidate+1 
  if(RAF[candidate, 'female','intervention', gen]<rotation_criterion){
    message(sprintf("generation %d, switching from insecticide %d to insecticide %d; RAF are %f and %f respectively\n",
                    gen, current_insecticide, candidate,
                    RAF[current_insecticide, 'female','intervention', gen], RAF[candidate, 'female','intervention', gen]))
    next_insecticide_found=1; current_insecticide=candidate; change_insecticide=0
      }
if(next_insecticide_found==1) break
} #end of temp_int loop
} #end of loop to try and change insecticide i.e. the "if(change_insecticide==1)" lopp

#now to store some results for ease of plotting (see later) before potentially terminating the simulation
results[gen,1]=gen; 
results[gen,2]=current_insecticide

df_results$insecticide[gen] <- current_insecticide 

if(next_insecticide_found==0){
message(sprintf("simulation terminating at generation %d because all RAFs above threshold of %f\n", gen,  rotation_criterion))
  for(temp_int in 1:no_insecticides){
    message(sprintf("frequency of resistance in females to insecticide %d is %f\n", temp_int, RAF[temp_int, 'female','intervention', gen]))  
  }
break #breaks out of looping generations and terminates the simulation
}    
  
 } #end of cycle running the gens up to max_no_generations
  
#****************************************************************  
 # NOW collate the data and draw plots

#andy trying to replace below with data frame  
#also can probably do on whole row so not require the loop
for(i_num in 1:no_insecticides)
{
  df_results[[paste0('r',i_num,'_active')]] <- 0.5*(RAF[i_num, 'male','intervention', ]+
                                                    RAF[i_num, 'female','intervention', ])
  df_results[[paste0('r',i_num,'_refuge')]] <- 0.5*(RAF[i_num, 'male','refugia', ]+
                                                    RAF[i_num, 'female','refugia', ]) 
  
  #df_res_active$region[[(i_num-1)*max_no_generations:(i_num)*max_no_generations]] <- paste0("insecticide",i_num)
  #df_res_active$resistance[[(i_num-1)*max_no_generations:(i_num)*max_no_generations]] <-  0.5*(RAF[i_num, 'male','intervention', ]+
  #                                                                                             RAF[i_num, 'female','intervention', ])
}
#but if I want to facet by intervention may want to structure differently
#generation, treatment, resistance
df_res2 <- df_results %>%
  gather('r1_refuge', 'r1_active',
         'r2_refuge', 'r2_active',
         'r3_refuge', 'r3_active',
         'r4_refuge', 'r4_active',
         #'r5_refuge', 'r5_active', 
         key=region, value=resistance)

for(temp_int in 1:max_no_generations){

results[temp_int,3]=0.5*(RAF[1, 'male','intervention', temp_int]+RAF[1, 'female','intervention', temp_int]) #locus 1
results[temp_int,4]=0.5*(RAF[1, 'male','refugia', temp_int]+RAF[1, 'female','refugia', temp_int]) #locus 1
if(no_insecticides>=2){
results[temp_int,5]=0.5*(RAF[2, 'male','intervention', temp_int]+RAF[2, 'female','intervention', temp_int]) #locus 2
results[temp_int,6]=0.5*(RAF[2, 'male','refugia', temp_int]+RAF[2,'female','refugia', temp_int]) #locus 2
}
if(no_insecticides>=3){
results[temp_int,7]=0.5*(RAF[3, 'male','intervention', temp_int]+RAF[3, 'female','intervention', temp_int]) #locus 3
results[temp_int,8]=0.5*(RAF[3, 'male','refugia', temp_int]+RAF[3, 'female','refugia', temp_int]) #locus 3
}
if(no_insecticides>=4){
results[temp_int,9]=0.5*(RAF[4, 'male','intervention', temp_int]+RAF[4, 'female','intervention', temp_int]) #locus 4
results[temp_int,10]=0.5*(RAF[4, 'male','refugia', temp_int]+RAF[4,'female','refugia', temp_int]) #locus 4
}
if(no_insecticides>=5){
results[temp_int,11]=0.5*(RAF[5, 'male','intervention', temp_int]+RAF[5, 'female','intervention', temp_int]) #locus 5
results[temp_int,12]=0.5*(RAF[5, 'male','refugia', temp_int]+RAF[5, 'female','refugia', temp_int]) #locus 5
}



} #end of temp_int loop


# xdata<-results[,1]
# ydata<-results[,2]
# plot(xdata, ydata) 
#plot(results[1], results[2])

library(ggplot2)

#ggplot( df_results, aes_string(x='generation',y='r1_active') ) + 
#  geom_point(shape=1, colour='red') #+


#facet by insecticide and active/refuge
#how can I add insecticide used to this plot ?
rot_plot_resistance <- function(df_res2) {
  ggplot( df_res2, aes_string(x='generation',y='resistance') ) + 
    #geom_point(shape=1, colour='red') +
    geom_line( colour='blue') +  
    facet_wrap('region', ncol=2) +
    #theme(axis.text.x = element_blank()) +
    theme_bw()
} 


#insecticide use (currently restricted to 4)
rot_plot_use <- function(df_res2) {
  ggplot( df_res2, aes_string(x='generation',y='insecticide') ) + 
    geom_point(shape=1, colour='red') +
    #geom_line( colour='blue') +  
    #facet_wrap('region', ncol=2) +
    ylim(1,4) +
    theme_bw() 
    #theme(axis.text.x = element_blank()) 
}

# do the plots
rot_plot_use(df_res2)
rot_plot_resistance(df_res2)
