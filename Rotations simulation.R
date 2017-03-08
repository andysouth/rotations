#this is to simulate the effect of rotations on teh spread of resistnce
#Written by Ian Hastings, but hopefully Andy South will erad it, give it a D-, and make teh code efficiant

#This can run any number of insecticides/loci but at prseentm, input will only allow a maximumum of 5


# <<<<<<<<<<<<<<<first up are user-defined parameters>>>>>>>>>>>
#I will eventually block this out and use a function to generate values from distributions for sensitivity analysis


max_no_gens=500 #the maximum number of mosquito generations to run the simulation
no_insectides=3 #the number of insecticides (and hence loci) in the simuation MAX IS 5<<<

#now set up some arrays to hold data. Can make some dimensions the number of insecticides but hard-code as 5 meanwhile
#because diretly write to these arrays below and need at least 5. 
RAF_male_intervention <- array(0, dim=c(5, max_no_generations)); #resistance allele freq in intervention site
RAF_female_intervention <- array(0, dim=c(5, max_no_generations));
RAF_male_refugia <- array(0, dim=c(5, max_no_generations)); #resistance allele freq in refugia
RAF_female_refugia <- array(0, dim=c(5, max_no_generations));
alpha_male_high <- array(0, dim=5); alpha_male_low <- array(0, dim=5); alpha_male_none <- array(0, dim=5);
alpha_female_high <- array(0, dim=5); alpha_female_low <- array(0, dim=5); alpha_female_none <- array(0, dim=5);
z_rr <- array(0, dim=5); h_rr <- array(0, 5);
w_ss_none <- array(0, dim=5);w_ss_low <- array(0, dim=5);w_ss_high <- array(0, dim=5);
w_rs_none <- array(0, dim=5);w_rs_low <- array(0, dim=5);w_rs_high <- array(0, dim=5);
w_rr_none <- array(0, dim=5);w_rr_low <- array(0, dim=5);w_rr_high <- array(0, dim=5);

#now resume entering data
migration_rate=0.01 # migration rate into and out-of the treated area. It is the proportion of the treated population that migrates. We assume that immigration=emigration.
prop_migrants_male=0.5 # the proportion of migrants that are male
coverage=0.8; # "coverage" of the intervention is defined as the proportion of mosquitoes that are covered by the intervention (and 1-C is the proportion of the population in the untreated refugia).

freq_rotation=10 #frequency of rotation (in generations) NB if set to zero mean RwR i.e. rotate when resistant
rotataion_criterion=0.5 #resistant allele frequency that triggers a RwR change.

#inital resistance allele frequency (i.e. generation 1) in the intervention and refugia
RAF_male_intervention[1,1]=0.001;RAF_female_intervention[1,1]=0.001; RAF_male_refugia[1,1]=0.001; RAF_female_refugia[1,1]=0.001 #locus 1
RAF_male_intervention[2,1]=0.001;RAF_female_intervention[2,1]=0.001; RAF_male_refugia[2,1]=0.001; RAF_female_refugia[2,1]=0.001
RAF_male_intervention[3,1]=0.001;RAF_female_intervention[3,1]=0.001; RAF_male_refugia[3,1]=0.001; RAF_female_refugia[3,1]=0.001
RAF_male_intervention[4,1]=0.001;RAF_female_intervention[4,1]=0.001; RAF_male_refugia[4,1]=0.001; RAF_female_refugia[4,1]=0.001
RAF_male_intervention[5,1]=0.001;RAF_female_intervention[5,1]=0.001; RAF_male_refugia[5,1]=0.001; RAF_female_refugia[5,1]=0.001

#exposure patterns for insecticide 1
alpha_male_high[1]=0.3; alpha_male_low[1]=0.7; alpha_male_none[1]=1-alpha_male_high-alpha_male_low
alpha_female_high[1]=0.3; alpha_female_low[1]=0.7; alpha_female_none[1]=1-alpha_female_high-alpha_female_low
#if(alpha_male_none[1]<0 or alpha_female_none[1]<0) print_error   #error because exposuer to low and high must exceed one

#other insectides: #we will initially assume these are the same as for insecticide 1; can add code later to over-ride this if required
alpha_male_high[2]=alpha_male_high[1]; alpha_male_low[2]=alpha_male_low[1]; alpha_male_none[2]=alpha_male_none[1];  
alpha_male_high[3]=alpha_male_high[1]; alpha_male_low[3]=alpha_male_low[1]; alpha_male_none[3]=alpha_male_none[1];
alpha_male_high[4]=alpha_male_high[1]; alpha_male_low[4]=alpha_male_low[1]; alpha_male_none[4]=alpha_male_none[1];
alpha_male_high[5]=alpha_male_high[1]; alpha_male_low[5]=alpha_male_low[1]; alpha_male_none[5]=alpha_male_none[1];
alpha_female_high[2]=alpha_female_high[1]; alpha_female_low[2]=alpha_female_low[1]; alpha_female_none[2]=alpha_female_none[1];
alpha_female_high[3]=alpha_female_high[1]; alpha_female_low[3]=alpha_female_low[1]; alpha_female_none[3]=alpha_female_none[1];
alpha_female_high[4]=alpha_female_high[1]; alpha_female_low[4]=alpha_female_low[1]; alpha_female_none[4]=alpha_female_none[1];
alpha_female_high[5]=alpha_female_high[1]; alpha_female_low[5]=alpha_female_low[1]; alpha_female_none[5]=alpha_female_none[1];

#genetic data for locus 1
z_rr[1]=0.9; #natural selection against the RR genotype
hz_rr[1]=0.9; # the dominance of natural selection against the RR genotype
w_ss_none[1]=1; w_rs_none[1]=1-hz_rr[1]*z_rr[1];w_rs_none[1]=1-hz_rr[1]*z_rr[1];
w_ss_low[1]=0.1
w_ss_high[1]=0.0

# <<<<<<<<<<<<< end of user-defined variable >>>>>>>>>>>>>>>>>>>>>>>>



#>>>>>>>> now to run the simulations <<<<<<<<<<<<<<<<<<<<<<<<<<<

current_insectide=1 #usually start the rotation sequence at #1 but can specify any one start

for(gen in 2:max_no_generations){    #start at generation 2 because generation 1 holds the user-defined initial allele frequenciess
  
#first find if we need to switch current insecticide
if(freq_rotation==0){  #i.e.  its a RwR policy:
  if(RAF_intervention[current_insecticide,gen]>rotation_criterion){
    if(current_insecticide=no_insecticides) current_insecticide=1 #restart the sequence of rotation
    else current_insecticide=current_insecticide+1 #else just move to the next insectide
    #Need a check in case they all exceed the critical frequency in which case we stop the simulation
    #probably need a function ebacsue check that they were not above teh threshold within teh last 2 or 3 generations
    #to avoid switching on a implauisbly short timescale.
  }  
 else{ #if freq_rotation>0 then its a routine, periodic rotation
 if (gen/freq_rotation)is an integer { #i.e. if its a multiple of ferauncy of rotation i.e. time to chnage   
   if(current_insecticide=no_insecticides) current_insecticide=1 #restart the sequence of rotation
   else current_insecticide=current_insecticide+1 #else just move to the next insectide
   #Need a check in case they all exceed the critical frequency in whichh case we stop the simulation 
 } 
  
 
for(insectide in 1:no_insecticides){

#first the intervention site  
 if(insecticide=current_insecticide){ #i.e. insecticide selection taking place
   temp_coeff_1=(RAF_male_intervention[insecticide,gen-1]*(1-RAF_female_intervention[insecticide,gen-1])+
                               RAF_female_intervention[insecticide,gen-1]*(1-RAF_male_intervention[insecticide,gen-1]))*0.5
   
   temp_coeff_2=alpha_male_none[insectide]*w_rs_none[insectide]+
     alpha_male_low[insectide]*w_rs_low[insectide]+
     alpha_male_high[insectide]*w_rs_low[insectide]
    
   temp_coeff_3=alpha_female_none[insectide]*w_rs_none[insectide]+
                   alpha_female_low[insectide]*w_rs_low[insectide]+
                   alpha_female_high[insectide]*w_rs_low[insectide]
   
#first the male resistant alleles>>
  temp_term=alpha_male_none[insectide]*w_rr_none[insectide]+
     alpha_male_low[insectide]*w_rr_low[insectide]+
     alpha_male_high[insectide]*w_r_low[insectide] 
  
  F_male_r_intervention=(RAF_male_intervention[insecticide,gen-1]*RAF_female_intervention[insecticide,gen-1]*temp_term)+
    temp_coeff_1*temp_coeff_2
  
  #now the male sensitive alleles>> 
  temp_term=alpha_male_none[insectide]*w_ss_none[insectide]+
    alpha_male_low[insectide]*w_ss_low[insectide]+
    alpha_male_high[insectide]*wss_low[insectide]  
  
  F_male_s_intervention=(1-RAF_male_intervention[insecticide,gen-1]*(1-RAF_female_intervention[insecticide,gen-1])*temp_term)+
    temp_coeff_1*temp_coeff_2
    
  #now the female resistant alleles>>
  temp_term=alpha_female_none[insectide]*w_rr_none[insectide]+
    alpha_female_low[insectide]*w_rr_low[insectide]+
    alpha_female_high[insectide]*w_r_low[insectide] 
  
  F_female_r_intervention=(RAF_male_intervention[insecticide,gen-1]*RAF_female_intervention[insecticide,gen-1]*temp_term)+
    temp_coeff_1*temp_coeff_3

  #now the female sensitive alleles>> 
  temp_term=alpha_female_none[insectide]*w_ss_none[insectide]+
    alpha_female_low[insectide]*w_ss_low[insectide]+
    alpha_female_high[insectide]*wss_low[insectide]  
  
  F_male_s_intervention=(1-RAF_male_intervention[insecticide,gen-1]*(1-RAF_female_intervention[insecticide,gen-1])*temp_term)+
    temp_coeff_1*temp_coeff_3 

  #now normalise the male gamete frequencies
  norm_coeff= F_male_r_intervention+F_male_s_intervention
  F_male_r_intervention=F_male_r_intervention/nomr_coeff
  F_males_intervention=F_male_s_intervention/nomr_coeff
  
  #now normalise the female gamete frequencies
  norm_coeff= F_female_r_intervention+F_female_s_intervention
  F_female_r_intervention=F_female_r_intervention/nomr_coeff
  F_females_intervention=F_female_s_intervention/nomr_coeff
  
 }  
 else{ #i.e no selection for this insecticide in the intervention site
   #first the coefficient for heterozygotes common to equations 2 and 3
   temp_coeff=(RAF_male_intervention[insecticide,gen-1]*(1-RAF_female_intervention[insecticide,gen-1])+
                 RAF_female_intervention[insecticide,gen-1]*(1-RAF_male_intervention[insecticide,gen-1]))
                 *0.5*w_rs_none[insecticide]
   #now the reistant and sensitive frequencies in untreated areas  
   F_male_r_intervention=RAF_male_intervention[insecticide,gen-1]*RAF_female_intervention[insecticide,gen-1]*w_rr_none[insecticide]+temp_coeff
   F_male_s_intervention=(1-RAF_male_intervention[insecticide,gen-1])*(1-RAF_female_intervention[insecticide,gen-1])*w_ss_none[insecticide]+temp_coeff
   #now to normalise them
   norm_coeff= F_male_r_intervention+F_male_s_intervention
   F_male_r_intervention=F_male_r_intervention/nomr_coeff
   F_male_s_intervention=F_male_s_intervention/nomr_coeff

#same allele frequncies in both sexes if no differential equetion so 
   F_female_r_intervention=F_male_r_intervention; F_female_s_intervention=F_male_s_intervention  
   
   
 }   
    
#now for the refugia
  #first the coefficient for heterozygotes common to equations 2 and 3
  temp_coeff=(RAF_male_refugia[insecticide,gen-1]*(1-RAF_female_refugia[insecticide,gen-1])+
                RAF_female_refugia[insecticide,gen-1]*(1-RAF_male_refugia[insecticide,gen-1]))
  *0.5*w_rs_none[insecticide]
  #now the resistant and sensitive frequencies   
  F_male_r_refugia=RAF_male_refugia[insecticide,gen-1]*RAF_female_refugia[insecticide,gen-1]*w_rr_none[insecticide]+temp_coeff
  F_male_s_refugia=(1-RAF_male_refugia[insecticide,gen-1])*(1-RAF_female_refugia[insecticide,gen-1])*w_ss_none[insecticide]+temp_coeff
  #now to normalise them
  norm_coeff= F_male_r_refugia+F_male_s_refugia
  F_male_r_refugia=F_male_r_refugia/norm_coeff
  F_male_s_refugia=F_male_s_refugia/norm_coeff  

  #same allele frequncies in both sexes if no differential equetion so 
  F_female_r_refugia=F_male_r_refugia; F_female_s_refugia=F_male_s_refugia  
  
  }   

  
#now for migration between refugia and intervention site


#now to update the allele frequencies
    
    
    
  } #end of cycling through the insecticides
   
  
  
  
} #end of cycle running the gens up to max_no_generations
  
  
 
