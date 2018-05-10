#' freq_fitness_simple to show very simply how fitness is expected to vary by frequency 
#' 
#' based on ians code, external to main rotations model
#' 
#' @param dom_sel dominance of selection
#' @param dom_cos dominance of cost
#' @param sel_rr selective advantage of resistance 
#' @param cos_rr fitness cost of RR in no insecticide
#' 
#' @param plot whether to plot results
#' 
#' @examples 
#' df1 <- freq_fitness_simple(cos_rr = 0.05, dom_cos = 0.9, sel_rr = 0.4, dom_sel = 0.1)
#' df2 <- freq_fitness_simple(cos_rr = 0.05, dom_cos = 0.1, sel_rr = 0.4, dom_sel = 0.1)
#' df3 <- freq_fitness_simple(cos_rr = 0.05, dom_cos = 0.1, sel_rr = 0.4, dom_sel = 0.9)
#' df4 <- freq_fitness_simple(cos_rr = 0.05, dom_cos = 0.9, sel_rr = 0.4, dom_sel = 0.9)
#' 
#' @import tidyverse 
#' @return dataframe of results
#' @export

freq_fitness_simple <- function( cos_rr = 0.05, #fitness cost of the RR genotype in absence of insecticide
                                 dom_cos = 0.95, #dominance of cost
                                 
                                 #may later need to couch selective advantage in terms of rr_resoration, proportion treated in intervention site  etc
                                 sel_rr = 0.1, #selective advantage of RR genotype
                                 dom_sel = 0.05, # dominance of selective advantage
                                 plot = TRUE
) {
  
  num_freqs <- 100 
  df_ex <- data_frame(freq=rep(NA,num_freqs),
                      cost=NA,
                      selection=NA,
                      dom_cos=dom_cos,
                      dom_sel=dom_sel) #put inputs into the out df so they can be used
  
  for (ii in 1:100 ) {
    
    freq <- ii/100
    df_ex$freq[ii] <- freq
    
    #now need weighted averages of fitness NB we assume Hardy-Weinburg which is only an approximation under selection
    pop_size <- 1000# multiply by pop-size to make things explicit...not necessary as cancels out
    # first term is number of alelles (multiply pop_size by 2 for diploidY; second term is Hardy-Weinberg proportions)
    no_in_hetero <- (pop_size*2)*(2*freq*(1-freq)) 
    # final multiplication by 2 because there are two R alleles in the homozygote
    no_in_homo <- (pop_size*2)*(freq*freq)*2        
    
    prop_hetero <- no_in_hetero/(no_in_hetero+no_in_homo)   #the proportion of R allelels in heterozygotes
    prop_homo <- 1-prop_hetero     #the proportion of R allelels in RR homozygotes
    
    fit_absence <- prop_hetero*(1-dom_cos*cos_rr) + prop_homo*(1-cos_rr)  
    
    df_ex$cost[ii] <- 1-fit_absence
    
    fit_presence = prop_hetero*(1+dom_sel*sel_rr) + prop_homo*(1+sel_rr) #todo why + here & - above ?
    
    df_ex$selection[ii]=fit_presence-1
    
  }
  
  #gather data to be longer
  #df_cost_long <- tidyr::gather(df_cost_long, key=input_name, value=input_value)
  
  dftidy <- tidyr::gather(df_ex, key=fitness_component, value=fitness_value, -freq, -dom_cos, -dom_sel)
  
  if (plot) 
  {
    plot( ggplot(dftidy, aes(x=freq, y=fitness_value, col=fitness_component)) +
            geom_line() +
            xlab('allele frequency'))
  }
  
  invisible(dftidy)
}