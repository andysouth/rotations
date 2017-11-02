#' rot_plot_resistance plot rotation simulation resistance results
#'
#' @param df_res2 dataframe of resistance results from run_rot()
#' @param refuge whether to plot refuge as well as intervention
#' @param logy whether to use log scale for y axis
#' @param add_gens_under50 whether to add a label of num generations under 50% resistance
#'
#'
#' @import ggplot2
#' @importFrom stringr str_detect
#' @return ggplot object
#' @export
#'
#' @examples
#' df_res2 <- run_rot()
#' rot_plot_resistance(df_res2)
#' 
rot_plot_resistance <- function(df_res2,
                                plot_refuge = TRUE,
                                logy = FALSE,
                                add_gens_under50 = TRUE) {
  
  # column names of input dataframe
  # "generation"  "insecticide"     "resist_gene"  "active_or_refuge" "resistance"
  
  # filter out refuge if not wanted
  if (!plot_refuge) df_res2 <- filter(df_res2, active_or_refuge != 'refuge')
  
  # to allow plotting of insecticide in use
  # add column which has a value if insecticide in use & NA if not
  # the value (currently 1.05) determines where the line appears on the y axis
  df_res2 <- df_res2 %>%
    #mutate( i_in_use = ifelse(insecticide==1,1,NA))
    #mutate( i_in_use = ifelse(stringr::str_detect(region,paste0(insecticide,"_active")),1.05,NA))    
    #now that active & refuge on same plot
    mutate( i_in_use = ifelse(resist_gene==paste0('insecticide',insecticide),1.05,NA))    
  
    
  gg <- ggplot( df_res2, aes_string(x='generation',y='resistance',colour='active_or_refuge') ) + 

    geom_line( alpha=0.5, lwd=1.5 ) + 
    
    facet_wrap('resist_gene', ncol=1) +
    
    #theme(axis.text.x = element_blank()) +
    
    #trying to get 2nd legend (this puts i_in_use in middle OK start)
    #geom_line( aes_string(x='generation',y='i_in_use', colour=factor('i_in_use')), lwd=2) +
    
    #annotate("text", x = 0, y = 1.15, label = "deployment", size = 2.5, hjust='left') +

    #scale_y now below dependent on logy arg
    
    #add insecticide use indication
    geom_line( aes_string(x='generation',y='i_in_use',linetype=factor('i_in_use')), colour='green2', lwd=2) +
    scale_linetype_manual("insecticide in use", values=rep(1,4),
      #trying to set labls in override.aes didn't work                    
      guide=guide_legend(keywidth = 5, label=FALSE, override.aes = list(colour=c("green2")))) +
    
    theme_minimal()
  
  if (logy) gg <- gg + scale_y_continuous(trans='log10', 
                                         breaks=c(0,0.001,0.1,1),
                                         minor_breaks=NULL,
                                         labels = scales::comma)
  else     gg <- gg + scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1))
  
  
  if (add_gens_under50)
    #can I add text of num gens below 50%
    gg <- gg + geom_text(aes(x=Inf, y=-Inf, label=gens_under50), colour='black', show.legend=FALSE, hjust=1, vjust=0)
    
  
  
  plot(gg)
  
  invisible(gg)
} 


#insecticide use (currently restricted to 4)
#superceded, now done within rot_plot_resistance
rot_plot_use <- function(df_res2) {
  ggplot( df_res2, aes_string(x='generation',y='insecticide') ) + 
    geom_point(shape=1, colour='red') +
    #geom_line( colour='blue') +  
    #facet_wrap('region', ncol=2) +
    ylim(1,4) +
    theme_bw() 
  #theme(axis.text.x = element_blank()) 
}