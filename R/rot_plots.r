#' rot_plot_resistance plot rotation simulation resistance results
#'
#' @param df_res2 dataframe of resistance results from run_rot()
#' @param refuge whether to plot refuge as well as intervention
#' @param logy whether to use log scale for y axis
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
                                logy = TRUE) {
  
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
    #geom_point(shape=1, colour='blue') +
    #geom_line( colour='blue') +  
    geom_line( alpha=0.5 ) + 
    facet_wrap('resist_gene', ncol=1) +
    #theme(axis.text.x = element_blank()) +
    #add insecticide use indication
    #geom_line( aes_string(x='generation',y='i_in_use'), colour='green4', lwd=2) +
    #trying to get 2nd legend (this puts i_in_use in middle OK start)
    geom_line( aes_string(x='generation',y='i_in_use', colour=factor('i_in_use')), lwd=2) +
    #the two below don't work to remove title
    #guide_legend(title=NULL) +
    #scale_fill_discrete(guide = guide_legend(title = NULL)) +
    #annotate("text", x = 0, y = 1.15, label = "deployment", size = 2.5, hjust='left') +
    #scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1)) +
    #scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1)) +
    #log axis scale to make clearer what's happening at low values
    #scale_y_continuous(trans='log10', labels = scales::comma) +
    #theme_bw()
    theme_minimal()
  
  if (logy) gg <- gg + scale_y_continuous(trans='log10', 
                                         breaks=c(0,0.001,0.1,1),
                                         minor_breaks=NULL,
                                         labels = scales::comma)
  else     gg <- gg + scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1))
  
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