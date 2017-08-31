#' rot_plot_resistance plot rotation simulation resistance results
#'
#' @param df_res2 dataframe of resistance results from run_rot()
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
rot_plot_resistance <- function(df_res2) {
  
  # to allow plotting of insecticide in use
  # add column which has a value if insecticide in use & NA if not
  # the value (currently 1.05) determines where the line appears on the y axis
  df_res2 <- df_res2 %>%
    #mutate( i_in_use = ifelse(insecticide==1,1,NA))
    mutate( i_in_use = ifelse(stringr::str_detect(region,paste0(insecticide,"_active")),1.05,NA))    
  
  gg <- ggplot( df_res2, aes_string(x='generation',y='resistance') ) + 
    geom_point(shape=1, colour='blue') +
    #geom_line( colour='blue') +  
    facet_wrap('region', ncol=2) +
    #theme(axis.text.x = element_blank()) +
    #add insecticide use indication
    geom_line( aes_string(x='generation',y='i_in_use'), colour='red', lwd=2) +
    annotate("text", x = 0, y = 1.15, label = "deployment", size = 2.5, hjust='left') +
    #scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1)) +
    scale_y_continuous(breaks=c(0,0.25,0.5,0.75,1)) +
    #theme_bw()
    theme_minimal()
  
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