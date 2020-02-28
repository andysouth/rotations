#' rot_plot_resistance plot rotation simulation resistance results
#'
#' @param df_res2 dataframe of resistance results from run_rot()
#' @param plot_refuge whether to plot refuge as well as intervention
#' @param mort_or_freq whether threshold for insecticide change is mortality 'mort' or resistance frequency 'freq'
#' @param mort_thresh mortality threshold for switching insecticides, default 0.9, only used if mort_or_freq is mort
#' @param freq_thresh resistance frequency threshold for switching insecticides, default 0.5, only used if mort_or_freq is freq
#' @param logy whether to use log scale for y axis
#' @param add_gens_under50 whether to add a label of num generations under 50percent resistance
#' @param df_resanother exploratory option to plot results of another scenario on the same graph
#' @param lwd line thickness for resistance curves
#' @param title optional title for plot, NULL for no title, 'auto' for auto generated title
#' @param plot whether to plot results
#' @param plot_mort whether to add mortality to plots
#'
# check said that namespace dependencies not required
# @import ggplot2
# @importFrom stringr str_detect
#' @return ggplot object
#' @export
#'
#' @examples
#' df_res2 <- run_rot()
#' rot_plot_resistance(df_res2)
#' 

#' 
rot_plot_resistance <- function(df_res2,
                                plot_refuge = TRUE,
                                mort_thresh = 0.9,
                                freq_thresh = 0.5,
                                mort_or_freq = 'mort',
                                logy = TRUE,
                                add_gens_under50 = FALSE,
                                df_resanother = NULL,
                                lwd = 1.5,
                                title = 'auto', #NULL
                                plot = TRUE,
                                plot_mort = TRUE) {
  
  # column names of input dataframe
  # "generation"  "insecticide"     "resist_gene"  "active_or_refuge" "resistance"
  
  # filter out refuge if not wanted (if coverage=1 no refuge anyway)
  #if (!plot_refuge) df_res2 <- filter(df_res2, active_or_refuge != 'refuge')
  if (!plot_refuge) df_res2 <- df_res2[df_res2$active_or_refuge=='active',]


  # set plot_mort true for mort    
  if ( is.null(plot_mort) ) plot_mort <- ifelse(mort_or_freq=='mort',TRUE,FALSE)
  # set logy false for mort    
  if ( is.null(logy) ) logy <- ifelse(mort_or_freq=='mort',FALSE,TRUE)  
  
  
  # to allow plotting of insecticide in use
  # add column which has a value if insecticide in use & NA if not
  # the value determines where the line appears on the y axis
  to_plot_in_use <- 1
  
  df_res2 <- df_res2 %>%
    #mutate( i_in_use = ifelse(stringr::str_detect(region,paste0(insecticide,"_active")),1.05,NA))    
    #now that active & refuge on same plot
    dplyr::mutate( i_in_use = ifelse(resist_gene==paste0('insecticide',insecticide), to_plot_in_use, NA))    
  
  # only colour by active_or_refuge if there is a refuge
  if (plot_refuge) {
    gg <- ggplot( df_res2, aes_string(x='generation',y='resistance',colour='active_or_refuge') ) +
    geom_line( alpha=0.5, lwd=lwd )
    #legend for the lines, allows me to set title & labels  
    if (!plot_mort & mort_or_freq=='freq')
    {
      gg <- gg + scale_colour_manual("areas connected\nby migration",values=c("red3","navy"), labels=c("treated","untreated refugia"))
    }
  } else
  {
    gg <- ggplot( df_res2, aes_string(x='generation',y='resistance',colour='active_or_refuge')) +
    geom_line( alpha=0.5, lwd=lwd) #, colour='red3' ) 
    #legend for the lines  
    #scale_colour_manual("",values=c("red3")) 
    gg <- gg + scale_colour_manual("complete coverage",values=c("red3"), labels=c("treated"))
    
  }

  # testing adding mortality to the plot
  
  # note mortality currently added in run_rot but actually we can calc from fixed conversion
  if (plot_mort) {

    # set y intercept for horiz line to threshold
    if (mort_or_freq=='mort') 
    {
      yintercept <- mort_thresh 
      thresh_name <- "threshold mortality"
      thresh_col <- "green"
    }
    else 
    {
      yintercept <- freq_thresh
      thresh_name <- "threshold resistance"
      thresh_col <- "red"      
    }

    
    dfactive <- df_res2[df_res2$active_or_refuge=='active',]
    gg <- gg +
      # add 90% mortality threshold
      # BEWARE the colour='dummy' in here is critical to get other lines to appear in legend
      geom_hline(aes(yintercept=yintercept, colour='dummy'), linetype=3) + #, show.legend=TRUE
    #  geom_line( aes_string(x='generation',y='mortality'), alpha=0.5, lwd=1, colour='purple', linetype=3 )    
      geom_line( data=dfactive, aes(x=generation,y=mortality,colour='mortality'), 
                 alpha=0.5, lwd=1, linetype=1) + #, colour='darkgreen'
      #this just added green on top of existing legend colours
      #, show.legend=TRUE )
      
      #trying & initially failing to get mortality line to appear in legend 
      #fiddled with the orders of values & labels to get to correspond to lines
      # TODO sort when plot_refuge==FALSE
      scale_colour_manual("areas connected\nby migration",values=c("red3",thresh_col,"darkgreen","navy"), 
                           labels=c("resistance\nin treated",thresh_name,"mortality in treated","resistance in\nuntreated refugia"),
                           guide = guide_legend(linetype=c(1,3,1,1)))
                           #guide = guide_legend(override.aes = list(fill = NA)))
    
      
      #legend for mortality, allows me to set title & labels  
      #but this overwrites other legend
      # scale_colour_manual("mortality",values=c("darkgreen","darkgreen"), labels=c("assay","threshold"),
      #                     guide = guide_legend(override.aes = list(fill = NA)))
    
    }  
   
  gg <- gg + 

    facet_wrap('resist_gene', ncol=1) +
    
    #add new insecticide use indication
    #when I do inherit.aes=FALSE the boxes don't appear
    #when TRUE the legend for the lines gets interfered with
    #geom_ribbon( data=df_res2, aes_string(x='generation', ymin=0, ymax='i_in_use'), inherit.aes = FALSE, fill = "grey90", alpha=0.1) + #,linetype=factor('i_in_use')), colour='green2', lwd=2) +
    geom_ribbon( aes_string(x='generation', ymin=0, ymax='i_in_use'), colour="grey30", fill = "grey80", alpha=0.2) + #,linetype=factor('i_in_use')), colour='green2', lwd=2) +
    
    #trying to add legend for the in_use ribbon, now doesn't work because colour is outside aes above
    #scale_fill_manual("",values="grey90") +
    
    #old insecticide use indication  
    # geom_line( aes_string(x='generation',y='i_in_use',linetype=factor('i_in_use')), colour='green2', lwd=2) +
    # scale_linetype_manual("insecticide in use", values=rep(1,4),
    #   #trying to set labls in override.aes didn't work                    
    #   guide=guide_legend(keywidth = 5, label=FALSE, override.aes = list(colour=c("green2")))) +
    
    theme_minimal() +
    
    # add line at resistance threshold
    # set y intercept for horiz line to threshold if freq, otherwise default to 0.5
    # stopped plotting this when thresh is mortality
    # BEWARE value of plot_mort too
    if (! is.null(freq_thresh) & mort_or_freq=='freq')
    {
      if (mort_or_freq=='freq') yintercept <- freq_thresh
      else yintercept <- mort_thresh
      geom_hline(yintercept=yintercept, linetype=3, colour='red')       
    }
  
    if (plot_mort) gg <- gg + ylab("resistance frequency or mortality")
    else gg <- gg + ylab("resistance allele frequency")
  
  
  
  # experimenting with plotting a 2nd scenario on the same graph
  if (!is.null(df_resanother))
  {
    #if (!plot_refuge) 
    #trying just plotting active
    df_resanother <- df_resanother[df_resanother$active_or_refuge=='active',]
    
    gg <- gg +
          #geom_line( data=df_resanother, alpha=0.5, lwd=2, colour='blue' )    
          geom_line( data=df_resanother, aes_string(x='generation',y='resistance'), alpha=0.5, lwd=lwd, colour='red3', linetype=3 )    

    # try adding insecticide use for 2nd scenario
    df_resanother <- df_resanother %>%
      dplyr::mutate( i_in_use = ifelse(resist_gene==paste0('insecticide',insecticide), to_plot_in_use, NA))  
    
    gg <- gg +    
          geom_ribbon( data=df_resanother, aes_string(x='generation', ymin=0, ymax='i_in_use'), colour="grey30", fill = "grey80", alpha=0.2, linetype=3 ) 
      
    
    
  }
  
  
  
  if (logy) {
    gg <- gg + scale_y_continuous(trans='log10', 
                                         breaks=c(0.001,0.01,0.5,1),
                                         labels=c('0.1%','1%','50%','100%'),
                                         #labels = scales::percent,
                                         #labels = scales::comma,
                                         minor_breaks=NULL) +
                       theme(axis.text.y = element_text(size = rel(0.8))) #, angle = 90))
  } else {
    
    gg <- gg + scale_y_continuous(breaks=c(0,0.5,1),
                                  labels=c('0','50%','100%'),
                                  minor_breaks=NULL)    
  }
  

  
  #gens_dep_under50 is repeated for all generations which make it look ugly when plotted with geom_text
  #this filters out so just one result per insecticide
  df_res3 <- dplyr::filter(df_res2, generation==1 & active_or_refuge=='active')
  
  # add text of num gens below 50%  
  if (add_gens_under50) {
    
    # for positioning label without this didn't work for log scale
    if (logy) y <- 0
    else y <- -Inf
    
    gg <- gg + geom_text(data=df_res3, aes(x=Inf, y=y, label=gens_dep_under50), colour='black', show.legend=FALSE, hjust=1, vjust=0)
    
    #problem with these options below is that they just accessed value for the first insecticide
    #gg <- gg + annotate("text",x=Inf, y=y, label=df_res2$gens_dep_under50[1], colour='black', size=5, hjust=1, vjust=0)
    #gg <- gg + ggtitle(paste0('generations insecticide in use and under resistance threshold : ',df_res2$gens_dep_under50[1]))
  }
  
  if (!is.null(title)) 
  {
    if (title=='auto')
    {
      #title <- paste0("Generations in-use under threshold :", sum(df_res3$gens_dep_under50))
      #change title to show generations until all insecticides at threshold
      title <- paste0("Generations until all insecticides at threshold :", df_res3$end_gens)     
      
      #would be nice to label as rotation or sequence but don't have that info here      
      #if (df_res2$rot_interval==0) title <- paste0("Sequence, generations in-use under threshold :", sum(df_res3$gens_dep_under50))
      #else title <- paste0("Rotation interval ", df_res2$rot_interval," generations in-use under threshold :", sum(df_res3$gens_dep_under50))
    }
    
    gg <- gg + ggtitle(label="",subtitle=title)
  }
    
  if (plot) plot(gg)
  
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