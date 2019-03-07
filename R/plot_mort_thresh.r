#' plot_mort_thresh to try plotting the regions of param space where mortality > 90%


plot_mort_thresh <- function( threshold = 0.9) {
 
# create vectors of inputs  
vrfreq <- 1/10^(1:10)
veff <- seq(0.8,1,0.05)
 
# dataframe with all combinations 
df1 <- expand.grid(rfreq=vfreq,
                   eff=veff)
  
# calc mortality
# first go assume set dom and rr
dom_sel <- 0.5
rr <- 0.5

#df1$mort <- mort_from_resist(df1$freq, eff=eff, dom_sel=dom_sel, rr=rr)
#sapply(mort_from_resist(eff=veff,rfreq=vfreq,rr=1,dom_sel=1))

#bodge start
df1$mort <- NA
for(i in 1:nrow(df1))
{
  df1$mort[i] <-mort_from_resist(rfreq=df1$rfreq[i], eff=df1$eff[i], dom_sel=dom_sel, rr=rr) 
}

# can change the scale so it just shows > 90% mort
ggplot(df1, aes(eff, rfreq )) +
  geom_tile(aes(fill = mort), color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  scale_y_continuous(trans='log10') #, 
                       #breaks=c(0.001,0.01,0.5,1),
                       #labels=c('0.1%','1%','50%','100%'),
  


#df1$mort <- sapply(df1$freq, mort_from_resist(eff=eff, dom_sel=dom_sel, rr=rr))

  # for(freq in vfreq)
  # {
  #   for(eff in veff)
  #   {
  #     cat("f:",freq, " eff:",eff,'\n')
  #   }
  # }
  
  
   
}