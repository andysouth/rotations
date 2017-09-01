#' set exposure to insecticides for rotations to a flexible number of insecticides
#' 
#' TODO could this be made generic to the resistance code too ? 
#' I suspect not. They do things quite differently.
#' This just needs 1 insecticide at a time, so is :
#' array_named(insecticide=1:n_insecticides, sex=c('m','f'), exposure=c('no','lo','hi')
#' In resistance it is :
#' array_named( sex=c('m','f'), niche1=c('0','a','A'), niche2=c('0','b','B') )
#' 
#' fills an array of exposure values
#' 
#' @param n_insecticides number of insecticides, optional can just be specified by number of items in vector expo
#' @param expo exposure to the insecticides
#' @param male_expo_prop proportion tht males are exposed relative to f, default 1, likely to be <1
#' @param plot whether to plot exposure    
#' 
#' @examples
#' a_exp <- set_exposure_rot( expo_hi=0.9 )
#' a_exp <- set_exposure_rot( expo_hi=c(0.5,0.9), male_expo_prop = 0.9)
#' 
#' #allowing array to be viewed differently
#' as.data.frame(a_exp)
#' 
#' @return array of exposure values for the different insecticides
#' @export
#' 
set_exposure_rot <- function( n_insecticides = NULL,
                              expo_hi = 0.8,
                              expo_lo = 0,                              
                              male_expo_prop = 1,
                              plot = FALSE)
{
  
  #get n_insecticides if it is not specified
  #todo add checks, allow single
  if ( is.null(n_insecticides)) n_insecticides <- length(expo_hi)
  
  #exposure to insecticide
  #exposure array initialise with 0s 
  #a <- array_named( sex=c('m','f'), niche1=c('0','a','A'), niche2=c('0','b','B') )
  
  a_expo <- array_named(insecticide=1:n_insecticides, sex=c('m','f'), exposure=c('no','lo','hi'))

  a_expo[,'f','hi'] <- expo_hi
  a_expo[,'m','hi'] <- expo_hi * male_expo_prop    

  a_expo[,'f','lo'] <- expo_lo
  a_expo[,'m','lo'] <- expo_lo * male_expo_prop    

  #set all no exposures to 1-(lo+hi) 
  a_expo[,, 'no'] <- 1-(a_expo[,, 'lo'] + a_expo[,, 'hi'])    
       
  #todo add check that lo+hi is not greater than 1
  #and check my understanding of the logic
  #error check for fitnesses > 1 or < 0
  if ( any( a_expo > 1  ) ) 
    warning( sum(a_expo > 1 ), " exposure values (a_expo) are >1 : ", a_expo[a_expo>1])
  if ( any( a_expo < 0 ) ) 
    warning( sum( a_expo < 0 ), " locus fitness values (a_expo) are <0")    
  
  #if (plot) plot_exposure(a_expo)
  
  return(a_expo)
}

#' set_exposure_rot_test allows exposures to be hardcoded
#' 
#' set exposure to insecticides for rotations to a flexible number of insecticides
#' fills an array of exposure values
#' 
#' @param n_insecticides number of insecticides, optional can just be specified by number of items in vector expo
# @param expo exposure to the insecticides
# @param male_expo_prop proportion tht males are exposed relative to f, default 1, likely to be <1
#' @param plot whether to plot exposure    
#' 
#' @examples
#' a_exp <- set_exposure_rot_test( )
#' 
#' #allowing array to be viewed differently
#' as.data.frame(a_exp)
#' 
#' @return array of exposure values for the different insecticides
#' @export
#' 
set_exposure_rot_test <- function( n_insecticides = NULL,
                                   plot = FALSE)
{
  
  #set n_insecticides if it is not specified
  if ( is.null(n_insecticides)) n_insecticides <- 4
  
  a_expo <- array_named(insecticide=1:n_insecticides, sex=c('m','f'), exposure=c('no','lo','hi'))
  
  #exposure patterns for insecticide 1
  a_expo[1, 'm', 'lo'] =0.1; a_expo[1, 'm', 'hi'] =0.5;
  a_expo[1, 'f', 'lo'] =0.1; a_expo[1, 'f', 'hi'] =0.6;
  #exposure patterns for insecticide 2
  if(n_insecticides>=2){ #need to avoid exceeding size of the array
    a_expo[2, 'm', 'lo'] =0.1; a_expo[2, 'm', 'hi'] =0.1;
    a_expo[2, 'f', 'lo'] =0.1; a_expo[2, 'f', 'hi'] =0.1;
  }
  #exposure patterns for insecticide 3
  if(n_insecticides>=3){
    a_expo[3, 'm', 'lo'] =0.1; a_expo[3, 'm', 'hi'] =0.1;
    a_expo[3, 'f', 'lo'] =0.1; a_expo[3, 'f', 'hi'] =0.1;
  }
  #exposure patterns for insecticide 4
  if(n_insecticides>=4){
    a_expo[4, 'm', 'lo'] =0.1; a_expo[4, 'm', 'hi'] =0.1;
    a_expo[4, 'f', 'lo'] =0.1; a_expo[4, 'f', 'hi'] =0.1;
  }
  #exposure patterns for insecticide 5
  if(n_insecticides>=5){
    a_expo[5, 'm', 'lo'] =0.1; a_expo[5, 'm', 'hi'] =0.1;
    a_expo[5, 'f', 'lo'] =0.1; a_expo[5, 'f', 'hi'] =0.1;
  }
  
  #set all no exposures to 1-(lo+hi) 
  a_expo[,, 'no'] <- 1-(a_expo[,, 'lo'] + a_expo[,, 'hi'])    
  
  #todo add check that lo+hi is not greater than 1
  #and check my understanding of the logic
  #error check for fitnesses > 1 or < 0
  if ( any( a_expo > 1  ) ) 
    warning( sum(a_expo > 1 ), " exposure values (a_expo) are >1 : ", a_expo[a_expo>1])
  if ( any( a_expo < 0 ) ) 
    warning( sum( a_expo < 0 ), " locus fitness values (a_expo) are <0")    
  
  #if (plot) plot_exposure(a_expo)
  
  return(a_expo)
}