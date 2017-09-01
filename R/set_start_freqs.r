#' set_start_freqs_test allows starting allele frequencies to be hardcoded
#' 
#' 
#' @param n_insecticides number of insecticides, optional can just be specified by number of items in vector expo
#' @param plot whether to plot exposure    
#' 
#' @examples
#' RAF <- set_start_freqs_test( )
#' 
#' #allowing array to be viewed differently
#' as.data.frame(RAF)
#' 
#' @return array of allele frequencies with just first record filled
#' @export
#' 
set_start_freqs_test <- function( n_insecticides = NULL,
                                  max_generations = 200,
                                   plot = FALSE)
{
  
  #set n_insecticides if it is not specified
  if ( is.null(n_insecticides)) n_insecticides <- 4
  
  #RAF stands for resistance allele frequency
  RAF <-      array_named(insecticide=1:n_insecticides, sex=c('m','f'), site=c('intervention','refugia'), gen=1:max_generations)
  
  #inital resistance allele frequency (i.e. generation 1) in the intervention and refugia
  #locus 1>>
  RAF[1, 'm', 'intervention',1]=0.002;  RAF[1, 'f', 'intervention',1]=0.002;
  RAF[1, 'm', 'refugia',1]=0.001;       RAF[1, 'f', 'refugia',1]=0.001
  #locus 2>>
  if(n_insecticides>=2){ #need to avoid exceeding size of the array
    RAF[2, 'm', 'intervention',1]=0.001;   RAF[2, 'f', 'intervention',1]=0.001;
    RAF[2, 'm', 'refugia',1]=0.001;        RAF[2, 'f', 'refugia',1]=0.001
  }
  #locus 3>>
  if(n_insecticides>=3){
    RAF[3, 'm', 'intervention',1]=0.09;  RAF[3, 'f', 'intervention',1]=0.09;
    RAF[3, 'm', 'refugia',1]=0.001;       RAF[3, 'f', 'refugia',1]=0.001
  }
  #locus 4>>
  if(n_insecticides>=4){
    RAF[4, 'm', 'intervention',1]=0.09;  RAF[4, 'f', 'intervention',1]=0.09;
    RAF[4, 'm', 'refugia',1]=0.001;       RAF[4, 'f', 'refugia',1]=0.001
  }#locus 5>>
  if(n_insecticides>=5){
    RAF[5, 'm', 'intervention',1]=0.09; RAF[5, 'f', 'intervention',1]=0.09;
    RAF[5, 'm', 'refugia',1]=0.001;      RAF[5, 'f', 'refugia',1]=0.001
  } 
  
  
  #if (plot) plot_exposure(a_expo)
  
  return(RAF)
}