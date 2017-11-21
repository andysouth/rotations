#' set_start_freqs allows starting allele frequencies to be set
#' 
#' 
#' @param n_insecticides number of insecticides, optional can just be specified by number of items in vector expo
#' @param max_gen maximum number of mosquito generations to run the simulation
#' @param freqs starting allele frequencies either one per insecticide or same for all
#'              in this version same for m,f,intervention & refuge    
#' 
#' @examples
#' #frequencies the same for all insecticides
#' RAF <- set_start_freqs(n=3, freqs=0.001)
#' RAF[,,,1] # to view generation 1
#' #frequencies different for each insecticide
#' RAF <- set_start_freqs(n=3, freqs=c(0.1,0.01,0.001))
#' RAF[,,,1] # to view generation 1
#' #testing mutliplying frequency arrays
#' RAF <- set_start_freqs(max_gen = 1, freqs=c(0.1,0.01,0.001))
#' RAF2 <- set_start_freqs(max_gen = 1, freqs=c(1,2,3))
#' RAF*RAF2
#' 
#' #allowing array to be viewed differently
#' as.data.frame(RAF)
#' 
#' @return array of allele frequencies with just first record filled
#' @export
#' 
set_start_freqs <- function( n_insecticides = NULL,
                             max_gen = 200,
                             freqs = 0.001 )
{
  
  #get n_insecticides if it is not specified
  #todo add checks, allow single
  if ( is.null(n_insecticides)) n_insecticides <- length(freqs)
  
  # create empty array
  # RAF stands for resistance allele frequency
  RAF <- array_named(insecticide=1:n_insecticides, sex=c('m','f'), site=c('intervention','refugia'), gen=1:max_gen)

  # fill generation 1
  # in this version same for m,f,intervention & refuge
  # RAF[, 'm', 'intervention', 1] = freqs
  # dim1 :n_insecticides
  # dim2 :sex: m,f
  # dim3 :site: intervention,refuge
  # dim4 : generations
  # I checked this fills correctly for dims2 & 3 when freqs is a vector
  RAF[ , , , 1] = freqs
   
  return(RAF)  
}  

#' set_start_freqs_test allows starting allele frequencies to be hardcoded
#' 
#' 
#' @param n_insecticides number of insecticides, optional can just be specified by number of items in vector expo
#' @param max_gen maximum number of mosquito generations to run the simulation
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
                                  max_gen = 200,
                                   plot = FALSE)
{
  
  #set n_insecticides if it is not specified
  if ( is.null(n_insecticides)) n_insecticides <- 4
  
  #RAF stands for resistance allele frequency
  RAF <-      array_named(insecticide=1:n_insecticides, sex=c('m','f'), site=c('intervention','refugia'), gen=1:max_gen)
  
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
  
  
  return(RAF)
}