#' read in a file specifying the input ranges for an experiment (series of runs)
#'
#' have a default file, be able to save files to reproduce experiments
#' 
#' @param file name of the text file containing the experiment ranges
#' @param nscenarios number of runs, set here and not in input file
#' 
# @examples 
#' 
#' @return  
#' @export


read_in_expt <- function( file = NULL,
                          nscenarios = 10)
{


  # have a default file
  if (is.null(file)) file <- system.file("extdata", "_in_expt_rotations_default.csv", package = "rotations", mustWork = TRUE)
  # or file in the extdata folder
  else file <- system.file("extdata", file, package = "rotations", mustWork = TRUE)
  # TODO allow file to be in other specified location too, i.e. accept a path
    
  # function to read input file with experiment specifications
  
  #read scenario input (single column)
  tmp <- read.csv(file)
  
  #transpose
  tmp2 <- t(tmp)
  
  #set variable names from the first row
  colnames(tmp2) <- tmp2[1,]
  
  #convert to dataframe
  tmp3 <- as.data.frame(tmp2, stringsAsFactors = F)
  
  #remove rows of names and comments
  tmp4 <- tmp3[2,]
  
  #convert variables to numeric
  #except named character variables
  #tmp5 <- dplyr::mutate_all(tmp4, ~as.numeric(as.character(.))) 
  tmp5 <- dplyr::mutate_at(tmp4,vars(-mort_or_freq), ~as.numeric(as.character(.)))
  
  #dplyr other options
  #df %>% mutate_if(is.factor, ~as.numeric(as.character(.)))
  # by specific columns:
  #df %>%   mutate_at(vars(x, y, z), ~as.numeric(as.character(.))) 
  
  tmp5$nscenarios <- nscenarios
  
  tmp5

}
