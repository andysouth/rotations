#' read in a file specifying the input ranges for an experiment (series of runs)
#'
#' have a default file, be able to save files to reproduce experiments
#' 
#' @param file name of the text file containing the experiment ranges
#' @param nscenarios number of runs, set here and not in input file
#' @param multi whether to read in a multi experiment excel file
#' @param id_expt if a multi experiment file which id_expt to use (first two chars of the column name)
#' 
#' @examples 
#' #single experiment
#' inex <- read_in_expt(nscenarios = 10, multi=FALSE)
#' 
#' #multi experiment
#' #filename of multiexpt file currently hardcoded into function
#' inex <- read_in_expt(nscenarios = 10, multi=TRUE, id_expt='A4')
#' 
# @importFrom readxl read_excel 
#' 
#' @return  
#' @export


read_in_expt <- function( file = NULL,
                          nscenarios = 10,
                          multi = FALSE,
                          id_expt = 'A2')
{


  if (multi)
  {
    infile_multi <- 'rotations-paper-inputs-201910.xlsx'
    
    infile_multi <- system.file("extdata", infile_multi, package = "rotations", mustWork = TRUE)
    
    #read excel file
    inex_multi <- readxl::read_excel(infile_multi)
    #get just names column and 1 column containing inputs for this experiment id
    #specified by first two chars of the column name
    inex_one <- dplyr::select(inex_multi,1, dplyr::starts_with(paste0(id_expt)))
    
    
  } else
  {
    # single experiment file
    # have a default file
    if (is.null(file)) file <- system.file("extdata", "_in_expt_rotations_default.csv", package = "rotations", mustWork = TRUE)
    # or file in the extdata folder
    else file <- system.file("extdata", file, package = "rotations", mustWork = TRUE)
    # TODO allow file to be in other specified location too, i.e. accept a path
    
    # function to read input file with experiment specifications
    
    #read scenario input (single column)
    inex_one <- read.csv(file)    
  }
  

  
  #transpose
  tmp2 <- t(inex_one)
  
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
