#' get inputs for a single named scenario from a multi-scenario object
#'
#' 
#' @param linmulti list of multiple scenario inputs
#' @param scen_num scenario number
#' @param convert_diff_insecticide_strings whether to convert strings for different insecticides into vectors for an individual run, default is TRUE, by setting to FALSE can save inputs as a single row in a datframe
#' 
#' 
#' @examples 
#' linmulti <- set_run_inputs()
#' 
#' linputs <- get_one_in(linmulti, scen_num=2)
#' # run one scenario
#' dfres <- do.call(run_rot, linputs)
#' 
#' @return a list of inputs for one scenario  
#' @export


get_one_in <- function( linmulti = NULL,
                        scen_num,
                        convert_diff_insecticide_strings = TRUE )
{
  
  # have a default file
  if (is.null(linmulti)) linmulti <- set_run_inputs()
  
  linputs <- purrr::map(linmulti, scen_num)
  
  # converting multiple insecticide inputs back from strings
  # just converts those that contain ', '
  # (so mort_or_freq which is a string is not converted)
  
  if (convert_diff_insecticide_strings)
  {
    #note the space after the comma in split=", "
    linputs <- lapply(linputs, function(x) if (!grepl(",",x)) x else
      as.numeric(unlist(strsplit(as.character(x),split=", "))))    
  }

  
  # to make sure it gets returned
  linputs
  
}
