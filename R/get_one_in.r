#' get inputs for a single named scenario from a multi-scenario object
#'
#' 
#' @param linmulti list of multiple scenario inputs
#' @param scen_num scenario number
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
                        scen_num )
{
  
  # have a default file
  if (is.null(linmulti)) linmulti <- set_run_inputs()
  
  
  linputs <- purrr::map(linmulti, scen_num)
  
  #converting multiple insecticide inputs back from strings
  #have to ignore the ones that aren't strings
  
  #note the space after the comma in split=", "
  linputs <- lapply(linputs, function(x) if (!grepl(",",x)) x else
    as.numeric(unlist(strsplit(as.character(x),split=", "))))
  
}
