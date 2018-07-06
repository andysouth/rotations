#' calculate resistance allele frequency at mutation-selection balance
#' 
#' @param mutation mutation rate
#' @param cost cost of resistance
#' @param dom_cos dominance of cost
#' 
#' @examples 
#' mutn_seln_bal()

#' @return resistance frequency at balance
#' @export

mutn_seln_bal <- function ( mutation = 1e-9,
                            cost = 0.1,
                            dom_cos = 0 )
{
  
  # q=u/(s*h)
  # 
  # where q is freq at mutation/selection balance
  # s= selection coefficient
  # h=dominance
  # i.e. fitnesses of SS, SR and RR are 1, 1-hs*s and 1-s
  # 
  # assuming a single codon change encodes resistance the rough figure for this mutation rate 
  # at a single codon is 1e-9 per generations. 
  # Yu could enter the values of h and s from the runs and just see how low the equilibrium is!!
  
  #from wikipedia
  #mutated deleterious has a small relative fitness disadvantage of s. 
  #In the case of complete dominance h=0, 
  #deleterious alleles are only removed by selection on BB homozygotes.
  #frequency of deleterious alleles is q=sqrt(u/s)
  #This equilibrium frequency is potentially substantially larger than for the case of partial dominance, because a large number of mutant alleles are carried in heterozygotes and are shielded from selection.  
  
  #in rotations the relevant inputs are cost and dom_cos because we are talking about balance in absence of insecticide
  #how do I convert cost to the selection_coeff ?
  
  #seems that our input 'cost' in rotations is equivalent to the selection disadvantage
  #i.e. the relative fitness disadvantage of the mutation
  
  # wikipedia says that when h=0 selection only acts on homozygous mutants
  # that is the same as out dominance of cost
  
  # I imagine there is a way of avoiding the if statement and getting 
  # one eq. to work for dominance 0 and >0 
  
  if (dom_cos == 0)
  {
    resist_freqs <- sqrt(mutation / cost )       
  } else
  {
    resist_freqs <- mutation / (cost * dom_cos)    
  }

  
  return(resist_freqs)
}


