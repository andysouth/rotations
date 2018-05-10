results.v<-matrix(1:300, ncol = 3)


c<-0.05; #fitness cost of the RR genotype in absence of insecticide
h_c<-0.95; #dominance of cost

#may later need to couch selective advantage in terms of rr_resoration, proportion treated in intervention site  etc
s<-0.1; #selective advantage of RR genotype
h_s<-0.05; # dominance of selective advantage

#s<-0.05; #selective advantage of RR genotype
#h_s<-0.95; # dominance of selective advantage

for (ii in 1:100 ) {

freq=ii/100;

#now need weighted averages of fitness NB we assume Hardy-Weinburg which is only an approximation under selection
pop_size=1000# multiply by pop-size to make things explicit...not necessary as cancels out
no_in_hetero= (pop_size*2)*(2*freq*(1-freq))  #first term is number of alelles (multiply pop_size by 2 for diploidY; second term is Hardy-Weinberg proportions)
no_in_homo= (pop_size*2)*(freq*freq)*2        # final multiplication by 2 because there are two R alleles in the homozygote

prop_hetero=no_in_hetero/(no_in_hetero+no_in_homo)   #the proportion of R allelels in heterozygotes
prop_homo=1-prop_hetero     #the proportion of R allelels in RR homozygotes

fit_absence=prop_hetero*(1-h_c*c) + prop_homo*(1-c)  
cost=1-fit_absence;
fitness = prop_hetero*(1+h_s*s) + prop_homo*(1+s)
selection=fitness-1

results.v[ii, 1]=freq
results.v[ii, 2]=cost
results.v[ii, 3]=selection

}

matplot(x = results.v[,1], y = results.v[,2:3], type="l",lty=1,xlab='Frequency of R allele',ylab='value')


# Blackline=cost, red line is selective advantage.
 
# To include in the figure caption of the ms : ?These calculations are based on Hardy-Weinberg proportions of genotypes. This will be untrue when the locus is under selection (as in this case) but serves as an approximation to illustrate the general effect?

# We can have the lines crossing which looks good but the obvious question then is ?if costs are greater than the advantage then surely resistance will never spread?. It may therefore be better to have selection always higher than cost and state the distance between the two lines (which grows as frequency increases) is a measure of overall selection. Actually we may have two panels. One like this, and one where the lines do cross?the second panel  may explain Andy?s observations that in some cases resistance never spreads if rotations are deployed ? the explanation is that costs and greater then advantage in some parameter space and rotations keep them there
 
 
# IH technical point STILL TO DO although it won?t make any great difference?its just for my peace of mind . The problem with ?allele counting? as below is that sensitive alleles also get a fitness >1 because of their presence in heterozygotes. May therefore have to scale the fitness of R by fitness of S. This does not arise when comparing genotypes (rather than alleles) because all fitness are scaled by the SS genotype.


# The method assumes Hardy-Weinburg (HW) equilibrium of genotypes. Textbooks warn us this is not the case when selection is occurring (as here, for IR) but we are still Ok because the calculation is done before selection moves genotypes away from H-W i.e. calculates genotypic frequencies immediately after random union of gametes. BUT we therefore have to assume there is equal frequency of alleles in both sexes i.e. that there is no differential selection on the male and female parents? to see why, consider the limiting case that all females are exposed to insecticide and only RR genotypes survive, while all males are unexposed and fitness costs only allow SS to survive?. overall allele frequency would be 50% in the population but only heterozygotes would be formed.