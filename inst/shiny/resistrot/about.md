A model to investigate the evolution of insecticide resistance in rotations and sequences.

Andy South (@southmapr) and Ian Hastings [2018]

**To use**
* Select 'UI'  
* Modify inputs using the sliders for each scenario and insecticide
* Press the 'Run ...' buttons to replot either scenario 

The curves on the plots show the change in frequency of the alleles giving resistance to the different insecticides. The shaded boxes show when the insecticide is in use.

Setting 'rotation interval' to 0 specifies a sequence where each insecticide is used until a resistance frequency threshold is reached. 

The inputs that can be changed are :

Model Input      | Description
------------------------- | ----------------------------------------------------
1. Start Freq.  | starting frequency of this resistance allele in the population
2. Exposure | proportion of insects exposed to insecticide
3. Effectiveness | proportion of susceptible (SS) insects killed by exposure to insecticide
4. R. restoration  | Resistance restoration, ability of resistance (RR) to restore fitness of insects exposed to insecticide
5. Dominance of r. | Dominance of resistance, sets fitness of heterozygous (SR) insects between that of SS & RR in presence of insecticide
6. Cost  |  reduction in fitness of resistant (RR) insects in absence of insecticide
7. Dominance of cost  | sets fitness of heterozygous (SR) insects between that of SS & RR in absence of insecticide   
8. n. insecticides  |  number of insecticides in rotation or sequence
9. rotation interval  |  how many generations to use an insecticide for before changing, set to 0 for sequential strategy where insecticide is changed when it reaches resistance threshold
10. coverage  |  proportion of area that is treated. With coverage=1 whole area is treated. When coverage < 1 an untreated area is also represented.
11. migration  |  if coverage < 1, exchange between treated and untreated areas. 1 = random mixing, 0 = no exchange.

Advanced options :
no_r_below_start : allows turning off stopping resistance frequencies from going below their starting values
min insecticide interval : a minimum number of generations for insecticide use, to stop rapid switching between insecticides

The code is hosted here : https://github.com/ian-hastings/rotations




