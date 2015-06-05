### Chip

- What LOD are you trying to claim? In theory, ddpcr can detect 1/20k or 0.005%. But that assumes that all 20k droplets are informative, let's assume that only 10% of drops are filled, that lieaves us with 2k droplets so minimum detection would be 0.05%. That's the theoretical absolute best of ddpcr if the assay and biology worked perfectly. I doubt you can claim this as the assay's LOD


### code

- if(num_peaks > 1) {message(dens_smooth$x[maxima_idx][1]/dens_smooth$x[maxima_idx][2])}

- cleanup code inside failures.R and decide on a method (fast vs slow) and make it work with generic inheritance

- do a bit of time/memory profiling with hadley::lineprof  

- parallelize some time consuming steps

- separate functions into proper files

- add tests

- add documentation



### stats

- matias (stats prof) - better stats for outlier detection?

- determining if a significant mutant cluster exists: use a more statistic method.
for example, use a binomial/chi square test. we can call a well as mutant if it is
statistically significantly more then 1% with a p-val < 0.01. For example, if 500
drops and 8 mutants, then we want P(x>=8) = 1-P(x<=8)+P(x=8) =
1-pbinom(8, 500, .01)+dbinom(8, 500, .01) = 0.13 so not significantly many mutants,
therefore wildtype.  But if same ratio but with 80 mutants out of 5000, we get
p=5e-05 which is very significant so well is mutant.

- bootstrap to determine if wildtype well or not (assume well is truly wildtype,
sample from the wt drops distribution and see how likely it is to get the few mutant
drops that we got)

- I tested for normality of wildtype drops HEX using shapiro-wilks and it always
comes back as significantly non-normal :(

- preliminary attempt at bootstrap using Normal assumption: it seems that if we
take the wildtype drops, use the mean and sd to see what's the probability of
seeing observations as extreme as the mutant drops, we'll always get an extremely
small p value (suggesting that even one mutant drop is significant)


### shiny

- about tab