### Chip

- What LOD are you trying to claim? In theory, ddpcr can detect 1/20k or 0.005%. But that assumes that all 20k droplets are informative, let's assume that only 10% of drops are filled, that lieaves us with 2k droplets so minimum detection would be 0.05%. That's the theoretical absolute best of ddpcr if the assay and biology worked perfectly. I doubt you can claim this as the assay's LOD


### code

- if(num_peaks > 1) {message(dens_smooth$x[maxima_idx][1]/dens_smooth$x[maxima_idx][2])}

- cleanup code inside failures.R and decide on a method (fast vs slow) and make it work with generic inheritance

- separate functions into proper files

- add tests

- add documentation

- chip: can i add sample data from crc 1-41 patients and from cellline data?



### stats

- matias (stats prof) - better stats for outlier detection?

- determining if a significant mutant cluster exists: using jenny's initial idea of bootstrap using Normal assumption: assume well is truly wildtype, sample from wt drops and see how likely it is to get the number of mutant drops we see. This doesn't work well because we ALWAYS get an extremely small p value - even having just one mutant drop seems to be very significant. Also, according to shapiro-wilks, the wildtype drops aren't normally distributed.

- determining if a significant mutant cluster exists: use a more statistic method.
for example, use a binomial/chi square test. we can call a well as mutant if it is
statistically significantly more then 1% with a p-val < 0.01. For example, if 500
drops and 8 mutants, then we want P(x>=8) = 1-P(x<=8)+P(x=8) =
1-pbinom(8, 500, .01)+dbinom(8, 500, .01) = 0.13 so not significantly many mutants,
therefore wildtype.  But if same ratio but with 80 mutants out of 5000, we get
p=5e-05 which is very significant so well is mutant.

### shiny

- about tab
