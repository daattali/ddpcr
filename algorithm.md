
## failure:

The goal here is to see if there is anything that is clearly wrong with the
data in the well.  First, I ensure enough drops were loaded (BioRad claims 
20k, we see 12k-17k usually) - less than 5000 (PARAMS$REMOVE_FAILURES$TOTAL_DROPS_T)
is considered a failed run. Next I make a few basic sanity checks about the 
general look of the data in clusters.
If we were to normalize all HEX and FAM values to be between 0-1, we'd expect to
always have a very large cluster of empty drops near (0,0), a cluster of
wild-type drops near (1,1), and possibly a cluster of mutant drops neat (0,1).
Using these assumptions on the data, we make the following three heuristic
checks after fitting a mixture of 2 normal distributions in the FAM values
of all the drops:
  - The centers (mu's) of the two populations need to be far enough (this
    if checked by seeing if the larger center is at least twice the value of the
    lower center.  If it is not, that usually indicates that both normal populations
    are really the same cluster)
  - The lower population cannot have too few drops (lambda < 0.3)
    (PARAMS$REMOVE_FAILURES$EMPTY_LAMBDA_LOW_T), as that means there is no
    empty cluster.  Likewise, there shouldn't be too many drops (lambda > 0.99)
    (PARAMS$REMOVE_FAILURES$EMPTY_LAMBDA_HIGH_T), as that means there are not
    enough drops with daTA
    

## outliers:

The idea borrows from outlier detection in normal populations (looking for
points that are further than k*IQR from the 1st/3rd quartiles), but since
our data is highly skewed and non-normal, I use a small tweak.
For each dimension ([FAM, HEX]): get the 1% (PARAMS$OUTLIERS$TOP_PERCENT) of
drops that have the highest value in that dimension.  Calculate the IQR of the
values only within these drops. Mark the outlier cutoff as the 3rd quantile
plus 5 (PARAMS$OUTLIERS$CUTOFF_IQR) IQR
Get the cutoff for outliers


## empty cutoff:

We fit a mixture of 2 normal populations in the Y values of all the drops.
The lower population corresponds to the empty drops, which form a fairly
dense cluster. To capture the cutoff of empty drops, we simply assume that
the Y intensity of empty drops can be roughly modeled by a normal distribution.
More specifically, we set the threshold at 7 (PARAMS$EMPTY$CUTOFF_SD)
standard deviations above the center of the distribution. Then repeat for X dimension
(only one of the dimensions if we know which one to expect)


## classify:



## mutant vs wildtype

We can call a well as mutant if it is statistically significantly more than
1% with a p-val < 0.01. For example, if there are 500 total drops and 7
mutant drops, then the mutant frequency is 1.4%, but is it statistically
significantly more than 1%?
P(x >= 7)
  = 1 - P(x <= 7) + P(x = 7)
  = 1 - pbinom(7, 500, .01) + dbinom(7, 500, .01)
  = 0.237
  > 0.01
  
So not statistically significantly enough, so we say it's a wildtype well.
But if there are 5000 drops and 70 mutant drops (same 1.4% frequency but
with higher absolute numbers), then
P(x >= 70) = 1 - pbinom(70, 5000, .01) + dbinom(70, 5000, .01) = 0.004
So this is indeed significant, and this well would be deemed mutant.

## reclassify:

wells without a significant mutant cluster have very few (if any) mutant
drops, so it's difficult and highly variable to identify them.  We can try
to leverage data from wells with many mutant drops to get a good idea of
where mutant drops should be found in a low mutant frequency well.  However,
this is only possible if there is enough data to learn from - ie. if there are 
less than 4 (PARAMS$RECLASSIFY_LOW_MT$MIN_WELLS_MT_CLUSTER) wells with 
high mutant frequency, then we skip this step.  
For every well with a high mutant frequency, we want to know where the
mutant cluster is relative to the wild-type cluster. To do this, 
we look at where the mutant cluster right-most (HEX) border is and
where the right-most wild-type drop is for every high mutant frequency well.
We calculate the ratio of the mutant border HEX over the largest wild-type HEX,
and call this the mutant-to-wildtype-border ratio.  In the wild-type cluster,
we decide to measure the highest HEX value of a drop instead of the actual border
because many times the right border is outside the range of the plot and is
less informative.
After calculating this ratio for all high mutant frequency wells, we
choose a single value to use as a "consensus" mutant-to-wildtype-border-ratio.
Instead of taking the mean or median, we take the the 3rd quartile
(PARAMS$RECLASSIFY_LOW_MT$BORDER_RATIO_QUANTILE) in order to be more
sensitive and not lose too many mutant drops.
After obtaining the consensus mutant-to-wildtype-border-ratio, we can
look at every low mutant frequency well, and based on the largest wild-type
HEX in each well, we can use the ratio to determine where to draw the mutant
cluster border.  In the FAM dimension, we make the FAM borders for
for the mutant cluster the same as the borders for the wild-type cluster.
With these new mutant cluster borders in place, we first reclassify all 
the previously assigned mutant drops as rain, and then assign the mutant
label to any drop that falls inside the new borders.