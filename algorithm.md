## empty cutoff:

We fit a mixture of 2 normal populations in the Y values of all the drops.
The lower population corresponds to the empty drops, which form a fairly
dense cluster. To capture the cutoff of empty drops, we simply assume that
the Y intensity of empty drops can be roughly modeled by a normal distribution.
More specifically, we set the threshold at 7 (PARAMS$EMPTY$CUTOFF_SD)
standard deviations above the center of the distribution. Then repeat for X dimension
(only one of the dimensions if we know which one to expect)


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