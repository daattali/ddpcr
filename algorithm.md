## empty cutoff:

We fit a mixture of 2 normal populations in the Y values of all the drops.
The lower population corresponds to the empty drops, which form a fairly
dense cluster. To capture the cutoff of empty drops, we simply assume that
the Y intensity of empty drops can be roughly modeled by a normal distribution.
More specifically, we set the threshold at 7 (PARAMS$EMPTY$CUTOFF_SD)
standard deviations above the center of the distribution. Then repeat for X dimension
(only one of the dimensions if we know which one to expect)


