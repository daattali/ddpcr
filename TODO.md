- add lol_to_df and merge_dfs_overwrite_col to rsalad
- add AGPL-3 license and one-line at each file:
## Copyright (C) 2015 Dean Attali
## This software is distributed under the AGPL-3 license

- if(num_peaks > 1) {message(dens_smooth$x[maxima_idx][1]/dens_smooth$x[maxima_idx][2])}

- cleanup code inside failures.R and decide on a method (fast vs slow) and make it work with generic inheritance

- use modifyList to modify params and maybe allow passing in extra params in init function
- use modifyList in define_params
- params for each step should have the name of the step in the param name


CURRENT
- make plot work with ppnp_assay
- make plot colours more general?
- make plot for ppnp_assay (or wtnegbraf) andd for crosshair_thresholds

- see if I can use more efficient cumsum method for finding indices of where sequence changes: function(v){which(c(TRUE,diff(v)!=0))}
