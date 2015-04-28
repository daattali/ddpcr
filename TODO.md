- add lol_to_df and merge_dfs_overwrite_col to rsalad
- verify that AGPL-3 license is good


- cleanup code inside failures.R and decide on a method (fast vs slow) and make it work with generic inheritance

- use modifyList to modify params and maybe allow passing in extra params in init function
- use modifyList in define_params


CURRENT
- make plot work with ppnp_assay
- make plot colours more general?


- see if I can use more efficient cumsum method for finding indices of where sequence changes: function(v){which(c(TRUE,diff(v)!=0))}
