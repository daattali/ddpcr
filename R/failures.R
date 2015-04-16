

# Determine if a well had a successful ddPCR run
#
# Args:
#   .wellData: The dataframe containing all the droplets
#   .well: The id of the well of interest
#   .plot: If true, plot the result (used mainly for development/debugging)
#
# Returns:
#   list:
#     result: TRUE if the well had a successful run, FALSE otherwise
#     comment: the reason the well was deemed unsuccessful, or NA for successful wells
#
# Algorithm:
#   The goal here is to see if there is anything that is clearly wrong with the
#   data in the well.  First, I ensure enough drops were loaded (BioRad claims 
#   20k, we see 12k-17k usually) - less than 5000 (PARAMS$WELLSUCCESS$TOTAL_DROPS_T)
#   is considered a failed run. Next I make a few basic sanity checks about the 
#   general look of the data in clusters.
#   If we were to normalize all HEX and FAM values to be between 0-1, we'd expect to
#   always have a very large cluster of empty drops near (0,0), a cluster of
#   wild-type drops near (1,1), and possibly a cluster of mutant drops neat (0,1).
#   Using these assumptions on the data, we make the following three heuristic
#   checks after fitting a mixture of 2 normal distributions in the FAM values
#   of all the drops:
#     - The centers (mu's) of the two populations need to be far enough (this
#       if checked by seeing if the larger center is at least twice the value of the
#       lower center.  If it is not, that usually indicates that both normal populations
#       are really the same cluster)
#     - The lower population cannot have too few drops (lambda < 0.3)
#       (PARAMS$WELLSUCCESS$NORMAL_LAMBDA_LOW_T), as that means there is no
#       empty cluster.  Likewise, there shouldn't be too many drops (lambda > 0.99)
#       (PARAMS$WELLSUCCESS$NORMAL_LAMBDA_HIGH_T), as that means there are not
#       enough drops with data
#     - The standard deviation (sigma) of the lower population should be fairly
#       small, below 200 (PARAMS$WELLSUCCESS$NORMAL_SIGMA_T), to ensure that
#       the empty cluster is indeed very dense as it should be
is_well_success <- function(plate, well_id) {
  params <- params(plate)

  well_data <- get_single_well(plate, well_id, empty = TRUE)

  # if this well doesn't actually have data (or is an invalid well) return NA
  if (nrow(well_data) == 0) {
    return(list(result = NA, comment = NA))
  }
  
  # First heuristic check: make sure there are enough droplets
  if (nrow(well_data) < params[['WELLSUCCESS']][['TOTAL_DROPS_T']]) {
    success <- FALSE
    msg <- sprintf("Not enough drops generated (%s)", nrow(well_data))
    return(list(result = success, comment = msg))
  }
  
  # fit two normal distributions in the data along the FAM dimension, check:
  # - the mu's of the two populations needs to be far enough
  # - bottom population needs to have lambda not too small and not too large
  # - sigma of bottom population should be fairly small
  set.seed(SEED)
  quiet(
    mixmdl_fam <- mixtools::normalmixEM(well_data[['FAM']], k = 2))
  smaller_comp_fam <- mixmdl_fam$mu %>% which.min
  larger_comp_fam <- mixmdl_fam$mu %>% which.max
  
  if ((mixmdl_fam$mu %>% diff %>% abs) < min(mixmdl_fam$mu)) {
    success <- FALSE
    msg <- paste0("There seems to be mostly empty drops (mu's of FAM normals: ",
                  paste0(round(mixmdl_fam$mu), collapse = " "), ")")
    return(list(result = success, comment = msg))
  }
  
  if (mixmdl_fam$lambda[smaller_comp_fam] < params[['WELLSUCCESS']][['NORMAL_LAMBDA_LOW_T']]) {
    success <- FALSE
    msg <- paste0("Could not find significant empty cluster (lambda of FAM normal: ",
                  signif(mixmdl_fam$lambda[smaller_comp_fam], 4), ")")
    return(list(result = success, comment = msg))
  }
  
  if (mixmdl_fam$lambda[smaller_comp_fam] > params[['WELLSUCCESS']][['NORMAL_LAMBDA_HIGH_T']]) {
    success <- FALSE
    msg <- paste0("There are too many empty drops (lambda of FAM normal: ",
                  signif(mixmdl_fam$lambda[smaller_comp_fam], 4), ")")
    return(list(result = success, comment = msg))
  }
  
  if (mixmdl_fam$sigma[smaller_comp_fam] > params[['WELLSUCCESS']][['NORMAL_SIGMA_T']]) {
    success <- FALSE
    msg <- paste0("Could not find a dense empty cluster (sigma of FAM normal: ",
                  round(mixmdl_fam$sigma[smaller_comp_fam]), ")")
    return(list(result = success, comment = msg))
  }    
  
  # if all the sanity checks passed, the run was successful
  return(list(result = TRUE, comment = NA))  
}

#' @export
remove_failures <- function(plate) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  stopifnot(plate %>% status >= STATUS_OUTLIERS_REMOVED)
  
  tstart <- proc.time()
  
  # ---
  
  well_success_map <-
    vapply(wells_used(plate),            # check every well if it failed
           function(x) is_well_success(plate, x),
           list(logical(1), character(1))) %>%
    lol_to_df %>%
    dplyr::rename_(.dots = setNames(     # rename result to success
      "result", "success"))

  meta <-
    merge_dfs_overwrite_col(plate_meta(plate), well_success_map,
                            c("success", "comment")) %>%
    dplyr::arrange_(~ desc(used), ~ desc(success), ~ row, ~ col)
  
  # ---
  
  plate_meta(plate) <- meta
  status(plate) <- STATUS_FAILED_REMOVED
  
  tend <- proc.time()
  message(sprintf("Time to find unsuccessful wells: %s seconds",
                  round(tend-tstart)[1]))
  
  plate
}
