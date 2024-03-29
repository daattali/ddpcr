## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

#' Analysis step: Remove empty droplets
#'
#' Find the empty droplets (double-negative droplets) in each well in a plate
#' and assign these droplets to the \emph{EMPTY} cluster. \cr\cr
#' \href{https://github.com/daattali/ddpcr#advanced-topic-2-algorithms-used-in-each-step}{See the README} for
#' more information about the algorithm used to find empty droplets.
#'
#' This function is recommended to be run as part of an analysis pipeline (ie.
#' within the \code{\link[ddpcr]{analyze}} function) rather than being called
#' directly.
#'
#' @param plate A ddPCR plate.
#' @return A ddPCR plate with the empty droplets marked as empty. The plate's
#' metadata will have a few new variables relating to the empty droplets.
#' @seealso \code{\link[ddpcr]{analyze}}\cr
#' \code{\link[ddpcr]{get_empty_cutoff}}
#' @note This is an S3 generic, which means that different ddPCR plate types can
#' implement this function differently.
#' \href{https://github.com/daattali/ddpcr#advanced-topic-3-creating-new-plate-types}{See the README} for
#' more information on how to implement custom ddPCR plate types.
#' @export
#' @keywords internal
remove_empty <- function(plate) {
  UseMethod("remove_empty")
}

#' Analysis step: Remove empty droplets
#' @inheritParams remove_empty
#' @export
#' @keywords internal
remove_empty.ddpcr_plate <- function(plate) {
  CURRENT_STEP <- plate %>% step("REMOVE_EMPTY")
  plate %>% check_step(CURRENT_STEP)
  step_begin("Identifying empty droplets")

  # ---

  if (length(wells_success(plate)) > 0) {
    # get the empty cutoff of every well
    empty_cutoff_map <-
      vapply(
        wells_success(plate),
        function(x) get_empty_cutoff(plate, x),
        list("x", "y")
      ) %>%
      lol_to_df()

    # set the cluster to EMPTY for every empty droplet in every well
    x_var <- x_var(plate)
    y_var <- y_var(plate)
    CLUSTERS_UNANALYZED <- unanalyzed_clusters(plate, "EMPTY")

    do_work <- function(well_data) {
      well_id <- well_data[["well"]][1]

      if (!well_id %in% empty_cutoff_map[["well"]]) {
        return(well_data)
      }

      # find the cutoffs for this well and mark all droplets below as empty
      cutoffs <- empty_cutoff_map %>% dplyr::filter(.data[["well"]] == well_id)
      cutoff_x <- cutoffs[["x"]]
      cutoff_y <- cutoffs[["y"]]

      empty_idx <- well_data[["cluster"]] %in% CLUSTERS_UNANALYZED
      if (!is.na(cutoff_x)) {
        empty_idx <- empty_idx & well_data[[x_var]] < cutoff_x
      }
      if (!is.na(cutoff_y)) {
        empty_idx <- empty_idx & well_data[[y_var]] < cutoff_y
      }
      well_data[empty_idx, "cluster"] <- plate %>% cluster("EMPTY")
      well_data
    }

    data <-
      plate_data(plate) %>%
      dplyr::group_by(.data[["well"]]) %>%
      dplyr::do(do_work(.)) %>%
      dplyr::ungroup()

    # calculate a few metadata variables
    meta <-
      data %>%
      dplyr::filter(.data[["cluster"]] == cluster(plate, "EMPTY")) %>%
      dplyr::group_by(.data[["well"]]) %>%
      dplyr::summarise("drops_empty" = dplyr::n()) %>%
      merge_dfs_overwrite_col(plate_meta(plate), ., "drops_empty") %>%
      dplyr::mutate(
        "drops_non_empty" = .data[["drops"]] - .data[["drops_empty"]],
        "drops_empty_fraction" = signif(.data[["drops_empty"]] / .data[["drops"]], 3)
      )
    # ---

    # set the new drop data and metadata into the plate
    plate_data(plate) <- data
    plate_meta(plate) <- meta

    # now that we have information about empty droplets, calculate template concentrations
    plate %<>% calculate_concentration
  }

  status(plate) <- CURRENT_STEP
  step_end()

  plate
}

#' Get the cutoff for empty droplets in a well
#'
#' Calculate the cutoff thresholds that define which droplets are empty in a well.
#' @return A list with 2 elements named \code{x} and \code{y} with the values
#' being the cutoff in the corresponding axis.
#' @export
#' @keywords internal
get_empty_cutoff <- function(plate, well_id) {
  UseMethod("get_empty_cutoff")
}

#' Get the cutoff for empty droplets in a well
#' @export
#' @keywords internal
get_empty_cutoff.ddpcr_plate <- function(plate, well_id) {
  well_data <- get_single_well(plate, well_id, empty = TRUE)

  # fit two normal distributions in the data along the Y dimension
  set.seed(params(plate, "GENERAL", "RANDOM_SEED"))
  quiet(
    mixmdl_y <- mixtools::normalmixEM(well_data[[y_var(plate)]], k = 2)
  )

  # set the Y cutoff as the mean (mu) of the 1st component + k standard deviations
  smaller_comp_y <- mixmdl_y$mu %>% which.min()
  cutoff_y <-
    (mixmdl_y$mu[smaller_comp_y] +
      params(plate, "REMOVE_EMPTY", "CUTOFF_SD") * mixmdl_y$sigma[smaller_comp_y]
    ) %>%
    ceiling() %>%
    as.integer()

  # repeat the above along the X dimension
  set.seed(params(plate, "GENERAL", "RANDOM_SEED"))
  quiet(
    mixmdl_x <- mixtools::normalmixEM(well_data[[x_var(plate)]], k = 2)
  )
  smaller_comp_x <- mixmdl_x$mu %>% which.min()
  cutoff_x <-
    (mixmdl_x$mu[smaller_comp_x] +
      params(plate, "REMOVE_EMPTY", "CUTOFF_SD") * mixmdl_x$sigma[smaller_comp_x]
    ) %>%
    ceiling() %>%
    as.integer()

  return(list("x" = cutoff_x, "y" = cutoff_y))
}
