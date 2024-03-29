## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

#' Subsetting a ddPCR plate
#'
#' Select specific wells or samples from a ddPCR plate.
#'
#' Keeps only data from the selected wells. If sample names are provided instead
#' of well IDs, then any well corresponding to any of the sample names will be
#' kept. Either well IDs or sample names must be provided, but not both.
#'
#' @section Range notation:
#' The most basic way to select wells is to provide a vector of wells such as
#' \code{c("B03", "C12")}. When selecting wells, a special range notation is
#' supported to make it easier to select many wells: use a colon (\code{:}) to specify a
#' range of wells, and use a comma (\code{,}) to add another well or range. When
#' specifying a range, all wells in the rectangular area between the two wells
#' are selected. For example, \code{B04:D06} is equivalent to
#' \code{B04, B05, A05, C04, C05, C06, D04, D05, D06}. You can combine multiple
#' ranges in one selection; see the Examples section below. Note that this
#' notation is only supported for the \code{wells} parameter, but not for the
#' \code{samples} parameter.
#'
#' @param x The ddPCR plate to subset from.
#' @param wells Vector or range notation of wells to select (see Range Notation
#' section for more information).
#' @param samples Vector of sample names to select.
#' @param targets_ch1 Vector of target names in channel 1 to select.
#' @param targets_ch2 Vector of target names in channel 2 to select.
#' @param ... Ignored
#' @return Plate with data only from the specified wells/samples.
#' @examples
#' plate <- new_plate(sample_data_dir())
#' plate %>% wells_used
#' plate %>% subset("C01") %>% wells_used
#' plate %>% subset(c("C01", "F05")) %>% wells_used
#' plate %>% subset("C01, F05") %>% wells_used
#' plate %>% subset("C01:F05") %>% wells_used
#' plate %>% subset("C01:F05, A01") %>% wells_used
#' plate %>% subset("A01:C03") %>% wells_used
#' plate %>% subset("A01:C05") %>% wells_used
#' plate %>% subset("A01, A05:F05") %>% wells_used
#' plate %>% subset("A01, A05:C05, F05") %>% wells_used
#' plate %>% subset("A01:A05, C01:C05, F05") %>% wells_used
#' plate %>% subset(samples = "Dean") %>% wells_used
#' plate %>% subset(samples = c("Dean", "Mike")) %>% wells_used
#' @export
subset.ddpcr_plate <- function(x, wells, samples,
                               targets_ch1, targets_ch2, ...) {
  if (!missing(wells) && !missing(samples)) {
    err_msg("Can only subset by either `wells` or `samples`, not both")
  }

  # figure out what wells to keep
  if (!missing(wells) && !is.null(wells) && all(nzchar(wells))) {
    wells %<>% paste(collapse = ",")
    wells %<>% toupper
    wells %<>% range_list_to_vec
  } else if (!missing(samples)) {
    wells <-
      plate_meta(x) %>%
      dplyr::filter(.data[["sample"]] %in% samples) %>%
      .[['well']]
  } else if (!missing(targets_ch1) || !missing(targets_ch2)) {
    wells <-
      plate_meta(x) %>%
      .[['well']]
  } else {
    return(x)  # if no arguments, just return the same plate
  }

  if (!missing(targets_ch1)) {
    wells_from_targets <-
      plate_meta(x) %>%
      dplyr::filter(.data[["target_ch1"]] %in% targets_ch1) %>%
      .[['well']]

    wells <- wells[wells %in% wells_from_targets]
  }

  if (!missing(targets_ch2)) {
    wells_from_targets <-
      plate_meta(x) %>%
      dplyr::filter(.data[["target_ch2"]] %in% targets_ch2) %>%
      .[['well']]

    wells <- wells[wells %in% wells_from_targets]
  }

  # if no valid wells were given, don't do anything
  if (sum(wells %in% (x %>% wells_used)) == 0) {
    warn_msg("No valid wells selected. Returning plate unchanged.")
    return(x)
  }

  # keep only the droplet data for these wells
  plate_data(x) %<>%
    dplyr::filter(.data[["well"]] %in% wells)

  # mark any other well as unused
  plate_meta(x) %<>%
    dplyr::filter(.data[["well"]] %in% wells) %>%
    merge_dfs_overwrite_col(DEFAULT_PLATE_META, .) %>%
    arrange_meta()

  x
}

#' Is the given parameter a range?
#' @examples
#' is_range("C05")            # FALSE
#' is_range(c("C05", "F05"))  # FALSE
#' is_range("C05")            # FALSE
#' is_range("C05, F05")       # TRUE
#' is_range("C05:F05")        # TRUE
#' is_range("C05.F05")        # FALSE
#' @keywords internal
#' @export
is_range <- function(x) {
  length(x) == 1 && grepl("[,:]", x)
}

#' Convert a list of ranges to a vector of its individual components
#' @examples
#' range_list_to_vec("A01")
#' range_list_to_vec("A01:A04")
#' range_list_to_vec("A01, B03")
#' range_list_to_vec("A01, B02:C04, C07")
#' @keywords internal
#' @export
range_list_to_vec <- function(rangel) {
  rangel <- gsub("[[:space:]]", "", rangel)
  ranges <- strsplit(rangel, ",") %>% unlist %>% .[. != ""]
  if (length(ranges) == 0) {
    return(NULL)
  }

  wells <-
    lapply(ranges, function(range) {
      endpoints <- range_to_endpoints(range)
      get_wells_btwn(endpoints[1], endpoints[2])
    }) %>%
    unlist %>%
    unique %>%
    sort
  wells
}

#' regex for a well ID
#' @keywords internal
#' @export
WELL_ID_REGEX <- "^[A-H]([0-1])?[0-9]$"

#' Extract the two endpoints of a range
#' @examples
#' range_to_endpoints("B05:G09")   # c("B05", "G09")
#' range_to_endpoints("B05")       # c("B05", "B05")
#' @keywords internal
#' @export
range_to_endpoints <- function(range) {
  endpoints <- strsplit(range, ":") %>% unlist
  if (endpoints %>% length == 1) {
    endpoints <- c(endpoints, endpoints)
  }

  if (!grepl(WELL_ID_REGEX, endpoints) %>% all) {
    err_msg("Invalid wells given to `subset`")
  }
  if (endpoints %>% length != 2) {
    err_msg("Invalid range given to `subset`")
  }
  endpoints
}

#' Convert a range to a vector of all elements between the endpoints
#' @examples
#' range_to_seq(c(5, 8))   # 5:8
#' range_to_seq(c(8, 5))   # 5:8
#' @keywords internal
#' @export
range_to_seq <- function(rng) {
  seq(min(rng), max(rng))
}

#' Get all wells between two wells (assume a rectangle layout)
#' @examples
#' get_wells_btwn("C04", "D06")
#' @keywords internal
#' @export
get_wells_btwn <- function(well1, well2) {
  rows <-
    get_row(c(well1, well2)) %>%
    row_to_num %>%
    range_to_seq %>%
    num_to_row

  cols <-
    get_col(c(well1, well2)) %>%
    col_to_num %>%
    range_to_seq %>%
    num_to_col

  wells <- lapply(rows, function(x) paste(x, cols, sep = "")) %>% unlist
  wells
}

#' Convert a plate row to a number
#' @examples
#' row_to_num("D")  # 4L
#' @keywords internal
#' @export
row_to_num <- function(row) {
  magrittr::is_in(LETTERS, row) %>% which
}

#' Convert a number to plate row
#' @examples
#' num_to_row(4)  # "D"
#' @keywords internal
#' @export
num_to_row <- function(num) {
  LETTERS[num]
}

#' Get row from well ID
#' @examples
#' get_row("C05" )  # "C"
#' @keywords internal
#' @export
get_row <- function(well) {
  substring(well, 1, 1)
}

#' Convert a plate column to a number
#' @examples
#' col_to_num("05")  # 5L
#' @keywords internal
#' @export
col_to_num <- function(col) {
  col %>% as.integer
}

#' Convert a number to plate column
#' @examples
#' num_to_col(5)  # "05"
#' @keywords internal
#' @export
num_to_col <- function(num) {
  sprintf("%02d", num)
}

#' Get column from well ID
#' @examples
#' get_col("C05" )  # "05"
#' @keywords internal
#' @export
get_col <- function(well) {
  substring(well, 2, 3)
}
