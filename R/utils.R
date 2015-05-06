## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali
## This software is distributed under the AGPL-3 license

err_msg <- function(x) {
  stop(sprintf("ddpcr: %s", x), call. = FALSE)
}

warn_msg <- function(x) {
  warning(sprintf("ddpcr: %s", x), call. = FALSE)
}

cat0 <- function(...) {
  cat(..., sep = "")
}

"%btwn%" <- function(x, rng) {
  stopifnot(is.numeric(x), is.numeric(rng), length(rng) == 2)
  x >= min(rng) & x <= max(rng)
}

#' Convert a list of lists outputted from vapply to dataframe
#' @keywords internal
lol_to_df <- function(lol, name = "well") {
  lol %<>%
    t %>% as.data.frame %>%
    dplyr::mutate_(.dots = setNames(list(~ row.names(.)), name))
  lol[] <- lapply(lol, unlist)
  lol
}

#' Suppresses all output from an expression. Works cross-platform.
#' @keywords internal
quiet <- function(expr, all = TRUE) {
  if (Sys.info()['sysname'] == "Windows") {
    file <- "NUL"
  } else {
    file <- "/dev/null"
  }
  
  if (all) {
    suppressWarnings(suppressMessages(suppressPackageStartupMessages(
      capture.output(expr, file = file) 
    )))
  } else {
    capture.output(expr, file = file)
  }
}

#' Get the two values that are equidistant from a specific number
#' @keywords internal
plus_minus <- function(x, y) {
  x + y * c(-1, 1)
}

#' Overwrite a column in a data.frame based on a matching column in another df
#' @keywords internal
merge_dfs_overwrite_col <- function(olddf, newdf, cols, bycol = "well") {
  result <- dplyr::left_join(olddf, newdf, by = bycol)
  
  # If user didn't specify which columns to keep, keep all original columns
  if (missing(cols)) {
    cols <- setdiff(colnames(olddf), bycol)
  }
  
  for (colname in cols) {
    colname_x <- sprintf("%s.x", colname)
    colname_y <- sprintf("%s.y", colname)
    
    if (all(c(colname_x, colname_y) %in% colnames(result))) {
      result %<>%
        dplyr::mutate_(.dots = setNames(
          list(lazyeval::interp(
            ~ ifelse(is.na(coly), colx, coly),
            colx = as.name(colname_x),
            coly = as.name(colname_y))),
          colname_x)) %>%
        dplyr::rename_(.dots = setNames(colname_x, colname)) %>%
        dplyr::select_(lazyeval::interp(~ -colname, colname = as.name(colname_y)))
    }
  }

  result
}

#' Get the indices of the local maxima in a list of numbers
#' @keywords internal
local_maxima <- function(x) {
  y <- (c(-.Machine$integer.max, x) %>% diff) > 0L
  y <- rle(y)$lengths %>% cumsum
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

#' Get the indices of the local minima in a list of numbers
#' @keywords internal
local_minima <- function(x) {
  local_maxima(-x)
}

#' Get the indices of the inflection points in a curve
#' @keywords internal
get_inflection_pts <- function(dat) {
  inf_points_idx <-
    dat %>%
    {smooth.spline(x = .$x, y = .$y)} %>%
    predict(deriv = 2) %>%
    .$y %>%
    sign %>%
    rle %>%
    .$lengths %>%
    cumsum
  bogus_idx <- c(1, length(dat$x) - 1, length(dat$x))
  inf_points_idx <- inf_points_idx[!inf_points_idx %in% bogus_idx]
  inf_points_idx
}

#' Determine if a given path is a valid directory
#' @keywords internal
is_dir <- function(path) {
  if (missing(path) | is.null(path)) {
    return(FALSE)
  }
  path %<>% as.character
  fileinfo <- file.info(path)
  if (is.na(fileinfo$isdir)) {
    return(FALSE)
  }
  fileinfo$isdir
}

#' Determine if a given path is a valid file
#' @keywords internal
is_file <- function(path) {
  if (missing(path) | is.null(path)) {
    return(FALSE)
  }
  path %<>% as.character
  fileinfo <- file.info(path)
  if (is.na(fileinfo$isdir)) {
    return(FALSE)
  }
  !(fileinfo$isdir)
}

#' Representation of a 2D point
#' @keywords internal
point2d <- function(x) {
  stopifnot(x %>% length == 2)
  structure(
    v %>% as.integer
    , class = "point2d"
  )
}

#' Euclidean distance between two points (if second point is not given,
#' calculate distance to the origin)
#' @keywords internal
diff.point2d <- function(v, w) {
  if (missing(w)) {
    w <- point2d(c(0, 0))
  }
  sqrt((v[1] - w[1]) ^ 2 + (v[2] - w[2]) ^ 2)
}

#' Format a point2d for pretty printing
#' @keywords internal
format.point2d <- function(x, ...) {
  sprintf("(%s, %s)", x[1], x[2])
}

#' @export
#' @keywords internal
print.point2d <- function(x, ...) {
   print(x %>% format)
}

#' Move columns to the front of a data.frame (taken from daattali/rsalad)
#' @keywords internal
move_front <- function(df, cols) {
  bind_df_ends(df, cols, 1)
}

#' Move columns to the back of a data.frame (taken from daattali/rsalad)
#' @keywords internal
move_back <- function(df, cols) {
  bind_df_ends(df, cols, -1)
}

#' Helper function for move_front and move_back
#' @keywords internal
bind_df_ends <- function(df, cols, dir = 1) {
  stopifnot(
    is.data.frame(df),
    cols %in% colnames(df)
  )
  
  is_tbl <- dplyr::is.tbl(df)
  
  # Bind together the two parts of the data.frame
  df <-
    cbind(
      df %>% dplyr::select_(~(dir * one_of(cols))),
      df %>% dplyr::select_(~(-dir * one_of(cols)))
    )
  
  # If the input was a tbl_df, make sure to return that object too
  if (is_tbl) {
    df <- dplyr::tbl_df(df)
  }
  
  df
}