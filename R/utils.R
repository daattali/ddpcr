## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

# This file contains many helper functions. None of them are explicitly exported,
# but some are "internal exports" to allow people to use them if they want to
# create their own plate types.

#' Get sample data
#' 
#' These functions return sample data files or folders and can be used to
#' load ddPCR plates with sample data. They are used primarily in the
#' documentation examples, but you can also use them for learning purposes.
#' There are two sample datasetes: a small dataset and a large dataset. The
#' small dataset contains the full raw data, but the large dataset only
#' includes the processed data because the raw data would be too large.
#' \cr\cr
#' \code{sample_data_dir}: get the directory of the small or large sample dataset\cr
#' \code{sample_data_file}: get path to one of the data files in the small sample dataset\cr
#' \code{sample_results_file}: get path to the results file of the small sample dataset\cr
#' \code{sample_plate}: get the ddpcr plate object containing the data of the small or large dataset\cr
#' @param size The dataset to retrieve, either \code{"small"} or \code{"large"}
#' @examples 
#' plate1 <- new_plate(dir = sample_data_dir())
#' plate2 <- new_plate(data_files = sample_data_file(), meta_file = sample_results_file())
#' plate3 <- sample_plate()
#' @name sample_data
NULL

#' @rdname sample_data
#' @keywords internal
#' @export
sample_data_dir <- function(size = c("small", "large")) {
  size <- match.arg(size)
  system.file("sample_data", size, package = "ddpcr")
}

#' @rdname sample_data
#' @keywords internal
#' @export
sample_data_file <- function() {
  data_files <- find_data_files(sample_data_dir())
  sample_file <- grep("F05_Amplitude", data_files, value = TRUE)
  sample_file
}

#' @rdname sample_data
#' @keywords internal
#' @export
sample_results_file <- function() {
  name <-
    sample_data_dir() %>%
    find_data_files() %>%
    get_consensus_name_from_data_files()
  results_file <- find_meta_file(sample_data_dir(), name)
  results_file
}

#' @rdname sample_data
#' @keywords internal
#' @export
sample_plate <- function(size = c("small", "large")) {
  size <- match.arg(size)
  dir <- system.file("sample_data", size, package = "ddpcr")
  plate <- load_plate(file.path(dir, size))
  plate
}

#' Get droplet data from a well
#' 
#' @param plate A ddPCR plate.
#' @param well_id A well ID.
#' @param empty Whether or not to include empty droplets.
#' @param outliers Whether or not to include outlier droplets.
#' @param clusters Whether or not to include cluster information.
#' @return A dataframe with the fluorescence value of all droplets in the given
#' well.
#' @keywords internal
#' @export
get_single_well <- function(plate, well_id,
                            empty = FALSE, outliers = FALSE, clusters = FALSE) {
  stopifnot(plate %>% inherits("ddpcr_plate"))
  
  well_id %<>% toupper
  
  result <-
    plate_data(plate) %>%
    dplyr::filter_(~ well == well_id) %>%
    dplyr::select_(quote(-well))
  
  if (!empty) {
    result %<>%
      dplyr::filter_(~ cluster != plate %>% cluster('EMPTY'))
  }
  if (!outliers) {
    result %<>%
      dplyr::filter_(~ cluster != plate %>% cluster('OUTLIER'))
  }
  if (!clusters) {
    result %<>%
      dplyr::select_(~ -cluster)
  }
  
  result
}

#' Show an error message
#' @param x The error message text
#' @keywords internal
#' @export
err_msg <- function(x) {
  stop(sprintf("ddpcr: %s", x), call. = FALSE)
}

#' Show a warning message
#' @param x The warning message text
#' @keywords internal
#' @export
warn_msg <- function(x) {
  warning(sprintf("ddpcr: %s", x), call. = FALSE)
}

#' Concatenate strings with no space between them
#' @param ... Strings to concatenate
#' @keywords internal
#' @export
cat0 <- function(...) {
  cat(..., sep = "")
}

#' Write a message to the user if the `ddpcr.verbose` option is on
#' 
#' Running a ddpcr analysis results in many messages being printed to the console.
#' By default, these messages are on when the user is using R interactively
#' and off otherwise. You can overwrite this setting with \code{options(ddpcr.verbose = FALSE)}. 
#' @param ... Parameters to pass to \code{message()}
#' @keywords internal
#' @export
msg <- function(...) {
  if(isTRUE(getOption("ddpcr.verbose", default = interactive()))) {
    message(...)
  }
}

#' Determine if a numeric value is within a range
#' @param x Numeric vector to check whether the values are within \code{rng}
#' @param rng The range to check if numbers in \code{x} are within it.
#' @keywords internal
#' @export
"%btwn%" <- function(x, rng) {
  stopifnot(is.numeric(x), is.numeric(rng), length(rng) == 2)
  x >= min(rng) & x <= max(rng)
}

#' Convert a list of lists returned from vapply to a dataframe
#'
#' When running a \code{vapply} function and each element returns a list with
#' multiple values, the return value is a list of lists.  This function can be
#' used to convert that return value into a data.frame.
#' @param lol List of lists that is a result of a vapply
#' @param name Column name to use for the name of each list
#' @examples 
#' vapply(c("a", "b", "c"),
#'        function(x) list(low = x, up = toupper(x)),
#'        list(character(1), character(1))) %>%
#'   lol_to_df("key")
#' @seealso \code{\link[ddpcr]{named_vec_to_df}}
#' @keywords internal
#' @export
lol_to_df <- function(lol, name = "well") {
  lol %<>% t %>% as.data.frame
  lol[[name]] <- row.names(lol)
  lol <- move_front(lol, name)
  lol[] <- lapply(lol, unlist)
  row.names(lol) <- NULL
  lol
}

#' Convert a named vector returned from vapply to a dataframe
#' 
#' When running a \code{vapply} function and each element returns a single value,
#' the return value is a named vector.  This function can be used to convert
#' that return value into a data.frame. Similar to \code{\link[ddpcr]{lol_to_df}},
#' but because the output format from \code{vapply} is different depending on
#' whether a single value or multiple values are returned, a different function
#' needs to be used.
#' @param v Named vector that is a result of a vapply
#' @param name Column name to use for the name of each element
#' @param rowname Column name to use for the values of the rownames
#' @examples 
#' vapply(c("a", "b", "c"),
#'        toupper,
#'        character(1)) %>%
#'   named_vec_to_df("capital", "letter")
#' @seealso \code{\link[ddpcr]{lol_to_df}}
#' @keywords internal
#' @export
named_vec_to_df <- function(v, name, rowname = "well") {
  v <- as.data.frame(v)
  colnames(v) <- name
  v[[rowname]] <- row.names(v)
  v
}

#' Suppress all output from an expression. Works cross-platform.
#' @param expr Expression to run.
#' @param all If \code{TRUE} then suppress warnings and messages as well;
#' otherwise, only suppress printed output (such as from \code{print} or
#' \code{cat}).
#' @keywords internal
#' @export
quiet <- function(expr, all = TRUE) {
  if (Sys.info()['sysname'] == "Windows") {
    file <- "NUL"
  } else {
    file <- "/dev/null"
  }
  
  if (all) {
    suppressWarnings(suppressMessages(suppressPackageStartupMessages(
      utils::capture.output(expr, file = file) 
    )))
  } else {
    utils::capture.output(expr, file = file)
  }
}

#' Overwrite a column in a data.frame based on a matching column in another df
#' 
#' Sometimes you want to merge two dataframes and specify that column X in
#' one dataframe should overwrite the same column in the other dataframe.
#' If there is a missing value in the column in the new dataframe, then the value
#' from the old dataframe is kept.
#' 
#' @param olddf The dataframe whose column will be overwritten.
#' @param newdf The dataframe that will use its columns to overwrite.
#' @param cols The names of the columns that exist in both dataframes that
#' should be overwritten. If not provided, then all columns that are common
#' to both dataframes are used.
#' @param bycol The names of the columns to use as the key for the merge.
#' @examples 
#' df <- function(...) data.frame(..., stringsAsFactors = FALSE)
#' 
#' df1 <- df(a = 1:4, b = c("one", NA, "three", "four"))
#' df2 <- df(a = 1:4, b = c("ONE", "TWO", NA, "FOUR"))
#' merge_dfs_overwrite_col(df1, df2, "b", "a")
#' merge_dfs_overwrite_col(df2, df1, "b", "a")
#' 
#' df3 <- df(a = 1:3, b = c("one", NA, "three"))
#' df4 <- df(a = 2:4, b = c("TWO", NA, "FOUR"))
#' merge_dfs_overwrite_col(df3, df4, "b", "a")
#' merge_dfs_overwrite_col(df4, df3, "b", "a")
#' 
#' df5 <- df(a = 1:3, b = c("one", "two", "three"), c = letters[1:3])
#' df6 <- df(b = c("ONE", "TWO", "THREE"), c = LETTERS[1:3], a = 1:3)
#' merge_dfs_overwrite_col(df5, df6, "b", "a")
#' merge_dfs_overwrite_col(df6, df5, "b", "a")
#' 
#' df7 <- df(a = 1:3, b = c("one", "two", "three"))
#' df8 <- df(a = 1:4)
#' merge_dfs_overwrite_col(df7, df8, "b", "a")
#' merge_dfs_overwrite_col(df8, df7, "b", "a")
#' 
#' df9 <- df(a = 1:3, b = c("one", "two", "three"), c = 1:3)
#' df10 <- df(a = 1:3, b = c("ONE", NA, "THREE"), c = 4:6)
#' merge_dfs_overwrite_col(df9, df10, c("b", "c"), "a")
#' merge_dfs_overwrite_col(df10, df9, c("b", "c"), "a")
#' @keywords internal
#' @export
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
      result[[colname_x]] <- ifelse(is.na(result[[colname_y]]),
                                    result[[colname_x]],
                                    result[[colname_y]])
      result %<>%
        dplyr::rename_(.dots = stats::setNames(colname_x, colname)) %>%
        dplyr::select_(lazyeval::interp(~ -colname, colname = as.name(colname_y)))
    }
  }
  
  result
}

#' Get the indices of the local maxima in a list of numbers
#' @param x Vector of numbers.
#' @return A vector containing the indices of the elements that are local maxima
#' in the given input.
#' @examples 
#' local_maxima(c(1, 5, 3, 2, 4, 3))
#' @keywords internal
#' @export
local_maxima <- function(x) {
  x <- as.numeric(x)
  y <- (c(-.Machine$integer.max, x) %>% diff) > 0L
  y <- rle(y)$lengths %>% cumsum
  y <- y[seq.int(1L, length(y), 2L)]
  if (x[[1]] == x[[2]]) {
    y <- y[-1]
  }
  y
}

#' Get the indices of the local minima in a list of numbers
#' @param x Vector of numbers.
#' @return A vector containing the indices of the elements that are local minima
#' in the given input.
#' @examples 
#' local_minima(c(1, 5, 3, 2, 4, 3))
#' @keywords internal
#' @export
local_minima <- function(x) {
  local_maxima(-x)
}

#' Determine if a given path is a valid directory
#' @param path A file path to test
#' @keywords internal
#' @export
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
#' @param path A file path to test
#' @keywords internal
#' @export
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
#' @param x A 2-element numeric vector.
#' @return An object of class \code{point2d} with the given coordinates.
#' @examples 
#' point2d(c(10, 20))
#' @keywords internal
#' @export
point2d <- function(x) {
  stopifnot(x %>% length == 2)
  structure(
    x %>% as.integer
    , class = "point2d"
  )
}

#' Euclidean distance between two points
#' 
#' Calculate the distance between two points in 2D space. If only one points is
#' given, then the distance to the origin is calculated.
#' @param x,y Points generated with \code{point2d}
#' @seealso \code{\link[ddpcr]{point2d}}
#' @keywords internal
#' @export
diff.point2d <- function(x, y, ...) {
  if (missing(y)) {
    y <- point2d(c(0, 0))
  }
  sqrt((x[1] - y[1]) ^ 2 + (x[2] - y[2]) ^ 2)
}

#' @export
format.point2d <- function(x, ...) {
  sprintf("(%s, %s)", x[1], x[2])
}

#' @export
print.point2d <- function(x, ...) {
   print(x %>% format)
}

#' Move columns to the front of a data.frame
#' 
#' This function is taken from daattali/rsalad R package.
#' 
#' @param df A data.frame.
#' @param cols A vector of column names to move to the front.
#' @examples 
#' df <- data.frame(a = character(0), b = character(0), c = character(0))
#' move_front(df, "b")
#' move_front(df, c("c", "b"))
#' @keywords internal
#' @export
move_front <- function(df, cols) {
  bind_df_ends(df, cols, 1)
}

#' Move columns to the back of a data.frame
#' 
#' This function is taken from daattali/rsalad R package.
#' 
#' @param df A data.frame.
#' @param cols A vector of column names to move to the back
#' @examples 
#' df <- data.frame(a = character(0), b = character(0), c = character(0))
#' move_back(df, "b")
#' move_back(df, c("b", "a"))
#' @keywords internal
#' @export
move_back <- function(df, cols) {
  bind_df_ends(df, cols, -1)
}

#' Helper function for move_front and move_back
#' @keywords internal
#' @export
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

#' Capitalize the first letter of a string
#' @keywords internal
#' @export
capitalize <- function(x) {
  paste(toupper(substring(x, 1, 1)), substring(x, 2),
        sep = "", collapse = " ")
}