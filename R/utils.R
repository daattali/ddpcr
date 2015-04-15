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

# convert a list of lists (lol) to dataframe
#TODO add to rsalad
lol_to_df <- function(lol, name = "well") {
  lol %<>%
    t %>% as.data.frame %>%
    dplyr::mutate_(.dots = setNames(list(~ row.names(.)), name))
  lol[] <- lapply(lol, unlist)
  lol
}

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

plus_minus <- function(x, y) {
  x + y * c(-1, 1)
}

# overwrite a column in a data.frame based on a matching column in another df
#TODO add to rsalad
merge_dfs_overwrite_col <- function(olddf, newdf, colnames, bycol = "well") {
  result <- dplyr::left_join(olddf, newdf, by = bycol)
  
  # yes yes, looks are horrible in R, but I couldn't find a better solution
  # to make sure this works on multiple columns at a time
  for (colname in colnames) {
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