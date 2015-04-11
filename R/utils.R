err_msg <- function(x) {
  stop(sprintf("ddpcr: %s", x), call. = FALSE)
}

warn_msg <- function(x) {
  warning(sprintf("ddpcr: %s", x), call. = FALSE)
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

# overwrite a column in a data.frame based on a matching column in another df
merge_dfs_overwrite_col <- function(olddf, newdf, colname, bycol = "well") {
  colname_x <- sprintf("%s.x", colname)
  colname_y <- sprintf("%s.y", colname)
  result <-
    dplyr::left_join(olddf, newdf, by = bycol) %>%
    dplyr::mutate_(.dots = setNames(
      list(lazyeval::interp(
        ~ ifelse(is.na(coly), colx, coly),
        colx = as.name(colname_x),
        coly = as.name(colname_y))),
      colname_x)) %>%
    dplyr::rename_(.dots = setNames(colname_x, colname)) %>%
    dplyr::select_(lazyeval::interp(~ -colname, colname = as.name(colname_y)))
  result
}



d %>% dplyr::mutate_(.dots=setNames(sprintf("sum(%s)", cc), "ff"))
d %>% dplyr::mutate_(new = lazyeval::interp(~sum(ccc), ccc=as.name(cc)))