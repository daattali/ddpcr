#' @export
# subset(plate, c("A02", "B05"))
# subset(plate, "A01, B05")
# subset(plate, "A01, B05:D07, F10")
subset.ddpcr_plate <- function(x, wells, samples, ...) {
  if (!missing(wells) && !missing(samples)) {
    err_msg("Can only subset by either `wells` or `samples`, not both")
  }
  
  if (!missing(wells)) {
    wells %<>% toupper
    if (is_range(wells)) {
      wells %<>% range_list_to_vec
    }
  } else if (!missing(samples)) {
    wells <-
      plate_meta(x) %>%
      dplyr::filter_(~ sample %in% samples) %>%
      .[['well']]
  } else {
    return(x)  # if no arguments, just return the same plate
  }
  
  plate_data(x) %<>%
    dplyr::filter_(~ well %in% wells)
  
  plate_meta(x) %<>%
    dplyr::filter_(~ well %in% wells) %>%
    merge_dfs_overwrite_col(DEFAULT_PLATE_META, .) %>%
    arrange_meta
  
  x
}

is_range <- function(x) {
  length(x) == 1 && grepl("[,:]", x)
}

# convert a range 
range_list_to_vec <- function(rangel) {
  rangel <- gsub("[[:space:]]", "", rangel)
  ranges <- strsplit(rangel, ",") %>% unlist
  
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

# "B05:G09" -> c("B05", "G09"), "B05" -> c("B05", "B05")
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

WELL_ID_REGEX <- "^[A-H][0-1][0-9]$"

# "D" -> 4 
row_to_num <- function(row) {
  magrittr::is_in(LETTERS, row) %>% which
}

# 4 -> "D"
num_to_row <- function(num) {
  LETTERS[num]
}

# "05" -> 5
col_to_num <- function(col) {
  col %>% as.integer
}

# 5 -> "05"
num_to_col <- function(num) {
  sprintf("%02d", num)
}

# c(8, 5) -> 5:8
range_to_seq <- function(rng) {
  seq(min(rng), max(rng))
}

# "C04", "D06" -> c("C04", "C05", "C06", "D04", "D05", "D06")
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

# "C05" -> "C"
get_row <- function(well) {
  substring(well, 1, 1)
}

# "C05" -> "05"
get_col <- function(well) {
  substring(well, 2, 3)
}