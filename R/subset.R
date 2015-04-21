#' @export
#' subset(plate, c("A02", "B05"))
#' subset(plate, "A01, B05")
#' subset(plate, "A01, B05:D07, F10")
subset.ddpcr_plate <- function(plate, wells) {
  
  if (length(wells) == 1) {
    wells %<>% range_list_to_vec
  }
  
  plate_data(plate) %<>%
    dplyr::filter_(~ well %in% wells)
  
  plate_meta(plate) %<>%
    dplyr::filter_(~ well %in% wells) %>%
    merge_dfs_overwrite_col(DEFAULT_PLATE_META, .) %>%
    arrange_meta
  
  plate
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

# convert a range such as "B05:G09" to vector of c("B05", "G09")
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

get_wells_btwn <- function(well1, well2) {
  rows <-
    get_row(c(well1, well2)) %>%
    magrittr::is_in(LETTERS, .) %>%
    which %>%
    {seq(min(.), max(.))} %>%
    LETTERS[.]
  
  cols <-
    get_col(c(well1, well2)) %>%
    as.integer %>%
    {seq(min(.), max(.))} %>%
    sprintf("%02d", .)
  
  wells <- lapply(rows, function(x) paste(x, cols, sep = "")) %>% unlist
  wells
}

get_row <- function(well) {
  substring(well, 1, 1)
}

get_col <- function(well) {
  substring(well, 2, 3)
}