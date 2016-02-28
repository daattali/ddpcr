## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

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
#' @export
sample_data_dir <- function(size = c("small", "large")) {
  size <- match.arg(size)
  system.file("sample_data", size, package = "ddpcr")
}

#' @rdname sample_data
#' @export
sample_data_file <- function() {
  data_files <- find_data_files(sample_data_dir())
  sample_file <- grep("F05_Amplitude", data_files, value = TRUE)
  sample_file
}

#' @rdname sample_data
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
#' @export
sample_plate <- function(size = c("small", "large")) {
  size <- match.arg(size)
  dir <- system.file("sample_data", size, package = "ddpcr")
  plate <- load_plate(file.path(dir, size))
  plate
}