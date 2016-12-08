context("read")

# -------- Utility functions ---------

test_that("get_name_from_data_file works", {
  expect_equal(
    get_name_from_data_file("one_two/three-four.five_B04_Amplitude.csv"),
    "one_two/three-four.five"
  )
})

test_that("get_well_from_data_file works", {
  expect_equal(
    get_well_from_data_file("one_two/three-four.five_B04_Amplitude.csv"),
    "B04"
  )
})

test_that("get_consensus_name_from_data_files works", {
  # all files have same name
  data_files <- c("one_two/three_B04_Amplitude.csv",
                  "one_two/three_B05_Amplitude.csv")
  expect_equal(
    get_consensus_name_from_data_files(data_files),
    "three"
  )
  
  # files have different names
  data_files <- c("one_two/three_B04_Amplitude.csv",
                  "one_two/three_B05_Amplitude.csv",
                  "three/four_B06_Amplitude.csv",
                  "three/four_B07_Amplitude.csv",
                  "three/four_B08_Amplitude.csv",
                  "three/three_B09_Amplitude.csv",
                  "three/three_B10_Amplitude.csv")
  suppressWarnings(
    expect_equal(
      get_consensus_name_from_data_files(data_files),
      "four"
    )
  )
  expect_warning(
    get_consensus_name_from_data_files(data_files),
    "same name"
  )
})

# -------- Finding the correct data/metadata files ---------

testdir <- function(dirname) {
  system.file("sample_data", dirname, package = "ddpcr")
}

test_that("find_data_files basic functionality works", {
  data_files <- find_data_files(testdir("read_simple"))
  expect_true(length(data_files) == 2)
  expect_true(grepl("test_A01_Amplitude.csv", data_files) %>% sum == 1)
  expect_true(grepl("test_B02_Amplitude.csv", data_files) %>% sum == 1)
})

test_that("find_data_files works when there are multiple data file names", {
  data_files <- find_data_files(testdir("read_complex"))
  expect_true(length(data_files) == 3)
  expect_true(grepl("diffname_C03_Amplitude.csv", data_files) %>% sum == 1)
  expect_true(grepl("test_A01_Amplitude.csv", data_files) %>% sum == 1)
  expect_true(grepl("test_B02_Amplitude.csv", data_files) %>% sum == 1)
})

test_that("find_meta_file basic functionality works", {
  # metadata file is found
  meta_file <- find_meta_file(testdir("read_simple"), "test")
  expect_true(grepl("test.csv", meta_file))
  
  # metadata file is not found
  expect_warning(
    meta_file <- find_meta_file(testdir("read_simple"), "wrong"),
    "could not find metadata file"
  )
  expect_null(meta_file)
  
  # metadata file is not found
  expect_warning(
    meta_file <- find_meta_file(testdir("read_simple"), ""),
    "could not find metadata file"
  )
  expect_null(meta_file)
})

# -------- Reading the data ---------

.empty_plate <-
  system.file("sample_data", "empty_plate.rds", package = "ddpcr") %>%
  load_plate
get_empty_plate <- function() {
  .empty_plate
}

test_that("read_dir basic functionality works", {
  plate <- get_empty_plate() %>% read_dir(testdir("read_simple"))
  expect_equal(name(plate), "test")
  expect_equal(plate_data(plate),
               readr::read_csv(file.path(testdir("read_simple"),
                                         "expected_data.csv"))
  )
  expect_false(is.null(plate_meta(plate)))
})

test_that("read_dir errors and warnings", {
  # given directory does not exist
  expect_error(
    get_empty_plate() %>% read_dir(testdir("notadir")),
    "could not find directory"
  )
  # given directory has no properly-named metadata file
  expect_warning(
    get_empty_plate() %>% read_dir(testdir("read_complex")),
    "could not find metadata file"
  )
  # given directory has data files with inconsistent names
  expect_warning(
    get_empty_plate() %>% read_dir(testdir("read_complex")),
    "same name"
  )
})

test_that("read_dir works in more complex cases", {
  # multiple data file names, no auto-detected metadata file
  suppressWarnings(
    plate <- get_empty_plate() %>% read_dir(testdir("read_complex"))
  )
  expect_equal(name(plate), "test")
  expected_data <- readr::read_csv(file.path(testdir("read_complex"),
                                             "expected_data.csv"))
  expect_equal(plate_data(plate),
               expected_data)
  expect_that(plate_meta(plate), is_null())
})

test_that("read_files errors and warnings", {
  data_files_simple <- find_data_files(testdir("read_simple"))
  meta_file_simple <- find_meta_file(testdir("read_simple"), "test")
  
  # no data files are given
  expect_error(
    get_empty_plate() %>% read_files(),
    "no data files"
  )
  # wrong data files are given
  expect_error(
    get_empty_plate() %>% read_files(data_files = c(data_files_simple, "nofile")),
    "could not find all data files"
  )
  # no metadata file is given
  expect_warning(
    get_empty_plate() %>% read_files(data_files_simple),
    "no metadata file"
  )
  # wrong metadata file is given
  expect_error(
    get_empty_plate() %>% read_files(data_files_simple, "nometa"),
    "could not find metadata"
  )
})

test_that("read_files basic functionality works", {
  data_files <- find_data_files(testdir("read_simple"))
  meta_file <- find_meta_file(testdir("read_simple"), "test")
  plate <- get_empty_plate() %>% read_files(data_files, meta_file)
  expect_equal(name(plate), "test")
  expect_equal(plate_data(plate),
               readr::read_csv(file.path(testdir("read_simple"),
                                         "expected_data.csv"))
  )
  expect_false(is.null(plate_meta(plate)))
})

test_that("read_files inconsistent data files, no metadata file", {
  data_files <- find_data_files(testdir("read_complex"))
  expected_data <- readr::read_csv(file.path(testdir("read_complex"),
                                             "expected_data.csv"))
  
  suppressWarnings({
    plate <- get_empty_plate() %>% read_files(data_files)
    plate2 <- get_empty_plate() %>% read_files(data_files, NULL)
  })
  expect_identical(plate, plate2)
  expect_equal(name(plate), "test")
  expect_equal(plate_data(plate),
               expected_data)
  expect_that(plate_meta(plate), is_null())
})

test_that("read_files inconsistent data files, correct metadata file", {
  data_files <- find_data_files(testdir("read_complex"))
  meta_file <- find_meta_file(testdir("read_complex"), "metadata")
  expected_data <- readr::read_csv(file.path(testdir("read_complex"),
                                             "expected_data.csv"))
  
  suppressWarnings(
    plate <- get_empty_plate() %>% read_files(data_files, meta_file)
  )
  expect_equal(name(plate), "test")
  expect_equal(plate_data(plate),
               expected_data)
  expect_false(is.null(plate_meta(plate)))
})

test_that("read_files targets for channel 1 and 2", {
  dir <- system.file("sample_data", "read_simple", package = "ddpcr")
  plate <- new_plate(dir) 
  meta <- plate %>% plate_meta(only_used = TRUE)
  expect_equal(meta$target_ch1, c("t1.fw", "t2.fw"))
  expect_equal(meta$target_ch2, c("t1.rev", "t2.rev"))
})
