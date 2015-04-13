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
  data_files <- c("one_two/three_B04_Amplitude.csv",
                  "one_two/three_B05_Amplitude.csv")
  expect_equal(
    get_consensus_name_from_data_files(data_files),
    "three"
  )
  
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
  system.file("testdata", dirname, package = "ddpcrS3")
}

test_that("find_data_files basic functionality works", {
  data_files <- find_data_files(testdir("find_files_simple"))
  expect_true(length(data_files) == 2)
  expect_true(grepl("test_A01_Amplitude.csv", data_files) %>% sum == 1)
  expect_true(grepl("test_B02_Amplitude.csv", data_files) %>% sum == 1)
})

test_that("find_meta_file basic functionality works", {
  meta_file <- find_meta_file(testdir("find_files_simple"), "test")
  expect_true(grepl("test.csv", meta_file))
  expect_warning(
    meta_file <- find_meta_file(testdir("find_files_simple"), "wrong"),
    "could not find metadata file"
  )
  expect_null(meta_file)
  expect_warning(
    meta_file <- find_meta_file(testdir("find_files_simple"), ""),
    "could not find metadata file"
  )
  expect_null(meta_file)
})

test_that("find_data_files works when there are multiple data file names", {
  data_files <- find_data_files(testdir("find_files_complex"))
  expect_true(length(data_files) == 3)
  expect_true(grepl("diffname_C03_Amplitude.csv", data_files) %>% sum == 1)
  expect_true(grepl("test_A01_Amplitude.csv", data_files) %>% sum == 1)
  expect_true(grepl("test_B02_Amplitude.csv", data_files) %>% sum == 1)
})


# -------- Reading the data ---------

# NOTE I've gotten a little lazy and didn't break up reading the files into
# little unit-testable chunks... instead I'm testing the whole function.

test_that("reading data basic functionality works", {
  plate <- empty_plate() %>% read_dir(testdir("find_files_simple"))
  expect_is(plate, "ddpcr_plate")
  expect_equal(status(plate), STATUS_INIT)
  expect_equal(name(plate), "test")
  plate_data <- plate_data(plate)
  expect_equal(dim(plate_data), c(7, 4))
  expect_equal(plate_data(plate) %>% as.data.frame,
               read.csv(file.path(testdir("find_files_simple"), "expected_data.csv"),
                        stringsAsFactors = FALSE))
  expect_equal(plate_meta(plate) %>% as.data.frame,
               read.csv(file.path(testdir("find_files_simple"), "expected_meta.csv"),
                        stringsAsFactors = FALSE))
})


#new_plate()  must be specified 
#new_plate(testdir("find_files_wrong_meta"))
