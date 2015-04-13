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
  system.file("testdata", dirname, package = "ddpcrS3")
}

test_that("find_data_files basic functionality works", {
  data_files <- find_data_files(testdir("find_files_simple"))
  expect_true(length(data_files) == 2)
  expect_true(grepl("test_A01_Amplitude.csv", data_files) %>% sum == 1)
  expect_true(grepl("test_B02_Amplitude.csv", data_files) %>% sum == 1)
})

test_that("find_data_files works when there are multiple data file names", {
  data_files <- find_data_files(testdir("find_files_complex"))
  expect_true(length(data_files) == 3)
  expect_true(grepl("diffname_C03_Amplitude.csv", data_files) %>% sum == 1)
  expect_true(grepl("test_A01_Amplitude.csv", data_files) %>% sum == 1)
  expect_true(grepl("test_B02_Amplitude.csv", data_files) %>% sum == 1)
})

test_that("find_meta_file basic functionality works", {
  # metadata file is found
  meta_file <- find_meta_file(testdir("find_files_simple"), "test")
  expect_true(grepl("test.csv", meta_file))
  
  # metadata file is not found
  expect_warning(
    meta_file <- find_meta_file(testdir("find_files_simple"), "wrong"),
    "could not find metadata file"
  )
  expect_null(meta_file)
  
  # metadata file is not found
  expect_warning(
    meta_file <- find_meta_file(testdir("find_files_simple"), ""),
    "could not find metadata file"
  )
  expect_null(meta_file)
})

# -------- Reading the data ---------

# NOTE I've gotten a little lazy and didn't break up reading the files into
# little unit-testable chunks... instead I'm testing the whole function.

test_that("read_dir basic functionality works", {
  plate <- empty_plate() %>% read_dir(testdir("find_files_simple"))
  expect_is(plate, "ddpcr_plate")
  expect_equal(status(plate), STATUS_INIT)
  expect_equal(name(plate), "test")
  expect_equal(plate_data(plate) %>% as.data.frame,
               read.csv(file.path(testdir("find_files_simple"), "expected_data.csv"),
                        stringsAsFactors = FALSE))
  expect_equal(plate_meta(plate) %>% as.data.frame,
               read.csv(file.path(testdir("find_files_simple"), "expected_meta.csv"),
                        stringsAsFactors = FALSE))
})

test_that("read_dir errors and warnings", {
  # given directory does not exist
  expect_error(
    empty_plate() %>% read_dir(testdir("notadir")),
    "could not find directory"
  )
  # given directory has no properly-named metadata file
  expect_warning(
    empty_plate() %>% read_dir(testdir("find_files_complex")),
    "could not find metadata file"
  )
  # given directory has data files with inconsistent names
  expect_warning(
    empty_plate() %>% read_dir(testdir("find_files_complex")),
    "same name"
  )
})

test_that("read_dir works in more complex cases", {
  # correct data is loaded when given a complex directory
  suppressWarnings(
    plate <- empty_plate() %>% read_dir(testdir("find_files_complex"))
  )
  expect_is(plate, "ddpcr_plate")
  expect_equal(status(plate), STATUS_INIT)
  expect_equal(name(plate), "test")
  expected_data <- read.csv(file.path(testdir("find_files_complex"), "expected_data.csv"),
                            stringsAsFactors = FALSE)
  expected_meta <- read.csv(file.path(testdir("find_files_complex"), "expected_meta_without.csv"),
                            stringsAsFactors = FALSE)
  mode(expected_meta$sample) <- "logical"
  expect_equal(plate_data(plate) %>% as.data.frame,
               expected_data)
  expect_equal(plate_meta(plate) %>% as.data.frame,
               expected_meta)
})

test_that("read_files errors and warnings", {
  data_files_simple <- find_data_files(testdir("find_files_simple"))
  meta_file_simple <- find_meta_file(testdir("find_files_simple"), "test")
  
  # no data files are given
  expect_error(
    empty_plate() %>% read_files(),
    "no data files"
  )
  # wrong data files are given
  expect_error(
    empty_plate() %>% read_files(data_files = c(data_files_simple, "nofile")),
    "could not find all data files"
  )
  # no metadata file is given
  expect_warning(
    empty_plate() %>% read_files(data_files_simple),
    "no metadata file"
  )
  # wrong metadata file is given
  expect_error(
    empty_plate() %>% read_files(data_files_simple, "nometa"),
    "could not find metadata"
  )
})

test_that("read_files basic functionality works", {
  data_files <- find_data_files(testdir("find_files_simple"))
  meta_file <- find_meta_file(testdir("find_files_simple"), "test")
  plate <- empty_plate() %>% read_files(data_files, meta_file)
  expect_is(plate, "ddpcr_plate")
  expect_equal(status(plate), STATUS_INIT)
  expect_equal(name(plate), "test")
  expect_equal(plate_data(plate) %>% as.data.frame,
               read.csv(file.path(testdir("find_files_simple"), "expected_data.csv"),
                        stringsAsFactors = FALSE))
  expect_equal(plate_meta(plate) %>% as.data.frame,
               read.csv(file.path(testdir("find_files_simple"), "expected_meta.csv"),
                        stringsAsFactors = FALSE))
})

test_that("read_files inconsistent data files, no metadata file", {
  data_files <- find_data_files(testdir("find_files_complex"))
  expected_data <- read.csv(file.path(testdir("find_files_complex"), "expected_data.csv"),
                            stringsAsFactors = FALSE)
  expected_meta <- read.csv(file.path(testdir("find_files_complex"), "expected_meta_without.csv"),
                            stringsAsFactors = FALSE)
  mode(expected_meta$sample) <- "logical"
  
  suppressWarnings({
    plate <- empty_plate() %>% read_files(data_files)
    plate2 <- empty_plate() %>% read_files(data_files, NULL)
  })
  expect_identical(plate, plate2)
  expect_is(plate, "ddpcr_plate")
  expect_equal(status(plate), STATUS_INIT)
  expect_equal(name(plate), "test")
  expect_equal(plate_data(plate) %>% as.data.frame,
               expected_data)
  expect_equal(plate_meta(plate) %>% as.data.frame,
               expected_meta)
})

test_that("read_files inconsistent data files, correct metadata file", {
  data_files <- find_data_files(testdir("find_files_complex"))
  meta_file <- find_meta_file(testdir("find_files_complex"), "metadata")
  expected_data <- read.csv(file.path(testdir("find_files_complex"), "expected_data.csv"),
                            stringsAsFactors = FALSE)
  expected_meta <- read.csv(file.path(testdir("find_files_complex"), "expected_meta_with.csv"),
                            stringsAsFactors = FALSE)
  
  suppressWarnings(
    plate <- empty_plate() %>% read_files(data_files, meta_file)
  )
  expect_is(plate, "ddpcr_plate")
  expect_equal(status(plate), STATUS_INIT)
  expect_equal(name(plate), "test")
  expect_equal(plate_data(plate) %>% as.data.frame,
               expected_data)
  expect_equal(plate_meta(plate) %>% as.data.frame,
               expected_meta)
})

test_that("new_plate works", {
  expect_error(
    new_plate(),
    "must be specified"
  )
  plate <- new_plate(testdir("find_files_simple"))
  expect_identical(
    plate,
    empty_plate() %>% read_dir(testdir("find_files_simple"))
  )
  expect_identical(name(plate), "test")
  plate <- new_plate(testdir("find_files_simple"), name = "myname") 
  expect_identical(name(plate), "myname")
})