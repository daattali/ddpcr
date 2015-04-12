context("read")

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
  
  data_files %<>%
                c("three/four_B06_Amplitude.csv",
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