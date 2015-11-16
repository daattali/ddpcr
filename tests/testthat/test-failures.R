context("failures")

test_that("is_well_success works", {
  plate <- new_plate(sample_data_dir())
  
  expect_true(is_well_success(plate, "B01"))
  expect_true(is_well_success(plate, "B06"))
  expect_true(is_well_success(plate, "C01"))
  expect_true(is_well_success(plate, "C08"))
  expect_false(is_well_success(plate, "C06"))
})