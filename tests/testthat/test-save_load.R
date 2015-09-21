context("save_load")

test_that("save and load work", {
  dir <- sample_data_dir()
  plate <- new_plate(dir)
  save_plate(plate, "myplate")
  plate2 <- load_plate("myplate")
  expect_identical(plate, plate2)
  unlink("myplate.rds")
})

test_that("normalize_to_rds works", {
  expect_identical(normalize_to_rds("somefile"), "somefile.rds")
  expect_identical(normalize_to_rds("somefile.rds"), "somefile.rds")
  expect_identical(normalize_to_rds("somefile.r"), "somefile.r.rds")
})
