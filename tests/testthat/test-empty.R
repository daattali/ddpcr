context("empty")

test_that("get_empty_cutoff works", {
  plate <- new_plate(sample_data_dir())
  
  expect_identical(
    plate %>% get_empty_cutoff("A01"),
    list(x = 1765L, y = 1598L)
  )
  
  expect_identical(
    plate %>% get_empty_cutoff("F05"),
    list(x = 1772L, y = 1607L)
  )
})

test_that("get_empty_cutoff for pnpp works", {
  plate <- new_plate(sample_data_dir(), type = plate_types$fam_positive_pnpp)
  
  expect_identical(
    plate %>% get_empty_cutoff("A01"),
    list(y = 1598L, x = NA)
  )
  
  expect_identical(
    plate %>% get_empty_cutoff("F05"),
    list(y = 1607L, x = NA)
  )
})