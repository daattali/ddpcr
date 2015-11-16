context("empty")

test_that("get_empty_cutoff works", {
  plate <- new_plate(sample_data_dir())
  
  expect_identical(
    plate %>% get_empty_cutoff("B01"),
    list(x = 1774L, y = 1557L)
  )
  
  expect_identical(
    plate %>% get_empty_cutoff("C08"),
    list(x = 1884L, y = 1788L)
  )
})

test_that("get_empty_cutoff for pnpp works", {
  plate <- new_plate(sample_data_dir(), type = plate_types$fam_positive_pnpp)
  
  expect_identical(
    plate %>% get_empty_cutoff("B01"),
    list(y = 1557L, x = NA)
  )
  
  expect_identical(
    plate %>% get_empty_cutoff("C08"),
    list(y = 1788L, x = NA)
  )
})