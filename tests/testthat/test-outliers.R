context("outliers")

test_that("get_outlier_cutoff works", {
  plate <- new_plate(sample_data_dir())
  
  expect_identical(
    plate %>% get_outlier_cutoff,
    list(HEX = 8168, FAM = 10855.25)
  )

  expect_identical(
    plate %>% subset("B06") %>% get_outlier_cutoff,
    list(HEX = 8358.25, FAM = 10642.75)
  )
})