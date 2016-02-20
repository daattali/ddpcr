context("outliers")

test_that("get_outlier_cutoff works", {
  plate <- new_plate(sample_data_dir())
  
  expect_identical(
    plate %>% get_outlier_cutoff,
    list(HEX = 7510, FAM = 9671.5)
  )

  expect_identical(
    plate %>% subset("A05") %>% get_outlier_cutoff,
    list(HEX = 7314, FAM = 9647)
  )
})