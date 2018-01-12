context("v174")

test_that("reading v174 works", {
  dir <- system.file("sample_data", "read_v174", package = "ddpcr")
  plate <- new_plate(dir) 
  expected_data <- readr::read_csv(file.path(dir, "expected_data.csv"), col_types = "ciii")
  expect_equal(expected_data, plate_data(plate))
  
  meta <- plate %>% plate_meta(only_used = TRUE)
  expect_equal(meta$target_ch1, c("t1.fw", "t2.fw"))
  expect_equal(meta$target_ch2, c("t1.rev", "t2.rev"))
})
