context("ppnp_assay-filled")

test_that("get_filled_border works", {
  file <- system.file("sample_data", "small", "analyzed_ppnp.rds", package = "ddpcr")
  plate <- load_plate(file)
  
  border_B06 <- get_filled_border(plate, "B06")
  expect_equal(border_B06, 8971)

  border_C09 <- get_filled_border(plate, "C09")
  expect_equal(border_C09, 6142)
})

test_that("get_filled_drops works", {
  dir <- system.file("sample_data", "small", package = "ddpcr")
  plate <- load_plate(file.path(dir, "analyzed_ppnp.rds"))
  
  border_B06 <- get_filled_border(plate, "B06")
  expect_identical(get_filled_drops(plate, "B06"),
                   get_filled_drops(plate, "B06", border_B06))
  
  expect_identical(get_filled_drops(plate, "B06"),
                   readr::read_csv(file.path(dir, "B06_filled.CSV")))
})
