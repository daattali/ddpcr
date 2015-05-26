context("ppnp_assay-filled")

test_that("get_filled_borders works", {
  file <- system.file("sample_data", "small", "analyzed_ppnp.rds", package = "ddpcr")
  plate <- load_plate(file)
  
  borders_B06 <- get_filled_borders(plate, "B06")
  expect_equal(borders_B06, c(8971, 10329))

  borders_C09 <- get_filled_borders(plate, "C09")
  expect_equal(borders_C09, c(6142, 7098))
})

test_that("get_filled_drops works", {
  dir <- system.file("sample_data", "small", package = "ddpcr")
  plate <- load_plate(file.path(dir, "analyzed_ppnp.rds"))
  
  borders_B06 <- get_filled_borders(plate, "B06")
  expect_identical(get_filled_drops(plate, "B06"),
                   get_filled_drops(plate, "B06", borders_B06))
  
  expect_identical(get_filled_drops(plate, "B06"),
                   readr::read_csv(file.path(dir, "B06_filled.CSV")))
  
  expect_identical(get_filled_drops(plate, "C09"),
                   readr::read_csv(file.path(dir, "C09_filled.CSV")))
})
