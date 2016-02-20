context("pnpp_experiment-filled")

test_that("get_filled_border works", {
  file <- system.file("sample_data", "small", "analyzed_pnpp.rds", package = "ddpcr")
  plate <- load_plate(file)
  
  border_A05 <- get_filled_border(plate, "A05")
  expect_equal(border_A05, 8136)

  border_F05 <- get_filled_border(plate, "F05")
  expect_equal(border_F05, 8294)
})

test_that("get_filled_drops works", {
  dir <- sample_data_dir()
  plate <- load_plate(file.path(dir, "analyzed_pnpp.rds"))
  
  border_A05 <- get_filled_border(plate, "A05")
  expect_identical(get_filled_drops(plate, "A05"),
                   get_filled_drops(plate, "A05", border_A05))
  
  expect_identical(get_filled_drops(plate, "A05"),
                   readr::read_csv(file.path(dir, "A05_filled.csv")))
})
