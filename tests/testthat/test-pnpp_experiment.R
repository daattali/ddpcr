context("type-pnpp_experiment")

.dir <- sample_data_dir()
.plate <- new_plate(dir = .dir, type = plate_types$pnpp_experiment)
get_plate <- function() {
  .plate
}

test_that("the right type is set", {
  expect_is(get_plate(), "pnpp_experiment")
  expect_is(get_plate(), "ddpcr_plate")
})

test_that("the right clusters are set", {
  expect_true(all(c("RAIN", "POSITIVE", "NEGATIVE") %in%
                    (get_plate() %>% clusters)))
})

test_that("the right steps are set", {
  expect_true(all(c("CLASSIFY", "RECLASSIFY") %in%
                    (get_plate() %>% steps %>% names)))
})

test_that("the right params are set", {
  expect_true(all(c("POSITIVE_NAME", "NEGATIVE_NAME") %in%
                    (get_plate() %>% params('GENERAL') %>% names)))
  expect_true(all(c("CLASSIFY", "RECLASSIFY") %in%
                    (get_plate() %>% params %>% names)))
})

test_that("positive_dim and variable_dim work", {
  plate <- get_plate()
  
  positive_dim(plate) <- "X"
  expect_identical(positive_dim(plate), "X")
  expect_identical(variable_dim(plate), "Y")
})

test_that("positive_dim_var and variable_dim_var work", {
  plate <- get_plate()
  x_var(plate) <- "XXX"
  y_var(plate) <- "YYY"
  
  positive_dim(plate) <- "X"
  expect_identical(positive_dim_var(plate), "XXX")
  expect_identical(variable_dim_var(plate), "YYY")
})

test_that("other_dim works", {
  expect_identical(other_dim("X"), "Y")
  expect_identical(other_dim("Y"), "X")
  expect_error(other_dim("x"))
  expect_error(other_dim("Z"))
})

test_that("meta_var_name works", {
  plate <- get_plate()
  expect_identical(meta_var_name(plate, "num_positive_drops"),
                   "num_positive_drops")
  
  params(plate, 'GENERAL', 'NEGATIVE_NAME') <- "mutant"
  params(plate, 'GENERAL', 'POSITIVE_NAME') <- "wildtype"
  expect_identical(meta_var_name(plate, "num_positive_drops"),
                   "num_wildtype_drops")
  expect_identical(meta_var_name(plate, "num_negative_drops"),
                   "num_mutant_drops")
})

test_that("wells_positive and wells_negative work", {
  file <- system.file("sample_data", "small", "analyzed_pnpp.rds", package = "ddpcr")
  plate <- load_plate(file)
  expect_identical(plate %>% wells_positive, c("B01", "C01"))
  expect_identical(plate %>% wells_negative, c("B06", "C08"))
})
