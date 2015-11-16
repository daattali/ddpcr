context("plate")

.plate <- new_plate(sample_data_dir(), type = plate_types$custom_thresholds)
get_plate <- function() {
  .plate
}

test_that("reset works", {
  plate <- get_plate()
  expect_equal(plate %>% status, 1L)
  expect_identical(plate %>% type, "custom_thresholds")
  plate <- plate %>% analyze
  expect_more_than(plate %>% status, 1L)
  plate <- plate %>% reset(keep_type = TRUE)
  expect_equal(plate %>% status, 1L)
  expect_identical(plate %>% type, "custom_thresholds")
  plate <- plate %>% reset(type = plate_types$fam_positive_pnpp)
  expect_identical(plate %>% type, "fam_positive_pnpp")
  plate <- plate %>% reset
  expect_identical(plate %>% type, "ddpcr_plate")
})

test_that("well_info works", {
  plate <- get_plate()
  expect_equal(
    plate %>% well_info(c("B06", "C06"), "drops"),
    c(13657, 14504)
  )
})

test_that("wells_used works", {
  plate <- get_plate()
  expect_equal(
    plate %>% wells_used,
    c("B01", "B06", "C01", "C06", "C08")
  )
  expect_equal(
    plate %>% subset("B06, C08") %>% wells_used,
    c("B06", "C08")
  )
})

test_that("next_step and analyze work", {
  plate <- get_plate()
  expect_equal(plate %>% status, 1L)
  expect_equal(plate %>% next_step %>% status, 2L)
  expect_equal(plate %>% next_step(2) %>% status, 3L)
  expect_equal(plate %>% next_step %>% next_step %>% status, 3L)
  expect_equal(plate %>% next_step(50) %>% status, plate %>% analyze %>% status)
})