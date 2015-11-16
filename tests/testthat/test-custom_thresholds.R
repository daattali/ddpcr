context("type-custom_thresholds")

.dir <- sample_data_dir()
.plate <- new_plate(dir = .dir, type = plate_types$custom_thresholds)
get_plate <- function() {
  .plate
}

test_that("the right type is set", {
  expect_is(get_plate(), "custom_thresholds")
  expect_is(get_plate(), "ddpcr_plate")
})

test_that("the right clusters are set", {
  expect_true(all(c("X_POSITIVE", "Y_POSITIVE", "BOTH_POSITIVE") %in%
                    (get_plate() %>% clusters)))
})

test_that("getting/setting thresholds works", {
  plate <- get_plate()
  
  expect_equal(plate %>% thresholds %>% unclass, c(5000, 5000))
  
  thresholds(plate) <- c(3000, 6000)
  expect_equal(plate %>% thresholds %>% unclass, c(3000, 6000))
  expect_equal(plate %>% x_threshold, 3000)
  expect_equal(plate %>% y_threshold, 6000)
  
  x_threshold(plate) <- 2000
  y_threshold(plate) <- 7000
  expect_equal(plate %>% thresholds %>% unclass, c(2000, 7000))
  
  expect_equal(plate %>% set_thresholds(c(40, 30)) %>% thresholds %>% unclass,
               c(40, 30))
})

test_that("classify works", {
  plate <- get_plate() %>% set_thresholds(c(4000, 7000)) %>% analyze
  
  expect_equal(
    plate %>% plate_data %>% dplyr::filter(HEX < 4000, FAM < 7000) %>% nrow,
    72273L
  )
  expect_equal(
    plate %>% plate_data %>% dplyr::filter(HEX < 4000, FAM < 7000) %>% .$cluster %>% unique,
    3L
  )
  expect_equal(
    plate %>% plate_data %>% dplyr::filter(HEX >= 4000, FAM < 7000) %>% nrow,
    48L
  )
  expect_equal(
    plate %>% plate_data %>%
      dplyr::filter(HEX >= 4000, FAM < 7000, cluster != 2) %>%
      .$cluster %>% unique,
    4L
  )
  expect_equal(
    plate %>% plate_data %>% dplyr::filter(HEX < 4000, FAM >= 7000) %>% nrow,
    346L
  )
  expect_equal(
    plate %>% plate_data %>%
      dplyr::filter(HEX < 4000, FAM >= 7000, cluster != 2) %>%
      .$cluster %>% unique,
    5L
  )
  expect_equal(
    plate %>% plate_data %>% dplyr::filter(HEX >= 4000, FAM >= 7000) %>% nrow,
    3032L
  )
  expect_equal(
    plate %>% plate_data %>% dplyr::filter(HEX >= 4000, FAM >= 7000) %>% .$cluster %>% unique,
    6L
  )
})