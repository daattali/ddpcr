context("pnpp_experiment-calculate_neg_freq")

test_that("calc_negative_freq_simple works", {
  expect_equal(calc_negative_freq_simple(5, 45), 10)
  expect_equal(calc_negative_freq_simple(5, 5), 50)
  expect_equal(calc_negative_freq_simple(13, 87), 13)
})

test_that("calculate_neg_freq_single works", {
  file <- system.file("sample_data", "small", "analyzed_pnpp.rds", package = "ddpcr")
  plate <- load_plate(file)
  expect_equal(plate %>% calculate_neg_freq_single("A05"),
               list("negative_num" = 368,
                    "positive_num" = 1224,
                    "negative_freq" = calc_negative_freq_simple(368, 1224)))
})

test_that("calculate_negative_freqs works", {
  file <- system.file("sample_data", "small", "analyzed_pnpp.rds", package = "ddpcr")
  plate <- load_plate(file)
  neg_freqs <-
    plate %>%
    calculate_negative_freqs %>%
    plate_meta %>%
    .[["negative_freq"]] %>%
    .[!is.na(.)]
  expect_equal(neg_freqs, c(0.218, 23.1, 0.24, 19.8))
})
