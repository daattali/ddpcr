context("pnpp_experiment-calculate_neg_freq")

test_that("calc_negative_freq_simple works", {
  expect_equal(calc_negative_freq_simple(5, 45), 10)
  expect_equal(calc_negative_freq_simple(5, 5), 50)
  expect_equal(calc_negative_freq_simple(13, 87), 13)
})

test_that("calculate_neg_freq_single works", {
  file <- system.file("sample_data", "small", "analyzed_pnpp.rds", package = "ddpcr")
  plate <- load_plate(file)
  expect_equal(plate %>% calculate_neg_freq_single("B06"),
               list("negative_num" = 140,
                    "positive_num" = 479,
                    "negative_freq" = calc_negative_freq_simple(140, 479)))
})

test_that("calculate_negative_freqs works", {
  file <- system.file("sample_data", "small", "analyzed_pnpp.rds", package = "ddpcr")
  plate <- load_plate(file)
  neg_freqs <-
    plate %>%
    calculate_negative_freqs %>%
    plate_meta %>%
    .[['negative_freq']] %>%
    .[!is.na(.)]
  expect_equal(neg_freqs, c(0.156, 22.6, 0.164, 26.7))
})
