context("subset")

test_that("subset works", {
  dir <- system.file("sample_data", "small", package = "ddpcr")
  plate <- new_plate(dir)
  
  expect_identical(plate %>% subset %>% wells_used,
                   plate %>% wells_used)
  expect_identical(plate %>% subset(NULL) %>% wells_used,
                   plate %>% wells_used)
  expect_identical(plate %>% subset("D08:D10") %>% wells_used,
                   plate %>% wells_used)
  expect_identical(plate %>% subset("C01") %>% wells_used,
                   "C01")
  expect_identical(plate %>% subset(c("C01", "C08")) %>% wells_used,
                   c("C01", "C08"))
  expect_identical(plate %>% subset(c("C01", "C08")) %>% wells_used,
                   c("C01", "C08"))
  expect_identical(plate %>% subset("C01, C08") %>% wells_used,
                   c("C01", "C08"))
  expect_identical(plate %>% subset("C01:C08") %>% wells_used,
                   c("C01", "C06", "C08"))
  expect_identical(plate %>% subset("C01:C08, B01") %>% wells_used,
                   c("B01", "C01", "C06", "C08"))
  expect_identical(plate %>% subset("B01:C03") %>% wells_used,
                   c("B01", "C01"))
  expect_identical(plate %>% subset("B01:C06") %>% wells_used,
                   c("B01", "B06", "C01", "C06"))
  expect_identical(plate %>% subset("B01, B06:C08") %>% wells_used,
                   c("B01", "B06", "C06", "C08"))
  expect_identical(plate %>% subset("B01, B06:C06, C08") %>% wells_used,
                   c("B01", "B06", "C06", "C08"))
  expect_identical(plate %>% subset("B01:B06, C01:C06, C08") %>% wells_used,
                   c("B01", "B06", "C01", "C06", "C08"))
  expect_identical(plate %>% subset(samples = "#1") %>% wells_used,
                   c("B01"))
  expect_identical(plate %>% subset(samples = c("#1", "#3")) %>% wells_used,
                   c("B01", "C01"))
})

test_that("is_range works", {
  expect_false(is_range("C05"))
  expect_false(is_range(c("C05", "C08")))
  expect_false(is_range("C05"))
  expect_true(is_range("C05, C08"))
  expect_true(is_range("C05:C08"))
  expect_false(is_range("C05.C08"))
})

test_that("range_list_to_vec works", {
  expect_identical(range_list_to_vec("A01"),
                   "A01")
  expect_identical(range_list_to_vec("A01:A04"),
                   c("A01", "A02", "A03", "A04"))
  expect_identical(range_list_to_vec("A01, B03"),
                   c("A01", "B03"))
  expect_identical(range_list_to_vec("A01, B02:C04, C07"),
                   c("A01", "B02", "B03", "B04", "C02", "C03", "C04", "C07"))
})

test_that("range_to_endpoints works", {
  expect_identical(range_to_endpoints("B05:G09"), c("B05", "G09"))
  expect_identical(range_to_endpoints("B05"), c("B05", "B05"))
})

test_that("range_to_seq works", {
  expect_identical(range_to_seq(c(5, 8)), 5:8)
  expect_identical(range_to_seq(c(8, 5)), 5:8)
})

test_that("get_wells_btwn works", {
  expect_identical(get_wells_btwn("C04", "D06" ),
                   c("C04", "C05", "C06", "D04", "D05", "D06"))
})

test_that("row_to_num works", {
  expect_identical(row_to_num("D"), 4L)
  expect_identical(row_to_num("B"), 2L)
})

test_that("num_to_row works", {
  expect_identical(num_to_row(4), "D")
  expect_identical(num_to_row(2), "B")
})

test_that("get_row works", {
  expect_identical(get_row("C05"), "C")
  expect_identical(get_row("D11"), "D")
})

test_that("col_to_num works", {
  expect_identical(col_to_num("05"), 5L)
  expect_identical(col_to_num("11"), 11L)
})

test_that("num_to_col works", {
  expect_identical(num_to_col(5), "05")
  expect_identical(num_to_col(11), "11")
})

test_that("get_col works", {
  expect_identical(get_col("C05"), "05")
  expect_identical(get_col("D11"), "11")
})
