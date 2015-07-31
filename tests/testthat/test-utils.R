context("utils")

test_that("is_dir is TRUE when passed a directory", {
  expect_true(is_dir("."))
  expect_true(is_dir("../testthat"))
  expect_true(is_dir("../../tests"))
  expect_true(is_dir("../../tests/"))
})

test_that("is_dir is FALSE when passed a file or non-existent dir", {
  expect_false(is_dir("nosuchdir"))
  expect_false(is_dir("nosuchdir/"))
  expect_false(is_dir("test-none.R"))
  expect_false(is_dir("./test-none.R"))
  expect_false(is_dir("../testthat.R"))
})

test_that("is_file is TRUE when passed a file", {
  expect_true(is_file("test-utils.R"))
  expect_true(is_file("./test-utils.R"))
  expect_true(is_file("../testthat.R"))
})

test_that("is_file is FALSE when passed a directory or non-existent file", {
  expect_false(is_file("."))
  expect_false(is_file("../testthat"))
  expect_false(is_file("../../tests"))
  expect_false(is_file("../../tests/"))
  expect_false(is_file("test-nosuchfile.R"))
})

test_that(".globals store works", {
  expect_null(.globals$get("x"))
  .globals$set("x", 50)
  expect_equal(.globals$get("x"), 50)
})

test_that("err_msg works", {
  expect_error(err_msg("can't do this"))
  expect_error(err_msg("can't do this"), "can't do this")
})

test_that("warn_msg works", {
  expect_warning(warn_msg("can't do this"))
  expect_warning(warn_msg("can't do this"), "can't do this")
})

test_that("cat0 works", {
  expect_output(cat0("a", "b\n", " c ", "d"),
                "^ab\n c d$")
})

test_that("quiet works", {
  expect_output(quiet(print("hello world")), "^$")
  expect_output(quiet(cat("hello world")), "^$")
  expect_that(quiet(message("hello world"), FALSE), shows_message())
  expect_that(quiet(message("hello world")), not(shows_message()))
  expect_that(quiet(warning("hello world"), all = FALSE), gives_warning())
  expect_that(quiet(warning("hello world")), not(gives_warning()))
})

test_that("%btwn% works", {
  expect_true(5 %btwn% c(1, 10))
  expect_true(5 %btwn% c(5, 10))
  expect_false(5 %btwn% c(6, 10))
  expect_true(6 %btwn% c(10, 5))
  expect_false(4 %btwn% c(10, 5))
  expect_equal(1:5 %btwn% c(4, 10),
               c(FALSE, FALSE, FALSE, TRUE, TRUE))  
})

test_that("plus_minus works", {
  expect_equal(plus_minus(50, 30), c(50 - 30, 50 + 30))
  expect_equal(plus_minus(50, 100), c(50 - 100, 50 + 100))
})

test_that("local_maxima works", {
  expect_equal(local_maxima(1:10), 10)
  expect_equal(local_maxima(10:1), 1)
  expect_equal(local_maxima(c(1, 5, 3, 2, 4, 3)), c(2, 5))
  expect_equal(local_maxima(c(7, 1, -4, -3, -5, 6, 8, 12)), c(1, 4, 8))
  expect_equal(local_maxima(c(7, 1, -4, -3, -5, 6, 13, 12)), c(1, 4, 7))
})

test_that("local_minima works", {
  expect_equal(local_minima(1:10), 1)
  expect_equal(local_minima(10:1), 10)
  expect_equal(local_minima(c(1, 5, 3, 2, 4, 3)), c(1, 4, 6))
  expect_equal(local_minima(c(7, 1, -4, -3, -5, 6, 8, 12)), c(3, 5))
  expect_equal(local_minima(c(7, 1, -4, -3, -5, 6, 13, 12)), c(3, 5, 8))
})

test_that("diff.point2d works", {
  expect_equal(diff(point2d(c(10, 10))), sqrt(200))
  expect_equal(diff(point2d(c(10, 10)), point2d(c(5, 20))), sqrt(125))
})

test_that("move_front and move_back work", {
  df <- data.frame(a = character(0), b = character(0), c = character(0))
  expect_equal(move_front(df, "c") %>% colnames,
               c("c", "a", "b"))
  expect_equal(move_front(df, c("c", "b")) %>% colnames,
               c("c", "b", "a"))
  expect_equal(move_front(df, c("b", "c")) %>% colnames,
               c("b", "c", "a"))
  expect_equal(move_back(df, c("b")) %>% colnames,
               c("a", "c", "b"))
  expect_equal(move_back(df, c("b", "a")) %>% colnames,
               c("c", "b", "a"))
})

test_that("lol_to_df works", {
  actual <-
    vapply(c("a", "b", "c"),
           function(x) list(low = x, up = toupper(x)),
           list(character(1), character(1))) %>%
    lol_to_df("key")
  expected <-
    dplyr::data_frame(key = c("a", "b", "c"),
                      low = key,
                      up = toupper(key)) %>%    
    as.data.frame
  expect_identical(actual, expected)
})