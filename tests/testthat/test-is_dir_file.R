context("is_dir_file")

test_that("is_dir is TRUE when passed a directory", {
  expect_true(is_dir("."))
  expect_true(is_dir("../testthat"))
  expect_true(is_dir("../../tests"))
  expect_true(is_dir("../../tests/"))
})

test_that("is_dir is FALSE when passed a file or non-existent dir", {
  expect_false(is_dir("nosuchdir"))
  expect_false(is_dir("nosuchdir/"))
  expect_false(is_dir("test-rutilsMock.R"))
  expect_false(is_dir("./test-rutilsMock.R"))
  expect_false(is_dir("../testthat.R"))
})

test_that("is_file is TRUE when passed a file", {
  expect_true(is_file("test-is_dir_file.R"))
  expect_true(is_file("./test-is_dir_file.R"))
  expect_true(is_file("../testthat.R"))
})

test_that("is_file is FALSE when passed a directory or non-existent file", {
  expect_false(is_file("."))
  expect_false(is_file("../testthat"))
  expect_false(is_file("../../tests"))
  expect_false(is_file("../../tests/"))
  expect_false(is_file("test-nosuchfile.R"))
})