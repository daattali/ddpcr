context("merge_dfs_overwrite_col")

df <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
} 

test_that("the new column overwrites the old column", {
  df1 <- df(a = 1:4, b = c("one", "two", "three", "four"))
  df2 <- df(a = 1:4, b = c("ONE", "TWO", "THREE", "FOUR"))
  expect_equal(
    merge_dfs_overwrite_col(df1, df2, "b", "a"),
    df2
  )
  expect_equal(
    merge_dfs_overwrite_col(df2, df1, "b", "a"),
    df1
  )
})

test_that("the new column doesn't overwrite when it has a missing value", {
  df1 <- df(a = 1:4, b = c("one", NA, "three", "four"))
  df2 <- df(a = 1:4, b = c("ONE", "TWO", NA, "FOUR"))
  expect_equal(
    merge_dfs_overwrite_col(df1, df2, "b", "a"),
    df(a = 1:4, b = c("ONE", "TWO", "three", "FOUR"))
  )
  expect_equal(
    merge_dfs_overwrite_col(df2, df1, "b", "a"),
    df(a = 1:4, b = c("one", "TWO", "three", "four"))
  )
})

test_that("only the rows from the old column are preserved", {
  df1 <- df(a = 1:3, b = c("one", NA, "three"))
  df2 <- df(a = 2:4, b = c("TWO", NA, "FOUR"))
  df3 <- df(a = 5, b = "five")
  expect_equal(
    merge_dfs_overwrite_col(df1, df2, "b", "a"),
    df(a = 1:3, b = c("one", "TWO", "three"))
  )
  expect_equal(
    merge_dfs_overwrite_col(df2, df1, "b", "a"),
    df(a = 2:4, b = c("TWO", "three", "FOUR"))
  )
  expect_equal(
    merge_dfs_overwrite_col(df1, df3, "b", "a"),
    df1
  )
})

test_that("NA value is kept if both columns are missing", {
  df1 <- df(a = 1:3, b = c("one", NA, "three"))
  df2 <- df(a = 1:3, b = c("ONE", NA, "THREE"))
  expect_equal(
    merge_dfs_overwrite_col(df1, df2, "b", "a"),
    df2
  )  
})

test_that("the position of the column in the old data.frame is preserved", {
  df1 <- df(a = 1:3, b = c("one", "two", "three"))
  df2 <- df(b = c("ONE", "TWO", "THREE"), a = 1:3)
  expect_equal(
    merge_dfs_overwrite_col(df1, df2, "b", "a"),
    df(a = 1:3, b = c("ONE", "TWO", "THREE"))
  )
  expect_equal(
    merge_dfs_overwrite_col(df2, df1, "b", "a"),
    df(b = c("one", "two", "three"), a = 1:3)
  ) 
})

test_that("the correct column is overwritten", {
  df1 <- df(a = 1:3, b = c("one", "two", "three"), c = letters[1:3])
  df2 <- df(a = 1:3, b = c("ONE", "TWO", "THREE"), c = LETTERS[1:3])
  expect_equal(
    merge_dfs_overwrite_col(df1, df2, "b", "a"),
    df(a = 1:3, b = c("ONE", "TWO", "THREE"), c.x = letters[1:3], c.y = LETTERS[1:3])
  )
  expect_equal(
    merge_dfs_overwrite_col(df1, df2, "c", "a"),
    df(a = 1:3, b.x = c("one", "two", "three"), c = LETTERS[1:3], b.y = c("ONE", "TWO", "THREE"))
  ) 
})

test_that("the correct column is overwritten", {
  df1 <- df(a = 1:3, b = c("one", "two", "three"), c = 1:3)
  df2 <- df(a = 1:3, b = c("ONE", "TWO", "THREE"), c = 3:1)
  expect_equal(
    merge_dfs_overwrite_col(df1, df2, "b", "a"),
    df(a = 1:3, b = c("ONE", "TWO", "THREE"), c.x = 1:3, c.y = 3:1)
  )
  expect_equal(
    merge_dfs_overwrite_col(df1, df2, "b", "c"),
    df(a.x = 1:3, b = c("THREE", "TWO", "ONE"), c = 1:3, a.y = 3:1)
  )
})

test_that("overwriting multiple columns works", {
  df1 <- df(a = 1:3, b = c("one", "two", "three"), c = 1:3)
  df2 <- df(a = 1:3, b = c("ONE", NA, "THREE"), c = 4:6)
  expect_equal(
    merge_dfs_overwrite_col(df1, df2, c("b", "c"), "a"),
    df(a = 1:3, b = c("ONE", "two", "THREE"), c = 4:6)
  )
  expect_equal(
    merge_dfs_overwrite_col(df2, df1, c("b", "c"), "a"),
    df(a = 1:3, b = c("one", "two", "three"), c = 1:3)
  )
})

test_that("if one dataframe doesn't have the column, the other df is used", {
  df1 <- df(a = 1:3, b = c("one", "two", "three"))
  df2 <- df(a = 1:4)
  expect_equal(
    merge_dfs_overwrite_col(df1, df2, "b", "a"),
    df1
  )
  expect_equal(
    merge_dfs_overwrite_col(df2, df1, "b", "a"),
    df(a = 1:4, b = c("one", "two", "three", NA))
  )  
})

test_that("if a non-existent column is given, the original df is returned", {
  df1 <- df(a = 1:4, b = c("one", "two", "three", "four"))
  df2 <- df(a = 1:4, b = c("ONE", "TWO", "THREE", "FOUR"))
  expect_equal(
    merge_dfs_overwrite_col(df1, df2, "x", c("a", "b")),
    df1
  )
  expect_equal(
    merge_dfs_overwrite_col(df2, df1, "x", c("a", "b")),
    df2
  )  
})

test_that("if `cols` is not supplied then all columns from old df are retained", {
  df1 <- df(a = 1:3, b = c("one", "two", "three"), c = 1:3)
  df2 <- df(a = 1:3, b = c("ONE", "TWO", "THREE"), c = 3:1)
  df3 <- df(a = 1:3, b = c("ONE", "TWO", "THREE"), d = 3:1)
  
  expect_equal(
    merge_dfs_overwrite_col(df1, df2, bycol = "a"),
    df2
  )
  expect_equal(
    merge_dfs_overwrite_col(df2, df1, bycol = "a"),
    df1
  )
  expect_equal(
    merge_dfs_overwrite_col(df1, df2, "b", bycol = "a"),
    df(a = 1:3, b = c("ONE", "TWO", "THREE"), c.x = 1:3, c.y = 3:1)
  )
  expect_equal(
    merge_dfs_overwrite_col(df1, df3, bycol = "a"),
    df(a = 1:3, b = c("ONE", "TWO", "THREE"), c = 1:3, d = 3:1)
  )
})