df <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}

df1 <- df(a = 1:4, b = c("one", NA, "three", "four"))
df2 <- df(a = 1:4, b = c("ONE", "TWO", NA, "FOUR"))

test1 <- df(a = 1:4, b = c("ONE", "TWO", "three", "FOUR"))
test2 <- df(a = 1:4, b = c("one", "TWO", "three", "four"))


olddf <- df1
newdf <- df2
cols <- "b"
bycol <- "a"

result <- dplyr::left_join(olddf, newdf, by = bycol)


for (colname in cols) {
colname_x <- sprintf("%s.x", colname)
colname_y <- sprintf("%s.y", colname)

if (all(c(colname_x, colname_y) %in% colnames(result))) {
    result[[colname_x]] <- ifelse(is.na(result[[colname_y]]),
                                result[[colname_x]],
                                result[[colname_y]])
    result %<>%
    dplyr::rename(!!colname:= colname_x) %>%
    dplyr::select(-all_of(colname_y))
}
}

  result