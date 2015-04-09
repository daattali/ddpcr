# Convert all columns of a dataframe to factors
factorize <- function(df) {
  df[] <- lapply(df, as.factor)
  df
}

quiet <- function(expr) {
  if (Sys.info()['sysname'] == "Windows") {
    capture.output(expr, file = "NUL")  
  } else {
    capture.output(expr, file = "/dev/null")
  }
}