## ddpcr - R package for analysis of droplet digital PCR data
## Copyright (C) 2015 Dean Attali

.pkg_globals_store <- function() {
  .store <- new.env()
  list(
    get = function(x) .store[[x]],
    set = function(x, v) .store[[x]] <- v
  )
}
.globals <- .pkg_globals_store()

# R CMD check complains about . in magrittr pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))