.pkg_globals_store <- function() {
  .store <- new.env()
  list(
    get = function(x) .store[[x]],
    set = function(x, v) .store[[x]] <- v
  )
}
.globals <- .pkg_globals_store()

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))