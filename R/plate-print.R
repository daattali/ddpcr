#' Print info about a ddPCR plate
#' @export
#' @keywords internal
print.ddpcr_plate <- function(x, ...) {
  if (is_empty_plate(x)) {
    cat0("Empty ddPCR plate")
    return()
  }
  
  # The following code is some very ugly and messy logic that simply tries
  # to print the information as beautifully as possible. Warning: VERY UGLY CODE!
  width <- getOption("width")
  if (width > 60) {
    if (analysis_complete(x)) {
      longest <- nchar("Dataset name")
    } else {
      longest <- nchar("Completed analysis steps")
    }
  } else {
    longest <- 0
  }
  sp <- function(x, offset = 0) {
    sprintf(paste0("% ", longest + offset, "s"), x)
  }
  sp_list <- function(x) {
    if (length(x) == 1) {
      return(x)
    }
    if (width < 60) {
      return(paste(x, collapse = ", "))
    }
    
    string <- x[1]
    total <- nchar(string)
    for (i in seq(2, length(x))) {
      total <- total + nchar(x[i]) + 2
      if (total + longest + 3 > width) {
        total <- nchar(x[i])
        string <- paste0(string, ",\n", sp(""), "   ", x[i])
      } else {
        string <- paste(string, x[i], sep = ", ")
      }
    }
    string
  }
  
  # print the actual information
  cat0(sp(" ddpcr plate\n", 8))
  cat0(sp("-------------\n", 9))
  cat0(sp("Dataset name"), " : ", x %>% name, "\n")
  cat0(sp("Data summary"), " : ", 
       x %>% wells_used %>% length, " wells; ",
       x %>% plate_data %>% nrow %>% prettyNum(big.mark = ","), " drops\n")
  cat0(sp("Plate type"), " : ", sp_list(type(x, TRUE)), "\n")
  if (analysis_complete(x)) {
    cat0(sp("Status"), " : Analysis completed\n")
  } else {
    cat0("Completed analysis steps : ",
         sp_list(step_name(x, seq(1, status(x)))),
         "\n"
    )
    cat0("Remaining analysis steps : ",
         sp_list(step_name(x, seq(status(x) + 1, length(steps(x))))),
         "\n"
    )
  }
  
  if (is_dirty(x)) {
    cat0("\nNote: some settings have changed, please re-run the analysis with `analyze(plate, restart = TRUE)` for changes to take effect")
  }
}