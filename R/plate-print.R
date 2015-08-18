#' Print info about a ddPCR plate
#' @export
#' @keywords internal
print.ddpcr_plate <- function(x, ...) {
  if (is_empty_plate(x)) {
    cat0("Empty ddPCR plate")
    return()
  }
  
  if (!analysis_complete(x)) {
    spaces <- "            "
  } else {
    spaces <- ""
  }
  
  cat0(spaces, "        ddpcr plate\n")
  cat0(spaces, "       -------------\n")
  cat0(spaces, "Dataset name : ", x %>% name, "\n")
  cat0(spaces, "Data summary : ", 
       x %>% wells_used %>% length, " wells; ",
       x %>% plate_data %>% nrow %>% prettyNum(big.mark = ","), " drops\n")
  cat0(spaces, "  Plate type : ", x %>% type(TRUE) %>% paste(collapse = ", "), "\n")
  if (analysis_complete(x)) {
    cat0("      Status : Analysis completed\n")
  } else {
    cat0("Completed analysis steps : ",
         step_name(x, seq(1, status(x))) %>% paste(collapse = ", "),
         "\n"
    )
    cat0("Remaining analysis steps : ",
         step_name(x, seq(status(x) + 1, length(steps(x)))) %>% paste(collapse = ", "),
         "\n"
    )
  }
}