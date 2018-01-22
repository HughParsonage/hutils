#' Report errors and warnings
#' 
#' @description Provides a consistent style for errors and warnings.
#' 
#' @param faulty_input Unquoted function argument that is the cause of the error condition.
#' @param error_condition A sentence explaining the condition that invoked the error.
#' @param requirements A sentence that explains what is required.
#' @param context (Optional) A sentence that contextualizes the error
#' @param advice Advice for the user to avoid the error.
#' @param hint If the input can be guessed, 
#' @param halt (logical, default: \code{TRUE}) Should the function signal an error and halt?
#' @export report_error

report_error <- function(faulty_input,
                         error_condition,
                         requirement, 
                         context = NULL,
                         advice, 
                         hint = NULL,
                         halt = TRUE) {
  if (halt) {
    # Ensure hint is surrounded in parentheses
    HINT <- if (!is.null(hint)) {
      format_hint(hint)
    } 
    faulty_input_deparsed <- deparse(substitute(faulty_input))
    stop("`", faulty_input_deparsed, "` ", 
         error_condition, " ",
         requirement, " ", 
         context,
         advice, 
         HINT, 
         call. = FALSE)
  }
    
}


report_warning <- function() {
  
}


# Difficult to test
# Performance not a problem as we are stopping
format_hint <- function(x) {
  sub("^([^\\(])", "(\\1", perl = TRUE,
      sub("([^\\)])$", "\\1?)" , perl = TRUE,
          sub("[[:punct:]][)]?$", "", perl = TRUE,
              x)))
}
stopifnot(identical(format_hint(c("(A parenthesis.)", "Non-parenthesis", "(A question?)", "B question?")),
                    c("(A parenthesis?)", "(Non-parenthesis?)", "(A question?)", "(B question?)")))
