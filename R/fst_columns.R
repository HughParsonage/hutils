#' Utilities for `fst` files
#' 
#' @param file.fst Path to file.
#' 
#' @return Various outputs:
#' \describe{
#' \item{\code{fst_columns}}{Returns the names of the columns in \code{file.fst}.}
#' \item{\code{fst_nrow}}{Returns the number of rows in \code{file.fst}.}
#' }
#' 
#' @export

fst_columns <- function(file.fst) {
  if (!requireNamespace("fst", quietly = TRUE)) {
    stop("`fst_columns` requires package:fst.")
  }
  
  assert_file_readable(file.fst, "file.fst")
  fst::metadata_fst(file.fst)[["columnNames"]]
}

#' @rdname fst_columns
#' @export
fst_nrow <- function(file.fst) {
  fst::metadata_fst(file_fst(file.fst))[["nrOfRows", exact = TRUE]]
}


assert_file_readable <- function(file.ext, vname) {
  if (length(file.ext) != 1) {
    stop("`length(", vname, ") = ", length(file.ext), "` but must be length-one.")
  }
  if (!is.character(file.ext)) {
    stop("`", vname, "` was type '", typeof(file.ext), "' but must be character.")
  }
  if (!file.exists(file.ext)) {
    stop("`", vname, "`:\n\t",
         file.ext, "\n",
         "did not exist.")
  }
  if (file.access(file.ext, mode = 4L)) {
    stop("`", vname, "`:\n\t",
         file.ext, "\n",
         "exists but was not readable.")
  }
}
