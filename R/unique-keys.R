#' Unique keys
#' @name unique-keys
#' @param DT A data.table
#' @param ... keys to set
#' @return \code{has_unique_key} returns \code{TRUE} if \code{DT} has a unique key, \code{FALSE} otherwise. 
#' \code{setuniquekey} runs \code{setkey(DT, ...)} then checks whether the key is unique, returning the keyed 
#' \code{data.table} if the key is unique, or an error message otherwise.
#' @export has_unique_key set_unique_key
NULL

#' @rdname unique-keys
has_unique_key <- function(DT){
  haskey(DT) && (uniqueN(DT, by = key(DT)) == length(.subset2(DT, 1)))
}

#' @rdname unique-keys
set_unique_key <- function(DT, ...){
  setkey(DT, ...)
  if (has_unique_key(DT)){
    return(DT)
  } else {
    stop("Key is not unique. DT's key has changed.")
  }
}
