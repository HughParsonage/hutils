#' Unique keys
#' @name unique-keys
#' @description A \code{data.table}'s \code{key} need not be unique, but there are frequently circumstances
#' where non-unique keys can wreak havoc.
#' \code{has_unique_key} reports the existence of a unique key, and 
#' \code{set_unique_key} both sets and ensures the uniqueness of keys.
#' @param DT A data.table
#' @param ... keys to set
#' @return \code{has_unique_key} returns \code{TRUE} if \code{DT} has a unique key, \code{FALSE} otherwise. 
#' \code{set_unique_key} runs \code{setkey(DT, ...)} then checks whether the key is unique, returning the keyed 
#' \code{data.table} if the key is unique, or an error message otherwise. 
#' @export has_unique_key set_unique_key
NULL

#' @rdname unique-keys
has_unique_key <- function(DT){
  haskey(DT) && uniqueN(DT, by = key(DT)) == length(.subset2(DT, 1L))
}

#' @rdname unique-keys
set_unique_key <- function(DT, ...){
  setkey(DT, ...)
  if (has_unique_key(DT)){
    return(DT)
  } else {
    stop("`DT` has a key but it is not unique. Note: DT's key has changed.")
  }
}
