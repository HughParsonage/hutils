#' Add a column of ntiles to a data table
#' @param DT A \code{data.table}.
#' @param col The column name (quoted or unquoted) for which quantiles are desired.
#' @param n A positive integer, the number of groups to split \code{col}.
#' @param weights If \code{NULL}, the default, use unweighted quantiles.
#'   Otherwise, a string designating the column that is passed to 
#'   \code{\link{weighted_ntile}}.
#' @param by,keyby Produce a grouped quantile column, as in \code{\link[data.table]{data.table}}.
#'   \code{keyby} will set a key on the result (\emph{i.e.} order by \code{keyby}).
#' @param new.col If not \code{NULL}, the name of the column to be added. 
#' If \code{NULL} (the default) a name will be inferred from \code{n}. 
#' (For example, \code{n = 100} will be \code{<col>Percentile}).
#' @param character.only (logical, default: \code{FALSE}) Do not contemplate 
#' \code{col} to be an unquoted column name.
#' @param overwrite (logical, default: \code{TRUE}) If \code{TRUE} and
#' \code{new.col} already exists in \code{DT}, the column will be overwritten.
#' If \code{FALSE}, attempting to overwrite an existing column is an error.
#' @param check.na (logical, default: \code{FALSE}) If \code{TRUE}, \code{NA}s
#' in \code{DT[[col]]} will throw an error. If \code{NA}'s are present, the 
#' corresponding n-tile may take any value.
#' 
#' @examples
#' library(data.table)
#' DT <- data.table(x = 1:20, y = 2:1)
#' mutate_ntile(DT, "x", n = 10)
#' mutate_ntile(DT, "x", n = 5)
#' mutate_ntile(DT, "x", n = 10, by = "y")
#' mutate_ntile(DT, "x", n = 10, keyby = "y")
#' 
#' y <- "x"
#' DT <- data.table(x = 1:20, y = 2:1)
#' mutate_ntile(DT, y, n = 5)                        # Use DT$y
#' mutate_ntile(DT, y, n = 5, character.only = TRUE) # Use DT$x
#' 
#' @return \code{DT} with a new integer column \code{new.col} containing the
#' quantiles. If \code{DT} is not a \code{data.table} its class may be preserved
#' unless \code{keyby} is used, where it will always be a \code{data.table}.
#' 
#' @export mutate_ntile



mutate_ntile <- function(DT,
                         col,
                         n,
                         weights = NULL,
                         by = NULL,
                         keyby = NULL,
                         new.col = NULL,
                         character.only = FALSE,
                         overwrite = TRUE,
                         check.na = FALSE) {
  if (length(n) != 1L) {
    stop("`length(n) = ", length(n), "`.", 
         "`n` must be a single positive whole number.")
  }
  if (!is.integer(n)) {
    if (!is.double(n)) {
      stop("`n` was type ", typeof(n), ".", 
           "`n` must be a single positive whole number.")
    }
    .n <- as.integer(n)
    if (.n != n) {
      stop("`n` was type double but `n != as.integer(n)`.", 
           "`n` must be a single positive whole number.")
    }
    n <- .n
  }
  
  if (missing(col)) {
    stop("Argument `col` is missing, with no default.")
  }
  if (character.only) {
    if (length(col) != 1L) {
      stop("`col` had length ", length(col), ". ",
           "Ensure `col` has length one.")
    }
    if (!is.character(col)) {
      stop("`col = ", col, "` was a ", class(col)[1L],
           ", but must be a length-one character.")
    }
    
    if (col %notchin% names(DT)) {
      stop("`col = ", col, "` but this is not a column in ",
           "`DT`. Ensure `col` is a string matching a name of `DT`.", 
           if (length(suggestion <- agrep(col, names(DT), value = TRUE)) == 1) {
             paste0("\n\n(Did you mean `col = ", suggestion, "`?)")
           })
    }
    
    .col <- col
    
  } else {
    # First see whether the unquoted name refers to an extant object
    # e.g.
    #  DT <- data.table(x = 1:5)
    # 
    #  x <- "foo"
    #  mutate_ntile(DT, x)  -> MSG: just use the x column, even though
    #                               'foo' might have been intended
    #
    #  y <- "foo"
    #  mutate_ntile(DT, y)  -> ERR: neither 'y' not 'foo' a column
    # 
    #  y <- "x"
    #  mutate_ntile(DT, y)  -> WARN: y may refer to 'y' or 'x'
    
    if (exists(deparse(substitute(col)),
               envir = parent.frame(),
               mode = "character")) {
      .col <- as.character(substitute(col))
      if (.col %chin% names(DT)) {
        if (col %in% names(DT)) {
          warning("Interpreting `col = ", deparse(substitute(col)), "` as ",
                  "column `DT[['", .col,  "']]`, not ",
                  "column `DT[['", col, "']]`, despite an extant object ", 
                  .col, ".")
        } else {
          message("Interpreting `col = ", deparse(substitute(col)), "` as ",
                  "column `DT[['", .col,  "']]`",
                  ",  despite an extant object `y`.")
        }
      } else {
        if (col %in% names(DT)) { # WARN
          .col <- col
          warning("Interpreting `col = ", deparse(substitute(col)), "` as ",
                  "`col = ", col, "`.")
        } else {  # Not a name of the table
          stop("`col = ", deparse(substitute(col)),
               "` but this was not a column of `DT`. ",
               "`col` must be a column name of `DT`.")
        }
      }
    } else {
      .col <- as.character(substitute(col))
      if (.col %notchin% names(DT)) {
        stop("`col = ", deparse(substitute(col)),
             "` but this was not a column of `DT`. ",
             "`col` must be a column name of `DT`.")
      }
    }
    
  }
  
  
  
  if (is.null(new.col)) {
    suffix <- 
      switch(as.character(n), 
             "3" = "Terciles",
             "4" = "Quartile", 
             "5" = "Quintile",
             "6" = "Sextile",
             "7" = "Septile",
             "8" = "Octile",
             "10" = "Decile", 
             "12" = "DuoDecile",
             "16" = "Hexadecile",
             "20" = "Vigintile",
             "100" = "Percentile", 
             "1000" = "Permilles",
             stop("`n = ", n, "` and new.col is NULL, ",
                  "but no default column suffix is supported.", 
                  "Supply a column name using `new.col`."))
    new.col <- paste0(.col, suffix)
  }
  
  if (!overwrite && new.col %chin% names(DT)) {
    stop("`DT` already has a column '", new.col, "' ",
         "and `overwrite = TRUE`, so stopping, as requested.",
         "Either supply a different name to `new.col` or set `overwrite = TRUE` ",
         "to allow the new column.")
  }
  
  # Needs to be after new.col construction to provide early return
  if (n <= 1L) {
    if (n == 1L) {
      message("`n = 1L` so adding a column of 1s. (This is an unlikely value for ntiles.)")
      if (is.data.table(DT)) {
        return(DT[, (new.col) := 1L][])
      } else {
        DT[[new.col]] <- 1L
        return(DT)
      }
    } else {
      stop("`n = ", n, "`.",
           "`n` must be a single positive whole number.")
    }
  }
  
  
  if (not_DT <- !is.data.table(DT)) {
    input_class <- class(DT)
    if (!is.data.frame(DT)) {
      stop("`DT` was a ", class(DT)[1], " but must be a data.frame.")
    }
    DT <- as.data.table(DT)
  }
  
  definitely_sorted <- function(ddt, nom, check_na) {
    if (haskey(ddt) && key(ddt)[1] == nom) {
      return(TRUE)
    }
    x <- .subset2(ddt, nom)
    if (anyNA(x)) {
      if (check_na) {
        stop("`check.na = TRUE` yet `DT[['", nom, "']]` ", 
             "so stopping, as requested.")
      }
      return(FALSE)
    }
    !is.unsorted(x)
  }
  
  
  if (is.null(weights) &&  # .ntile can't use weights
      definitely_sorted(DT, .col, check.na)) {
    if (is.null(by) && is.null(keyby)) {
      DT[, (new.col) := .ntile(.SD[[1L]], n, check.na = check.na),
         .SDcols = c(.col)]
    }
    if (!is.null(by) && !is.null(keyby)) {
      stop("`by` is NULL, yet `keyby` is NULL too. ", 
           "Only one of `by` and `keyby` may be provided.")
    }

    if (!is.null(by)) {      
      DT[, (new.col) := .ntile(.SD[[.col]], n, check.na = check.na),
         .SDcols = c(.col),
         by = c(by)]
    }
    if (!is.null(keyby)) {
      DT[, (new.col) := .ntile(.SD[[.col]], n, check.na = check.na),
         .SDcols = c(.col),
         keyby = c(keyby)]
      return(setkeyv(DT, keyby))
    }
    
  } else {
    if (!is.null(by) && !is.null(keyby)) {
      stop("`by` is NULL, yet `keyby` is NULL too. ", 
           "Only one of `by` and `keyby` may be provided.")
    }
    
    
    if (is.null(by) && is.null(keyby)) {
      DT[, (new.col) := weighted_ntile(.SD[[.col]],
                                       weights = if (!is.null(weights)){
                                         .SD[[weights]]
                                       },
                                       n = n),
         .SDcols = c(.col, weights)]
    } else if (!is.null(by)) {     
      DT[, (new.col) := weighted_ntile(.SD[[.col]],
                                       weights = if (!is.null(weights)) {
                                         .SD[[weights]]
                                       },
                                       n = n),
         .SDcols = c(.col, weights),
         by = c(by)]
    } else if (!is.null(keyby)) {
      DT[, (new.col) := weighted_ntile(.SD[[.col]],
                                       weights = if (!is.null(weights)) .SD[[weights]],
                                       n = n),
         .SDcols = c(.col, weights),
         keyby = c(keyby)]
      return(setkeyv(DT, keyby)[])
    }
    
  }
  if (not_DT) {
    setDF(DT)
    if (!identical(input_class, "data.frame")) {
      class(DT) <- input_class
    }
  }
  DT[]
}

.ntile <- function(x, n, check.sorted = FALSE, check.na = FALSE) {
  if (check.sorted && is.unsorted(x)) {
    stop("`x` must be already sorted.")
  }
  if (check.na && anyNA(x)) {
    stop("`anyNA(x)` is TRUE.")
  }
  
  as.integer(n * {seq_along(x) - 1L} / length(x) + 1L)
}



