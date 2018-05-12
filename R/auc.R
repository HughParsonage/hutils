#' AUC 
#' @description Returns the area under the curve ("AUC") of a receiver-operating characteristic curve for the given predicted and actual values.
#' @param actual Logical vector: \code{TRUE} for positive class. 
#' If not a logical vector, the result is interpreted as one if safe to do so,
#'  \emph{viz.} if \code{actual} contains precisely two unique values and is either a numeric vector,
#'  an ordered factor, or the unique values are \code{FALSE} and \code{TRUE} (case-insensitively).
#'  Anything else is an error.
#' @param pred Numeric (double) vector the same length as \code{actual} giving the predicted probability of \code{TRUE}. Must be a numeric vector the same length as \code{actual}.
#' @source Source code based on \code{Metrics::auc} from Ben Hamner and Michael Frasco and Erin LeDell from the Metrics package.
#' @export auc

#' @author



auc <- function(actual, pred) {
  if (length(pred) != length(actual)) {
    stop("`pred` and `actual` had unequal lengths: length(pred) = ", length(pred),
         ", length(actual) = ", length(actual), ". ",
         "Ensure they have equal lengths.")
  }
  
  if (anyNA(actual)) {
    stop("`actual` contained NAs. Remove or impute NAs as desired.")
  }
  if (anyNA(pred)) {
    stop("`pred` contained NAs. Remove or impute NAs as desired.")
  }
  
  if (!is.double(pred)) {
    # Switch the values
    if (is.logical(pred) && is.double(actual)) {
      # Immediate return
      message("`pred` was type logical and `actual` was type double, so interpreting as auc(pred, actual). Set to auc(actual, pred) for standard behaviour.")
      return(auc(actual = pred, pred = actual))
    }
    
    if (is.integer(pred) || is.logical(pred)) {
      pred <- as.logical(pred)
      actual <- as.logical(actual)
      if (identical(pred, actual)) {
        return(1)
      } else {
       return(0)
      }
    } else {
      stop("`pred` was type ", typeof(pred), ", which is unsupported. ",
           "Supply a numeric vector whose values are all between 0 and 1.")
    }
  }
  
  if (length(actual) <= 1L) {
    if (length(actual) == 1L) {
      if (pred < 0 || pred > 1) {
        stop("`pred = ", pred, "` was not between 0 and 1. ", 
             "pred must between 0 and 1.")
      }
      if (actual) {
        return(pred)
      } else {
        return(1 - pred)
      }
    } else {
      return(double(0L))
    }
  }
  
  
  if (min(pred) < 0 || max(pred) > 1) {
    stop("`range(pred) = ", range(pred), ". ", 
         "All values of `pred` must be between 0 and 1 (inclusive).")
  }
  
  switch(typeof(actual),
         "logical" = {
           # OK
         },
         "integer" = {
           if (!identical(sort(unique(actual)), 0:1)) {
             # Also for ordered factors
             if (uniqueN(actual) == 2L) {
               if (is.factor(actual) && !is.ordered(actual)) {
                 if (identical(toupper(levels(actual)), c("FALSE", "TRUE"))) {
                   actual <- as.logical(actual)
                 } else {
                   stop("`actual` was an factor with two levels but had no ordering, ",
                        "so the AUC cannot be calculated. ",
                        "Make actual a logical vector or impose an ordering on the two levels.")
                 }
               } else {
                 actual <- actual == max(actual)
               }
             } else {
               stop("`actual` was an integer vector ", 
                    "but could not be safely mapped to a logical vector ",
                    "as it had ", uniqueN(actual), " values. ", 
                    "Set `actual` to a logical vector.")
             }
           }
         },
         "double" = {
           if (uniqueN(actual) == 2L) {
             actual <- actual == max(actual)
           } else {
             stop("`actual` had type double, but had more than 2 unique values, ",
                  "so the AUC cannot be calculated. ",
                  "Supply a logical vector for `actual`.")
           }
         },
         "character" = {
           if (identical(sort(toupper(unique(actual))), c("FALSE", "TRUE"))) {
             actual <- as.logical(actual)
           } else {
             stop("`actual` was type character: this is only allowed if actual only ",
                  "contains 'TRUE' and 'FALSE'.")
           }
         },
         stop("`actual` had type ", typeof(actual), ", which is not supported. ",
              "Supply a logical vector for actual."))
  
  #' Copyright (c) 2012, Ben Hamner
  #' Author: Ben Hamner (ben@benhamner.com)
  #' All rights reserved.
  #' 
  #' Redistribution and use in source and binary forms, with or without
  #' modification, are permitted provided that the following conditions are met:
  #' 
  #'   1. Redistributions of source code must retain the above copyright notice, this
  #' list of conditions and the following disclaimer.
  #' 2. Redistributions in binary form must reproduce the above copyright notice,
  #' this list of conditions and the following disclaimer in the documentation
  #' and/or other materials provided with the distribution.
  #' 
  #' THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
  #' ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  #' WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  #' DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
  #' ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  #' (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  #'   LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
  #' ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  #' (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  #' SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  
  r <- frank(pred)
  actual <- as.logical(actual)
  ra <- r[actual]
  n_pos <- length(ra)
  len_actual <- length(actual)
  n_neg <- len_actual - n_pos
  sum(ra)/n_pos/n_neg - (n_pos + 1)/2/(n_neg)
}
