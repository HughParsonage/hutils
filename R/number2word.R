

number2word <- function(n, zero = "zero") {
  stopifnot(is.integer(n) ||
            max(n - as.integer(n)) < sqrt(.Machine$double.eps),
            min(n) >= 0L)
  out <- character(length(n))
  maxn <- max(n)
  out[n == 0L] <- zero
  
  out[n %between% c(1L, 100L)] <-
    .subset2(Table_1100, "ans")[n[n %between% c(1L, 100L)]]
  
  if (maxn > 99L) {
    
    n_blw_99 <- number2word(n %% 100L,
                            # don't want 'one hundred and zero'
                            zero = "")
    n_blw_99[nzchar(n_blw_99)] <- sprintf(" and %s", n_blw_99[nzchar(n_blw_99)])
    
    if (max(n) <= 999L) {
      out <- if_else(between(n, 100L, 999L),
                     paste0(number2word(n %/% 100L),
                            " hundred", n_blw_99),
                     out)
    } else {
      out <- if_else(between(n, 100L, 999L),
                     paste0(number2word(n %/% 100L),
                            " hundred", n_blw_99),
                     if_else(n > 999L,
                             paste0(number2word(n %/% 1000L),
                                    " thousand, ", 
                                    number2word(n %% 1000L)),
                             out))
    }
  }
  out
}

.validWords2Numbers <- function(w) {
  out <- match(w, .subset2(Table_1100, "ans"), nomatch = NA_integer_)
  if (anyNA(out)) {
    out[is.na(out)] <- w[is.na(out)]
  }
  out
}

.thousands2Expr <- function(w) {
  
  o <- w
  jj <- paste(number2word(999:1), "thousand")
  
  for (i in seq_along(o)) {
    if (!nzchar(w[i])) {
      o[i] <- 0
    } else {
      if (grepl("thousand", o[i], fixed = TRUE)) {
        for (j in jj) {
          if (grepl(j, o[i], fixed = TRUE)) {
            break
          }
        }
        o[i] <- paste(1000L - match(j, jj, nomatch = NA_integer_),
                      "* 1000")
        if (!endsWith(w[i], "thousand")) {
          # Currently, o[i] ends with the thousands
          # term. Need to retain the rest.
          o[i] <- paste(o[i],
                        "+",
                        sub("^.*thousand", "", w[i], perl = TRUE))
        }
      }
    }
  }
  o
}

word2number <- function(w) {
  if (length(w) > 100L) {
    DT <- setDT(list(w = w))
    return(DT[, "res" := word2number(.BY[[1L]]), by = "w"][["res"]])
  }
  
  if (any_grepl(w, "million")) {
    millions <- 
      if_else(grepl("million", w, fixed = TRUE),
              word2number(sub(" million.*$", "", w)),
              0)
  } else {
    millions <- 0
  }
  
  gsub2 <- function(x, pattern, replacement, ...) {
    gsub(pattern, replacement, x, ...)
  }
  res_minus_M <- 
    .thousands2Expr(sub("^.*million", "", w)) %>%
    gsub2(paste0("(", paste0(one_to_nine, collapse = "|"), ")\\s*", "hundred"),
          "\\1 * 100") %>%
    gsub2("( and ?)+", # and  and  can occur
          " + ") %>% 
    gsub2(",\\s*", " + ") %>%
    gsub(x = ., pattern = "-", fixed = TRUE, replacement = " + ") %>%
    strsplit(split = " ", perl = TRUE) %>%
    lapply(.validWords2Numbers) %>%
    lapply(paste0, collapse = " ") %>%
    vapply(function(x) eval(parse(text = x)), double(1))
  
  millions * 10^6 + res_minus_M
}


