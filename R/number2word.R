

number2word <- function(n, zero = "zero") {
  stopifnot(is.integer(n), min(n) >= 0L)
  out <- character(length(n))
  maxn <- max(n)
  out[n == 0L] <- zero
  
  out[n %between% c(1L, 100L)] <- .subset2(Table_1100, "ans")[n[n %between% c(1L, 100L)]]
  
  if (maxn > 99L) {
    
    n_blw_99 <- number2word(n %% 100L,
                            # don't want 'one hundred and zero'
                            zero = "")
    n_blw_99[nzchar(n_blw_99)] <- sprintf(" and %s", n_blw_99[nzchar(n_blw_99)])
    
    out <- if_else(n >= 100L & n <= 999L,
                   paste0(number2word(n %/% 100L),  " hundred", n_blw_99),
                   out)
                   
                   
  }
  out
}

.word2number <- function(w) {
  match(w, .subset2(Table_1100, "ans"), nomatch = NA_integer_)
}

.validWords2Numbers <- function(w) {
  out <- match(w, .subset2(Table_1100, "ans"), nomatch = NA_integer_)
  out[is.na(out)] <- w[is.na(out)]
  out
}

word2number <- function(w) {
  gsub(" and ", " + ",
       gsub(paste0("(", paste0(one_to_nine, collapse = "|"), ")\\s*", "hundred"),
            "\\1 * 100", w)) %>% 
    gsub(x = ., pattern = "-", fixed = TRUE, replacement = " + ") %>%
    strsplit(split = " ", perl = TRUE) %>%
    lapply(.validWords2Numbers) %>%
    lapply(paste0, collapse = " ") %>%
    vapply(function(x) eval(parse(text = x)), double(1))
}


