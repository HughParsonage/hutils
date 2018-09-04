

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

word2number <- function(w) {
  W <- strsplit(w, split = "([[:punct:][:space:]]|(and))+")
  
  lapply(W, function(v) {
    u <- .word2number(v)
    print(data.table(u, v))
    if (anyNA(u)) {
      u <- if_else(v == "hundred", 100L, u)
      u <- if_else(v == "thousand", 1000L, u)
    }
    u
  }) %>% 
    lapply(function(u) {
      if (length(u) <= 1L) {
        return(u[[1L]])
      }
      for (i in seq_along(u)) {
        out <- 0L
        if (i == length(u)) {
          
        } 
          if (u[i] < 10L && {u[i + 1L] %% 10L} == 0L) {
            
          }
        }
      }
      
      switch(length(u),
             u, 
             if (max(u) <= 99L || u[1] > u[2]) sum(u) else u[2] * u[1],
             
             
    })
}


