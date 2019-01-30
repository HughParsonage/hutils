
stopifnot(!isNamespaceLoaded("hutils"))
# Test previously released packages

library(magrittr)
library(data.table)
library(microbenchmark)

fwrite <- function(x, file, ...) {
  hutils::provide.dir(dirname(file))
  data.table::fwrite(x, file, ...)
}

cur_dir <- function() {
  basename(dirname(normalizePath(dir(full.names = TRUE)[1L])))
}

do_benchmark <- function(..., TIMES = 100L) {
  LL <- substitute(alist(...))
  a <- microbenchmark(list = LL, times = TIMES)
  b <- as.data.table(a)
  b[, Source := cur_dir()]
}

z <- as.logical(sample.int(1e5L) %% 3L)
y <- sample(c(FALSE, TRUE, NA), size = 1e8, prob = c(0.5, 0.4, 0.1), replace = TRUE)
x <- rep_len(z, 1e8L)
N <- rnorm(1e5)
M <- as.character(round(N, 2))
C <- as.character(round(N, 3))
CC <- as.character(round(rnorm(1e8), 1))
DD <- as.character(round(rnorm(1e8), 1))
Z <- sample.int(100L, size = 1e8, replace = TRUE)
Y <- sample.int(1e8L)
U01_1e7 <- runif(1e7)
U01_1e6 <- runif(1e5)
U01_1e2 <- runif(1e2)
TF_1e7 <- rnorm(1e7) > 0.1
TF_1e6 <- TF_1e7[1:1e6]
TF_1e2 <- TF_1e6[1:100]

for (version in c("1.0.0", "1.1.0", "1.2.0", "1.3.0")) {
  dir.create(temp_lib <- tempfile())
  devtools::install_version("hutils", version = version, lib = temp_lib)
  
  for (v.R in dir("timings",
                  pattern = "\\.R$",
                  full.names = TRUE,
                  recursive = TRUE)) {
    if (dirname(v.R) == "timings") {
      next
    }
    library("hutils", lib.loc = temp_lib)
    source(v.R)
    detach("package:hutils", unload = TRUE)
  }
}



