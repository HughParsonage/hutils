
if (version >= "1.2.0") {
a1 <- c(0, 1)
a2 <- -5:5
a3 <- 1:12
a4 <- sample(1:12)
a5 <- 0:1e6
a6 <- 1:1e6
a7 <- sample(a6)
a8 <- rnorm(100e3)
a9 <- a8 + min(a8) + 1
a10 <- rep_len(a4, 1e8)
a11 <- rep_len(a8, 1e8)
a12 <- c(a1, NA)
a13 <- c(a3, NA)
a14 <- c(a8, NA)

the_filename <- parent.frame(2)$ofile
out_csv <-
  file.path(dirname(the_filename),
            "releases",
            paste0(strsplit(version, split = ".", fixed = TRUE)[[1L]], collapse = "-"),
            sub("\\.R$", ".csv", basename(the_filename)))

microbenchmark(
  A1 = all_same_sign(a1),
  A2 = all_same_sign(a2),
  A3 = all_same_sign(a3),
  A4 = all_same_sign(a4),
  A5 = all_same_sign(a5),
  A6 = all_same_sign(a6),
  A7 = all_same_sign(a7),
  A8 = all_same_sign(a8),
  A9 = all_same_sign(a9),
  A10 = all_same_sign(a10),
  A11 = all_same_sign(a11),
  A12 = all_same_sign(a12),
  A13 = all_same_sign(a13),
  A14 = all_same_sign(a14)) %>%
  as.data.table %>%
  .[, Filename := the_filename] %>%
  .[, Version := as.character(version)] %>%
  fwrite(out_csv)
}

