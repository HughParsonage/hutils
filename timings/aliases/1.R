
if (FALSE && version >= "1.2.0") {
the_filename <- parent.frame(2)$ofile
out_csv <-
  file.path(dirname(the_filename),
            "releases",
            paste0(strsplit(version, split = ".", fixed = TRUE)[[1L]], collapse = "-"),
            sub("\\.R$", ".csv", basename(the_filename)))

microbenchmark(A1 = XOR(TRUE, NA),
               A2 = XOR(FALSE, TRUE),
               A3 = XOR(TRUE, TRUE)) %>%
  as.data.table %>%
  .[, Filename := the_filename] %>%
  .[, Version := as.character(version)] %>%
  fwrite(out_csv)
}
cat(basename(dirname(normalizePath(dir(full.names = TRUE)[1]))), "\n")

