

the_filename <- parent.frame(2)$ofile
out_csv <-
  file.path(dirname(the_filename),
            "releases",
            paste0(strsplit(version, split = ".", fixed = TRUE)[[1L]], collapse = "-"),
            sub("\\.R$", ".csv", basename(the_filename)))

invisible(gc(reset = TRUE, full = TRUE))
list(microbenchmark(A1e5_1_1 = if_else(z, 1, 1),
                    A1e5_N_1 = if_else(z, N, 1),
                    A1e5_1_N = if_else(z, 1, N),
                    A1e5_N_N = if_else(z, N, N)),
     microbenchmark(A1e8_Z_1 = if_else(x, Z, 1L),
                    A1e8_Z_Z = if_else(x, Z, Y),
                    B1e8_1_1 = if_else(y, "", " "),
                    B1e8_1_N = if_else(y, "", CC),
                    B1e8_N_1 = if_else(y, CC, " "),
                    B1e8_N_N = if_else(y, CC, DD),
                    times = 3L)) %>%
  lapply(as.data.table) %>%
  rbindlist %>%
  .[, Filename := the_filename] %>%
  .[, Version := as.character(version)] %>%
  fwrite(out_csv)


