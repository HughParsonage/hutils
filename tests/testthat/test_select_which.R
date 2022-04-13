library(data.table)
DT <- data.table(xyz = 1:5,
                 abc = 1:5 + 0,
                 ab = as.raw(1:5),
                 cdf = letters[1:5])
expect_equal(select_which(DT, is.integer, .and.grep = "^a"),
             DT[, .(xyz, abc, ab)])
