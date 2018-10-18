context("mutate_ntile (non-stable)")

test_that("Error handling", {
  skip_if_not_installed("dplyr")
  library(data.table)
  skip_if(exists("y"))
  DT <- data.table(x = 1:101)
  expect_error(mutate_ntile(DT, n = 1:2),
               regexp = "length(n) = 2", 
               fixed = TRUE)
  expect_error(mutate_ntile(DT, n = "1"),
               regexp = "type character", 
               fixed = TRUE)
  expect_error(mutate_ntile(DT, n = 2.5),
               regexp = "n != as.integer(n)", 
               fixed = TRUE)
  expect_error(mutate_ntile(DT, y, n = 5),
               regexp = "`col = y` but this was not a column of `DT`.")
  expect_error(mutate_ntile(DT, y, n = 2),
               regexp = "`col = y` but this was not a column of `DT`.")
  expect_error(mutate_ntile(DT, y, n = 5, character.only = TRUE),
               "object.*not found")
  
  expect_error(mutate_ntile(DT, n = 2),
               regexp = "`col` is missing", 
               fixed = TRUE)
  expect_error(mutate_ntile(DT, x, n = 17),
               regexp = "no default column suffix is supported", 
               fixed = TRUE)
  
  expect_error(mutate_ntile(DT, x, n = 10, new.col = "x", overwrite = FALSE),
               regexp = "overwrite = TRUE", 
               fixed = TRUE)
  
  expect_message(mutate_ntile(DT, x, n = 1, new.col = "Single1"),
                 regexp = "adding a column of 1s",
                 fixed = TRUE)
  
  expect_error(mutate_ntile(DT, x, n = -1, new.col = "xy", overwrite = FALSE),
               regexp = "must be a single positive whole number", 
               fixed = TRUE)
  
  expect_error(mutate_ntile(as.list(DT), x, n = 2, new.col = "xy", overwrite = FALSE),
               regexp = "must be a data.frame", 
               fixed = TRUE)
  DT <- data.table(x = 1:10)
  DT[5L, x := NA_integer_]
  expect_equal(mutate_ntile(DT, "x", n = 2L, new.col = "x1", check.na = FALSE)[-5],
               # Not guaranteed:
               data.table(x = DT$x, x1 = dplyr::ntile(DT$x, 2))[-5])
  expect_error(mutate_ntile(DT, "x", n = 4L, new.col = "x1", check.na = TRUE),
               regexp = "check.na = TRUE",
               fixed = TRUE)
  DT[1L, x := 3L]
  expect_error(mutate_ntile(DT, "x", n = 4L, new.col = "x11", check.na = TRUE),
               regexp = "check.na = TRUE",
               fixed = TRUE)
  
})

test_that("NSE 1", {
  library(data.table)
  DT2 <- data.table(x = 1:200, y = rep(1:10, 20L))
  y <- "x"
  expect_warning(mutate_ntile(DT2, y, n = 5),
                 regexp = "DT[['x']]", 
                 fixed = TRUE)
  y <- "y_y"
  expect_message(mutate_ntile(DT2, y, n = 5),
                 "extant object")
  
})

test_that("NSE 2", {
  DT2 <- data.table(x = 1:200, y = rep(1:10, 20L))
  y <- "x"
  expect_warning(mutate_ntile(DT2, y, n = 5))
  yy <- "x"
  expect_warning(mutate_ntile(DT2, yy, n = 5),
                 regexp = "Interpreting `col = yy` as `col = x`.",
                 fixed = TRUE)
  yy <- "xx"
  expect_error(mutate_ntile(DT2, yy, n = 5),
               regexp = "not a column of `DT`",
               fixed = TRUE)
  
})

test_that("tibble", {
  skip_on_cran()
  skip_if_not_installed("tibble")
  TIB <- tibble::tibble(hadley = 1:4)
  expect_identical(mutate_ntile(TIB, "hadley", n = 2, new.col = "hadleyTile"),
                   tibble::tibble(hadley = 1:4, hadleyTile = rep(1:2, each = 2L)))
  asf <- function(x) as.data.frame(x)
  TIB <- asf(tibble::tibble(hadley = 1:4))
  expect_identical(mutate_ntile(TIB, "hadley", n = 2, new.col = "hadleyTile"),
                   asf(tibble::tibble(hadley = 1:4, hadleyTile = rep(1:2, each = 2))))
  
})

test_that(".ntile", {
  expect_error(.ntile(c(1, 2, 5, 4), n = 1, check.sorted = TRUE), 
               regexp = "`x` must be already sorted",
               fixed = TRUE)
  expect_error(.ntile(c(1, 2, 5, 4, NA), n = 1, check.na = TRUE), 
               regexp = "anyNA(x)` is TRUE",
               fixed = TRUE)
})

test_that("bys", {
  skip_if_not_installed("nycflights13")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tibble")
  library(tibble)
  library(nycflights13)
  library(dplyr)
  Flights <- select(flights, origin, dep_time)
  Planes <- select(planes, -tailnum)
  
  expect_error(mutate_ntile(Planes, 
                            "seats", 
                            n = 5L,
                            by = "manufacturer",
                            keyby = "manufacturer"),
               regexp = "`by` is NULL, yet `keyby` is NULL too.",
               fixed = TRUE)
  
  
  Res1 <- 
    Planes %>%
    filter(seats > 20) %>%
    mutate_ntile(seats, n = 5, by = "manufacturer") %>%
    group_by(manufacturer, seatsQuintile) %>%
    summarise(seats = mean(seats)) %>%
    filter(manufacturer == "BOEING", 
           seatsQuintile == 5)
  
  expect_identical(as.integer(Res1[["seats"]]), 272L)
  expect_true(is_tibble(Res1))
  
  ResKey <- 
    Planes %>%
    filter(seats > 20) %>%
    mutate_ntile(seats, n = 5, keyby = "manufacturer")
  
  expect_false(is_tibble(ResKey))
  expect_true(data.table::haskey(ResKey))
  
  Res1 <- 
    Planes %>%
    arrange(seats) %>%
    filter(seats > 20) %>%
    mutate_ntile(seats, n = 5, by = "manufacturer") %>%
    group_by(manufacturer, seatsQuintile) %>%
    summarise(seats = mean(seats)) %>%
    filter(manufacturer == "BOEING", 
           seatsQuintile == 5)
  
  expect_identical(as.integer(Res1[["seats"]]), 272L)
  expect_true(is_tibble(Res1))
  
  
})

test_that("Error handling (bys, definitely sorted)", {
  library(data.table)
  DT <- setDT(list(col = sample(1:5, size = 100L, replace = TRUE),
                   by = sample(LETTERS, size = 100, replace = TRUE),
                   key = runif(100)))
  setkey(DT, col)
  expect_error(mutate_ntile(DT, col, n = 5L, by = "key", keyby = "key"),
               regexp = "`by` is NULL, yet `keyby` is NULL too.",
               fixed = TRUE)
  
  mutate_ntile(DT, "col", keyby = "by", n = 10L)
  expect_identical(key(DT), "by")
  
  mutate_ntile(DT, by, keyby = "col", n = 10L)
  expect_identical(key(DT), "col")
  expect_true("byDecile" %in% names(DT))
})

