library(data.table)
library(magrittr)
library(hutils)
one_to_nineteen <- 
  c("one", "two", "three", "four", "five", 
    "six", "seven", "eight", "nine", "ten",
    "eleven", "twelve", "thirteen", "fourteen", "fifteen", 
    "sixteen", "seventeen", "eighteen", "nineteen")

one_to_nine <- one_to_nineteen[1:9]

twenty_etc <- 
  c("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")

hundred_thousand <- 
  c("hundred", "thousand", "ten-thousand", "hundred-thousand", "million", "billion")


Table_1100 <- 
  rbind(data.table(Tens = "",
                   unit = one_to_nineteen,
                   n = 1:19),
        
        CJ(Tens = c(twenty_etc),
           unit = c("", one_to_nine),
           sorted = FALSE) %>%
          .[, n := 20:99] %>%
          .[])
Table_1100[, ans := if_else(n > 19,
                            if_else(n %% 10L > 0L,
                                    paste(Tens, unit, sep = "-"),
                                    Tens),
                            unit)]

stopifnot(Table_1100[56, n] == 56, 
          Table_1100[52, paste0(Tens, unit)] == "fiftytwo")

Table_Hundreds <- 
  data.table(Hundreds = 1:9,
             res = paste(one_to_nine, "hundred"))

CJ(Thousands = 0:999,
   Hundreds = 0:9)




