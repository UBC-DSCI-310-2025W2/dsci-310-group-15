library(tidyverse)

normal1_df <- tibble("target" = as.factor(c("Y", "N", "N", "N", "Y", "Y", "N", "Y", "N", "N")),
                     "a" = 1:10,
                     "b" = 1:10,
                     "c" = 1:10)

target_df <- tibble("target" = as.factor(c("Y", "N", "N", "N", "Y", "Y", "N", "Y", "N", "N")))

fake_df <- list("Y", "N", "N", "N", "Y", "Y", "N", "Y", "N", "N")