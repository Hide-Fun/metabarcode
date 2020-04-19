library(tidyverse)
test <- read_csv("data/SampleSheet.csv", skip = 20)
index <- make_index("data/SampleSheet.csv", 20)

