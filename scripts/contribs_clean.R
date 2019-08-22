

library(tidyverse)

path <- getwd()


# read in contrib data

contrib_data <- read.table(file=paste(path, "/raw_data/campaignfinancecontributions.csv", sep=""), header=TRUE, sep=",")

head(contrib_data)