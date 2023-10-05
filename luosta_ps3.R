### Lauri Luosta
### Problem Set 3
### 10/3/2023

rm(list = ls())

getwd()
setwd("C:/Users/lauri/Box/Graduate classes (luosta@wisc.edu 2)/SOC 756 Demographic Techniques II")

library(tidyverse)
library(readxl)
library(HMDHFDplus)

# Importing data

lt_2005 <- subset(readHMDweb("USA", "bltper_1x1", username = "luosta@wisc.edu", password = "Kameli1897!"), Year == 2005)

# Probability of experiencing a non-fatal motor vehicle accident

subset_lt <- lt_2005[17:32, ]

subset_lt$prob_accident <- 0.062 - 0.000053 * (subset_lt$Age^2)
subset_lt$qx_both <- subset_lt$prob_accident + subset_lt$qx

# lx for both decrements

options(scipen = 500)

subset_lt$lx_both <- NA
subset_lt$lx_both[1] <- 100000

for(i in 2:nrow(subset_lt)) {
  subset_lt$lx_both[i] <- subset_lt$lx_both[i-1] - (subset_lt$lx_both[i-1] * subset_lt$qx_both[i-1])
}

# A.

print(subset_lt$lx_both[16]/subset_lt$lx_both[1])

# B.

subset_lt$lx_accident <- NA
subset_lt$lx_accident[1] <- 100000

for(i in 2:nrow(subset_lt)) {
  subset_lt$lx_accident[i] <- subset_lt$lx_accident[i-1] - (subset_lt$lx_accident[i-1] * subset_lt$prob_accident[i-1])
}

print(1 - (subset_lt$lx_accident[16]/subset_lt$lx_accident[10]))

# C.

print(1 - (subset_lt$lx[16]/subset_lt$lx[1]))
print(1 - (subset_lt$lx_accident[16]/subset_lt$lx_accident[1]))

