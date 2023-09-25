### Lauri Luosta
### Problem Set 1
### 9/25/2023

rm(list = ls())

getwd()
setwd("C:/Users/lauri/Box/Graduate classes (luosta@wisc.edu 2)/SOC 756 Demographic Techniques II")

#install.packages("tidyverse")
library(tidyverse)

#install.packages("LifeTables")
library(LifeTables)

#install.packages("readxl")
library(readxl)

france_1985 <- read_csv("ps1_data_F2023.csv")
france_1985 <- france_1985 %>%
  mutate(n = lead(x),
         N = (n - x),
         nmx = nDx/nNx,
         nqx = ((n * nmx)/(1+(n-nax)*nmx)),
         npx = 1 - nqx,
         )

france_1985[19, "nqx"] <- 1
france_1985[19, "npx"] <- 0
france_1985[1, "l"] <- 100000

#france_1985 <- france_1985 %>% 
#  mutate(l = 100000 * lag(npx))

for(i in 2:nrow(france_1985)) {
  france_1985[i, "l"] <- france_1985[i-1, "l"] * france_1985[i-1, "npx"]
}

france_1985 <- france_1985 %>%
  mutate(ndx = l - lead(l),
         nLx = (N * lead(l)) + (nax * ndx))

options(scipen=999)

france_1985[1, "Tx"] <- sum(france_1985$nLx, na.rm = TRUE)

for (i in 2:nrow(france_1985)) {
  france_1985[i, "Tx"] <- france_1985[i-1, "Tx"] - france_1985[i-1, "nLx"]
}

france_1985 <- france_1985 %>%
  mutate(ex = Tx / l)

library(ggplot2)

# Plotting lx vs. age
p1 <- ggplot(france_1985, aes(x=x, y=l)) +
  geom_line() +
  ggtitle("lx vs. Age") +
  xlab("Age") +
  ylab("lx")

# Plotting ndx vs. age
p2 <- ggplot(france_1985, aes(x=x, y=ndx)) +
  geom_line() +
  ggtitle("ndx vs. Age") +
  xlab("Age") +
  ylab("ndx")

# Plotting nmx vs. age
p3 <- ggplot(france_1985, aes(x=x, y=nmx)) +
  geom_line() +
  ggtitle("nmx vs. Age") +
  xlab("Age") +
  ylab("nmx")

# Display plots
print(p1)
print(p2)
print(p3)

# using the lifetable function

?LifeTables
?lt.mx

LifeTables <- lt.mx(france_1985$nmx, age = c(0,1,seq(5,85,5)), nax=NULL)
df_LifeTables <- as.data.frame(LifeTables)

