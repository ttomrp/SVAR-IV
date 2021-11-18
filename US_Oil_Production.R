library(tidyverse)
library(readxl)
library(stargazer)
library(xtable)
library(ggplot2)
library(tseries)

setwd("C:/Users/Tom/OneDrive/Documents/Thesis Project/Thesis Data")
data = read_excel("MCRFPUS2m.xls", sheet = 2)
data

oilp = ts(data[,2], start = 1920, frequency = 12)

ts.plot(oilp, type='l', lty=1, col=1,ylab="Thousand Barrels per Day", xlab="",xlim=c(1923.5, 2016.5), ylim=c(1000, 13000))
grid()
