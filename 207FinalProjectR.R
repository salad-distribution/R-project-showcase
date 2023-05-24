setwd ('C:/Users/julia/OneDrive/Desktop/WGU/206 Data Cleaning')
library(plyr)
library('tidyverse')
library(readxl)
cleanchurn <- read_excel('cleaned206churn.xlsx')

AreaChurn <- data.frame(Churn=cleanchurn$Churn, Area=cleanchurn$Area_num)
AreaTable <- table(AreaChurn$Churn, AreaChurn$Area)
chisq.test(AreaTable, simulate.p.value = TRUE)
