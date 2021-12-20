
library(caret)
library(FNN)
library(tidyverse)
library(here)
SouthAfrica <- read.csv2(here("../data/SouthAfrica.csv"))

Singles <- SouthAfrica[SouthAfrica$z10==1,4:5]  
#Singles Subset
Singles <- Singles[order(Singles$ltexp),1:2]	 
#Reorder so that log expenditure is in increasing order
FoodShr <- Singles$FoodShr 
ltexp   <- Singles$ltexp
MyData <- data.frame(FoodShr,ltexp) %>%
  arrange(ltexp)






