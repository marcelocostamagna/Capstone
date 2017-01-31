library(dplyr)
ames_train %>% group_by( Lot.Frontage ) %>%   select(Lot.Frontage) %>% 
  count(is.na(Lot.Frontage))

# Misc.Feature  - 971
# Fence -798
# Pool.QC - 997
# Alley 993
# Fireplace.qu -491
# Lot.Frontage -

##question 1: B -Misc.Feature, Alley, Pool.QC

head(ames_train)

ames_train <- as.data.frame(ames_train)

str(ames_train)

# Question 2 : 3 
#MS.SubClass  Overall.Qual  Overall.Cond 
ames_train$MS.SubClass <- as.factor(ames_train$MS.SubClass)
ames_train$Overall.Qual <- as.factor(ames_train$Overall.Qual)
ames_train$Overall.Cond <- as.factor(ames_train$Overall.Cond)
