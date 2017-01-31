library(dplyr)
ames_train %>% group_by( Lot.Frontage ) %>%   select(Lot.Frontage) %>% 
  count(is.na(Lot.Frontage))
#https://ww2.amstat.org/publications/jse/v19n3/decock/datadocumentation.txt

# Misc.Feature  - 971
# Fence -798
# Pool.QC - 997
# Alley 993
# Fireplace.qu -491
# Lot.Frontage -

##question 1: B -Misc.Feature, Alley, Pool.QC
#question 2
head(ames_train)

ames_train <- as.data.frame(ames_train)

str(ames_train)

# Question 2 : 3 
#question 3
#MS.SubClass  Overall.Qual  Overall.Cond 
ames_train$MS.SubClass <- as.factor(ames_train$MS.SubClass)
ames_train$Overall.Qual <- as.factor(ames_train$Overall.Qual)
ames_train$Overall.Cond <- as.factor(ames_train$Overall.Cond)

StoneBr<-ames_train %>%  filter(Neighborhood == "StoneBr") %>% select(price)
Timber<-ames_train %>%  filter(Neighborhood == "Timber") %>% select(price)
Veenker<-ames_train %>%  filter(Neighborhood == "Veenker") %>% select(price)
NridgHt<-ames_train %>%  filter(Neighborhood == "NridgHt") %>% select(price)

sd(StoneBr$price)
sd(Timber$price)
sd(Veenker$price)
sd(NridgHt$price)

#question 3: StoneBr
#question 4
subset<-ames_train %>% select(price,Lot.Area,Bedroom.AbvGr,Overall.Qual,Year.Built) 
pairs(subset)
#question 4:Overall.Qual

#question 5
cor(ames_train$price,ames_train$area)
cor(ames_train$price,log(ames_train$area))
cor(log(ames_train$price),ames_train$area)
cor(log(ames_train$price),log(ames_train$area))
#//check log transfor causes
#question 5: Log-transform both price and area

#question 6
#INCOMPLETA
# question 6 

#question 7
summary(ames_train$price);quantile((ames_train$price)) 
build<-ames_train %>% select(Year.Built) %>% mutate(plus99=if_else(Year.Built > 1999,1,0))
table(build$plus99)
272*100/728

#question 7: A.over 30 percent of houses


