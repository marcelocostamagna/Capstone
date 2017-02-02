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

# Question 2 : 2
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
hg<-ames_train %>% mutate(haveGar= if_else( is.na(Garage.Type),0,1 )) %>% select(haveGar)
table(hg)

# question 6 :Beta(963, 47) 

#question 7
summary(ames_train$price);quantile((ames_train$price)) 
build<-ames_train %>% select(Year.Built) %>% mutate(plus99=if_else(Year.Built > 1999,1,0))
table(build$plus99)
272*100/1000 # 27.2

mt<-ames_train %>% mutate( ba=is.na(Bsmt.Qual),0,1) %>% select(ba)
table(mt)
#question 7: 21 houses do not have basement


#question 8
garage<-ames_train %>% mutate(gar=if_else(is.na(Garage.Type),"NO","YES")) %>% select(area,gar)

?inference()
inference(y = area, x = as.factor(gar), data = garage, statistic = "mean", type = "ht", null = 0, 
          alternative = "less", method = "theoretical", sig_level = 0.05, conf_level = 0.95)

#question 8: p-value < 0.0001

#question 9
subsetmodel<-ames_train %>% select(price, area, Lot.Frontage,Lot.Area,Year.Built,Year.Remod.Add,Mas.Vnr.Area,BsmtFin.SF.1,BsmtFin.SF.2,Total.Bsmt.SF,Full.Bath,Bedroom.AbvGr,Kitchen.AbvGr,TotRms.AbvGrd,Fireplaces,Garage.Cars,Garage.Area,Yr.Sold)

ggpairs(subsetmodel) #area(0.71) ,Year.build(0.577),Total.Bsmt.SF(0.688),garage.cars(0.665)

subs2<-ames_train %>% select(price,area,Total.Bsmt.SF) %>% filter(!is.na(Total.Bsmt.SF))
mod<- lm(log(price) ~  log(area) + Total.Bsmt.SF ,data = subs2)
summary(mod)
#question 9 : 0.752

#question 10
#question 10:Mean: 3.62, SD: 0.16

#question 11
#When regressing log(price) on log(area), there are some outliers. 
#Which of the following do the three most outlying points have in common?
plot(log(ames_train$price),log(ames_train$area))
ames_train %>% select(price,area) %>% ggplot(aes(x=log(area), y=log(price))) + 
  geom_point() +
  geom_smooth(method = "lm")

mdl <- lm(log(price)~ log(area), data=ames_train)
summary(mdl)
resDF<-data.frame(res=abs(mdl$residuals))
am_res <- bind_cols(ames_train,resDF)

am_res %>% arrange(desc( res)) %>% top_n(3) %>% select(res,Bedroom.AbvGr,Overall.Qual,Year.Built)


#question 11: They where built before 1930 

#question 12
hist(ames_train$price)
summary(ames_train$price)
#question 12: price is right skewed

#question 13
asd<-ames_train %>% mutate(one_fam=if_else(Bldg.Type=="1Fam","yes","no")) %>%
  group_by(Neighborhood) %>% count(one_fam) 
asd  
#question 13: 3

#question 14
plot(log(ames_train$area), ames_train$Bedroom.AbvGr)
cor(log(ames_train$area), ames_train$Bedroom.AbvGr)
ames_train %>% select(Bedroom.AbvGr, area) %>% 
  group_by(Bedroom.AbvGr) %>% 
  summarise(mean=mean(log(area)),median(log(area)))
#question 14: YES

#question 15
ames_train %>% 
  filter(!is.na(Bsmt.Unf.SF)) %>% 
  filter( BsmtFin.Type.2 =="Unf" || BsmtFin.Type.1 =="Unf" ) %>% 
  select(Bsmt.Unf.SF) %>% summary()
 
ames_train %>% 
  filter(!is.na(Bsmt.Unf.SF)) %>% 
  filter( BsmtFin.Type.1 =="Unf" ) %>% 
  select(Bsmt.Unf.SF) %>% summary()
  
  summary()
#question 15: 595









   
