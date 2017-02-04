# Install the tool to download packages from Github
install.packages("devtools")
install.packages("dplyr")
library(devtools)
install_github("StatsWithR/statsr")
library(dplyr)
library(statsr)

# quest 1

ames_train %>%
  mutate(age=2017-Year.Built) %>%
  select(age) %>% 
  ggplot(aes(x=age)) + 
  geom_histogram(bins=30) +
  ggtitle("Histogram by house years") +
  theme(axis.text.x =   element_text(angle=40,  size=12))  

# quest 1

#q2

nbh<-ames_train %>%
  select(Neighborhood)
table(nbh)

library(scales)
ames_train %>% 
  group_by(Neighborhood) %>% 
  ggplot(aes(x=Neighborhood,y=price))+
  geom_boxplot() +
  ggtitle("Price Variations by Neighborhood") +
  theme(axis.text.x =   element_text(angle=45,  size=10)) +
   scale_y_continuous( labels = comma)

nbh_dist<- ames_train %>% 
  group_by(Neighborhood) %>% 
  summarise(max=max(price), min=min(price),average=mean(price),median=median(price),std=sd(price))
nbh_dist<- data.frame(nbh_dist)

nbh_dist %>% select(Neighborhood,max)  %>% top_n(1)
nbh_dist %>% select(Neighborhood,min)  %>% top_n(-1) 
nbh_dist %>% select(Neighborhood,average)  %>% top_n(1)
nbh_dist %>% select(Neighborhood,std)  %>% top_n(1)



#q2

#Q3
summary(ames_train)


tmp<-data.frame(apply(ames_train, 2, function(x) length(which(is.na(x)))))
nas <- cbind(rownames(tmp),tmp);
rownames(nas)<-NULL;colnames(nas)<-c("Neib","Nas")
nas %>% top_n(1)
#Q3


#Q4
library(dplyr)
library(GGally)
library(BAS)
install.packages(BAS)
mod_ds<-ames_train %>% select(price,Lot.Area,Land.Slope,Year.Built,Year.Remod.Add,Bedroom.AbvGr)
ggpairs(mod_ds)

lm_price <- lm(log(price)~Lot.Area+Land.Slope+Year.Built+Year.Remod.Add+Bedroom.AbvGr, data= mod_ds)
lm_price <- lm(log(price)~Lot.Area,Land.Slope,Year.Built,Year.Remod.Add,Bedroom.AbvGr , data= mod_ds)
summary(lm_price)


 bma_mod_bic <- bas.lm(log(price) ~ . , data=mod_ds,prior = "BIC", 
                                       modelprior = uniform())

image(bma_mod_bic,rotate=F)

bma_mod_zel <- bas.lm(log(price) ~ . , 
                  data=mod_ds,prior = "ZS-null",  #Zellner-Siow Cauchy prior
                  modelprior = uniform())


image(bma_mod_zel,rotate=F)

plot(bma_mod_zel, which=1, add.smooth=F)

summary(bma_mod_zel)
bma_mod_zel$residuals
data.frame(bma_mod_zel,which=1)
bma_mod_zel
?bas.lm

coef.bas(bma_mod_zel)
bma_mod_zel$which=2
residuals.lm(bma_mod_zel)


lm_price <- lm(log(price)~Lot.Area+Land.Slope+Year.Built+Year.Remod.Add+Bedroom.AbvGr , data= mod_ds)
resDF<-data.frame(residuals=(abs(lm_price$residuals))^2)
mod_ds_res<-cbind(mod_ds ,resDF)
mod_ds_res %>% arrange(desc(residuals)) %>% head(5) 

top_n(5,residuals) %>% 

plot(ames_train$Year.Built,ames_train$price)

plot(sqrt(abs(lm_price$residuals)))

summary(ames_train$Year.Built)
#Q4

#Q5

bma_mod_bic_log <- bas.lm(log(price) ~log( Lot.Area)+Land.Slope+Year.Built+Year.Remod.Add+Bedroom.AbvGr , 
                      data=mod_ds,prior = "BIC", 
                      modelprior = uniform())

image(bma_mod_bic_log,rotate=F)

bma_mod_zel_log <- bas.lm(log(price) ~ log(Lot.Area)+Land.Slope+Year.Built+Year.Remod.Add+Bedroom.AbvGr , 
                      data=mod_ds,prior = "ZS-null",  #Zellner-Siow Cauchy prior
                      modelprior = uniform())


image(bma_mod_zel_log,rotate=F)
#Q5

#Q6

bma_mod_zel_final <- bas.lm(log(price) ~ Lot.Area+Year.Built+Year.Remod.Add+Bedroom.AbvGr , 
                      data=mod_ds,prior = "ZS-null",  
                      modelprior = uniform())

bma_mod_zel_log_final <- bas.lm(log(price) ~ log(Lot.Area)+Year.Built+Year.Remod.Add+Bedroom.AbvGr , 
                      data=mod_ds,prior = "ZS-null",  
                      modelprior = uniform())

plot(bma_mod_zel_final, which=1, add.smooth=F)
plot(bma_mod_zel_log_final, which=1, add.smooth=F)

pra<-predict.bas(bma_mod_zel_final,ames_train_sub, prediction = T)
tmp<-as.data.frame( as.vector(pra$Ypred))
colnames(tmp) <-"pred_p"
bc<-bind_cols(data.frame(ames_train$price),tmp)

(pra$Ypred)

pr<-as.list(pra$Ypred)

hist(ames_train$Lot.Area)

library(ggplot2)
ames_train %>% select(Lot.Area) %>% ggplot(aes(x=Lot.Area)) +geom_histogram()
library(dplyr)















