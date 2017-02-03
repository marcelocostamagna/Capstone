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

mod_ds<-ames_train %>% select(price,Lot.Area,Land.Slope,Year.Built,Year.Remod.Add,Bedroom.AbvGr)

#Q4















