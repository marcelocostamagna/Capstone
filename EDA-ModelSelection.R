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

ames_train %>% 
  group_by(Neighborhood) %>% 
  summarise(max=max(price), min=min(price),average=mean(price),median=median(price))
  

#q2



















