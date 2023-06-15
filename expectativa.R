library(tidyverse)
library(modelr)

data <- read_csv("Life Expectancy Data.csv")

#Dividar las variables en variables de salud / socio-economicas.


# Expectativa de vida vs desarrollado/no desarrollado
data %>%
  ggplot(aes(x=Status, y=`Life expectancy`))+geom_boxplot()
  
#Expectativa de vida vs PBI  (en miles de dolares)

data %>%
  ggplot(aes(y=log(GDP), x=`Life expectancy`, color=Status))+geom_point()

#Expectativa de vida vs BMI  (en miles de dolares)

data %>%
  filter(BMI > 15) %>% 
  ggplot(aes(x=BMI, y=`Life expectancy`, color=Status))+geom_point()

# TOP10 MORTALIDAD INFANTIL

data %>% 
  filter(`infant deaths` < 900 ) %>%  #ver mejor imputacion de estos datos. (/10?)
  group_by(Country) %>% 
  summarise(avg = mean(`infant deaths` ,na.rm=T)) %>% 
  arrange(desc(avg)) %>% 
  head(10) %>% 
  ggplot(aes(y=avg, x=Country))+geom_col()+
  coord_flip()

# TOP10 Consumo de alcohol.

data %>% 
  group_by(Country) %>% 
  summarise(avg = mean(Alcohol ,na.rm=T)) %>% 
  arrange(desc(avg)) %>% 
  head(20) %>% 
  ggplot(aes(y=avg, x=Country))+geom_col()+
  coord_flip()


# Indice de desarrollo humano

data %>%
  filter(`Income composition of resources`>0) %>% 
  ggplot(aes(x=`Income composition of resources`, y=`Life expectancy`, color=Status))+geom_point()
  
# Indice de desarrollo humano

data %>%
  filter(Schooling>0) %>% 
  ggplot(aes(x=Schooling, y=`Life expectancy`, color=Status))+geom_point()


#######

mod1 <-  lm(data=data,`Life expectancy` ~ `Income composition of resources`+ Schooling+ GDP+ BMI+
              `Adult Mortality`+ `HIV/AIDS`+ Polio+ Measles)  
summary(mod1)
plot(mod1)

data_reg <- data %>% select(-Status, -Country, -Year, -Population)
mod2 <-  lm(data=data_reg,`Life expectancy` ~ .)
summary(mod2)


### filtrando poblacion subdesarrolada
data_sub <- data %>% filter(Status == "Developing")

mod3 <-  lm(data=data_sub,`Life expectancy` ~ `Income composition of resources`+ Schooling+ GDP+ BMI+
              `Adult Mortality`+ `HIV/AIDS`+ Polio+ Measles )  
summary(mod3)
