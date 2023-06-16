library(tidyverse)
library(modelr)


data <- read_csv("Life-Expectancy-Data-Updated.csv")

glimpse(data) #Categoricas -> Economy_status_Developed, Economy_status_Developing, Region, Country, Year*


##MOTIVADO POR EL EDA: GDP -> log(GDP)#

data <- data %>% 
  mutate(GDP_per_capita = log(GDP_per_capita)) %>% 
  rename(log_GDP = GDP_per_capita)


## Separando los datos

data_pred <- data %>% filter(Year != 2015) %>% select(-Region, -Country, -Year)

data_test <- data %>% filter(Year == 2015) %>% select(-Region, -Country, -Year)


## Primer modelo, todas las variables.

mod_full = lm(data=data_pred, Life_expectancy ~ .)
summary(mod_full)  

#agrego prediciones al dataset de test (2015)

data_test <- add_predictions(data_test, mod_full, var="pred mod_full")

#grafico  valor vs pred para el set de validacion. Linea de 45 grados seria la predicción ideal.

data_test %>% ggplot(aes(y=Life_expectancy, x=`pred mod_full`))+
  geom_point() + 
  geom_abline(slope=1, intercept = 0, color="red")
