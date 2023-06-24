library(tidyverse)
library(modelr)


data <- read_csv("Life-Expectancy-Data-Updated.csv")

glimpse(data) #Categoricas -> Economy_status_Developed, Economy_status_Developing, Region, Country, Year*


##MOTIVADO POR EL EDA: GDP -> log(GDP)#

data <- data %>% 
  mutate(GDP_per_capita = log(GDP_per_capita)) %>% 
  rename(log_GDP = GDP_per_capita) %>% 
  select(-Economy_status_Developing, -Region, -Country, -Year)


## Primer modelo, todas las variables.

mod_full = lm(data=data, Life_expectancy ~ .)
summary(mod_full)  

## segundo modelo

data <- data %>% select(-Population_mln, -Measles, -Thinness_five_nine_years, -Diphtheria, -Under_five_deaths, -Incidents_HIV )

mod_1 = lm(data=data, Life_expectancy ~ .)
summary(mod_1) 


#tercer modelo. #todos los terminos de interaccion para chusmear.

mod_2 = lm(data=data, Life_expectancy ~ .*.)
summary(mod_2) 

#cuarto modelo
mod_3 = lm(data=data, Life_expectancy ~ . +Alcohol_consumption*log_GDP+ BMI*log_GDP)
summary(mod_3) 


