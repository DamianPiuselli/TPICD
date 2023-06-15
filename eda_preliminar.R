library(tidyverse)
library(modelr)
library(ggcorrplot)
library(ggridges)

data <- read_csv("Life-Expectancy-Data-Updated.csv")

glimpse(data) #Categoricas -> Economy_status_Developed, Economy_status_Developing, Region, Country, Year*


### Matriz de correlacion.

#set_plot_dimensions(16,10) 
corr <- round(cor(subset(data, select =-c(Economy_status_Developed, Economy_status_Developing, Region, Country, Year))), 3) 
ggcorrplot(corr,type = "upper", lab = TRUE, outline.color = "black", lab_size = 4, legend.title = "Correlation")+
  ggtitle("Correlation Matrix")



### Distribución de la expectativa de vida

data %>% 
  ggplot(aes(x=Life_expectancy))+
  geom_histogram(aes(y= ..density..), color = "black", fill = "salmon") #Por ahora colores std, despues elegimos paleta de colore

## comparando desarrollado vs sub

data %>% 
  ggplot(aes(x=Life_expectancy, fill=as.factor(Economy_status_Developed)))+
  geom_density(alpha=0.6)

data %>%   
  ggplot(aes(y=Life_expectancy, fill=as.factor(Economy_status_Developed)))+
  geom_boxplot()  #Creo que queda mejor un boxplot.


## Comparando por Region

data %>% 
  ggplot(aes(x=Life_expectancy, y=as.factor(Region)))+
  geom_density_ridges2(rel_min_height = 0.001, alpha=0.7, color = "black", fill= "salmon")
  
  