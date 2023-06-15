library(tidyverse)
library(modelr)
library(ggcorrplot)
library(ggridges)

data <- read_csv("Life-Expectancy-Data-Updated.csv")

glimpse(data) #Categoricas -> Economy_status_Developed, Economy_status_Developing, Region, Country, Year*


### Matriz de correlacion.

#set_plot_dimensions(16,10) #Para setear cuando exportemos graficos a powerpoint.

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
  #Las distribuciones Bimodales son interesantes, indicaria heterogeneidad entre las distintas regiones
  #America del norte, america del sur distribuciones mas unimodales. Oceania/Africa mas bimodales.


### Factores economicos
## GDP per capita

data %>% 
  ggplot(aes(y=Life_expectancy, x=GDP_per_capita, color=as.factor(Economy_status_Developed)))+
  geom_point()  
#Paises con pocos recursos per capita experimentan un gran cambio en la expectativa de vida al aumentar recursos econom.
#Distribucion logaritmica. A medida que aumenta GDP la pendiente se plancha, como cabria esperar (si no en los paises ricos serian inmortales)
#No se si graficarlo o no con gdp en escala log. Los dos graficos tienen su gracia.
  
  
  
  