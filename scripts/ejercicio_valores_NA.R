#Cargando paquetes
install.packages("DataExplorer")
install.packages("funModeling")
install.packages("flextable")
install.packages("dlookr")
install.packages("visdat")
library(DataExplorer)
library(dplyr)
library(funModeling) #Para crear tablas
library(flextable)
library(dlookr) # Para gráficos de na
library(visdat)
library(plotly)

#Cargando datos
data("airquality")
View(airquality)

#Primera vista de los datos
introduce(airquality) %>% t()

#Información por cada columna, como cantidad de 0
#cantidad de NA
status(airquality) %>% flextable()


#gráfico de los na
#Gráfico de Pareto para conocer el porcentaje de NA
plot_na_pareto(airquality,only_na = T)

#Más información sobre los datos perdidos
plot_na_intersect(airquality)

#Patron de comportamiento dentro de los valores periddos
vis_miss(airquality) %>%  ggplotly()



# Paso 2: Diagnóstico de aleatoriedad de valores perdidos -----------------

# 2.1 Primer procedimiento: comparación de media --------------------------

#Comparar las medias de los grupos que tienen NA con los que no tienen

airquality$Ozone_na_flag <- ifelse(is.na(airquality$Ozone),0,1)
airquality$SolaR_na_flag <- ifelse(is.na(airquality$Solar.R),0,1)

#Si valor p es >0.05 se acepta H0, los NA no siguen un partido
t.test(Ozone~SolaR_na_flag, alternative="two.sided",
       conf.level=.95, var.equal=T, data=airquality)

t.test(Solar.R~Ozone_na_flag, alternative="two.sided",
       conf.level=.95, var.equal=T, data=airquality)

#Si son completamente aleatorios, se puede hacer imputación


install.packages("missRanger")
install.packages("grid")
library(missRanger)
library(grid)
set.seed(111)


airquality_NA <- generateNA(airquality) %>% 
  mutate(Month=factor(Month))

plot_na_pareto(airquality_NA,only_na = T)
plot_na_intersect(airquality_NA)


blip <- imputate_na(airquality_NA,Ozone, Temp, method="median")
plot(blip)

bla <-  imputate_na(airquality_NA,Ozone,Temp, method = "rpart")
plot(bla)

blup <-  imputate_na(airquality_NA,Ozone,Temp, method = "mice", seed=111)
plot(blup)
