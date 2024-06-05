
# PRIMERA PARTE DEL SCRIPT ---------------------------------

# 1.-Construcción de variables, incluyendo las necesarias para el estudio
# 2.-Se específica el diseño muestral
# 3.-Se realiza una comparación de algunas estimaciones con los resultados del INEC para tener
#    seguridad de que se este realizando el proceso correcto


#Cargando paquetes
library(haven)
library(dplyr)
library(survey)
library(srvyr)


library(DataExplorer)
library(dplyr)
library(funModeling) #Para crear tablas
library(flextable)
library(dlookr) # Para gráficos de na
library(visdat)
library(plotly)


#Importando la base de datos
enemdu_persona_2024_02 <- read_sav("bases/enemdu_persona_2024_02.sav")

#CONSTRUCCIÓN DE VARIABLES 

#condact representa la división entre población economicamente activa e inactiva
df2 <- enemdu_persona_2024_02 %>%
  mutate(
    # Inicializar t_a a 1
    t_a = 1,                          #población total
    # Crear indicador basado en p03
    menor15 = if_else(p03 < 15, 0, condact), #Fuera de la edad de trabajar
    petn = if_else(p03 >= 15 & p03 <= 99, 1, 0), #Población en edad de trabajar
    # Recodificaciones de condact        
    pean = if_else(condact >= 1 & condact <= 8, 1, 0), #Económicamente activa
    empleo = if_else(condact >= 1 & condact <= 6, 1, 0), #Empleados de cualquier tipo
    adec = if_else(condact == 1, 1, 0),                   #Empleo adecuado
    sub = if_else(condact >= 2 & condact <= 3, 1, 0),     #Sub empleo por horas e ingreso
    sub_h = if_else(condact == 2, 1, 0),                  #Sub empleo por horas
    sub_w = if_else(condact == 3, 1, 0),                  #Sub empleo por ingresos
    oinad = if_else(condact == 4, 1, 0),                  #Otro empleo inadecuado
    nr = if_else(condact == 5, 1, 0),                     #Empleo no remunerado
    nc = if_else(condact == 6, 1, 0),                     #Empleo no clasificado
    desem = if_else(condact >= 7 & condact <= 8, 1, 0),   #Desempleo abierto y oculto
    desemab = if_else(condact == 7, 1, 0),                #Desempleo abierto
    desemoc = if_else(condact == 8, 1, 0),                #Desempleo oculto
    desem1 = if_else(condact >= 7 & condact <= 8 & p37 == 1, 1, 0), #Desempleo cesante  #p37 - Trabajo anteriormente? 1 Sí y 2 NO
    desem2 = if_else(condact >= 7 & condact <= 8 & p37 == 2, 1, 0), #Desempleo nuevo
    pein = if_else(condact == 9, 1, 0)) %>%  #Población inactiva
  
  #Factor de algunas variables demograficas
  mutate(area=factor(area, levels=c(1,2),labels=c("Urbano","Rural")),
         p02=factor(p02, levels=c(1,2),labels=c("Hombre","Mujer"))) %>% 
  
  #Convirtiendo a factor el nivel de instrucción
  mutate(nnivins2=factor(nnivins, levels=c(1,2,3,4,5),labels=c("Ninguno","Centro de Alfabetización",
                                                               "Educación Básica","Educación Media/Bachillerato",
                                                               "Superior"))) %>% 

  #Años de escolaridad - Se creo esta variable tomando en cuenta el nivel de instrucción y el último año aprobado
  mutate(años_esco=case_when(nnivins2=="Ninguno"~ 0,
                             nnivins2=="Educación Media/Bachillerato"~10+p10b,
                             nnivins2=="Superior"&(p10a==8|p10a==9)~13+p10b, #Superior Universitario o no Universitario
                             nnivins2=="Superior"& p10a==10~17+p10b, #Posgrado
                             TRUE~p10b)) %>%  
  
  #Años de experiencia calculada, el 3 puede ser remplazado en valores de 3 a 5, pues es la edad en la que inician
  #los estudios en Ecuador.
  mutate(exp_cal=p03-años_esco-3) %>% 
 
   #Valos no validos
  mutate(ingrl_trans = ifelse(ingrl <= -1 | ingrl >= 999999, NA, ingrl)) %>% 
  
  #deflactor del ipc año base diciembre 2023
  mutate(ingrl_rc=ingrl_trans*(111.715101/111.8551314)) %>%  
  mutate(log_ingrl_rc =log(ingrl_rc+0.01))


#El siguiente comando permite calcular las tasas de los indicadores

df2 <- df2 %>%
  mutate(
    templeob = case_when(empleo==1~1,petn==1~0), #empleo bruto
    templeog =  case_when(empleo==1~1,pean==1~0), #empleo global
    tadec = case_when(adec==1~1,pean==1~0), #empleo adecuado
    tsub = case_when(sub==1~1,pean==1~0), #subempleo
    tsub_h = case_when(sub_h==1~1,pean==1~0),
    tsub_w = case_when(sub_w==1~1,pean==1~0),
    toinad = case_when(oinad==1~1,pean==1~0),
    tnr = case_when(nr==1~1,pean==1~0),
    tnc = case_when(nc==1~1,pean==1~0),
    tdesem = case_when(desem==1~1,pean==1~0),
    tdesemab =case_when(desemab==1~1,pean==1~0),
    tdesemoc = case_when(desemoc==1~1,pean==1~0),
    tdesem1 = case_when(desem1==1~1,pean==1~0),
    tdesem2 = case_when(desem2==1~1,pean==1~0),
    tinformal=case_when(secemp == 2 ~1,empleo==1~0),
    tformal=case_when(secemp == 1 ~1,empleo==1~0))



#Diseño muestral complejo

d2 <- df2 %>% as_survey_design(ids = upm,
                              strata = estrato,
                              weights = fexp,
                              nest = T)
options(survey.lonely.psu = "certainty")





# Comparación de resultados con los del INEC 


### Tasa de desempleo

#Utilizamos las funciones de ambos paquetes, pero la diferencia solo es en la presentación de los resultados
d2 %>% group_by(area) %>% summarise(Desempleo =survey_ratio(tdesem, pean, vartype=c("se","ci","cv"), na.rm = T, deff = T))
svyby(~tdesem, ~area, d2, svymean,na.rm = T,vartype=c("se","ci","cv"))


### Tasa de empleo 
d2 %>% group_by(area) %>% summarise(empleo =survey_ratio(templeog, pean, vartype=c("se","ci","cv"), na.rm = T, deff = T))


### Ingresos medios nacionales 
#Se realiza el análisis para el total de empleados (asalariados e independientes)
#Por tal motivo es necesario filtrar
pob_empl <- d2 %>%  filter(empleo == 1) 

svymean(~ingrl_rc, pob_empl, na.rm = TRUE) 

#ingresos medios por sexo
svyby(~ingrl_rc, ~p02, pob_empl, svymean,na.rm = T)


#tasa de empleo sector informal
svymean(~tinformal, pob_empl, na.rm = TRUE) 

##### AQUÍ FINALIZA LA PRIMERA PARTE DEL SCRIPT






# EXTRA ------------------------------------------------------------

#Antes de ver como se está relacionando el ingreso con la experiencia, 
#echaremos un vistazo breve de el ingreso por el nivel de instrucción

#Se utiliza pob_empl dado que contiene solo a la población empleada

#Ingresos por nivel de instrucción resumida
svyby(~ingrl_rc, ~nnivins2, pob_empl, svymean,na.rm = T)

#Ingresos por nivel de instrucción resumida y area
svyby(~ingrl_rc, ~nnivins2 +area, pob_empl, svymean,na.rm = T)


#Agrupado por el sector en el que trabajan
pob_empl %>% group_by(secemp,nnivins2) %>% summarise(ingreso =survey_mean(ingrl_rc, vartype=c("se","ci","cv"), na.rm = T, deff = T))

# Los resultados pueden servir de base para otra pequeña publicación.........






# SEGUNDA PARTE -----------------------------------------------------------

# Hay alguna relación entre la p45(experiencia en la ocupación actual) o la exp calculada y el ingreso laboral?

# Para responder a la pregunta, se realiza una breve exploración de la composición de las variables de interés

# Del mismo modo se utilizará solo la población empleada

exploracion <- df2 %>% filter(empleo==1) %>% select(upm,estrato,fexp,ingrl_rc,log_ingrl_rc,exp_cal,p45)
#Vista rápida
introduce(exploracion) %>% t()

#Para conocer información más detallada de la composición de las varibles
status(exploracion) %>% flextable()

#Con el gráfico de Pareto podemos observar que el porcentaje de NA en la variable ingresos
# es de 11.8%, lo que no es tan malo dado que no supera el 20%
plot_na_pareto(exploracion,only_na = T)



blip <- imputate_na(exploracion,ingrl_rc, method="median")
plot(blip)

bla <-  imputate_na(exploracion,ingrl_rc, method = "rpart")
plot(bla)

#El que sería el mejor método
blup  <- imputate_na(exploracion,ingrl_rc, method = "mice", seed=111)  
plot(blup)


exploracion2 <-  exploracion %>% mutate(ingrl_imp=imputate_na(exploracion,ingrl_rc, method = "mice", seed=111))
exploracion2 <- exploracion2 %>% mutate(ingrl_imp_log=log(ingrl_imp+0.01))

d3 <- exploracion2 %>% as_survey_design(ids = upm,
                                        strata = estrato,
                                        weights = fexp,
                                        nest = T)
options(survey.lonely.psu = "certainty")
status(exploracion2) %>% flextable()

model <- svyglm(ingrl_imp_log ~ p45+I(p45^2), design = d3)

svyplot(ingrl_imp_log ~ p45, d3, style="subsample",pch=1, main = "Diagrama de Dispersión con Diseño Muestral")
abline(model, col = "red", lwd = 2)


#cuando no aplico el filtro hay una relación positiva

