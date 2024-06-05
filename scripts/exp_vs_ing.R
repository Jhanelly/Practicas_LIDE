
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
library(ggplot2)


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
  
  #Años de experiencia calculada, el 3 puede ser remplazado en valores de 3 a 6, pues es la edad en la que inician
  #los estudios en Ecuador.

  mutate(exp_cal3=p03-años_esco-3,
         exp_cal4=p03-años_esco-4,
         exp_cal5=p03-años_esco-5,
         exp_cal6=p03-años_esco-6) %>% #6 por las investigaciones, pero en Ecuador es de 3 a 5
  #Seguramente, de 4 a 6 vuelva la experiencia negativa, así que esos valores se volveran 0
  mutate(exp_cal3=ifelse(exp_cal3<0,0,exp_cal3),
         exp_cal4=ifelse(exp_cal4<0,0,exp_cal4),
         exp_cal5=ifelse(exp_cal5<0,0,exp_cal5),
         exp_cal6=ifelse(exp_cal6<0,0,exp_cal6)) %>% 

  
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



#Modelo con p45
model <- svyglm(log_ingrl_rc ~ p45 + I(p45^2), design = pob_empl)
model
summary(model)



pob_empl2 <- pob_empl  %>% 
                filter(!is.na(ingrl_rc)) %>% 
                mutate(p45_cut=cut(p45,
                  breaks=seq(0,86,length.out=10),
                  dig.lab=5,right = FALSE)) 

resultados <- pob_empl2 %>% group_by(p45_cut) %>% summarise(ingreso =survey_median(ingrl_rc, vartype=c("se","ci","cv"), na.rm = T, deff = T))



ggplot(resultados, aes(x = p45_cut, y = ingreso, fill = p45_cut)) +
  geom_bar(stat = "identity")+
  geom_errorbar(aes(ymin = ingreso_low , ymax = ingreso_upp ), width = 0.2)+
  labs(title = "Ingreso por Grupo de experiencia laboral",
       x = "Grupo de experiencia laboral",
       y = "Mediana del ingreso laboral") +
  theme_minimal() +
  theme(legend.position = "none",  plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



#Modelo con experiencia calculada, con 3 años cuando los niños ingresan a estudiar

model2 <- svyglm(log_ingrl_rc ~ exp_cal+I(exp_cal^2), design = pob_empl)
model2
summary(model2)

pob_empl2 <- pob_empl  %>% 
  filter(!is.na(ingrl_rc)) %>% 
  mutate(exp_cal_cut=cut(exp_cal3,
                     breaks=seq(0,92,length.out=10),
                     dig.lab=5,right = FALSE)) %>% 
  mutate(exp_cal_cut=factor(exp_cal_cut, levels = rev(levels(exp_cal_cut))))

resultados2 <- pob_empl2 %>% group_by(exp_cal_cut) %>% summarise(ingreso =survey_median(ingrl_rc, vartype=c("se","ci","cv"), na.rm = T, deff = T))



ggplot(resultados2, aes(x = exp_cal_cut, y = ingreso, fill = exp_cal_cut)) +
  geom_bar(stat = "identity") +
  coord_flip()+
  geom_errorbar(aes(ymin = ingreso_low , ymax = ingreso_upp ), width = 0.2)+
  labs(title = "Ingreso por Grupo de experiencia laboral",
       x = "Grupo de experiencia laboral",
       y = "Mediana del ingreso laboral") +
  theme_minimal() +
  theme(legend.position = "none",  plot.title = element_text(hjust=0.5))



# Probar con 3 4 5 y ver los errores
# rangos de 5 hasta el 30 y luego de 30 en adelante
# retorno de la experiencia en porcentaje
# conocer la edad exacta en la que empieza a decrecer
# podria ser por actividad laboral o por nivel de educacion
# fuente del por que empieza a decrecer
##

