
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
    #mutate(exp_cal=p03-años_esco-5) %>% 
    mutate(exp_cal3=p03-años_esco-3,
           exp_cal4=p03-años_esco-4,
           exp_cal5=p03-años_esco-5,
           exp_cal6=p03-años_esco-6) %>% #6 por las investigaciones, pero en Ecuador es de 3 a 5
  #Seguramente, de 4 a 6 vuelva la experiencia negativa, así que esos valores se volveran 0
  #mutate(exp_cal3=ifelse(exp_cal3<0,0,exp_cal3),
         #exp_cal4=ifelse(exp_cal4<0,0,exp_cal4),
         #exp_cal5=ifelse(exp_cal5<0,0,exp_cal5),
         #exp_cal6=ifelse(exp_cal6<0,0,exp_cal6)) # %>%

      #filter(exp_cal>=0) %>% 
  
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


####Funciones####

#Para poder obtener el año de experiencia a partir del cual empieza a decrecer el ingreso
inflexion <- function(model){
  coef(model)[[2]]/-(coef(model)[[3]]*2)
}


# Para obterner el gráfico de la tasa de crecimiento de la experiencia
tasa_crecimiento <- function(model,upper_low) {
  exp_range <- seq(0, upper_low, by = 0.1)
  tasa_values <- coef(model)[[2]] + 2 * coef(model)[[3]] * exp_range
  data <- data.frame(años_exp = exp_range, tasa_crecimiento = tasa_values)
  grafico <- ggplot(data, aes(x = años_exp, y = tasa_crecimiento)) +
    geom_line(color = "blue") +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(title = "Tasa de Crecimiento del Logaritmo del Ingreso con Respecto a la Experiencia",
         x = "Experiencia (años)",
         y = "Tasa de Crecimiento") +
    theme_minimal()
  return(grafico)
}


#Para los gráficos de exp_cal

gra_exp_cal <- function(resultados){
  ggplot(resultados, aes(x = exp_cal_cut, y = ingreso, fill = exp_cal_cut)) +
    geom_bar(stat = "identity") +
    coord_flip()+
    geom_errorbar(aes(ymin = ingreso_low , ymax = ingreso_upp ), width = 0.2)+
    labs(title = "Ingreso por Grupo de experiencia laboral",
         x = "Grupo de experiencia laboral",
         y = "Mediana del ingreso laboral") +
    theme_minimal() +
    theme(legend.position = "none",  plot.title = element_text(hjust=0.5))}


#Para ver el retorno de la experiencia en porcentaje
graf_exp_retorno <- function(resultados){
data <- resultados %>%
  arrange(desc(exp_cal_cut)) %>%
  mutate(tasa_crecimiento = (ingreso - lag(ingreso)) / lag(ingreso)*100)

grafico <- ggplot(data, aes(x = exp_cal_cut, y = tasa_crecimiento)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip()+
  geom_text(aes(label = round(tasa_crecimiento, 2)), vjust = -0.5, size = 3.5) +
  labs(title = "Tasa de Crecimiento del Ingreso por Grupo",
       x = "Grupo de Experiencia",
       y = "Tasa de Crecimiento (%)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1))
  return(grafico)}


###Modelo con p45 ####
model <- svyglm(log_ingrl_rc ~ p45 + I(p45^2), design = pob_empl)
model
summary(model)


inflexion(model)


#Se ajusta los intervalos, para poder observar un poco mejor el punto de inflexión en el gráfico
pob_empl2 <- pob_empl %>% 
  filter(!is.na(ingrl_rc)) %>% 
  mutate(
    p45_cut = case_when(p45 < 3 ~ "[0,3)",
                        p45 >= 3 & p45 < 6 ~ "[3,6)",
                        p45 >= 6 & p45 < 9 ~ "[6,9)",
                        p45 >= 9 & p45 < 12 ~ "[9,12)",
                        p45 >= 12 & p45 < 15 ~ "[12,15)",
                        p45 >= 15 & p45 < 18 ~ "[15,18)",
                        p45 >= 18~ ">=18")) %>% 
  mutate(p45_cut = factor(p45_cut,
                          levels = rev(c("[0,3)", "[3,6)", "[6,9)", 
                                         "[9,12)", "[12,15)","[15,18)",">=18")),
                          ordered = TRUE))


resultados <- pob_empl2 %>% group_by(p45_cut) %>% summarise(ingreso =survey_median(ingrl_rc, vartype=c("se","ci","cv"), na.rm = T, deff = T))



ggplot(resultados, aes(x = p45_cut, y = ingreso, fill = p45_cut)) +
  geom_bar(stat = "identity")+
  coord_flip()+
  geom_errorbar(aes(ymin = ingreso_low , ymax = ingreso_upp ), width = 0.2)+
  labs(title = "Ingreso por Grupo de experiencia laboral",
       x = "Grupo de experiencia laboral",
       y = "Mediana del ingreso laboral") +
  theme_minimal() +
  theme(legend.position = "none",  plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


tasa_crecimiento(model,25)



data <- resultados %>%
  arrange(desc(p45_cut)) %>%
  mutate(tasa_crecimiento = (ingreso - lag(ingreso)) / lag(ingreso)*100)

ggplot(data, aes(x = p45_cut, y = tasa_crecimiento)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  coord_flip()+
  geom_text(aes(label = round(tasa_crecimiento, 2)), vjust = -0.5, size = 3.5) +
  labs(title = "Tasa de Crecimiento del Ingreso por Grupo",
       x = "Grupo de Experiencia",
       y = "Tasa de Crecimiento (%)") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 1))


#Modelo con experiencia calculada, con 3 años cuando los niños ingresan a estudiar####

model3 <- svyglm(log_ingrl_rc ~ exp_cal3+I(exp_cal3^2), design = pob_empl)
model3
summary(model3)

inflexion(model3)

años_3 <- pob_empl %>% 
  filter(!is.na(ingrl_rc)) %>% 
  mutate(
    exp_cal_cut = case_when(exp_cal3 < 5 ~ "[0,5)",
                            exp_cal3 >= 5 & exp_cal3 < 10 ~ "[5,10)",
                            exp_cal3 >= 10 & exp_cal3 < 15 ~ "[10,15)",
                            exp_cal3 >= 15 & exp_cal3 < 20 ~ "[15,20)",
                            exp_cal3 >= 20 & exp_cal3 < 25 ~ "[20,25)",
                            exp_cal3 >= 25 & exp_cal3 < 30 ~ "[25,30)",
                            exp_cal3 >= 30~ ">=30")) %>% 
  mutate(exp_cal_cut = factor(exp_cal_cut,
                              levels = rev(c("[0,5)", "[5,10)", "[10,15)", 
                                             "[15,20)", "[20,25)","[25,30)",">=30")),
                              ordered = TRUE))


res_años_3 <- años_3 %>% group_by(exp_cal_cut) %>% summarise(ingreso =survey_mean(ingrl_rc, vartype=c("se","ci","cv"), na.rm = T, deff = T))


gra_exp_cal(res_años_3)
tasa_crecimiento(model3,40)
graf_exp_retorno(res_años_3)

#Modelo con experiencia calculada, con 4 años ####

model4 <- svyglm(log_ingrl_rc ~ exp_cal4+I(exp_cal4^2), design = pob_empl)
model4
summary(model4)
inflexion(model4)

años_4 <- pob_empl %>% 
  filter(!is.na(ingrl_rc)) %>% 
  mutate(
    exp_cal_cut = case_when(exp_cal4 < 6 ~ "[0,6)",
                            exp_cal4 >= 6 & exp_cal4 < 12 ~ "[6,12)",
                            exp_cal4 >= 12 & exp_cal4 < 18 ~ "[12,18)",
                            exp_cal4 >= 18 & exp_cal4 < 24 ~ "[18,24)",
                            exp_cal4 >= 24 & exp_cal4 < 30 ~ "[24,30)",
                            exp_cal4 >= 30 & exp_cal4 < 36 ~ "[30,36)",
                            exp_cal4 >= 36~ ">=36")) %>% 
  mutate(exp_cal_cut = factor(exp_cal_cut,
                              levels = rev(c("[0,6)", "[6,12)", "[12,18)", 
                                             "[18,24)", "[24,30)","[30,36)",">=36")),
                              ordered = TRUE))


res_años_4 <- años_4 %>% group_by(exp_cal_cut) %>% summarise(ingreso =survey_median(ingrl_rc, vartype=c("se","ci","cv"), na.rm = T, deff = T))

gra_exp_cal(res_años_4)

tasa_crecimiento(model4,40)
graf_exp_retorno(res_años_4)

#Modelo con experiencia calculada, con 5####

model5 <- svyglm(log_ingrl_rc ~ exp_cal5+I(exp_cal5^2), design = pob_empl)
model5
summary(model5)
inflexion(model5)

años_5 <- pob_empl %>% 
  filter(!is.na(ingrl_rc)) %>% 
  mutate(
    exp_cal_cut = case_when(exp_cal5 < 4 ~ "[0,4)",
                            exp_cal5 >= 4 & exp_cal5 < 8 ~ "[4,8)",
                            exp_cal5 >= 8 & exp_cal5 < 12 ~ "[8,12)",
                            exp_cal5 >= 12 & exp_cal5 < 16 ~ "[12,16)",
                            exp_cal5 >= 16 & exp_cal5 < 20 ~ "[16,20)",
                            exp_cal5 >= 20 & exp_cal5 < 24 ~ "[20,24)",
                            exp_cal5 >= 24 ~ ">=24")) %>% 
  mutate(exp_cal_cut = factor(exp_cal_cut,
                              levels = rev(c("[0,4)", "[4,8)", "[8,12)", 
                                             "[12,16)","[16,20)","[20,24)",">=24")),
                              ordered = TRUE))



res_años_5 <- años_5 %>% group_by(exp_cal_cut) %>% summarise(ingreso =survey_median(ingrl_rc, vartype=c("se","ci","cv"), na.rm = T, deff = T))



gra_exp_cal(res_años_5)
tasa_crecimiento(model5,40)
graf_exp_retorno(res_años_5)


#Modelo con experiencia calculada, con 6 la de las investigaciones####
model6 <- svyglm(log_ingrl_rc ~ exp_cal6+I(exp_cal6^2), design = pob_empl)
model6
summary(model6)
inflexion(model6)



años_6<-  pob_empl %>% 
  filter(!is.na(ingrl_rc)) %>% 
  mutate(
    exp_cal_cut = case_when(exp_cal6 < 4 ~ "[0,4)",
                            exp_cal6 >= 4 & exp_cal6 < 8 ~ "[4,8)",
                            exp_cal6 >= 8 & exp_cal6 < 12 ~ "[8,12)",
                            exp_cal6 >= 12 & exp_cal6 < 16 ~ "[12,16)",
                            exp_cal6 >= 16 & exp_cal6 < 20 ~ "[16,20)",
                            exp_cal6 >= 20 & exp_cal6 < 24 ~ "[20,24)",
                            exp_cal6 >= 24 ~ ">=24")) %>% 
  mutate(exp_cal_cut = factor(exp_cal_cut,
                              levels = rev(c("[0,4)", "[4,8)", "[8,12)", 
                                             "[12,16)","[16,20)","[20,24)",">=24")),
                              ordered = TRUE))



res_años_6 <- años_6 %>% group_by(exp_cal_cut) %>% summarise(ingreso =survey_median(ingrl_rc, vartype=c("se","ci","cv"), na.rm = T, deff = T))
gra_exp_cal(res_años_6)
tasa_crecimiento(model6,40)
graf_exp_retorno(res_años_6)

#TAREAS PENDIENTES ####
# Ver los errores con la edad,3,4,5,6 ------------listo
# gráficos con diferentes rangos -----------------listo
# conocer la edad exacta en la que empieza a decrecer ---pendiente
# retorno de la experiencia en porcentaje------ listo
# podria ser por actividad laboral o por nivel de educacion
# fuente del por que empieza a decrecer

