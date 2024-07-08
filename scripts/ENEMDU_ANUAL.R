
#### Mapa ####
library(sf)
library(dplyr)
library(haven)
library(survey)
library(srvyr)
library(ggplot2)
enemdu_persona_anual <- read_sav("bases/BDDenemdu_personas_2023_anual.sav")
mapa_ecuador <- st_read("bases/cartografia/LIMITE_PROVINCIAL_CONALI_CNE_2022.shp")


mapa_ecuador <- mapa_ecuador %>%   mutate(CODPRO = ifelse(substr(CODPRO, 1, 1) == "0", substr(CODPRO, 2, nchar(CODPRO)), CODPRO)) %>% 
                      filter(CODPRO!="ISLA")



enemdu_persona_prov <- enemdu_persona_anual %>% 
  mutate(CODPRO = ifelse(nchar(ciudad) == 6, substr(ciudad, 1, 2),
                                ifelse(nchar(ciudad) == 5, substr(ciudad, 1, 1), NA))) %>% 
  left_join(mapa_ecuador,by="CODPRO")


df3 <- enemdu_persona_prov %>%
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
  
  #deflactor del ipc año base diciembre 2022
  mutate(
    ipc_ = case_when(
      periodo %in% 202301 ~ 110.227316943081,
      periodo %in% 202302 ~ 110.360236749334,
      periodo %in% 202303 ~ 110.380407616705,
      periodo %in% 202304 ~ 110.452138585459,
      periodo %in% 202305 ~ 110.668865019608,
      periodo %in% 202306 ~ 110.767591152824,
      periodo %in% 202307 ~ 111.181541477615,
      periodo %in% 202308 ~ 111.780858152468,
      periodo %in% 202309 ~ 112.343081099914,
      periodo %in% 202310 ~ 112.385606673683,
      periodo %in% 202311 ~ 112.188911619948,
      periodo %in% 202312 ~ 111.741053926941))  %>%
  mutate(defla_22 = (110.227316943081 / ipc_),
          ing_defla_22 = (ingrl_trans * defla_22)) %>% 
  mutate(log_ingrl_rc =log(ing_defla_22+0.01))





#El siguiente comando permite calcular las tasas de los indicadores


df3 <- df3 %>%
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

d3 <- df3 %>% as_survey_design(ids = upm,
                               strata = estrato,
                               weights = fexp,
                               nest = T)

options(survey.lonely.psu = "certainty")


#Por tal motivo es necesario filtrar

pob_empl2 <- d3 %>%  filter(empleo == 1) 
d3 %>% group_by(area) %>% summarise(Desempleo =survey_ratio(tdesem, pean, vartype=c("se","ci","cv"), na.rm = T, deff = T)*100)
d3 %>% group_by(prov) %>% summarise(Desempleo =survey_ratio(tdesem, pean, vartype=c("se","ci","cv"), na.rm = T, deff = T)*100)


svymean(~ing_defla_22, pob_empl2, na.rm = TRUE) 
svyby(~ing_defla_22, ~p02, pob_empl2, svymean,na.rm = T)




model <- svyglm(log_ingrl_rc ~ p45 + I(p45^2), design = pob_empl2)
model
summary(model)

inflexion(model)


pob_empl2 <- pob_empl2 %>% 
  filter(!is.na(ing_defla_22)) %>% 
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


resultados <- pob_empl2 %>% group_by(p45_cut) %>% summarise(ingreso =survey_median(ing_defla_22, vartype=c("se","ci","cv"), na.rm = T, deff = T))



ggplot(resultados, aes(x = p45_cut, y = ingreso, fill = p45_cut)) +
  geom_bar(stat = "identity")+
  coord_flip()+
  geom_errorbar(aes(ymin = ingreso_low , ymax = ingreso_upp ), width = 0.2)+
  labs(title = "Mediana de ingreso por Grupo de experiencia laboral",
       x = "Grupo de experiencia laboral",
       y = "Mediana del ingreso laboral") +
  theme_minimal() +
  theme(legend.position = "none",  plot.title = element_text(hjust=0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  scale_fill_viridis_d(option = "plasma")




pob_empl2 %>% group_by(p02) %>% summarise(ingreso =survey_median(ing_defla_22, vartype=c("se","ci","cv"), na.rm = T, deff = T))





model <- svyglm(log_ingrl_rc ~ p45 + I(p45^2), design = (pob_empl2 %>% filter(grupo1==2)))
model
summary(model)


inflexion(model)


#Se ajusta los intervalos, para poder observar un poco mejor el punto de inflexión en el gráfico
pob_empl2 <- pob_empl2 %>% 
  filter(!is.na(ing_defla_22)) %>% 
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


resultados <- pob_empl2 %>% filter(rama1==14) %>% group_by(p45_cut) %>% summarise(ingreso =survey_median(ing_defla_22, vartype=c("se","ci","cv"), na.rm = T, deff = T))



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



