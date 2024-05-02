library(haven)
enemdu_persona_2024_02 <- read_sav("bases/enemdu_persona_2024_02.sav")

library(dplyr)

#CONSTRUCCIÓN DE VARIABLES 

#condact representa la división entre población economicamente activa e inactiva
df <- enemdu_persona_2024_02 %>%
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
  mutate(area=factor(area, levels=c(1,2),labels=c("Urbano","Rural")),
         p02=factor(p02, levels=c(1,2),labels=c("Hombre","Mujer"))) %>% 
  mutate(ingrl = ifelse(ingrl <= -1 | ingrl >= 999999, NA, ingrl)) %>%  #Valos no validos
  mutate(ingrl_rc=ingrl*(111.715101/111.8551314)) #deflactor del ipc año base diciembre 2023


#Clasificación del sector en el que trabajan

df <- df %>%
  mutate(
    # Asigna nuevas variables basadas en condiciones
    formal = ifelse(secemp == 1 & p03 >= 15, 1, 0),
    informal = ifelse(secemp == 2 & p03 >= 15, 1, 0),
    empdom = ifelse(secemp == 3 & p03 >= 15, 1, 0), #Empleo domestico
    nocla = ifelse(secemp == 4 & p03 >= 15, 1, 0) #no clasificado
  ) 

#Clasificación por rango de edad
df <- df %>%
  mutate(
    # Inicializar las nuevas variables a 0
    menor10 = 0, pobla10 = 0, menor15 = 0, pobla15 = 0, pobla24 = 0, 
    pobla34 = 0, pobla44 = 0, pobla64 = 0, pobla65 = 0,
    # Asignar 1 a las variables basado en las condiciones de edad
    menor10 = if_else(p03 < 10, 1, menor10),
    menor15 = if_else(p03 < 15, 1, menor15),
    pobla10 = if_else(p03 >= 10, 1, pobla10),
    pobla15 = if_else(p03 >= 15, 1, pobla15),
    pobla24 = if_else(p03 >= 15 & p03 <= 24, 1, pobla24),
    pobla34 = if_else(p03 >= 25 & p03 <= 34, 1, pobla34),
    pobla44 = if_else(p03 >= 35 & p03 <= 44, 1, pobla44),
    pobla64 = if_else(p03 >= 45 & p03 <= 64, 1, pobla64),
    pobla65 = if_else(p03 >= 65, 1, pobla65)
  )


#Creacion de variables para crear las tasas.

df <- df %>%
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
    tdesem2 = case_when(desem2==1~1,pean==1~0))

#PENDIENTEEE  tpartib15 = if_else(t_a == 1, 0, NA),
#tpartig = if_else(petn == 1, 0, NA),
#tsubu = if_else(pean == 1, 0, NA)



#Clasificación del tipo de empleado - público y privado
df <- df %>%
  mutate(
    claempl = case_when(
      p42 == 1 ~ 1,          #Empleado/Obrero de Gobierno\ Estado
      p42 >= 2 & p42 <= 10 ~ 2, #Empleado privado
    )
  ) %>% 
  mutate(claempl=factor(claempl, levels=c(1,2),labels=c("Empleado público", "Empleado privado")))



#Asalariado e indepentiente
df <- df %>%
  mutate(
    asalind = case_when(
      p42 >= 1 & p42 <= 4 ~ 1, # asalariado
      p42 == 10 ~ 1,          # empleada domestico asalariado
      p42 >= 5 & p42 <= 6 ~ 2 # independiente
    )
  ) %>% 
  mutate(asalind=factor(asalind, levels=c(1,2),labels=c("Asalariado", "Independiente")))


#Nivel de instrucción
df <- df %>%
  mutate(
    nnivins = case_when(
      p10a == 1 ~ 1, #Ninguno
      p10a == 2 ~ 2, #Centro de Alfabetización
      (p10a == 3 | p10a == 4 | p10a == 5 | (p10a == 6 & p10b < 4)) ~ 3, #Educación Básica
      (p10a == 7 | (p10a == 6 & p10b > 3)) ~ 4, #Educación Media/Bachillerato
      p10a >= 8 & p10a <= 10 ~ 5, #Superior
    )
  ) %>% 
  mutate(nnivins=factor(nnivins, levels=c(1,2,3,4,5),labels=c("Ninguno","Centro de Alfabetización",
                                                              "Educación Básica","Educación Media/Bachillerato",
                                                              "Superior")))


#Horas de trabajo total
df <- df %>%
  mutate(
    p51a = na_if(p51a, 999), #horas de trabajo principal
    p51b = na_if(p51b, 999), #horas de trabajo secundario
    # Calcula la suma de p51a y p51b para obtener 'ht'
    ht = rowSums(select(., p51a, p51b), na.rm = TRUE)
  )


#ETNIA
df <- df %>%
  mutate(
    etnia = case_when(
      p15 == 1 ~ 1,
      p15 == 2 ~ 2,
      p15 == 3 ~ 2,
      p15 == 4 ~ 2,
      p15 == 5 ~ 5,
      p15 == 6 ~ 3,
      p15 == 7 ~ 4,
      p15 == 8 ~ 6
    )
  ) %>%
  mutate(
    etnia = factor(etnia, levels = c(1, 2, 3, 4, 5, 6),
                   labels = c('Indígena', 'Afroecuatoriano', 'Mestizo/a', 'Blanco', 'Montuvio', 'Otro'))
  )

#Horas de trabajo en la semana anterior

df <- df %>%
  mutate(
    p24 = na_if(p24, 999)
  )


#Edad de la población
df <- df %>%
  mutate(
    gedad = case_when(
      p03 >= 15 & p03 <= 24 ~ 1,
      p03 >= 25 & p03 <= 34 ~ 2,
      p03 >= 35 & p03 <= 44 ~ 3,
      p03 >= 45 & p03 <= 64 ~ 4,
      p03 >= 65 ~ 5
    )
  ) %>%
  mutate(
    gedad = factor(gedad, levels = c(1, 2, 3, 4, 5),
                   labels = c('Poblacion 15-24 años', 
                              'Poblacion 25-34 años', 
                              'Poblacion de 35-44 años', 
                              'Poblacion de 45-64 años', 
                              'Poblacion de 65 años y más'))
  )

#CLASIFICACIÓN DEL EMPLEO

df <- df %>%
  mutate(
    sector = case_when(
      formal == 1 ~ 1,
      informal == 1 ~ 2,
      empdom == 1 ~ 3,
      nocla == 1 ~ 4
    )
  ) %>%
  mutate(
    sector = factor(sector, levels = c(1, 2, 3, 4),
                   labels = c('Población con empleo en el sector formal', 
                              'Población con empleo en el sector informal', 
                              'Población con empleo Doméstico', 
                              'Población con empleo no clasificado por sector'))
  )



df <- df %>%
  mutate(
    desemb = case_when(
      desem1 == 1 ~ 1,
      desem2 == 1 ~ 2
    ),
    desema = case_when(
      desemab == 1 ~ 1,
      desemoc == 1 ~ 2
    ),
    desemb = factor(desemb, levels = c(1, 2),
                    labels = c('Cesante', 'Nuevo')),
    desema = factor(desema, levels = c(1, 2),
                    labels = c('Desempleo abierto', 'Desempleo oculto'))
  )


########################################################################
library(srvyr)


design <- df  %>% as_survey_design(ids = upm,
                                   strata = estrato,
                                   weights = fexp,
                                   nest = T)
options(survey.lonely.psu = "certainty")

library(survey)





#Para calcular la tasa de desempleo



design %>% group_by(area) %>% summarise(Desempleo =survey_ratio(tdesem, pean, vartype=c("se","ci","cv"), na.rm = T, deff = T))



#Poblacion empleada


design %>% group_by(area) %>% summarise(empleo =survey_ratio(templeog, pean, vartype=c("se","ci","cv"), na.rm = T, deff = T))




#ingresos medios nacionales 
#Se realiza el análisis para el total de empleados (asalariados e independientes)
#Por tal motivo es necesario filtrar
ingresos <- design %>%  filter(empleo == 1) 
svymean(~ingrl_rc, ingresos, na.rm = TRUE) 

#ingresos medios por sexo
svyby(~ingrl_rc, ~p02, ingresos, svymean,na.rm = T)






###Años de escolaridad - Se creo esta variable tomando en cuenta el nivel de instrucción y el último año aprobado

prueba <- df %>% mutate(años_esco=case_when(nnivins=="Educación Media/Bachillerato"~10+p10b,
                                            nnivins=="Superior"& (p10a==8|p10a==9)~13+p10b, #Superior Universitario o no Universitario
                                            nnivins=="Superior"& p10a==10~17+p10b, #Posgrado
                                            TRUE~p10b)) 

d1 <- prueba %>% as_survey_design(ids = upm,
                                  strata = estrato,
                                  weights = fexp,
                                  nest = T)
options(survey.lonely.psu = "certainty")

#Ingresos por nivel de instrucción
svyby(~ingrl_rc, ~p10a, d1, svymean,na.rm = T)

#Ingresos por nivel de instrucción resumida
svyby(~ingrl_rc, ~nnivins, d1, svymean,na.rm = T)

#Ingresos por nivel de instrucción resumida y area
svyby(~ingrl_rc, ~nnivins +area, d1, svymean,na.rm = T)




#año de escolaridad en el sector informal
prueba2 <- df %>% 
  filter(sector=='Población con empleo en el sector informal')


d1 <- prueba2 %>% as_survey_design(ids = upm,
                                  strata = estrato,
                                  weights = fexp,
                                  nest = T)
options(survey.lonely.psu = "certainty")

#Ingresos por nivel de instrucción

svyby(~ingrl_rc, ~p10a, d1, svymean,na.rm = T)

#Ingresos por nivel de instrucción resumida

svyby(~ingrl_rc, ~nnivins, d1, svymean,na.rm = T)



#realizar el calculo para los años de experiencia del INEC y la calculada con la formúla del paper (comparación)

#graficos con intervalo de confianza del 95%.


