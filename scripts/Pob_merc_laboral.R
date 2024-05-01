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
    # Inicializar desem1 y desem2 a 0 y luego recodificar basado en condact y p37
    desem1 = 0,
    desem2 = 0) %>%
  # Ahora aplicamos las condiciones para desem1 y desem2 
  #p37 - Trabajo anteriormente? 1 Sí y 2 NO
  mutate(
    desem1 = if_else(condact >= 7 & condact <= 8 & p37 == 1, 1, desem1), #Desempleo cesante
    desem2 = if_else(condact >= 7 & condact <= 8 & p37 == 2, 1, desem2), #Desempleo nuevo
    # Recodificación final que faltaba
    pein = if_else(condact == 9, 1, 0) #Población inactiva
  ) %>% 
  mutate(area=factor(area, levels=c(1,2),labels=c("Urbano","Rural")),
         p02=factor(p02, levels=c(1,2),labels=c("Hombre","Mujer")))


#Clasificación del sector en el que trabajan

df <- df %>%
  mutate(
    # Asigna nuevas variables basadas en condiciones
    formal = ifelse(secemp == 1 & p03 >= 15, 1, NA),
    informal = ifelse(secemp == 2 & p03 >= 15, 1, NA),
    empdom = ifelse(secemp == 3 & p03 >= 15, 1, NA), #Empleo domestico
    nocla = ifelse(secemp == 4 & p03 >= 15, 1, NA) #no clasificado
  ) %>%
  # Convierte los 0 a NA para una lista de variables
  mutate(across(c(petn, pean, empleo, adec, sub, sub_h, sub_w, oinad, nr, nc, desem, desemab, desemoc, desem1, desem2, pein, nocla, formal, informal, empdom), ~na_if(., 0)))

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


df <- df %>%
  mutate(
    # Inicialización de la variable t
    t = 1,
    # Inicializar todas las variables target a 0
    templeob = if_else(petn == 1, 0, NA),
    templeog = if_else(pean == 1, 0, NA),
    tadec = if_else(pean == 1, 0, NA),
    tsub = if_else(pean == 1, 0, NA),
    tsub_h = if_else(pean == 1, 0, NA),
    tsub_w = if_else(pean == 1, 0, NA),
    toinad = if_else(pean == 1, 0, NA),
    tnr = if_else(pean == 1, 0, NA),
    tnc = if_else(pean == 1, 0, NA),
    tdesem = if_else(pean == 1, 0, NA),
    tdesemab = if_else(pean == 1, 0, NA),
    tdesemoc = if_else(pean == 1, 0, NA),
    tdesem1 = if_else(pean == 1, 0, NA),
    tdesem2 = if_else(pean == 1, 0, NA),
    tpartib15 = if_else(t_a == 1, 0, NA),
    tpartig = if_else(petn == 1, 0, NA),
    tsubu = if_else(pean == 1, 0, NA),
    # Asignar 100 según las condiciones
    templeob = if_else(empleo == 1, 100, templeob),
    templeog = if_else(empleo == 1, 100, templeog),
    tadec = if_else(adec == 1, 100, tadec),
    tsub = if_else(sub == 1, 100, tsub),
    tsub_h = if_else(sub_h == 1, 100, tsub_h),
    tsub_w = if_else(sub_w == 1, 100, tsub_w),
    toinad = if_else(oinad == 1, 100, toinad),
    tnr = if_else(nr == 1, 100, tnr),
    tnc = if_else(nc == 1, 100, tnc),
    tdesem = if_else(desem == 1, 100, tdesem),
    tdesemab = if_else(desemab == 1, 100, tdesemab),
    tdesemoc = if_else(desemoc == 1, 100, tdesemoc),
    tdesem1 = if_else(desem1 == 1, 100, tdesem1),
    tdesem2 = if_else(desem2 == 1, 100, tdesem2),
    tpartib15 = if_else(pean == 1 & p03 >= 15, 100, tpartib15),
    tpartig = if_else(pean == 1 & p03 >= 15, 100, tpartig),
    tsubu = if_else(sub == 1 | desem == 1, 100, tsubu)
  )


#Clasificación del tipo de empleado - público y privado
df <- df %>%
  mutate(
    claempl = case_when(
      p42 == 1 ~ 1,          #Empleado/Obrero de Gobierno\ Estado
      p42 >= 2 & p42 <= 10 ~ 2, #Empleado privado
      TRUE ~ NA       
    )
  ) %>% 
  mutate(claempl=factor(claempl, levels=c(1,2),labels=c("Empleado público", "Empleado privado")))
  


#Asalariado e indepentiente
df <- df %>%
  mutate(
    asalind = case_when(
      p42 >= 1 & p42 <= 4 ~ 1, # asalariado
      p42 == 10 ~ 1,          # empleada domestico asalariado
      p42 >= 5 & p42 <= 6 ~ 2, # independiente
      TRUE ~ NA         
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
    # Recodificar valores de 999 a NA en p51a y p51b
    p51a = na_if(p51a, 999), #horas de trabajo principal
    p51b = na_if(p51b, 999), #horas de trabajo secundario
    # Calcula la suma de p51a y p51b para obtener 'ht'
    # Nota: na.rm = TRUE asegura que los NA sean ignorados en la suma
    ht = rowSums(select(., p51a, p51b), na.rm = TRUE)
  )


#ETNIA
df <- df %>%
  mutate(
    # Recodificar p15 a etnia según las reglas dadas
    etnia = case_when(
      p15 == 1 ~ 1,
      p15 == 2 ~ 2,
      p15 == 3 ~ 2,
      p15 == 4 ~ 2,
      p15 == 5 ~ 5,
      p15 == 6 ~ 3,
      p15 == 7 ~ 4,
      p15 == 8 ~ 6,
      TRUE ~ NA  # Asignar NA para cualquier otro caso no especificado
    )
  ) %>%
  # Convertir etnia a un factor con etiquetas para cada grupo étnico
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
    # Recodificar p03 a gedad según las reglas dadas
    gedad = case_when(
      p03 >= 15 & p03 <= 24 ~ 1,
      p03 >= 25 & p03 <= 34 ~ 2,
      p03 >= 35 & p03 <= 44 ~ 3,
      p03 >= 45 & p03 <= 64 ~ 4,
      p03 >= 65 ~ 5,
      TRUE ~  NA  # Asignar NA para cualquier otro caso no especificado
    )
  ) %>%
  # Convertir gedad a un factor con etiquetas para cada grupo de edad
  mutate(
    gedad = factor(gedad, levels = c(1, 2, 3, 4, 5),
                   labels = c('Poblacion 15-24 años', 
                              'Poblacion 25-34 años', 
                              'Poblacion de 35-44 años', 
                              'Poblacion de 45-64 años', 
                              'Poblacion de 65 años y más'))
  )



df <- df %>%
  mutate(
    # Asignar valores a secto basado en el estado de empleo
    secto = case_when(
      formal == 1 ~ 1,
      informal == 1 ~ 2,
      empdom == 1 ~ 3,
      nocla == 1 ~ 4,
      TRUE ~ NA  # Asignar NA para cualquier otro caso no especificado
    )
  ) %>%
  # Convertir secto a un factor con etiquetas para cada categoría de empleo
  mutate(
    secto = factor(secto, levels = c(1, 2, 3, 4),
                   labels = c('Población con empleo en el sector formal', 
                              'Población con empleo en el sector informal', 
                              'Población con empleo Doméstico', 
                              'Población con empleo no clasificado por sector'))
  )


df <- df %>%
  mutate(
    # Asignar valores a tinformal basado en las condiciones
    tinformal = case_when(
      empleo == 1 ~ 0,      # Si empleo es 1, entonces tinformal será 0
      informal == 1 ~ 100, # Si informal es 1, entonces tinformal será 100
      TRUE ~ NA  
    )
  )


df <- df %>%
  mutate(
    # Crear o modificar variables de filtro basadas en 'area'
    fil00 = if_else(area == 1 | area == 2, 1, NA),
    fil01 = if_else(area == 1, 1, NA),
    fil02 = if_else(area == 2, 1, NA)
  )


#Agrupar la clasificación del empleo en una sola columna
df <- df %>%
  mutate(
    # Asignar valores a secto basado en el estado de empleo
    secto = case_when(
      formal == 1 ~ 1,
      informal == 1 ~ 2,
      empdom == 1 ~ 3,
      nocla == 1 ~ 4,
      TRUE ~ NA 
    ),
    # Convertir secto a un factor con etiquetas para cada categoría de empleo
    secto = factor(secto, levels = c(1, 2, 3, 4),
                   labels = c('Población con empleo en el sector formal', 
                              'Población con empleo en el sector informal', 
                              'Población con empleo Doméstico', 
                              'Población con empleo no clasificado por sector'))
  )


df <- df %>%
  mutate(
    # Asignar valores a desemb y desema basado en las condiciones
    desemb = case_when(
      desem1 == 1 ~ 1,
      desem2 == 1 ~ 2,
      TRUE ~ NA  # Asignar NA para cualquier otro caso no especificado
    ),
    desema = case_when(
      desemab == 1 ~ 1,
      desemoc == 1 ~ 2,
      TRUE ~ NA
    ),
    # Convertir desemb y desema a factores con etiquetas para cada categoría
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


enemdu <- mutate(df, desem_rc = ifelse(desem==0 & pean==1,0,desem))

d1 <- enemdu %>% as_survey_design(ids = upm,
                                  strata = estrato,
                                  weights = fexp,
                                  nest = T)
options(survey.lonely.psu = "certainty")



d1 %>% group_by(area) %>% summarise(Desempleo =survey_ratio(desem_rc, pean, vartype=c("se","ci","cv"), na.rm = T, deff = T))



#Poblacion empleada
enemdu <- mutate(df, empleo_rc = ifelse(empleo==0 & pean==1,0,empleo)) 

d1 <- enemdu %>% as_survey_design(ids = upm,
                                  strata = estrato,
                                  weights = fexp,
                                  nest = T)
options(survey.lonely.psu = "certainty")
d1 %>% group_by(area) %>% summarise(empleo =survey_ratio(empleo_rc, pean, vartype=c("se","ci","cv"), na.rm = T, deff = T))



#ingresos

datos <- df %>%
  mutate(ingrl = ifelse(ingrl <= -1 | ingrl >= 999999, NA, ingrl)) %>% 
  mutate(ingrl_rc=ingrl*(111.715101/111.8551314)) %>% filter(empleo == 1 & pean==1) #deflactor del ipc 

d1 <- datos %>% as_survey_design(ids = upm,
                                  strata = estrato,
                                  weights = fexp,
                                  nest = T)
options(survey.lonely.psu = "certainty")

svyby(~ingrl_rc, ~p02, d1, svymean,na.rm = T)



svymean(~ingrl_rc, d1, na.rm = TRUE)






###Años de escolaridad 

prueba <- df %>% mutate(años_esco=case_when(nnivins=="Educación Media/Bachillerato"~10+p10b,
                                  nnivins=="Superior"& (p10a==8|p10a==9)~13+p10b, #Superior Universitario o no Universitario
                                  nnivins=="Superior"& p10a==10~17+p10b, #Posgrado
                                  TRUE~p10b)) %>% 
  mutate(ingrl = ifelse(ingrl <=-1 | ingrl >= 999999, NA, ingrl)) %>% 
  mutate(ingrl_rc=ingrl*(111.72/111.86)) 

d1 <- prueba %>% as_survey_design(ids = upm,
                                  strata = estrato,
                                  weights = fexp,
                                  nest = T)
options(survey.lonely.psu = "certainty")

svyby(~ingrl_rc, ~p10a, d1, svymean,na.rm = T)
svyby(~ingrl_rc, ~nnivins, d1, svymean,na.rm = T)
svyby(~ingrl_rc, ~nnivins +area, d1, svymean,na.rm = T)




#año de escolaridad en el sector informal
prueba <- df %>% mutate(años_esco=case_when(nnivins=="Educación Media/Bachillerato"~10+p10b,
                                            nnivins=="Superior"& (p10a==8|p10a==9)~13+p10b,
                                            nnivins=="Superior"& p10a==10~17+p10b,
                                            TRUE~p10b)) %>% 
  mutate(ingrl = ifelse(ingrl <=-1 | ingrl >= 999999, NA, ingrl)) %>% 
  mutate(ingrl_rc=ingrl*(111.72/111.86)) %>% 
  filter(secto=='Población con empleo en el sector informal')


d1 <- prueba %>% as_survey_design(ids = upm,
                                  strata = estrato,
                                  weights = fexp,
                                  nest = T)
options(survey.lonely.psu = "certainty")

svyby(~ingrl_rc, ~p10a, d1, svymean,na.rm = T)
svyby(~ingrl_rc, ~nnivins, d1, svymean,na.rm = T)



#investigar los años de ocupación en el trabajo actualmente y los años de experiencia laboral

#graficos con intervalo de confianza del 95%


