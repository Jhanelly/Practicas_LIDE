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
  mutate(ingrl_trans = ifelse(ingrl <= -1 | ingrl >= 999999, NA, ingrl)) %>%  #Valos no validos
  mutate(ingrl_rc=ingrl_trans*(111.715101/111.8551314)) %>%   #deflactor del ipc año base diciembre 2023
  mutate(log_ingrl_rc =log(ingrl_rc))


pob_empl <- df %>% filter(empleo==1) 
d3 <- pob_empl %>% as_survey_design(ids = upm,
                                    strata = estrato,
                                    weights = fexp,
                                    nest = T)

options(survey.lonely.psu = "certainty")

svyplot(log_ingrl_rc ~ p03, d3, style="subsample",pch=1, main = "Diagrama de Dispersión con Diseño Muestral",
        xlab="experiencia",ylab="ingresos")






exploracion <- df %>% select(ingrl_rc,p45)
introduce(exploracion) %>% t()
status(exploracion) %>% flextable()
plot_na_pareto(exploracion,only_na = T)
