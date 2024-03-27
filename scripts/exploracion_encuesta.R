#Cargando librerias
library(foreign)
library(survey)
library(dplyr)
library(srvyr)
library(stringr)
library(haven)


#cargando encuesta
enemdu_persona_2024_01 <- read_sav("enemdu_persona_2024_01.sav")


#recodificación de variable
enemdu <- mutate(enemdu_persona_2024_01, desem_rc = ifelse(is.na(desempleo) & condact==1,0,desempleo))

#creando el diseño de la encuesta
d1 <- enemdu %>% as_survey_design(ids = upm,
                                  strata = estrato,
                                  weights = fexp,
                                  nest = T)
options(survey.lonely.psu = "certainty")

#obtención de la media aplicando el diseño de la encuesta
tabla1<-d1 %>% summarise(ingperc = survey_mean(ingpc, vartype=c("se"), na.rm=T))
View(tabla1)


#obtención de la media sin el diseño de la encuesta

enemdu %>% summarise(mean(ingpc,na.rm=TRUE))


#obtención de la media por area
tabla2<-d1 %>% group_by(area) %>% summarise(ingperc = survey_mean(ingpc,
                                                                  vartype=c("se"), na.rm=T))
View(tabla2)


#calculo de la mediana
mediana_ingpc <- svyquantile(~ingpc, d1, 0.5, na.rm=TRUE)


#La variable p45 corresponde a cuantos años trabaja en la ocupación declarada
experiencia<-d1 %>% mutate(p45=as.numeric(p45))

#Mediana de la experiencia laboral
svyquantile(~p45, experiencia, 0.5, na.rm=TRUE)




#CALCULAR LAS VARIABLES DE EMPLEO 
#REVISAR LO QUE ENVIE DANIEL