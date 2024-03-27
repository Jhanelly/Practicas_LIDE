library(readxl) 
enemdu_persona_2024_02 <- read_excel("bases/enemdu_persona_2024_02.sav")

# Crea indicadores
enemdu_persona_2024_02 $t_a <- 1
enemdu_persona_2024_02 $condact <- ifelse(enemdu_persona_2024_02 $p03 < 15, 0, enemdu_persona_2024_02 $condact) # Esta línea ajusta condact basado en p03<15 como en el primer comentario
enemdu_persona_2024_02 $petn <- ifelse(enemdu_persona_2024_02 $p03 >= 15 & enemdu_persona_2024_02 $p03 <= 99, 1, 0)
enemdu_persona_2024_02 $pean <- ifelse(enemdu_persona_2024_02 $condact >= 1 & enemdu_persona_2024_02 $condact <= 8, 1, 0)
enemdu_persona_2024_02 $empleo <- ifelse(enemdu_persona_2024_02 $condact >= 1 & enemdu_persona_2024_02 $condact <= 6, 1, 0)
enemdu_persona_2024_02 $adec <- ifelse(enemdu_persona_2024_02 $condact == 1, 1, 0)
enemdu_persona_2024_02 $sub <- ifelse(enemdu_persona_2024_02 $condact >= 2 & enemdu_persona_2024_02 $condact <= 3, 1, 0)
enemdu_persona_2024_02 $sub_h <- ifelse(enemdu_persona_2024_02 $condact == 2, 1, 0)
enemdu_persona_2024_02 $sub_w <- ifelse(enemdu_persona_2024_02 $condact == 3, 1, 0)
enemdu_persona_2024_02 $oinad <- ifelse(enemdu_persona_2024_02 $condact == 4, 1, 0)
enemdu_persona_2024_02 $nr <- ifelse(enemdu_persona_2024_02 $condact == 5, 1, 0)
enemdu_persona_2024_02 $nc <- ifelse(enemdu_persona_2024_02 $condact == 6, 1, 0)
enemdu_persona_2024_02 $desem <- ifelse(enemdu_persona_2024_02 $condact >= 7 & enemdu_persona_2024_02 $condact <= 8, 1, 0)
enemdu_persona_2024_02 $desemab <- ifelse(enemdu_persona_2024_02 $condact == 7, 1, 0)
enemdu_persona_2024_02 $desemoc <- ifelse(enemdu_persona_2024_02 $condact == 8, 1, 0)
enemdu_persona_2024_02 $pein <- ifelse(enemdu_persona_2024_02 $condact == 9, 1, 0)

# Inicializar desem1 y desem2 como 0
enemdu_persona_2024_02 $desem1 <- 0
enemdu_persona_2024_02 $desem2 <- 0

# Actualizar desem1 y desem2 basado en las condiciones
enemdu_persona_2024_02 $desem1[enemdu_persona_2024_02 $condact >= 7 & enemdu_persona_2024_02 $condact <= 8 & enemdu_persona_2024_02 $p37 == 1] <- 1
enemdu_persona_2024_02 $desem2[enemdu_persona_2024_02 $condact >= 7 & enemdu_persona_2024_02 $condact <= 8 & enemdu_persona_2024_02 $p37 == 2] <- 1


#*DESAGREGACIÓN DE LA SECEMP.
# Inicializa las nuevas columnas con 0
enemdu_persona_2024_02 $formal <- 0
enemdu_persona_2024_02 $informal <- 0
enemdu_persona_2024_02 $empdom <- 0
enemdu_persona_2024_02 $nocla <- 0

# Actualiza las columnas basado en las condiciones dadas
enemdu_persona_2024_02 $formal[enemdu_persona_2024_02 $secemp == 1 & enemdu_persona_2024_02 $p03 >= 15] <- 1
enemdu_persona_2024_02 $informal[enemdu_persona_2024_02 $secemp == 2 & enemdu_persona_2024_02 $p03 >= 15] <- 1
enemdu_persona_2024_02 $empdom[enemdu_persona_2024_02 $secemp == 3 & enemdu_persona_2024_02 $p03 >= 15] <- 1
enemdu_persona_2024_02 $nocla[enemdu_persona_2024_02 $secemp == 4 & enemdu_persona_2024_02 $p03 >= 15] <- 1


#GRUPO DE EDAD
# Inicializar las nuevas columnas con 0
enemdu_persona_2024_02 $menor10 <- 0
enemdu_persona_2024_02 $pobla10 <- 0
enemdu_persona_2024_02 $pobla15 <- 0
enemdu_persona_2024_02 $pobla24 <- 0
enemdu_persona_2024_02 $pobla34 <- 0
enemdu_persona_2024_02 $pobla44 <- 0
enemdu_persona_2024_02 $pobla64 <- 0
enemdu_persona_2024_02 $pobla65 <- 0
enemdu_persona_2024_02 $menor15 <- 0

# Actualizar los valores basado en las condiciones
enemdu_persona_2024_02 $menor10[enemdu_persona_2024_02 $p03 < 10] <- 1
enemdu_persona_2024_02 $pobla10[enemdu_persona_2024_02 $p03 >= 10] <- 1
enemdu_persona_2024_02 $pobla15[enemdu_persona_2024_02 $p03 >= 15] <- 1
enemdu_persona_2024_02 $pobla24[enemdu_persona_2024_02 $p03 >= 15 & enemdu_persona_2024_02 $p03 <= 24] <- 1
enemdu_persona_2024_02 $pobla34[enemdu_persona_2024_02 $p03 >= 25 & enemdu_persona_2024_02 $p03 <= 34] <- 1
enemdu_persona_2024_02 $pobla44[enemdu_persona_2024_02 $p03 >= 35 & enemdu_persona_2024_02 $p03 <= 44] <- 1
enemdu_persona_2024_02 $pobla64[enemdu_persona_2024_02 $p03 >= 45 & enemdu_persona_2024_02 $p03 <= 64] <- 1
enemdu_persona_2024_02 $pobla65[enemdu_persona_2024_02 $p03 >= 65] <- 1
enemdu_persona_2024_02 $menor15[enemdu_persona_2024_02 $p03 < 15] <- 1

# ==============================================================================
# VARIABLES PARA DE TRABAJO PARA GENERACIÓN DE TABULADOS DE MERCADO LABORAL
# ==============================================================================

# Se asume que los datos ya están cargados en un data.frame llamado enemdu_persona_2024_02 

# Generación de Indicadores de Mercado Laboral - Tasas
enemdu_persona_2024_02 $templeob <- ifelse(enemdu_persona_2024_02 $petn == 1, 0, ifelse(enemdu_persona_2024_02 $empleo == 1, 100, NA))
enemdu_persona_2024_02 $templeog <- ifelse(enemdu_persona_2024_02 $pean == 1, 0, ifelse(enemdu_persona_2024_02 $empleo == 1, 100, NA))
enemdu_persona_2024_02 $tadec <- ifelse(enemdu_persona_2024_02 $pean == 1, 0, ifelse(enemdu_persona_2024_02 $adec == 1, 100, NA))
enemdu_persona_2024_02 $tsub <- ifelse(enemdu_persona_2024_02 $pean == 1, 0, ifelse(enemdu_persona_2024_02 $sub == 1, 100, NA))
enemdu_persona_2024_02 $tsub_h <- ifelse(enemdu_persona_2024_02 $pean == 1, 0, ifelse(enemdu_persona_2024_02 $sub_h == 1, 100, NA))
enemdu_persona_2024_02 $tsub_w <- ifelse(enemdu_persona_2024_02 $pean == 1, 0, ifelse(enemdu_persona_2024_02 $sub_w == 1, 100, NA))
enemdu_persona_2024_02 $toinad <- ifelse(enemdu_persona_2024_02 $pean == 1, 0, ifelse(enemdu_persona_2024_02 $oinad == 1, 100, NA))
enemdu_persona_2024_02 $tnr <- ifelse(enemdu_persona_2024_02 $pean == 1, 0, ifelse(enemdu_persona_2024_02 $nr == 1, 100, NA))
enemdu_persona_2024_02 $tnc <- ifelse(enemdu_persona_2024_02 $pean == 1, 0, ifelse(enemdu_persona_2024_02 $nc == 1, 100, NA))
enemdu_persona_2024_02 $tdesem <- ifelse(enemdu_persona_2024_02 $pean == 1, 0, ifelse(enemdu_persona_2024_02 $desem == 1, 100, NA))
enemdu_persona_2024_02 $tdesemab <- ifelse(enemdu_persona_2024_02 $pean == 1, 0, ifelse(enemdu_persona_2024_02 $desemab == 1, 100, NA))
enemdu_persona_2024_02 $tdesemoc <- ifelse(enemdu_persona_2024_02 $pean == 1, 0, ifelse(enemdu_persona_2024_02 $desemoc == 1, 100, NA))
enemdu_persona_2024_02 $tdesem1 <- ifelse(enemdu_persona_2024_02 $pean == 1, 0, ifelse(enemdu_persona_2024_02 $desem1 == 1, 100, NA))
enemdu_persona_2024_02 $tdesem2 <- ifelse(enemdu_persona_2024_02 $pean == 1, 0, ifelse(enemdu_persona_2024_02 $desem2 == 1, 100, NA))
enemdu_persona_2024_02 $tpartib15 <- ifelse(enemdu_persona_2024_02 $t_a == 1, 0, ifelse(enemdu_persona_2024_02 $pean == 1 & enemdu_persona_2024_02 $p03 >= 15, 100, NA))
enemdu_persona_2024_02 $tpartig <- ifelse(enemdu_persona_2024_02 $petn == 1, 0, ifelse(enemdu_persona_2024_02 $pean == 1 & enemdu_persona_2024_02 $p03 >= 15, 100, NA))
enemdu_persona_2024_02 $tsubu <- ifelse(enemdu_persona_2024_02 $pean == 1, 0, ifelse(enemdu_persona_2024_02 $sub == 1 | enemdu_persona_2024_02 $desem == 1, 100, NA))

# Empleado público y privado
enemdu_persona_2024_02 $claempl <- ifelse(enemdu_persona_2024_02 $p42 == 1, 1, ifelse(enemdu_persona_2024_02 $p42 >= 2 & enemdu_persona_2024_02 $p42 <= 10, 2, NA))
# Asalariado e Independiente
enemdu_persona_2024_02 $asalind <- ifelse(enemdu_persona_2024_02 $p42 %in% c(1:4, 10), 1, ifelse(enemdu_persona_2024_02 $p42 %in% c(5, 6), 2, NA))

# Nivel de instrucción
enemdu_persona_2024_02 $nnivins <- ifelse(enemdu_persona_2024_02 $p10a == 1, 1,
                                          ifelse(enemdu_persona_2024_02 $p10a == 2, 2,
                                                 ifelse(enemdu_persona_2024_02 $p10a %in% c(3, 4, 5, 6) & enemdu_persona_2024_02 $p10b < 4, 3,
                                                        ifelse(enemdu_persona_2024_02 $p10a %in% c(7, 8) | (enemdu_persona_2024_02 $p10a == 6 & enemdu_persona_2024_02 $p10b > 3), 4,
                                                               ifelse(enemdu_persona_2024_02 $p10a >= 8 & enemdu_persona_2024_02 $p10a <= 10, 5, NA)))))


# Horas de trabajo
enemdu_persona_2024_02  <- enemdu_persona_2024_02  %>%
  mutate(p51a = ifelse(p51a == 999, NA, p51a),
         p51b = ifelse(p51b == 999, NA, p51b),
         ht = p51a + p51b)


# Horas Efectivas
enemdu_persona_2024_02 $p24 <- ifelse(enemdu_persona_2024_02 $p24 == 999, NA, enemdu_persona_2024_02 $p24)

# Grupos de edad
enemdu_persona_2024_02 $gedad <- case_when(
  enemdu_persona_2024_02 $p03 >= 15 & enemdu_persona_2024_02 $p03 <= 24 ~ 1,
  enemdu_persona_2024_02 $p03 >= 25 & enemdu_persona_2024_02 $p03 <= 34 ~ 2,
  enemdu_persona_2024_02 $p03 >= 35 & enemdu_persona_2024_02 $p03 <= 44 ~ 3,
  enemdu_persona_2024_02 $p03 >= 45 & enemdu_persona_2024_02 $p03 <= 64 ~ 4,
  enemdu_persona_2024_02 $p03 >= 65 ~ 5
)

# Sectorización de la población empleada
enemdu_persona_2024_02  <- enemdu_persona_2024_02  %>%
  mutate(secto = case_when(
    formal == 1 ~ 1,
    informal == 1 ~ 2,
    empdom == 1 ~ 3,
    nocla == 1 ~ 4
  ))

# Sector Informal
enemdu_persona_2024_02  <- enemdu_persona_2024_02  %>%
  mutate(tinformal = ifelse(empleo == 1, 0, ifelse(informal == 1, 100, NA)))

# Filtros
enemdu_persona_2024_02  <- enemdu_persona_2024_02  %>%
  mutate(fil00 = ifelse(area == 1 | area == 2, 1, NA),
         fil01 = ifelse(area == 1, 1, NA),
         fil02 = ifelse(area == 2, 1, NA))

# Caracterización de los desempleados
enemdu_persona_2024_02  <- enemdu_persona_2024_02  %>%
  mutate(desemb = ifelse(desem1 == 1, 1, ifelse(desem2 == 1, 2, NA)),
         desema = ifelse(desemab == 1, 1, ifelse(desemoc == 1, 2, NA)))


d1 <- enemdu_persona_2024_02 %>% as_survey_design(ids = upm,
                                                  strata = estrato,
                                                  weights = fexp,
                                                  nest = T)


medias_ponderadas <- svyby(~templeob + templeog + tadec + tsub + tsub_h + tsub_w + tnr + toinad + tnc + tdesem + tdesemab + tdesemoc + tpartig + tpartib15, 
                           ~t_a + area + p02, 
                           d1, 
                           svymean,
                           na.rm.by=T)

# Renombrar las variables para incluir los nombres de los indicadores
nombres_indicadores <- c("Empleo Bruto (%)", "Empleo Global (%)", "Empleo Adecuado/pleno (%)", 
                         "Subempleo (%)", "Subempleo por insuficiencia de tiempo de trabajo (%)", 
                         "Subempleo por insuficiencia de ingresos (%)", "Empleo no remunerado (%)", 
                         "Otro no pleno (%)", "Empleo no clasificado (%)", "Desempleo (%)", 
                         "Desempleo Abierto (%)", "Desempleo Oculto (%)", "Participación Global (%)", 
                         "Participación Bruta (%)")

names(medias_ponderadas)[5:ncol(medias_ponderadas)] <- nombres_indicadores

# Opcionalmente, crea una tabla bonita con `gt` para visualizar los resultados
library(gt)
tabla <- medias_ponderadas %>%
  gt() %>%
  tab_header(
    title = "Indicadores de Mercado Laboral (15 años y más)",
    subtitle = "Fuente: INEC - ENEMDU\nElaborado por: Dirección de Estadísticas Sociodemográficas - DIES"
  )

print(tabla)


