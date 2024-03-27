# Crea indicadores
enemdu_persona_2024_01 $t_a <- 1
enemdu_persona_2024_01 $condact <- ifelse(enemdu_persona_2024_01 $p03 < 15, 0, enemdu_persona_2024_01 $condact) # Esta línea ajusta condact basado en p03<15 como en el primer comentario
enemdu_persona_2024_01 $petn <- ifelse(enemdu_persona_2024_01 $p03 >= 15 & enemdu_persona_2024_01 $p03 <= 99, 1, 0)
enemdu_persona_2024_01 $pean <- ifelse(enemdu_persona_2024_01 $condact >= 1 & enemdu_persona_2024_01 $condact <= 8, 1, 0)
enemdu_persona_2024_01 $empleo <- ifelse(enemdu_persona_2024_01 $condact >= 1 & enemdu_persona_2024_01 $condact <= 6, 1, 0)
enemdu_persona_2024_01 $adec <- ifelse(enemdu_persona_2024_01 $condact == 1, 1, 0)
enemdu_persona_2024_01 $sub <- ifelse(enemdu_persona_2024_01 $condact >= 2 & enemdu_persona_2024_01 $condact <= 3, 1, 0)
enemdu_persona_2024_01 $sub_h <- ifelse(enemdu_persona_2024_01 $condact == 2, 1, 0)
enemdu_persona_2024_01 $sub_w <- ifelse(enemdu_persona_2024_01 $condact == 3, 1, 0)
enemdu_persona_2024_01 $oinad <- ifelse(enemdu_persona_2024_01 $condact == 4, 1, 0)
enemdu_persona_2024_01 $nr <- ifelse(enemdu_persona_2024_01 $condact == 5, 1, 0)
enemdu_persona_2024_01 $nc <- ifelse(enemdu_persona_2024_01 $condact == 6, 1, 0)
enemdu_persona_2024_01 $desem <- ifelse(enemdu_persona_2024_01 $condact >= 7 & enemdu_persona_2024_01 $condact <= 8, 1, 0)
enemdu_persona_2024_01 $desemab <- ifelse(enemdu_persona_2024_01 $condact == 7, 1, 0)
enemdu_persona_2024_01 $desemoc <- ifelse(enemdu_persona_2024_01 $condact == 8, 1, 0)
enemdu_persona_2024_01 $pein <- ifelse(enemdu_persona_2024_01 $condact == 9, 1, 0)

# Inicializar desem1 y desem2 como 0
enemdu_persona_2024_01 $desem1 <- 0
enemdu_persona_2024_01 $desem2 <- 0

# Actualizar desem1 y desem2 basado en las condiciones
enemdu_persona_2024_01 $desem1[enemdu_persona_2024_01 $condact >= 7 & enemdu_persona_2024_01 $condact <= 8 & enemdu_persona_2024_01 $p37 == 1] <- 1
enemdu_persona_2024_01 $desem2[enemdu_persona_2024_01 $condact >= 7 & enemdu_persona_2024_01 $condact <= 8 & enemdu_persona_2024_01 $p37 == 2] <- 1


#*DESAGREGACIÓN DE LA SECEMP.
# Inicializa las nuevas columnas con 0
enemdu_persona_2024_01 $formal <- 0
enemdu_persona_2024_01 $informal <- 0
enemdu_persona_2024_01 $empdom <- 0
enemdu_persona_2024_01 $nocla <- 0

# Actualiza las columnas basado en las condiciones dadas
enemdu_persona_2024_01 $formal[enemdu_persona_2024_01 $secemp == 1 & enemdu_persona_2024_01 $p03 >= 15] <- 1
enemdu_persona_2024_01 $informal[enemdu_persona_2024_01 $secemp == 2 & enemdu_persona_2024_01 $p03 >= 15] <- 1
enemdu_persona_2024_01 $empdom[enemdu_persona_2024_01 $secemp == 3 & enemdu_persona_2024_01 $p03 >= 15] <- 1
enemdu_persona_2024_01 $nocla[enemdu_persona_2024_01 $secemp == 4 & enemdu_persona_2024_01 $p03 >= 15] <- 1


#GRUPO DE EDAD
# Inicializar las nuevas columnas con 0
enemdu_persona_2024_01 $menor10 <- 0
enemdu_persona_2024_01 $pobla10 <- 0
enemdu_persona_2024_01 $pobla15 <- 0
enemdu_persona_2024_01 $pobla24 <- 0
enemdu_persona_2024_01 $pobla34 <- 0
enemdu_persona_2024_01 $pobla44 <- 0
enemdu_persona_2024_01 $pobla64 <- 0
enemdu_persona_2024_01 $pobla65 <- 0
enemdu_persona_2024_01 $menor15 <- 0

# Actualizar los valores basado en las condiciones
enemdu_persona_2024_01 $menor10[enemdu_persona_2024_01 $p03 < 10] <- 1
enemdu_persona_2024_01 $pobla10[enemdu_persona_2024_01 $p03 >= 10] <- 1
enemdu_persona_2024_01 $pobla15[enemdu_persona_2024_01 $p03 >= 15] <- 1
enemdu_persona_2024_01 $pobla24[enemdu_persona_2024_01 $p03 >= 15 & enemdu_persona_2024_01 $p03 <= 24] <- 1
enemdu_persona_2024_01 $pobla34[enemdu_persona_2024_01 $p03 >= 25 & enemdu_persona_2024_01 $p03 <= 34] <- 1
enemdu_persona_2024_01 $pobla44[enemdu_persona_2024_01 $p03 >= 35 & enemdu_persona_2024_01 $p03 <= 44] <- 1
enemdu_persona_2024_01 $pobla64[enemdu_persona_2024_01 $p03 >= 45 & enemdu_persona_2024_01 $p03 <= 64] <- 1
enemdu_persona_2024_01 $pobla65[enemdu_persona_2024_01 $p03 >= 65] <- 1
enemdu_persona_2024_01 $menor15[enemdu_persona_2024_01 $p03 < 15] <- 1

# ==============================================================================
# VARIABLES PARA DE TRABAJO PARA GENERACIÓN DE TABULADOS DE MERCADO LABORAL
# ==============================================================================

# Se asume que los datos ya están cargados en un data.frame llamado enemdu_persona_2024_01 

# Generación de Indicadores de Mercado Laboral - Tasas
enemdu_persona_2024_01 $templeob <- ifelse(enemdu_persona_2024_01 $petn == 1, 0, ifelse(enemdu_persona_2024_01 $empleo == 1, 100, NA))
enemdu_persona_2024_01 $templeog <- ifelse(enemdu_persona_2024_01 $pean == 1, 0, ifelse(enemdu_persona_2024_01 $empleo == 1, 100, NA))
enemdu_persona_2024_01 $tadec <- ifelse(enemdu_persona_2024_01 $pean == 1, 0, ifelse(enemdu_persona_2024_01 $adec == 1, 100, NA))
enemdu_persona_2024_01 $tsub <- ifelse(enemdu_persona_2024_01 $pean == 1, 0, ifelse(enemdu_persona_2024_01 $sub == 1, 100, NA))
enemdu_persona_2024_01 $tsub_h <- ifelse(enemdu_persona_2024_01 $pean == 1, 0, ifelse(enemdu_persona_2024_01 $sub_h == 1, 100, NA))
enemdu_persona_2024_01 $tsub_w <- ifelse(enemdu_persona_2024_01 $pean == 1, 0, ifelse(enemdu_persona_2024_01 $sub_w == 1, 100, NA))
enemdu_persona_2024_01 $toinad <- ifelse(enemdu_persona_2024_01 $pean == 1, 0, ifelse(enemdu_persona_2024_01 $oinad == 1, 100, NA))
enemdu_persona_2024_01 $tnr <- ifelse(enemdu_persona_2024_01 $pean == 1, 0, ifelse(enemdu_persona_2024_01 $nr == 1, 100, NA))
enemdu_persona_2024_01 $tnc <- ifelse(enemdu_persona_2024_01 $pean == 1, 0, ifelse(enemdu_persona_2024_01 $nc == 1, 100, NA))
enemdu_persona_2024_01 $tdesem <- ifelse(enemdu_persona_2024_01 $pean == 1, 0, ifelse(enemdu_persona_2024_01 $desem == 1, 100, NA))
enemdu_persona_2024_01 $tdesemab <- ifelse(enemdu_persona_2024_01 $pean == 1, 0, ifelse(enemdu_persona_2024_01 $desemab == 1, 100, NA))
enemdu_persona_2024_01 $tdesemoc <- ifelse(enemdu_persona_2024_01 $pean == 1, 0, ifelse(enemdu_persona_2024_01 $desemoc == 1, 100, NA))
enemdu_persona_2024_01 $tdesem1 <- ifelse(enemdu_persona_2024_01 $pean == 1, 0, ifelse(enemdu_persona_2024_01 $desem1 == 1, 100, NA))
enemdu_persona_2024_01 $tdesem2 <- ifelse(enemdu_persona_2024_01 $pean == 1, 0, ifelse(enemdu_persona_2024_01 $desem2 == 1, 100, NA))
enemdu_persona_2024_01 $tpartib15 <- ifelse(enemdu_persona_2024_01 $t_a == 1, 0, ifelse(enemdu_persona_2024_01 $pean == 1 & enemdu_persona_2024_01 $p03 >= 15, 100, NA))
enemdu_persona_2024_01 $tpartig <- ifelse(enemdu_persona_2024_01 $petn == 1, 0, ifelse(enemdu_persona_2024_01 $pean == 1 & enemdu_persona_2024_01 $p03 >= 15, 100, NA))
enemdu_persona_2024_01 $tsubu <- ifelse(enemdu_persona_2024_01 $pean == 1, 0, ifelse(enemdu_persona_2024_01 $sub == 1 | enemdu_persona_2024_01 $desem == 1, 100, NA))

# Empleado público y privado
enemdu_persona_2024_01 $claempl <- ifelse(enemdu_persona_2024_01 $p42 == 1, 1, ifelse(enemdu_persona_2024_01 $p42 >= 2 & enemdu_persona_2024_01 $p42 <= 10, 2, NA))
# Asalariado e Independiente
enemdu_persona_2024_01 $asalind <- ifelse(enemdu_persona_2024_01 $p42 %in% c(1:4, 10), 1, ifelse(enemdu_persona_2024_01 $p42 %in% c(5, 6), 2, NA))

# Nivel de instrucción
enemdu_persona_2024_01 $nnivins <- ifelse(enemdu_persona_2024_01 $p10a == 1, 1,
                                          ifelse(enemdu_persona_2024_01 $p10a == 2, 2,
                                                 ifelse(enemdu_persona_2024_01 $p10a %in% c(3, 4, 5, 6) & enemdu_persona_2024_01 $p10b < 4, 3,
                                                        ifelse(enemdu_persona_2024_01 $p10a %in% c(7, 8) | (enemdu_persona_2024_01 $p10a == 6 & enemdu_persona_2024_01 $p10b > 3), 4,
                                                               ifelse(enemdu_persona_2024_01 $p10a >= 8 & enemdu_persona_2024_01 $p10a <= 10, 5, NA)))))


# Horas de trabajo
enemdu_persona_2024_01  <- enemdu_persona_2024_01  %>%
  mutate(p51a = ifelse(p51a == 999, NA, p51a),
         p51b = ifelse(p51b == 999, NA, p51b),
         ht = p51a + p51b)



# Para etiquetar valores y variables, considerar usar haven o simplemente asignar los nombres directamente
# Si tienes el paquete haven, podrías usar algo como:
# labels(enemdu_persona_2024_01 $etnia) <- set_labels(c(Indigena = 1, Afroecuatoriano = 2, Mestizo/a = 3, Blanco = 4, Montuvio = 5, Otro = 6))

# Horas Efectivas
enemdu_persona_2024_01 $p24 <- ifelse(enemdu_persona_2024_01 $p24 == 999, NA, enemdu_persona_2024_01 $p24)

# Grupos de edad
enemdu_persona_2024_01 $gedad <- case_when(
  enemdu_persona_2024_01 $p03 >= 15 & enemdu_persona_2024_01 $p03 <= 24 ~ 1,
  enemdu_persona_2024_01 $p03 >= 25 & enemdu_persona_2024_01 $p03 <= 34 ~ 2,
  enemdu_persona_2024_01 $p03 >= 35 & enemdu_persona_2024_01 $p03 <= 44 ~ 3,
  enemdu_persona_2024_01 $p03 >= 45 & enemdu_persona_2024_01 $p03 <= 64 ~ 4,
  enemdu_persona_2024_01 $p03 >= 65 ~ 5
)

# Sectorización de la población empleada
enemdu_persona_2024_01  <- enemdu_persona_2024_01  %>%
  mutate(secto = case_when(
    formal == 1 ~ 1,
    informal == 1 ~ 2,
    empdom == 1 ~ 3,
    nocla == 1 ~ 4
  ))

# Sector Informal
enemdu_persona_2024_01  <- enemdu_persona_2024_01  %>%
  mutate(tinformal = ifelse(empleo == 1, 0, ifelse(informal == 1, 100, NA)))

# Filtros
enemdu_persona_2024_01  <- enemdu_persona_2024_01  %>%
  mutate(fil00 = ifelse(area == 1 | area == 2, 1, NA),
         fil01 = ifelse(area == 1, 1, NA),
         fil02 = ifelse(area == 2, 1, NA))

# Caracterización de los desempleados
enemdu_persona_2024_01  <- enemdu_persona_2024_01  %>%
  mutate(desemb = ifelse(desem1 == 1, 1, ifelse(desem2 == 1, 2, NA)),
         desema = ifelse(desemab == 1, 1, ifelse(desemoc == 1, 2, NA)))


d1 <- enemdu_persona_2024_01 %>% as_survey_design(ids = upm,
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


