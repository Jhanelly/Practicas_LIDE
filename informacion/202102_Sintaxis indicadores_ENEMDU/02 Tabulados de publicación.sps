* Encoding: UTF-8.
*==============================================================================*.
* TÍTULO DE LA SINTAXIS:							   
* Cálculo de indicadores de la nueva condición de actividad		y sectorización de la PEA	   				   
* OPERACIÓN ESTADÍSTICA:													   
* Encuesta Nacional de Empleo, Desempleo y Subempleo (ENEMDU)				   
* UNIDAD TÉCNICA RESPONSABLE: 												   
* Dirección de Estadísticas Sociodemográficas (DIES)						   
* ENTIDAD EJECUTORA:														   
* Instituto Nacional de Estadística y Censos (INEC)							   
*==============================================================================*.
* Fecha de elaboración:          Septiembre - 2014							   
* Fecha última modificación:    Febrero - 2021					   
*==============================================================================*.
* Sintaxis elaborada en base a la "Nota metodológica para la medición del empleo en Ecuador".
*==============================================================================*.
* Elaborado por:													   
* Dirección de Estadísticas Sociodemográficas (DIES)						   
* Gestión de Estadísticas Permanentes a Hogares 	   						   
* Aprobado por:													   
* Dirección de Estudios Laborales y Económicos (DELE)				       
* Unidad de Estudios de Mercado Laboral								       
*==============================================================================*.
* Revisado por:																   
* 1. Dirección de Estudios Laborales y Económicos (DELE)				       
*    Unidad de Estudios de Mercado Laboral								       
* 2. Dirección de Estadísticas Sociodemográficas (DIES)						   
*    Gestión de Estadísticas Permanentes a Hogares 	   						   
*==============================================================================*.
*Indicaciones.
*1- Ejecute esta sintaxis en la base de datos: enemdu_persona_xxxx_xx

*=====================================================================================.
*1 POBLACIÓN DE LOS PRINCIPALES INDICADORES DE MERCADO LABORAL
*=====================================================================================.
weight by fexp.
ctables /vlabels var= area  p02  display=none
            /table (t_a[c] + menor15[c] + pobla15[c]> (petn[c] + pean[c]  + empleo[c] + adec[c] + sub[c] + 
                                sub_h[c] + sub_w[c] + nr[c] + oinad[c]  + nc[c] + desem[c] + desemab[c] + desemoc[c] + desem1[c] + 
                                desem2[c] + pein[c])) by area[c]  + p02[c] 
           /slabels visible=no
           /categories variables = area  p02  t_a  empty=exclude
           /categories variables= area total = yes position=before
           /titles title='Indicadores de Mercado Laboral (15 años y más) - Población'
                    caption='Fuente: INEC - ENEMDU' 'Elaborado por: Dirección de Estadísticas Sociodemográficas - DIES'.


*=====================================================================================.
* 2 TASAS DE LOS PRINCIPALES INDICADORES DE MERCADO LABORAL
*=====================================================================================.
weight by fexp.
ctables /vlabels var=  pobla15 t_a area  p02  
                               templeob templeog tadec tsub tsub_h tsub_w tnr toinad tnc tdesem tdesemab tdesemoc 
                               tpartig tpartib15 display=none 
            /table (pobla15[c]> (templeob[s][mean 'Empleo Bruto (%)'] +  
                                templeog[s][mean 'Empleo Global (%)'] + 
                                tadec[s][mean 'Empleo Adecuado/pleno (%)'] + 
                                tsub[s][mean 'Subempleo (%)'] + 
                                tsub_h[s][mean 'Subempleo por insuficiencia de tiempo de trabajo (%)'] + 
                                tsub_w[s][mean 'Subempleo por  insuficiencia de ingresos (%)'] +  
                                tnr[s][mean 'Empleo no remunerado (%)'] + 
                                toinad[s][mean 'Otro no pleno (%)'] + 
                                tnc[s][mean 'Empleo no clasificado (%)'] + 
                                tdesem[s][mean 'Desempleo (%)'] + 
                                tdesemab[s][mean 'Desempleo Abierto (%)'] +
                                tdesemoc[s][mean 'Desempleo Oculto (%)'] + 
                                tpartig[s][mean 'Participación Global (%)']) + 
                                tpartib15[s][mean 'Participación Bruta (%)']) by t_a[c]+AREA[c]+p02[c]
           /slabels position=row
           /titles title='Indicadores de Mercado Laboral (15 años y más)'
                    caption='Fuente: INEC - ENEMDU' 'Elaborado por: Dirección de Estadísticas Sociodemográficas - DIES'.

*=========================================================================================.
* 3.1.- CARACTERIZACIÓN DE LA POBLACIÓN CON EMPLEO
*=========================================================================================.

ctables /vlabels var = p02 claempl  asalind   gedad   nnivins etnia  formal informal empdom nocla  empleo  display = label
            /table p02[c][count colpct.count]  + claempl[c][count colpct.count]  + asalind[c][count colpct.count]   + gedad[c][count colpct.count] + nnivins[c][count colpct.count] +
                      etnia[c][count colpct.count]  + secto[c][count colpct.count]   + ht[mean]> p02[c]  BY empleo[c] >pobla15[c] 
            /categories variables = p02 order=a key=value empty=include  total=yes position=before
            /categories variables =  claempl asalind     gedad  nnivins etnia  secto   empleo  order=a key=value empty=include 
            /titles title='Caracterización de la Población Empleada'
                     caption='Fuente: INEC - ENEMDU' 'Elaborado por: Dirección de Estadísticas Sociodemográficas - DIES'.

*=========================================================================================.
* 3.2.- CARACTERIZACIÓN DE LA POBLACIÓN CON EMPLEO ADECUADO
*=========================================================================================.

ctables /vlabels var = p02  claempl asalind  gedad nnivins etnia  formal informal empdom nocla  adec  display = label
            /table p02[c][count colpct.count] + claempl[c][count colpct.count]  + asalind[c][count colpct.count]   + gedad[c][count colpct.count] + nnivins[c][count colpct.count]   + 
                     etnia[c][count colpct.count]  + secto[c][count colpct.count]    by adec[c] >pobla15[c] 
            /categories variables = p02 order=a key=value empty=include  total=yes position=before
            /categories variables =  claempl asalind  gedad  nnivins etnia secto   adec  order=a key=value empty=include 
            /titles title='Caracterización de la Población con Empleo Adecuado'
                     caption='Fuente: INEC - ENEMDU' 'Elaborado por: Dirección de Estadísticas Sociodemográficas - DIES'.

*=========================================================================================.
* 3.3.- CARACTERIZACIÓN DE LA POBLACIÓN SUBEMPLEADA
*=========================================================================================.

ctables /vlabels var = p02  claempl asalind  gedad  etnia  formal informal empdom nocla sub  display = label
            /table p02[c][count colpct.count] + claempl[c][count colpct.count]  + asalind[c][count colpct.count]   + gedad[c][count colpct.count]   + 
                     etnia[c][count colpct.count]  + secto[c][count colpct.count] by sub[c] >pobla15[c] 
            /categories variables = p02 order=a key=value empty=include total=yes position=before
            /categories variables = claempl asalind  gedad  etnia secto  sub  order=a key=value empty=include 
            /titles title='Caracterización de la Población Subempleada'
                     caption='Fuente: INEC - ENEMDU' 'Elaborado por: Dirección de Estadísticas Sociodemográficas - DIES'.

*=========================================================================================.
* 3.4.- CARACTERIZACIÓN DE LA POBLACIÓN CON OTRO EMPLEO NO PLENO
*=========================================================================================.

ctables /vlabels var = p02  claempl asalind  gedad  etnia  formal informal empdom nocla  oinad  display = label
            /table p02[c][count colpct.count] + claempl[c][count colpct.count]  + asalind[c][count colpct.count]   + gedad[c][count colpct.count]   + 
                     etnia[c][count colpct.count]  + secto[c][count colpct.count] by oinad[c] >pobla15[c] 
            /categories variables = p02 order=a key=value empty=include total=yes position=before
            /categories variables = claempl asalind  gedad  etnia secto  oinad  order=a key=value empty=include 
            /titles title='Caracterización de la Población con Otro empleo no pleno'
                     caption='Fuente: INEC - ENEMDU' 'Elaborado por: Dirección de Estadísticas Sociodemográficas - DIES'.

*=========================================================================================.
* 3.5.- CARACTERIZACIÓN DE LOS DESEMPLEADOS
*=========================================================================================.

ctables /vlabels var = p02 gedad desema desemb  desem display = label
            /table p02[c][count colpct.count]   + gedad[c][count colpct.count]  + desema[c][count colpct.count] + desemb[c][count colpct.count]  
                     by desem[c] >pobla15[c] 
            /categories variables = p02 order=a key=value empty=include total=yes position=before
            /categories variables = gedad desema desemb desem order=a key=value empty=include 
            /titles title='Caracterización de la Población Desempleada'
                     caption='Fuente: INEC - ENEMDU' 'Elaborado por: Dirección de Estadísticas Sociodemográficas - DIES'.
*=========================================================================================.
* 4.- SECTORIZACIÓN DE LOS EMPLEADOS
*=========================================================================================.

ctables /vlabels var = area secto display=none
            /table area > secto [colpct.count]
            /categories variables = area total = yes position =before
            /titles title='Sectorización de la Población Empleada'
                    caption='Fuente: INEC - ENEMDU' 'Elaborado por: Dirección de Estadísticas Sociodemográficas - DIES'.



