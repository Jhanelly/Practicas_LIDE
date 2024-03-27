* Encoding: UTF-8.
* Encoding: .
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
* Fecha última modificación:   Febrero - 2021				   
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
*1- Descargue la Bases de datos de PERSONAS de la página web: http://www.ecuadorencifras.gob.ec/

*2- Guarde la base y copie el directorio en:.
*get file = 'C:\....\enemdu_persona_xxxx_xx.sav'.
*==============================================================================*.
*                            CONSTRUCCIÓN DE VARIABLES DE MERCADO LABORAL                                       *
*==============================================================================*.

compute t_a=1.
* Crea indicadores.
    if (p03<15) condact=0.
    recode p03 ( 15 thru 99= 1) (else=0) into petn.
    recode condact (1 thru 8 = 1) (else=0) into pean.
    recode condact ( 1 thru 6= 1) (else=0) into empleo.
    recode condact ( 1 = 1) (else=0) into adec.
    recode condact ( 2 thru 3= 1) (else=0) into sub.
    recode condact ( 2 = 1) (else=0) into sub_h.
    recode condact ( 3 = 1) (else=0) into sub_w.
    recode condact ( 4 = 1) (else=0) into oinad.
    recode condact ( 5 = 1) (else=0) into nr.
    recode condact ( 6 = 1) (else=0) into nc.
    recode condact ( 7 thru 8 = 1) (else=0) into desem.
    recode condact ( 7 = 1) (else=0) into desemab.
    recode condact ( 8 = 1) (else=0) into desemoc.
    compute desem1=0.
    if ((condact >= 7 & condact <= 8) & p37=1)  desem1=1.
    compute desem2=0.
    if ((condact >= 7 & condact <= 8) & p37=2) desem2=1.
    recode condact ( 9 = 1) (else=0) into pein.
exe.

*==============================================================================*.
*DESAGREGACIÓN DE LA SECEMP.
*==============================================================================*.

    if (secemp=1 and p03>=15) formal=1.    /*Población con empleo en el sector formal.
    if (secemp=2 and p03>=15) informal=1.  /*Población con empleo en el sector informal.
    if (secemp=3 and p03>=15) empdom=1. /*Población con empleo Doméstico.
    if (secemp=4 and p03>=15) nocla=1.      /* 'Población con empleo no clasificado por sector.
exe.
    recode petn pean empleo adec sub sub_h sub_w oinad nr nc desem desemab desemoc desem1 desem2 pein nocla formal informal empdom (0=sysmis).
exe.
*==============================================================================*.
*ETIQUETAS DE LAS VARIABLES DE MERCADO LABORAL.
*==============================================================================*.
    value labels petn 1'Población en Edad de Trabajar'.
    value labels pein 1'Población Económicamente Inactiva'.
    value labels pean 1'Población Económicamente Activa'.
    value labels empleo 1'Población con Empleo'.
    value labels adec 1'Empleo Adecuado/Pleno'.
    value labels sub 1'Subempleo'.
    value labels sub_h 1'Subempleo por insuficiencia de tiempo de trabajo'.
    value labels sub_w 1'Subempleo por insuficiencia de ingresos'.
    value labels oinad 1'Otro empleo no pleno'.
    value labels nr 1'Empleo no remunerado'.
    value labels nc 1'Empleo no clasificado'.
    value labels desem 1'Desempleo'.
    value labels desemab 1'Desempleo abierto'. 
    value labels desemoc 1'Desempleo oculto'.
    value labels desem1 1'Desempleo cesante'.
    value labels desem2 1'Desempleo nuevo'.
    value labels t_a 1'Población total'.
    value labels nocla 1'Empleo no clasificado por sector'.

   var lev  petn pean empleo adec sub sub_h sub_w oinad nr nc desem desemab desemoc desem1 desem2 pein nocla formal informal empdom (nominal).

* Etiquetas de la variable.
    var lab fexp 'Factor de expansión'
    /petn 'Población en Edad de Trabajar (PET)'
    /pean 'Población Económicamente Activa (PEA)'
    /empleo 'Empleo'
    /adec 'Empleo Adecuado/Pleno'
    /sub 'Subempleo'
    /sub_h 'Subempleo por insuficiencia de tiempo de trabajo'
    /sub_w 'Subempleo por insuficiencia de ingresos'
				/oinad 'Otro empleo no pleno'
    /nr 'Empleo no remunerado'
    /nc 'Empleo no clasificado'
    /Desem 'Desempleo'
    /Desem1 'Desempleo cesante'
    /Desem2 'Desempleo nuevo'
    /desemab 'Desempleo abierto'
    /desemoc 'Desempleo oculto'
    /pein 'Población Económicamente Inactiva (PEI)'
    /formal  'Población con empleo en el sector formal'
    /informal 'Población con empleo en el sector informal'
    /empdom 'Población con empleo Doméstico'
    /nocla 'Población con empleo no clasificado por sector'
    /t_a 'Población Total'.
exe.
*=======================.
*GRUPOS DE EDAD*.
*=======================.
if p03<10 menor10=1.
if p03>=10 pobla10=1.
if p03>=15 pobla15=1.
if (p03 >= 15 and p03<=24) pobla24 = 1.
if (p03 >= 25 and p03 <= 34) pobla34 = 1.
if (p03 >= 35 and p03 <= 44) pobla44 = 1.
if (p03 >= 45 and p03 <= 64 ) pobla64 = 1.
if (p03 >=65) pobla65 = 1.
if p03<15 menor15=1.
exe.
val lab menor10 1 'Población menor a 10 años'.
val lab menor15 1 'Población menor a 15 años'.
val lab pobla10 1 'Población mayor o igual a 10 años'.
val lab pobla15 1 'Población mayor o igual a 15 años'.
       
val lab pobla15 1'Poblacion de 15 años y más'.
val lab pobla10 1'Poblacion de 10 años y más'.
val lab pobla24 1'Poblacion 15-24 años'. 
val lab pobla34 1'Poblacion  25-34 años'.
val lab pobla44 1'Poblacion de 35-44 años'.
val lab pobla64 1'Poblacion de 45-64 años'.
val lab pobla65 1'Poblacion de 65 años y más'.
val lab menor15 1'Poblacion menor de 15 años'.
val lab menor10 1'Población menor a 10 años'.
exe.
 var level menor10 menor15 pobla10 pobla15 (nominal).


