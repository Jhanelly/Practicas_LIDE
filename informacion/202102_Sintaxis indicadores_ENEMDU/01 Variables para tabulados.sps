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
*1- Ejecute esta sintaxis en la base de datos: enemdu_persona_ xxxx_xx

*==============================================================================*.
*VARIABLES PARA DE TRABAJO PARA GENERACIÓN DE TABULADOS DE MERCADO LABORAL
*==============================================================================*.


*=============================================.
compute t=1.
var lab t 'Poblacion Total'.
*=============================================.
*Generación de Indicadores de Mercado Laboral - Tasas.
*=============================================.
if( petn=1) templeob=0.
if (empleo=1) templeob=100.
if (pean=1) templeog=0.
if (empleo=1) templeog=100.
if (pean=1) tadec=0.
if (adec=1) tadec=100.
if( pean=1) tsub=0.
if (sub=1) tsub=100.
if( pean=1) tsub_h=0.
if (sub_h=1) tsub_h=100.
if( pean=1) tsub_w=0.
if (sub_w=1) tsub_w=100.
if ( pean=1) toinad=0.
if (oinad=1) toinad=100.
if( pean=1) tnr=0.
if (nr=1) tnr=100.
if( pean=1) tnc=0.
if (nc=1) tnc=100.
if( pean=1) tdesem=0.
if (desem=1) tdesem=100.
if( pean=1) tdesemab=0.
if (desemab=1) tdesemab=100.
if( pean=1) tdesemoc=0.
if (desemoc=1) tdesemoc=100.
if( pean=1) tdesem1=0.
if (desem1=1) tdesem1=100.
if( pean=1) tdesem2=0.
if (desem2=1) tdesem2=100.
if( t_a=1) tpartib15=0.
if (pean=1 and p03>=15) tpartib15=100.
if( petn=1) tpartig=0.
if (pean=1 and p03>=15) tpartig=100.
if( pean=1) tsubu=0.
if (sub=1 or desem=1) tsubu=100.
exe.

*============================. 
*Empleado público y privado.
*============================.
*Esta variable incluye a los trabajadores domésticos 
para la población de empleados provados.
RECODE P42 (1 = 1) (2 THRU 10=2) INTO claempl.
var label claempl 'Tipó de Empleador'.
value label claempl
1'Empleado Público'
2'Empleado Privado'.
var lev claempl (NOMINAL).
exe.

*============================. 
*.Asalariado e Independiente
*============================.
recode P42 (1 THRU 4= 1) (10= 1) (5 THRU 6 =2) INTO asalind.
var label asalind 'Tipo de Trabajo'.
value label asalind
1'Empleado Asalariado'
2'Empleado Independiente'.
var lev claempl (NOMINAL).
EXE.

*============================. 
*Nivel de instrucción.
*============================.
if(p10a=1) nnivins=1.
if(p10a=2) nnivins=2.
if(p10a=3 or p10a=4 or p10a=5 or p10a=6 and p10b<4) nnivins=3.
if(p10a=7 or p10a=8 or p10a=6 and p10b>3) nnivins=4.
if( p10a>=8 and p10a<=10) nnivins=5.

var label nnivins'nnivins. Nivel de instrucción'.
val label nnivins
1'Ninguno'
2'Centro de Alfabetización'
3'Educación Básica'
4'Educación Media/Bachillerato'
5'Superior'.

*============================.
*Horas de trabajo.
*============================.
recode p51a p51b (999=sysmis).
compute ht=sum(p51a,p51b).

*=======================.
*Grupos étnicos
*=======================.
recode p15(1=1) (2=2)(3=2)(4=2)(5=5) (6=3) (7=4) (8=6) into etnia.
var lab etnia 'Etnia'.
val lab etnia 1'Indigena' 2'Afroecuatoriano'   3'Mestizo/a' 4'Blanco' 5'Montuvio' 6'Otro'.
exe.
var lev etnia (NOMINAL).

*============================. 
*Horas Efectivas.
*============================.
recode p24(999=sysmis).
exe.

*=======================.
*Grupos de edad*.
*=======================.
recode p03 (15 thru 24=1) (25 thru 34=2) (35 thru 44=3) (45 thru 64=4) (65 thru HIGHEST=5) into gedad.
var lab gedad 'grupos de edad'.
val lab gedad
1'Poblacion 15-24 años'
2'Poblacion  25-34 años'
3'Poblacion de 35-44 años'
4'Poblacion de 45-64 años'
5'Poblacion de 65 años y más'.
exe.

*=======================================.
*Sectorización de la población empleada*.
*=======================================.
if formal=1 secto=1.
if informal=1 secto=2.
if empdom=1 secto=3.
if nocla=1 secto=4.
var lab secto 'Sectorizacion de la PEA'.
val  lab secto
1'Población con empleo en el sector formal'
2'Población con empleo en el sector informal'
3'Población con empleo Doméstico'
4'Población con empleo no clasificado por sector'.
exe.

*=========================.
*Sector Informal.
*=========================.

if empleo=1 tinformal=0.
if informal=1 tinformal=100.
exe.

*=========================.
*Filtros.
*=========================.
IF ((area=1 or area=2 ) ) fil00=1.
if area=1   fil01=1.
if area=2  fil02=1.
exe.
var lab fil00'Nacional'
/fil01'Urbano' 
/fil02'Rural'.
*=========================.
*Sectorización de la PEA.
*=========================.

if formal=1 secto=1.
if informal=1 secto=2.
if empdom=1 secto=3.
if nocla=1 secto=4.
var lab secto 'sectorizacion pean'.
val lab secto
1'Población con empleo en el sector formal'
2'Población con empleo en el sector informal'
3'Población con empleo Doméstico'
4'Población con empleo no clasificado por sector'.
exe.

*=========================.
*Caracterización de los desempleados.
*=========================.
if desem1=1 desemb=1.
if desem2=1 desemb=2.
if desemab=1 desema=1.
if desemoc=1 desema=2.
exe.
var lab desema 'Según busqueda de empleo'.
var lab desemb 'Según experiencia previa'.
val lab desemb 1'Cesante' 2'Nuevo'.
val lab desema 1'Desempleo abierto' 2'Desempleo oculto'.


