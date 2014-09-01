**Datos analizados:**
Los datos consisten en los resultados en las calificaciones (nº de no presentados, suspensos, aprobados, notables, sobresalientes y matrículas) de todos los grupos de la asignatura de Fundamentos de Tecnología de Computadores durante los cursos 9/10, 10/11, 11/12, 12/13, 13/14
136 observaciones de 14 variables

Variables:
"AÑO"        "CONVOCATORIA" "PLAN"         "NOM.PLAN"     "ASIG"         "NOM.ASIG"    
 "CALIFIC"      "DEPTO"        "NOM.DEPTO"    "TOTAL.CALIF"  "GRUPO_ASSIG"  "PROFESOR"    
"USOVIDEOS"    "CALIFIC.NUM"

**Observaciones:**
En los años 9/10, 10/11, 11/12, 12/13 no se distingue entre los grupos mañana y tarde, solo por Grados.
En el año 9/10 No hubo grupo de G59
Ha sido necesario para distinguir dentro de los grupos mixtos G58 y G59 quienes han sido alumnos del Profesor 1 y  quienes del Profesor 2 por lo que se ha incluido la variable GRUPO ASSIG

**TOTAL-CALIF**
Representa el número de alumnos que ha obtenido la nota especificada en  TOTAL-CALIF
Equivalente numérico de la escala 0-10 ponderada: 
SUSPENSO = 2,50
APROBADO = 5,5 
NOTABLE = 7,5 
SOBRESALIENTE = 9,0 
MATRÍCULA DE HONOR = 10 
NO PRESENTADO = NP
En el caso de "NO PRESENTADO" es difícil asignar un valor numérico, pero se ha considerado que sí debe ser tenido en cuenta y que es el caso menos deseado, Hemos asignado el valor de 1.

**PLAN**
G58 = Grado Sistemas de Información
G59 = Grado Ing. de Computadores
 G780 = Grado de Ing. Informática.

**GRUPO ASSIG**
1	SisMañana
2	SisTarde
3	Ing.Comp
4	InfMañana
5	InfTarde
6	Inglés
7	G5859_Octubre
0	En los datos no están separados los grupos de mañana y de tarde

**PROFESOR**
1	Profesor 1
2	Profesor 2
3	Profesor 3.
12	Profesores 1 y 2 conjuntamente ya que no se puede distinguir profesor debido a que en los datos no están separados los grupos de  mañana y tarde.

**USOVIDEOS**
0	No
1	Si
